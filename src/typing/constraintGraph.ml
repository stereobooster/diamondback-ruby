open Utils
open Config

let dbg = env_is_set "DRUBY_DEBUG_CONSTRAINTS"

let ep = Printf.eprintf

(*type d = {mutable debug_constraints : bool}
let conf = {debug_constraints = dbg}*)

module MyInt64 = struct
  include Int64
  let equal x y = compare x y = 0
  let hash = Hashtbl.hash
end

module Key = Int

module type Edge = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string

  val src : t -> Key.t
  val dst : t -> Key.t

  val solve : t -> unit
  val close : t -> t -> unit
  val check_remaining_unsat: t -> unit
  val collapse : t -> unit
end

module type S = sig
  type t
  type vertex = Key.t
  type edge
  val create : unit -> t

  val add_edge : t -> edge -> unit
  val mem_edge : t -> vertex -> vertex -> bool
  val requeue : t -> vertex -> unit
  val succs : t -> vertex -> edge list
  val preds : t -> vertex -> edge list

  val fixpoint_dependency : t -> vertex -> vertex -> unit

  val solve_constraints : t -> unit
  val union_vars : t -> 'a Variable.t -> 'a Variable.t -> unit
end

module Make(E : Edge) : S with type edge=E.t = struct
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Key)
  module GPath = Graph.Path.Check(G)
  module Scc = Graph.Components.Make(G)
  module Dfs = Graph.Traverse.Dfs(G)
  module EdgeSet = Set.Make(E)
  module DepMap = Map.Make(Key)

  type vertex = Key.t
  type edge = E.t

  type t = {
    mutable pending : EdgeSet.t;
    mutable dynamic_close : bool;
    extra_deps : G.t;
    checker : GPath.path_checker;
    edges : ((vertex*vertex),E.t) Hashtbl.t;
    graph : G.t;
  }
      
  let create () = 
    let g = G.create () in
      {pending = EdgeSet.empty;
       dynamic_close = false;
       extra_deps = G.create();
       checker = GPath.create g;
       edges = Hashtbl.create 127;
       graph = g;
    }

  let fixpoint_dependency t src dst = G.add_edge t.extra_deps src dst

  let mem_edge t l r = G.mem_edge t.graph l r

  let succs t v = 
    if G.mem_vertex t.graph v
    then List.map (fun sv -> Hashtbl.find t.edges (v,sv)) (G.succ t.graph v)
    else []

  let preds t v = 
    if G.mem_vertex t.graph v 
    then List.map (fun pv -> Hashtbl.find t.edges (pv,v)) (G.pred t.graph v)
    else []

  let requeue cg v = 
    if G.mem_vertex cg.graph v
    then
      let set = 
	List.fold_left
	  (fun acc succv ->
	     let e = Hashtbl.find cg.edges (v,succv) in
	       EdgeSet.add e acc
	  ) cg.pending (G.succ cg.graph v)
      in
      let set = 
	List.fold_left
	  (fun acc predv ->
	     let e = Hashtbl.find cg.edges (predv,v) in
	       EdgeSet.add e acc
	  ) set (G.pred cg.graph v)
      in
	cg.pending <- set
    else
      ()


  module Topological = struct
  (* This module is copied from the ocamlgraph library to fix
     non-tail recursive behavior in version 0.99c
     Copyright: 
     (C) 2004-2008 Sylvain Conchon <Sylvain.Conchon@lri.fr>
     (C) 2004-2008 Jean-Christophe Filliâtre <Jean-Christophe.Filliatre@lri.fr>
     (C) 2004-2008 Julien Signoles <Julien.Signoles@lri.fr>
     License: LGPL-2 + exception
    *)
    module H = Hashtbl.Make(G.V)

    let fold f g acc =
      let degree = H.create 997 in
      let todo = Queue.create () in
      let rec walk acc = 
        if Queue.is_empty todo then acc
        else 
          let v = Queue.pop todo in
	  let acc = f v acc in
	    G.iter_succ 
	      (fun x-> let d = H.find degree x in
	         if d=1 then Queue.push x todo
	         else H.replace degree x (d-1))
	      g v; 
	    walk acc
      in
        G.iter_vertex 
          (fun v -> 
	     let d = G.in_degree g v in 
	       if d = 0 then Queue.push v todo
	       else H.add degree v d)
          g;
        walk acc

    let iter f g = fold (fun v () -> f v) g ()

  end

  (*module Topological = Graph.Topological.Make(G)  *)

  let build_scc t = 
    try
      let s = Unix.gettimeofday() in
      let scc_list = Scc.scc_list t.graph in
      let e = Unix.gettimeofday() in
	if conf.debug_constraints
	then ep "build scc took: %f\n%!" (e -. s);
	scc_list
    with Not_found -> Log.fatal Log.empty "scc raised notfound"

  let build_scc_counter = ref 0.0
  let build_scc t = timef build_scc_counter build_scc t

  let get_eset tbl v = 
    try Hashtbl.find tbl v
    with Not_found -> 
      Hashtbl.add tbl v EdgeSet.empty;
      EdgeSet.empty

  let add_closures t v edge_v edges = 
    EdgeSet.iter (fun e -> if e != edge_v then E.close e edge_v) edges

  let close_dag t = 
    let f v = 
      let succs = G.succ t.graph v in
      let preds = G.pred t.graph v in
        List.iter 
          (fun p ->
	     let p_e = Hashtbl.find t.edges (p,v) in
               List.iter
                 (fun s ->
                    let s_e = Hashtbl.find t.edges (v,s) in
                      E.close p_e s_e
                 ) succs
          ) preds
    in
      Topological.iter f t.graph
	
  let dump_dot ?(suffix="") t = 
    let oc = open_out ("constraints.dot" ^ suffix) in
      output_string oc "strict digraph constraint_graph {\n";
      output_string oc " size=\"14,10\"\n";
      G.iter_edges (fun x y -> Printf.fprintf oc "%d -> %d\n" x y) t.graph;
      output_string oc "}\n";
      close_out oc

  (* Takes a list of vertices and returns the list of every edge
     between those vertices.  O(n^2), but we expect cycles in the
     constraint graph to be relatively short *)
  let scc_edges t v_list = 
    let rec work acc = function
      | [] -> acc
      | x::rest ->
          let acc = List.fold_left
            (fun acc y ->
               let acc = try (Hashtbl.find t.edges (x,y))::acc
               with Not_found -> acc
               in
                 try (Hashtbl.find t.edges (y,x))::acc
                 with Not_found -> acc
            ) acc rest
          in work acc rest
    in work [] v_list




  let collapse_edge t e = 
    let lhs = E.src e in
    let rhs = E.dst e in 
      if lhs != rhs (* skip self loops *)
      then begin
        E.collapse e;
	if (E.src e) != (E.dst e)
	then Log.fatal Log.empty "edge failed to collapse: %s" (E.to_string e);
        let removed = if lhs == (E.src e) then rhs else lhs in
          Hashtbl.remove t.edges (lhs,rhs);
          G.remove_vertex t.graph removed;
          
      end else Hashtbl.remove t.edges (lhs,rhs) (* self edge *)

  (* collapse a strongly connected component into a single vertex *)      
  let rec collapse_scc t lst = 
    let lst = List.filter (G.mem_vertex t.graph) lst in
    let edges = scc_edges t lst in
      List.iter (fun e -> collapse_edge t e) edges

  let close_graph_scc t =
    if conf.debug_constraints then ep "closing graph\n%!";
    let s0 = Unix.gettimeofday() in
    let scc_list = build_scc t in
      List.iter (fun z -> collapse_scc t z) scc_list;
      let s = Unix.gettimeofday() in
        if conf.debug_constraints then ep "collapse graph took %f\n%!" (s -. s0);
        close_dag t;
	if conf.debug_constraints
	then ep "close dag took: %f\n%!" ((Unix.gettimeofday()) -. s)

  let add_edge t e = 
    let l = E.src e in
    let r = E.dst e in
      if l = r 
      then () (* don't add self loops *)

      else if G.mem_edge t.graph l r (* already present, adding would be no-op *)
      then assert(Hashtbl.mem t.edges (l,r))

      else begin
	Hashtbl.add t.edges (l,r) e;
	G.add_edge t.graph l r;
	t.pending <- EdgeSet.add e t.pending;
      end

  let solve_pending' t pend = 
    if conf.debug_constraints then begin
      let total = G.nb_vertex t.graph in
	ep "pending: %d (%d)\n%!" (EdgeSet.cardinal pend) total;
    end;
    let s = Unix.gettimeofday() in
      EdgeSet.iter
        (fun e -> 
           E.solve e;
           let dst = E.dst e in
             if G.mem_vertex t.extra_deps dst 
             then List.iter (requeue t) (G.succ t.extra_deps dst)
        ) pend;
      if conf.debug_constraints
      then ep "solve pending took: %f\n%!" ((Unix.gettimeofday()) -. s)

  let solve_pending_counter = ref 0.0;;
  let solve_pending t pend = timef solve_pending_counter (solve_pending' t) pend

  let check_unsatisfied t = 
    let s = Unix.gettimeofday() in
      G.iter_edges
        (fun src dst ->
	   try  E.check_remaining_unsat (Hashtbl.find t.edges (src,dst))
	   with Not_found -> ()
        ) t.graph;
	if conf.debug_constraints
	then ep "check unsat took: %f\n%!" ((Unix.gettimeofday()) -. s)

  let solve_constraints t = 
    let dot_iter = ref 0 in
    let rec work () = 
      incr dot_iter;
      if conf.debug_constraints then ep "iter %d\n%!" !dot_iter;
      let pend = t.pending in
	t.pending <- EdgeSet.empty;
	solve_pending t pend;

	if EdgeSet.is_empty t.pending
	then begin
          close_graph_scc t;
	  if EdgeSet.is_empty t.pending
          then ()
          else work ()
        end else work ()
    in
      close_graph_scc t;
      work ();
      check_unsatisfied t;
      (*if conf.debug_constraints then dump_dot ~suffix:"" t;*)
      ()

  let change_state stbl ~orig_l ~orig_r ~new_l ~new_r = 
    try
      let e = Hashtbl.find stbl (orig_l,orig_r) in
	assert ((E.src e) == new_l);
	assert ((E.dst e) == new_r);
	Hashtbl.remove stbl (orig_l,orig_r);
        if not (Hashtbl.mem stbl (new_l,new_r))
	then Hashtbl.add stbl (new_l,new_r) e;
        (*else Printf.eprintf "skip!\n";*)
	e
    with Not_found ->
      Log.fatal Log.empty "change_state: WTF"

  let fix_deps t old new_ = 
    if G.mem_vertex t.extra_deps old
    then
      let forws = G.succ t.extra_deps old in
      let backs = G.pred t.extra_deps old in
        G.remove_vertex t.extra_deps old;
        List.iter (fun v -> G.add_edge t.extra_deps new_ v) forws;
        List.iter (fun v -> G.add_edge t.extra_deps v new_) backs
        
  let union_vars cg v1 v2 = 
    let id1 = Variable.vid v1 in
    let id2 = Variable.vid v2 in
      if conf.debug_constraints then Log.fixme "union %d => %d" id1 id2;
      if Variable.same v1 v2
      then ()
      else if not (G.mem_vertex cg.graph id1)
      then Variable.union v1 v2
      else begin
	let forws = G.succ cg.graph id1 in
	let backs = G.pred cg.graph id1 in
	  Variable.union v1 v2;
          fix_deps cg id1 id2;
	  G.remove_vertex cg.graph id1;
	  List.iter 
	    (fun v -> 
	       let e = change_state cg.edges ~orig_l:id1 ~orig_r:v ~new_l:id2 ~new_r:v in
		 add_edge cg e
	    ) forws;
	  List.iter
	    (fun v -> 
	       let e = change_state cg.edges ~orig_l:v ~orig_r:id1 ~new_l:v ~new_r:id2 in
		 add_edge cg e
	    ) backs;
	  requeue cg id2
      end

end
