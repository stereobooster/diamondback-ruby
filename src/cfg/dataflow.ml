
open Printf
open Cfg
open Utils

let rec exists_fp visited stmt exits = 
  if StmtSet.is_empty stmt.succs
  then StmtSet.add stmt exits
  else
    let todo = StmtSet.diff stmt.succs visited in
    let visited' = StmtSet.union visited todo in
      StmtSet.fold
	(fun stmt exits ->
	  exists_fp
	    visited'
	    stmt
	    exits
	) todo exits
      
let exits stmt = exists_fp StmtSet.empty stmt StmtSet.empty

module type DataFlowProblem = 
sig
  type t
  val top : t
  val eq : t -> t -> bool
  val to_string : t -> string

  val transfer : t -> stmt -> t
  val meet : t list -> t
end

module DataFlowVisitor(DFP : DataFlowProblem) = struct

  open Visitor

  class dataFlowVisitor fixpoint stmt = 
    let inf,outf = fixpoint stmt in
  object(self)
    inherit default_visitor as super
    val in_facts = inf
    val out_facts = outf

    method visit_super_stmt stmt = super#visit_stmt stmt

    method visit_stmt stmt = match stmt.snode with
      | Begin body
      | End body
      | Class(_,_,body)
      | Module(_,_,body)
      | Method(_,_,body) -> 
          let in', out' = fixpoint body in
          let me = {<in_facts = in'; out_facts = out'>} in
            me#visit_super_stmt stmt
          
      | _ -> DoChildren
  end
    
end

module Forwards(DFP : DataFlowProblem) : sig
  val fixpoint : Cfg.stmt -> DFP.t ->
    (Cfg.stmt, DFP.t) Hashtbl.t * (Cfg.stmt, DFP.t) Hashtbl.t  
end = struct
    
  let fixpoint stmt init = 
    let in_tbl = Hashtbl.create 127 in
    let out_tbl =  Hashtbl.create 127 in
    let q = Queue.create () in
      Queue.push stmt q;
      Hashtbl.add in_tbl stmt init;
      while not (Queue.is_empty q) do
	let stmt = Queue.pop q in
	let in_list = 
	  StmtSet.fold
	    (fun pred acc ->
	      try (Hashtbl.find out_tbl pred) :: acc
	      with Not_found -> 
		Hashtbl.add out_tbl pred DFP.top;
		DFP.top :: acc
	    ) stmt.preds [init]
	in
       let in_facts = DFP.meet in_list in
       let () = Hashtbl.replace in_tbl stmt in_facts in
       let new_facts = DFP.transfer in_facts stmt in
	  try 
	    let old_facts = Hashtbl.find out_tbl stmt in
	      if DFP.eq old_facts new_facts
	      then ()
	      else begin 
		  StmtSet.iter (fun x -> Queue.push x q) stmt.succs;
		  Hashtbl.replace out_tbl stmt new_facts
		end
	  with Not_found ->
	    StmtSet.iter (fun x -> Queue.push x q) stmt.succs;
	    Hashtbl.replace out_tbl stmt new_facts
      done;
      in_tbl, out_tbl

end

module Backwards(DFP : DataFlowProblem) : sig
  val fixpoint : Cfg.stmt -> DFP.t ->
    (Cfg.stmt, DFP.t) Hashtbl.t * (Cfg.stmt, DFP.t) Hashtbl.t  
end = struct    
  let fixpoint stmt init = 
    let in_tbl = Hashtbl.create 127 in
    let out_tbl =  Hashtbl.create 127 in
    let q = Queue.create () in
      StmtSet.iter
	(fun x -> 
	  Queue.push x q;
	  Hashtbl.add in_tbl x init
	) (exits stmt);
      while not (Queue.is_empty q) do
	let stmt = Queue.pop q in
	let in_list = 
	  StmtSet.fold
	    (fun stmt acc ->
	      try (Hashtbl.find out_tbl stmt) :: acc
	      with Not_found -> 
		Hashtbl.add out_tbl stmt DFP.top;
		DFP.top :: acc
	    ) stmt.succs [init]
	in
       let in_facts = DFP.meet in_list in
       let () = Hashtbl.replace in_tbl stmt in_facts in
       let new_facts = DFP.transfer in_facts stmt in
	  try 
	    let old_facts = Hashtbl.find out_tbl stmt in
	      if DFP.eq old_facts new_facts
	      then ()
	      else begin 
		  StmtSet.iter (fun x -> Queue.push x q) stmt.preds;
		  Hashtbl.replace out_tbl stmt new_facts
		end
	  with Not_found ->
	    StmtSet.iter (fun x -> Queue.push x q) stmt.preds;
	    Hashtbl.replace out_tbl stmt new_facts
      done;
      in_tbl, out_tbl

end

