
open Cfg
open Cfg_printer.CodePrinter
open Printf
open Utils

type scope = 
  | Absolute_Scope of string list
  | Relative_Scope of string list

type links = scope list 

type 'a t = 
  | Root of 'a t StrMap.t * links
  | Node of 'a t StrMap.t * links * 'a 

type 'a ctx = 
  | Top
  | Path of 'a ctx * string * 'a t
  | Link of 'a ctx * scope * 'a ctx  (* src parent, scope, target parent *)

type 'a cursor = 'a ctx * 'a t

let empty = Root(StrMap.empty,[])

let to_cursor t = (Top,t)

let up_scope = Relative_Scope [".."]

let root = Absolute_Scope []

let at_root = function (Top,_) -> true | _ -> false

let rec string_of_scope s = 
  let rec string_of_list = function
    | [] -> ""
    | hd::[] -> hd
    | hd::tl -> hd ^ "::" ^ string_of_list tl
  in
    match s with
      | Absolute_Scope l -> "::" ^ string_of_list l
      | Relative_Scope l -> string_of_list l
	
let abs_scope (c,_) = 
  let rec helper acc = function
    | Top -> acc
    | Path(ctx,name,_) -> helper (name :: acc) ctx
    | Link(src_ctx,sc,targ_ctx) -> helper acc targ_ctx
  in
    helper [] c

let current_scope c = Absolute_Scope (abs_scope c)

let append_scope scope str = match scope with
  | Absolute_Scope l -> Absolute_Scope (List.append l [str])
  | Relative_Scope l -> Relative_Scope (List.append l [str])

let absolute_scope scope curs = match scope with
  | Absolute_Scope _ -> scope
  | Relative_Scope l -> 
      let lr = match current_scope curs with
	| Absolute_Scope l -> List.rev l
	| _ -> assert false
      in
      let rec helper acc = function
	| [] -> acc
	| ".."::tl -> helper (List.tl acc) tl
	| hd::tl -> helper (hd::acc) tl
      in
	Absolute_Scope (List.rev (helper lr l))

let dump_children = function
  | _,Root(m,links) | _,Node(m,links,_) ->
      StrMap.fold (fun k _ acc -> sprintf "%s\n%s" acc k) m "" 

let string_of_curs c = string_of_scope (current_scope c)

let rec enter_scope_ name curs been_theres = 
match curs with
  | ctx, (Root(bind_map,inc_lst) as t) 
  | ctx, (Node(bind_map,inc_lst,_) as t) ->
      begin try
	let t' = StrMap.find name bind_map in
	let new_ctx = Path(ctx,name,t) in
	  new_ctx, t'
      with Not_found -> enter_scope_link name curs inc_lst been_theres
      end

and enter_scope_link name ((ctx,t) as curs) inc_list been_theres = 
  let cur_scope = current_scope curs in
    try_each
      (fun inc ->
	 (* Avoid revisiting cycles in the scope graph *)
	 let abs_inc = absolute_scope inc curs in
	 let () = if List.mem abs_inc been_theres then raise Not_found in
	 let been_theres = abs_inc::been_theres in
	 let (ctx',t') = move_to_scope_ inc curs been_theres in
	   enter_scope_ name (Link(ctx,cur_scope,ctx'),t') been_theres
      ) inc_list

and move_up_ (c,t) been_theres = match c with
  | Top -> failwith "namespace:move_up at top"
  | Path(ctx,name,parent) -> begin match parent with
      | Root(m,l) -> ctx, Root (StrMap.add name t m,l)
      | Node(m,l,d) -> ctx, Node (StrMap.add name t m,l,d)
    end
  | Link(ctx,scope,ctx') -> move_to_scope_ scope (ctx',t) been_theres

and leave_scope_ c bt = move_up_ c bt

and move_to_scope_ s curs been_theres = 
  (*Printf.eprintf "moving to %s from %s\n%!"
    (string_of_scope s) (string_of_scope (current_scope curs));*)
  let rec helper scope curs = match scope with
    | [] -> curs
    | ".."::tl ->  helper tl (leave_scope_ curs been_theres)
    | hd::tl -> helper tl (enter_scope_ hd curs been_theres)
  in
    match s with
      | Absolute_Scope sl -> helper sl (move_to_root curs)
      | Relative_Scope sl -> 
	  try helper sl curs
	  with Not_found -> 
	    if at_root curs then raise Not_found
	    else move_to_scope_ s (move_up_ curs been_theres) been_theres

and from_cursor ((ctx,t) as c) = match ctx with
  | Top -> t
  | _ -> from_cursor (move_up_ c [])

and move_to_root curs = to_cursor (from_cursor curs)

let enter_scope s curs = enter_scope_ s curs []
let move_up curs = move_up_ curs []
let move_to_scope s curs = move_to_scope_ s curs []
let leave_scope c = leave_scope_ c []

let add_new_subtree name subt t = match t with
  | Node(m,l,d) -> 
      if StrMap.mem name m then t
      else Node (StrMap.add name subt m,l,d)
  | Root(m,l) -> 
      if StrMap.mem name m then t
      else Root(StrMap.add name subt m,l)

let add_cursor_subtree name subt ((c,t) as curs) = 
  try 
    let t' = add_new_subtree name subt t in
      if t' == t then curs else c,t'
  with Failure _ ->
    Log.fatal Log.empty "unable to add subtree %s at scope %s"
      name (string_of_scope (current_scope curs))
      
let add binding value curs = 
  add_cursor_subtree binding (Node(StrMap.empty,[],value)) curs

let add_link scope ((c,t) as curs) = 
  let scope = absolute_scope scope curs in
  let this_scope = current_scope curs in
    if scope = this_scope then curs
    else begin
      (*Printf.eprintf "add link to %s from %s\n"
	(string_of_scope scope) (string_of_scope (current_scope curs));
      *)
      match t with
	| Root(m,l) -> if List.mem scope l then curs else c,Root(m,scope::l)
	| Node(m,l,d) -> if List.mem scope l then curs else c,Node(m,scope::l,d)
    end

let replace b v (c,t) = match t with
  | Root(m,l) -> c, Root(StrMap.add b (Node(StrMap.empty,[],v)) m, l)
  | Node(m,l,nv) -> c, Node(StrMap.add b (Node(StrMap.empty,[],v)) m, l, nv)

let value_at = function
  | Node(_,_,v) -> v
  | Root _ -> failwith "value_at"

let find_local s (c,t) = 
  let rec helper seen ((c,t) as curs) = 
    match t with
      | Root(m,links)
      | Node(m,links,_) -> 
	  try Some (value_at (StrMap.find s m))
	  with Not_found -> 
	    try try_each 
	      (fun lnk_scope ->
		 let abs = absolute_scope lnk_scope curs in
		   if List.mem abs seen then raise Not_found;
		   helper (abs::seen) (move_to_scope lnk_scope curs)
	      ) links
	    with Not_found -> None
  in helper [] (c,t)

let rec mem_local s curs = 
  match find_local s curs with
    | Some _ -> true 
    | None -> false

let rec find s ((c,t) as curs) = 
  match find_local s curs with
    | None ->
	if at_root curs then None
	else find s (move_up curs)
    | x -> x

let rec lookup_scope scope curs = current_scope (move_to_scope scope curs)

let mem s curs = match find s curs with
  | Some _ -> true
  | None -> false

let current_val (c,t) = 
  try value_at t
  with Failure _ -> failwith "current_val"

let (++) f g = g f

let do_in_scope f s c = 
  let orig_s = current_scope c in
    move_to_scope s c ++ f ++ move_to_scope orig_s

let fold_in_scope f acc s curs = 
  let orig_s = current_scope curs in
  let curs = move_to_scope s curs in
  let (acc,curs) = f acc curs in
    acc, (move_to_scope orig_s curs)

let scope_of_identifier e = 
  let rec helper = function
    | `ID_UScope id1 -> ["";id1]
    | `ID_Scope(id1,id2) ->
	(helper id1) @ [id2]
    | id -> [format_to_string format_identifier id]
  in match helper e with
    | ""::tl -> Absolute_Scope tl
    | tl -> Relative_Scope tl
	
let scope_of_identifier_split e = 
  (* flatten the list of scoped identifiers, saving the right most
     element separately.  Also, the head of the list is "" if this is an
     absolute path *)
  let rec helper (x:identifier) : string list * string = match x with
    | `ID_UScope id1 -> [""], id1
    | `ID_Scope(id1,id2) -> 
	let l,last = helper id1 in
	  l @ [last], id2
    | id -> [],(format_to_string format_identifier id)
  in
    match helper e with
      | ""::tl, last -> Absolute_Scope tl, last
      | tl, last -> Relative_Scope tl, last
	  
