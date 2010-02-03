

type 'a t = TypeVar of 'a * 'a t option ref

let create t = TypeVar(t,ref None)

let current t = match t with TypeVar(c,_) -> c

let rec ecr t = match t with TypeVar(r,opt) ->
  match !opt with
    | None -> t
    | Some t2 -> 
        let root = ecr t2 (* path compression *)
        in opt := Some root;root
                  
let value t = current (ecr t)

(* point t1 to t2 *)
let union t1 t2 = 
  let e1 = ecr t1 in
  let e2 = ecr t2 in
    if e1 == e2 
    then failwith "cowardly refusing to create a unification cycle"
    else 
      match e1 with
	| TypeVar(_,n) -> n := Some e2;e2


(* leaf -> root fold of the tree *)
let rec fold func t acc = 
  match t with TypeVar(e,next) -> match !next with
    | None -> func e acc
    | Some t2 -> fold func t2 (func e acc)
