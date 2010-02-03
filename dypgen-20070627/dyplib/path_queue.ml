type 'a path_queue = Empty | Pq of ('a * ('a path_queue) * ('a path_queue))



let empty = (Empty,0)


let heap_min compare pq =
  let rec aux pq cont = match pq with
    
    | Empty | Pq(_,Empty,Empty) -> cont pq
    
    | Pq (a,Pq (b,Empty,Empty),Empty) ->
        let c = compare a b in
        if c = -1 then cont pq
        else if c = 1 then cont (Pq (b,Pq (a,Empty,Empty),Empty))
        else failwith "heap_min"
        
    | Pq (a, ((Pq (b,b1,b2)) as pq1), ((Pq (c,c1,c2)) as pq2)) ->
        let d = compare a b in
        if d=1 then
          let cont1 r = cont (Pq (b, r, pq2)) in
          aux (Pq (a,b1,b2)) cont1
        else
        let e = compare a c in
        if e=1 then
          let cont1 r = cont (Pq (c, pq1, r)) in
          aux (Pq (a,c1,c2)) cont1
        else if d= -1 && e= -1 then cont pq
        else failwith "heap_min"
        
    | _ -> failwith "heap_min"
  in
  aux pq (fun x -> x)


(* Takes the "tail" of the binary digits, reverse it and append
to the "head", like : 10010110 -> 10110100
This function returns a number which makes possible to decide
easily how to visit the tree to find the node number n (with
numbering "in width"). It is used in replace_root.
If loc is the returned number:
loc=1 means you reached the node,
loc mod 2=0 means visit first sub tree,
loc mod 2=2 means visit second sub tree. *)
let transform_bin n =
  if n=0 || n=1 then n else
  
  let rec aux r n =
    if n=0 then r
    else aux (r+1) (n/2)
  in
  let len = aux 0 n in
  
  let rec aux r n i =
    if i=0 then r+(n mod 2) else
    aux (r+(n mod 2)*(1 lsl i)) (n/2) (i-1)
  in
  
  (aux 0 n (len-2)) + (1 lsl (len-1))



let replace_root pq new_root loc =
  let rec aux pq nr loc cont = match pq with
    | Pq (a,pq0,Empty) ->
        cont (Pq (nr,Pq(a,Empty,Empty),pq0))
    | Pq (a,((Pq _) as pq1),((Pq _) as pq2)) ->
        let cont1,pq0 =
          if loc mod 2=0 then
            (fun res -> cont (Pq (nr,res,pq2))),pq1
          else (fun res -> cont (Pq (nr,pq1,res))),pq2
        in
        aux pq0 a (loc/2) cont1
    | Pq (_,Empty,_) -> assert false
    | Empty -> assert false
  in
  aux pq new_root loc (fun x -> x)


let insert compare x (pq,card) =
  let rec aux pq loc cont = match pq with
    | Pq (a,_,_) when compare x a= -1 ->
        cont (replace_root pq x loc)
    | Pq (a,pq1,pq2) ->
        let cont1,pq0 =
          if loc mod 2=0 then
            (fun res -> cont (Pq (a,res,pq2))),pq1
          else (fun res -> cont (Pq (a,pq1,res))),pq2
        in aux pq0 (loc/2) cont1
    | Empty -> cont (Pq (x,Empty,Empty))
  in
  (aux pq (transform_bin (card+1)) (fun x -> x)),(card+1)


exception Empty_queue

let pop compare (pq,card) = match pq with
  | Pq (min,Empty,Empty) -> min,empty
  | Pq (min,pq1,pq2) ->
      let loc = transform_bin card in
      let rec aux pq loc cont = match pq with
        | Pq (a,Empty,Empty) -> cont (a,Empty)
        | Pq (_,Empty,_) -> assert false
        | Pq (a,pq1,pq2) ->
            let cont1,pq0 =
              if loc mod 2=0 then
                (fun (x,pq1) -> cont (x,(Pq (a,pq1,pq2)))),pq1
              else
                (fun (x,pq2) -> cont (x,(Pq (a,pq1,pq2)))),pq2
            in
            aux pq0 (loc/2) cont1
        | Empty -> assert false
      in
      let pq =
        if loc mod 2=0 then
          let x,pq1 = aux pq1 (loc/2) (fun x->x) in
          Pq (x,pq1,pq2)
        else
          if pq2 = Empty then
            (print_endline ("card = "^(string_of_int card)); assert false)
          else
          let x,pq2 = aux pq2 (loc/2) (fun x->x) in
          Pq (x,pq1,pq2)
      in min,((heap_min compare pq),card-1)
  | Empty -> raise Empty_queue




