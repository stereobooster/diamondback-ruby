open Utils
open Format

type yaml = 
  | YUnit
  | YInt of int
  | YBigint of Big_int.big_int
  | YFloat of float
  | YString of string
  | YBool of bool
  | YList of yaml list
  | YAssoc of (yaml*yaml) list
  | YOther of YamlNode.t

let rec yaml_to_string = function
  | YUnit -> "unit"
  | YInt i -> "int"
  | YBigint bi -> "bigint"
  | YFloat f -> "float"
  | YString s -> Printf.sprintf "string(%d): '%s'" (String.length s) s
  | YBool b -> "bool"
  | YList lst -> 
      let buf = Buffer.create 17 in
        Buffer.add_string buf "list(";
        List.iter(fun y -> Buffer.add_string buf ((yaml_to_string y) ^ ",")) lst;
        Buffer.add_string buf ")";
        Buffer.contents buf

  | YAssoc lst ->
      let buf = Buffer.create 17 in
        List.iter
          (fun (x,y) -> 
             Buffer.add_string buf
               (Printf.sprintf "(%s,%s)," (yaml_to_string x) (yaml_to_string y))
          ) lst;
        Buffer.contents buf

  | YOther o -> "<other>"


let rec convert ~other node = match node with
  | YamlNode.SCALAR("null",s) ->  YUnit
  | YamlNode.SCALAR("int",s) -> 
      let num = Big_int.big_int_of_string s in
        if Big_int.is_int_big_int num
        then YInt (Big_int.int_of_big_int num)
        else YBigint num

  | YamlNode.SCALAR("str",s) -> YString s

  | YamlNode.SCALAR("float#fix",s) -> YFloat (float_of_string s)
  | YamlNode.SCALAR("float#inf",s) -> YFloat infinity
  | YamlNode.SCALAR("float#neginf",s) -> YFloat neg_infinity
  | YamlNode.SCALAR("float#nan",s) -> YFloat nan

  | YamlNode.SCALAR("bool#no",s) -> assert(s = "false");YBool false
  | YamlNode.SCALAR("bool#yes",s) -> assert(s = "true");YBool true
        
  | YamlNode.SEQUENCE("",lst) -> YList (List.rev_map (convert ~other) (List.rev lst))

  | YamlNode.MAPPING("",lst) ->
      let map = List.fold_left
        (fun acc (k,v) -> (convert ~other k,convert ~other v)::acc) 
        [] lst 
      in
        YAssoc (List.rev map)

  | YamlNode.SCALAR(uri,_)
  | YamlNode.SEQUENCE(uri,_)
  | YamlNode.MAPPING(uri,_) ->
      if other then YOther node  
      else begin
        Printf.eprintf "unknown yaml node: %s\n" uri;
        exit 1
      end

let rec to_yamlNode yaml = match yaml with
  | YUnit -> YamlNode.SCALAR("null","")

  | YInt i -> YamlNode.SCALAR("int",string_of_int i)
  | YBigint bi -> YamlNode.SCALAR("int",Big_int.string_of_big_int bi)

  | YFloat f when f == nan -> YamlNode.SCALAR("float#nan",".NaN")
  | YFloat f when f == infinity -> YamlNode.SCALAR("float#inf",".Inf")
  | YFloat f when f == neg_infinity -> YamlNode.SCALAR("float#neginf","-.Inf")
  | YFloat f -> YamlNode.SCALAR("float#fix",string_of_float f)

  | YString s -> YamlNode.SCALAR("str",s)
  | YBool false -> YamlNode.SCALAR("bool#no","false")
  | YBool true -> YamlNode.SCALAR("bool#yes","true")

  | YList ylist -> 
      let lst = List.rev_map to_yamlNode (List.rev ylist) in
        YamlNode.SEQUENCE("",lst)

  | YAssoc ylst ->
      let map = List.fold_left
        (fun acc (k,v) -> (to_yamlNode k,to_yamlNode v)::acc) 
        [] (List.rev ylst )
      in
        YamlNode.MAPPING("",map)

  | YOther y -> y

let emit_yaml_string yaml = 
  let node = to_yamlNode yaml in
    YamlEmitter.emit_string (YamlEmitter.create ()) node

let emit_yaml_channel oc yaml = 
  let node = to_yamlNode yaml in
    YamlEmitter.emit_channel oc (YamlEmitter.create ()) node

let parse_string ?(other=false) str =
  let parse_t = YamlParser.make () in
    convert ~other (YamlParser.parse_string parse_t str)

let parse_file ?(other=false) fname =
  let str = file_contents fname in
    parse_string ~other str

exception Parse_error of string

let perr expected actual = 
  let msg = Printf.sprintf 
    "YAML parse error, expected %s, but got %s"
    expected (yaml_to_string actual)
  in
    raise (Parse_error msg)

module type YType = sig
  type t
  val of_yaml : yaml -> t
  val to_yaml : t -> yaml
  val compare : t -> t -> int
end

module YRaw = struct
  type t = yaml
  let of_yaml x = x
  let to_yaml x = x
  let compare = Pervasives.compare
end

module YUnit : YType with type t=unit = struct
  type t = unit
  let of_yaml = function
    | YUnit -> ()
    | y ->  perr "unit" y
  let to_yaml () = YUnit
  let compare () () = 0
end      

module YInt : YType with type t=int = struct
  type t = int
  let of_yaml = function
    | YInt i -> i
    | y ->  perr "int" y
  let to_yaml i = YInt i
  let compare (x:int) (y:int) = compare x y
end

module YBig_int : YType with type t=Big_int.big_int = struct
  type t = Big_int.big_int
  let of_yaml = function
    | YBigint i -> i
    | y ->  perr "bit_int" y
  let to_yaml i = YBigint i
  let compare x y = Big_int.compare_big_int x y
end

module YFloat : YType with type t=float = struct
  type t = float
  let of_yaml = function
    | YFloat i -> i
    | y ->  perr "float" y
  let to_yaml f = YFloat f
  let compare (x:float) (y:float) = compare x y
end

module YString : YType with type t=string = struct
  type t = string
  let of_yaml = function
    | YString i -> i
    | y ->  perr "string" y
  let to_yaml s = YString s
  let compare = String.compare
end

module YBool : YType with type t=bool = struct
  type t = bool
  let of_yaml = function
    | YBool i -> i
    | y -> perr "bool" y
  let to_yaml b = YBool b
  let compare (x:bool) (y:bool) = compare x y
end      

module YOption(A:YType) : YType with type t = A.t option = struct
  type t = A.t option
  let of_yaml = function
    | YUnit -> None
    | y -> Some (A.of_yaml y)
  let to_yaml = function
    | None -> YUnit
    | Some y -> A.to_yaml y
  let compare t1 t2 = cmp_opt A.compare t1 t2
end

module YPair(A:YType)(B:YType) : YType with type t=A.t*B.t = struct
  type t = A.t*B.t
  let of_yaml = function
    | YList [a;b] -> A.of_yaml a, B.of_yaml b
    | y ->  perr "pair" y
  let to_yaml (a,b) = YList[A.to_yaml a; B.to_yaml b]
  let compare = compare
end

module Y3Tuple(A:YType)(B:YType)(C:YType)
  : YType with type t=A.t*B.t*C.t = struct
  type t = A.t*B.t*C.t
  let of_yaml = function
    | YList [a;b;c] -> 
        A.of_yaml a, B.of_yaml b, C.of_yaml c
    | y ->  perr "3tuple" y
  let to_yaml (a,b,c) = YList[A.to_yaml a;B.to_yaml b;C.to_yaml c]
  let compare = compare
  end

module Y4Tuple(A:YType)(B:YType)(C:YType)(D:YType)
  : YType with type t=A.t*B.t*C.t*D.t = struct
  type t = A.t*B.t*C.t*D.t
  let of_yaml = function
    | YList [a;b;c;d] -> 
        A.of_yaml a, B.of_yaml b, C.of_yaml c, D.of_yaml d
    | y -> perr "4tuple" y
  let to_yaml (a,b,c,d) = 
    YList [A.to_yaml a; B.to_yaml b; C.to_yaml c; D.to_yaml d]
  let compare = compare
  end

module Y5Tuple(A:YType)(B:YType)(C:YType)(D:YType)(E:YType)
  : YType with type t=A.t*B.t*C.t*D.t*E.t = struct
  type t = A.t*B.t*C.t*D.t*E.t
  let of_yaml = function
    | YList [a;b;c;d;e] -> 
        A.of_yaml a, B.of_yaml b, C.of_yaml c, D.of_yaml d, E.of_yaml e
    | y -> perr "5tuple" y
  let to_yaml (a,b,c,d,e) = 
    YList [A.to_yaml a; B.to_yaml b; C.to_yaml c; D.to_yaml d; E.to_yaml e]
  let compare = compare
end

module YList(T:YType) : YType with type t=T.t list = struct
  type t = T.t list
  let of_yaml = function
    | YList l -> List.rev_map T.of_yaml (List.rev l)
    | y ->  perr "list" y
  let to_yaml l = YList (List.rev_map T.to_yaml (List.rev l))
  let compare = compare
end

module YAssocList(A:YType)(B:YType) : YType with type t=(A.t*B.t) list = struct
  type t = (A.t*B.t) list
  let of_yaml = function
    | YAssoc l -> 
        List.rev_map (fun (a,b) -> A.of_yaml a, B.of_yaml b) (List.rev l)
    | y -> perr "assoc list" y
  let to_yaml l = 
    let lst = List.rev_map (fun (a,b) -> A.to_yaml a, B.to_yaml b) (List.rev l) in
      YAssoc lst
  let compare = compare
end

module YSet(Elt:YType) = struct
  module Set = Set.Make(Elt)
  type t = Set.t
  let of_yaml = function
    | YList l -> 
        List.fold_left
          (fun acc x -> Set.add (Elt.of_yaml x) acc)
          Set.empty l
    | y -> perr "set" y
  let to_yaml set = 
    YList (Set.fold (fun x acc -> (Elt.to_yaml x)::acc) set [])
  let compare = Set.compare
end

module Check_Set = (YSet(YInt) : YType)

module YMap(Key:YType)(Value:YType) = struct
  module Map = Map.Make(Key)
  type t = Value.t Map.t
  let of_yaml = function
    | YAssoc l -> 
        List.fold_left
          (fun acc (k,v) -> 
             Map.add (Key.of_yaml k) (Value.of_yaml v) acc
          ) Map.empty l
    | y ->  perr "map" y
  let to_yaml map = 
    let lst = Map.fold (fun k v acc -> (Key.to_yaml k,Value.to_yaml v)::acc) map [] in
      YAssoc lst
  let compare x y = Map.compare Value.compare x y
end
module Check_Map = (YMap(YInt)(YInt) : YType)

module YHashtbl(Key:YType)(Value:YType) = struct
  type t = (Key.t,Value.t) Hashtbl.t
  let of_yaml y = match y with
    | YAssoc lst -> 
        let tbl = Hashtbl.create 17 in
          List.iter
            (fun (k,v) -> 
               Hashtbl.add tbl (Key.of_yaml k) (Value.of_yaml v)
            ) lst;
          tbl
    | y -> perr "hashtbl" y
  let to_yaml tbl = 
    let lst = Hashtbl.fold (fun k v acc -> (Key.to_yaml k,Value.to_yaml v)::acc) tbl [] in
      YAssoc lst

  module M = Map.Make(Key)
  let to_map tbl = Hashtbl.fold M.add tbl M.empty
  let compare x y = M.compare Value.compare (to_map x) (to_map y)
    
end

module Check_Hashtbl = (YHashtbl(YInt)(YInt) : YType)

module YOr(A:YType)(B:YType) : YType with type t = [`A of A.t | `B of B.t] = struct
  type t = [`A of A.t | `B of B.t]
      
  let compare t1 t2 = match t1,t2 with
    | `A a1, `A a2 -> A.compare a1 a2
    | `A _, `B _ -> -1
    | `B _, `A _ -> 1
    | `B b1, `B b2 -> B.compare b1 b2

  let of_yaml y = 
    try `A (A.of_yaml y)
    with e1 ->
      try `B (B.of_yaml y)
      with e2 ->
        let msg = Printf.sprintf "YOr: (%s) (%s)" 
          (Printexc.to_string e1) (Printexc.to_string e2)
        in
          raise (Invalid_argument msg)

  let to_yaml = function
    | `A a -> A.to_yaml a
    | `B b -> B.to_yaml b
end

