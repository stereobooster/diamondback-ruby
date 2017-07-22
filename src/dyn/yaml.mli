
type yaml = private
    YUnit
  | YInt of int
  | YBigint of Big_int.big_int
  | YFloat of float
  | YString of string
  | YBool of bool
  | YList of yaml list
  | YAssoc of (yaml * yaml) list
  | YOther of YamlNode.t

val convert : other:bool -> YamlNode.t -> yaml

val parse_file : ?other:bool -> string -> yaml
val parse_string : ?other:bool -> string -> yaml

(** a crude printing function, for debuggin *)
val yaml_to_string : yaml -> string

val to_yamlNode : yaml -> YamlNode.t
val emit_yaml_string : yaml -> string
val emit_yaml_channel : out_channel -> yaml -> unit

module type YType = sig
  type t
  val of_yaml : yaml -> t 
  val to_yaml : t -> yaml
  val compare : t -> t -> int 
end

exception Parse_error of string

module YRaw : YType with type t = yaml
module YUnit : YType with type t = unit
module YInt : YType with type t = int
module YBig_int : YType with type t = Big_int.big_int
module YFloat : YType with type t = float
module YString : YType with type t = string
module YBool : YType with type t = bool

module YOption :
  functor(A : YType) ->
    YType with type t = A.t option

module YPair : 
  functor (A : YType) ->
    functor (B : YType) -> 
      YType with type t = A.t * B.t
module Y3Tuple : 
  functor (A : YType) ->
    functor (B : YType) -> 
      functor (C : YType) -> 
      YType with type t = A.t * B.t * C.t
module Y4Tuple : 
  functor (A : YType) -> functor (B : YType) -> 
    functor (C : YType) -> functor (D : YType) -> 
      YType with type t = A.t * B.t * C.t * D.t
module Y5Tuple : 
  functor (A : YType) -> functor (B : YType) -> 
    functor (C : YType) -> functor (D : YType) -> 
      functor (E : YType) ->
        YType with type t = A.t * B.t * C.t * D.t * E.t

module YList : 
  functor (T : YType) -> 
    YType with type t = T.t list

module YAssocList :
  functor (A : YType) ->
    functor (B : YType) -> 
      YType with type t = (A.t * B.t) list

module YSet :
 functor (Elt : YType) -> sig
   module Set : Set.S with type elt=Elt.t
   include YType with type t = Set.t
 end

module YMap :
  functor (Key : YType) ->
    functor (Value : YType) -> sig 
      module Map : Map.S with type key=Key.t
      include YType with type t = Value.t Map.t
    end

module YHashtbl : 
  functor (Key : YType) -> 
    functor (Value : YType) -> 
      YType with type t = (Key.t,Value.t) Hashtbl.t

module YOr : 
  functor (A:YType) ->
    functor (B:YType) ->
      YType with type t = [`A of A.t | `B of B.t]
