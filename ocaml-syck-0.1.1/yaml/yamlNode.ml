type uri = string

type t =
    | SCALAR of uri * string
    | SEQUENCE of uri * t list
    | MAPPING of uri * (t * t) list

