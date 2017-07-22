
type t

val create : unit -> t

val emit : (string -> unit) -> t -> YamlNode.t -> unit

val emit_channel : out_channel -> t -> YamlNode.t -> unit

val emit_string : t -> YamlNode.t -> string
