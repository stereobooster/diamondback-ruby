type t

exception Error of string

let _ = Callback.register_exception "YamlParser_Error" (Error "")

external make : unit -> t = "yamlParser_make"

external parse_string : t -> string -> YamlNode.t = "yamlParser_parse_string"

