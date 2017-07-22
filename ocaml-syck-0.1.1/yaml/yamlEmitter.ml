
type t

external create : unit -> t = "yamlEmitter_create"

external emit : (string -> unit) -> t -> YamlNode.t -> unit = "yamlEmitter_emit"

let emit_channel oc t node = emit (output_string oc) t node

let emit_string t node = 
  let buf = Buffer.create 128 in
    emit (Buffer.add_string buf) t node;
    Buffer.contents buf
