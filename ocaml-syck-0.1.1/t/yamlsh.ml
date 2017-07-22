
open YamlNode
let ten = SCALAR("int", "10")
let strten = SCALAR("str", "10")
let fals = SCALAR("bool#no", "false")


let ilst = SEQUENCE("",[ten;ten])

let imap = MAPPING("",[(ten,ten)])

let nan = SCALAR("float#nan",".NaN")
let inf = SCALAR("float#inf",".Inf")
let ninf = SCALAR("float#neginf","-.Inf")
let flt = SCALAR("float#fix","3.0")

let () = 
  let em = YamlEmitter.create () in
  let s = YamlEmitter.emit_channel stdout em ninf in
    ignore(s);(*Printf.printf "got %s\n" s*)

(*
let rec spew depth = function
  | YamlNode.SCALAR (uri, value) ->
      Printf.printf "!<%s> %s\n" uri value
  | YamlNode.SEQUENCE (tag, seq) ->
      Printf.printf "spewing SEQUENCE (%s)\n" tag ;
      List.iter
        (fun value ->
           Printf.printf "- " ;
           spew (depth + 1) value)
        seq
  | YamlNode.MAPPING (tag, map) ->
      Printf.printf "spewing MAPPING (%s)\n" tag ;
      List.iter
        (fun (key, value) ->
           Printf.printf "? " ;
           spew (depth + 1) key ;
           Printf.printf ": " ;
           spew (depth + 1) value)
        map

let _ =
  let p = YamlParser.make () in
  let v = YamlParser.parse_string p "---\n- not" in
    spew 0 v


*)
