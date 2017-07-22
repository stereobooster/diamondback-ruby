

let () = 
  let ast = Parse_helper.parse_file "../stubs/1.8/base_types.rb" in
  let cfg = Cfg_refactor.refactor_ast ast in
  (*let env = Typing.type_cfg cfg in*)
  let oc = open_out "base_types.cfg" in
    Marshal.to_channel oc (cfg) []
