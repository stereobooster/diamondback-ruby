
open Printf
open Utils
open Config

module C = Cfg.Abbr

let prev_time = ref (Unix.gettimeofday ())

let checkpoint str = 
  if conf.show_time then
    let now = Unix.gettimeofday () in
    let diff = now -. !prev_time in
      prev_time := now;
      eprintf "%s: %02f secs\n" str diff;
      ignore(diff)
  else
    ()

(* analyzes the ruby program represented by cfg *)
let analyze_cfg loader cfg = 
  let () = checkpoint "start" in
  let cfg = 
    if conf.use_base_types
    then
      let base_mc = {Cfg.mc_target = Some (C.access_path["DRuby";"Profile";"Runtime"]);
                     mc_msg = `ID_MethodName "safe_require";
                     mc_args = [`Lit_String "base_types.rb";
                                `Lit_String (Filename.concat
                                              conf.stub_dir "base_types.rb")];
                     mc_cb = None}
      in
      let base = Cfg.mkstmt (Cfg.MethodCall(None,base_mc)) Lexing.dummy_pos in
        Cfg.mkstmt (Cfg.Seq [base;cfg]) Lexing.dummy_pos
    else cfg
  in 
  let () = checkpoint "resolved scopes" in 
  let env = Typing.type_cfg loader conf.ruby_file cfg in
    checkpoint "type cfg";
    Typing.solve_constraints env;
    checkpoint "solve constraints";
    eprintf "DRuby analysis complete.\n"

let clean_cache () = 
  File_loader.clean_cached conf.ruby_file;
  exit(1)

let merge_rlibs loader = 
  let reqs = 
    List.map
      (fun file ->
         C.mcall (`ID_MethodName "require") [C.str file] Lexing.dummy_pos
      ) conf.rlibrary in
  let main = File_loader.load_file loader conf.ruby_file in
    C.seq (reqs @ [main]) Lexing.dummy_pos

let build_cfg loader = 
  let module DList = 
    Dynamic.Cons(Method_missing)
      (Dynamic.Cons(Evals)
         (Dynamic.Cons(Send)
            (Dynamic.Singleton(Reflection))))
  in
  let module Dyn = Dynamic.Make(DList) in
  let () = if conf.profile then Dyn.profile loader conf.ruby_file in
  let cfg = 
    if conf.transform
    then Dyn.transform loader conf.ruby_file
    else merge_rlibs loader
  in
    (* Cfg_printer.print_cfg cfg; *)
    if conf.cache then File_loader.write_cache_file loader conf.ruby_file;
    cfg
      
let run_original () = 
  print_endline "\nnow running Ruby...\n";
  let cmd = sprintf "ruby %s %s %s" 
    (String.concat " " conf.ruby_options) 
    conf.ruby_file 
    (String.concat " " conf.ruby_args) 
  in
    print_endline cmd;
    exit (Sys.command cmd)

(* parses the command line, run the analysis, and run ruby if requested *)
let () = 
  parse_cmdline ();
  if conf.clean_cached then clean_cache();
  let loader = File_loader.create File_loader.EmptyCfg conf.idirectory in
  let cfg = build_cfg loader in
    (*Cfg_printer.print_stmt stderr cfg;*)
    if conf.type_inference then analyze_cfg loader cfg;
    Log.flush ();
    flush stdout;
    flush stderr;
    if conf.run_ruby then run_original ();
    ()
    
