
open Utils
open Printf
open Config
open Cfg_printer

type ('a,'b) yaml_map = 'a -> Cfg.pos -> 'b list

module type DynamicAnalysis = sig
  module Domain : Yaml.YType
  module CoDomain : Yaml.YType

  val name : string
  val instrument_ast : Ast.expr list -> Ast.expr list
  val instrument_cfg : Cfg.stmt -> Cfg.stmt

  val transform_ast : (Domain.t,CoDomain.t) yaml_map -> Ast.expr list -> Ast.expr list
  val transform_cfg : (Domain.t,CoDomain.t) yaml_map -> Cfg.stmt -> Cfg.stmt
end

module type AnalysisList = sig
  type t

  val empty : t
  val profile_libs : string list

  val load_runtime_data : (string -> Yaml.yaml) -> t

  val instrument_ast : Ast.expr list -> Ast.expr list
  val instrument_cfg : Cfg.stmt -> Cfg.stmt

  val transform_ast : t -> Ast.expr list -> Ast.expr list
  val transform_cfg : t -> Cfg.stmt -> Cfg.stmt
end

module Make(DL : AnalysisList) : sig
  val profile : File_loader.t -> string -> unit
  val transform : File_loader.t -> string -> Cfg.stmt
  val run : File_loader.t -> string -> Cfg.stmt
end = struct

  type t = {
    tmp_dir : string;
    rubylib : string;
    args : string list;
    norm_filename : string;
    orig_filename : string;
    rlibrary_file : string;
  }
      
  (* aborts when status for a subprocess returns with an error *)
  let abort_on_exit_status msg = function
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED d -> Log.fatal Log.empty "%s: ruby process failed: %d\n" msg d
    | Unix.WSIGNALED d -> 
        Log.fatal Log.empty "%s: ruby process died by exception %d\n" msg d
    | Unix.WSTOPPED d -> 
        Log.fatal Log.empty "%s: ruby process stopped by exception %d\n" msg d
        
  (* signal handle to abort on a SIGPIPE *)
  let abort_on_sigpipe abort_msg i = 
    assert (i == Sys.sigpipe);
    Log.fatal Log.empty "Ruby died before file was written to pipe: %s" abort_msg

  let construct_ruby_args libs real_args = 
    (* create the arugment list putting "ruby" at position 0 and then
       created "-r" "lib" pairs for each preloaded library *)
    let lib_len = List.length libs in
    let arg_len = List.length  real_args in
      (*                  ruby (-r lib)* args *)
    let arr = Array.create (1 + 2*lib_len + arg_len) "-r" in
      arr.(0) <- "ruby";
      (* add each lib to the even positions *)
      ignore(List.fold_left (fun idx x -> arr.(idx) <- x;idx+2) 2 libs);
      (* change the end of the array to the args *)
      let (_:int) = List.fold_left
        (fun idx x -> arr.(idx) <-x;idx+1
        ) (1+2*lib_len) real_args
      in
        arr

  let remove_if_exists x = 
    if Sys.file_exists x && (not conf.save_temps)
    then Sys.remove x

  (* return an environment array with the RUBYLIB variable set as the
     argument. *)
  let change_env_rubylib rubylib = 
    let env = Unix.environment () in
    let _,pos = Array.fold_left
      (fun (i,acc) s -> 
         if substr "RUBYLIB=" s then (i+1, Some i)
         else (i+1,acc)
      ) (0,None) env
    in match pos with
      | None -> 
          let len = Array.length env in
          let env' = Array.create (len+1) "" in
            Array.blit env 0 env' 0 len;
            env'.(len) <- rubylib;
            env'
      | Some i -> 
          env.(i) <- rubylib;
          env

  (* Attempt to execute the code given as the string [code].  The
     output and error streams of the ruby process are redirected to
     the current process's respective streams.  If the execution fails
     (the interpreter exits with an error), print the abort_msg and
     abort the program.  *)
  let execute_ruby_code t libs code abort_msg = 
    let old_handler = Sys.signal Sys.sigpipe
      (Sys.Signal_handle (abort_on_sigpipe abort_msg)) 
    in
    let p_in,p_out = Unix.pipe () in
    let () = Unix.set_close_on_exec p_out in
    let env = change_env_rubylib t.rubylib in
    let args = construct_ruby_args libs t.args in
    let pid = Unix.create_process_env "ruby" args
      env p_in Unix.stdout Unix.stderr 
    in
    let oc = Unix.out_channel_of_descr p_out in
    let fout = open_out "output.rb" in 
      output_string fout code; close_out fout;
      output_string oc code;
      close_out oc; 
      let pid,status = Unix.waitpid [] pid in
        Unix.close p_in;
        abort_on_exit_status abort_msg status;
        Sys.set_signal Sys.sigpipe old_handler

  let execute_ruby_file t preload file output = 
    let argv = construct_ruby_args preload (file::t.args) in
    let env = change_env_rubylib t.rubylib in 
      (*Array.iter (fun s -> Printf.eprintf "%s " s) env;*)
      (*Array.iter (fun s -> Printf.eprintf "%s " s) argv;
      Printf.eprintf "\n%!";*)
    let out_name,out_oc = Filename.open_temp_file "ruby_output" ".out" in
    let err_name,err_oc = Filename.open_temp_file "ruby_output" ".err" in
    let out_fd = Unix.descr_of_out_channel out_oc in
    let err_fd = Unix.descr_of_out_channel err_oc in
    let pid = Unix.create_process_env "ruby" argv env 
      Unix.stdin out_fd err_fd
    in
    let pid,status = Unix.waitpid [] pid in
    let () = close_out out_oc in
    let () = close_out err_oc in
    let err = file_contents err_name in
      if output then begin
        output_string stdout (file_contents out_name);
        output_string stderr err;
      end;
      abort_on_exit_status err status;
      remove_if_exists out_name;
      remove_if_exists err_name

  let rec construct_argv buf = function
    | [] -> ()
    | [a] -> Buffer.add_string buf ("\"" ^ a ^ "\"")
    | h::t -> 
        Buffer.add_string buf "\"";
        Buffer.add_string buf h;
        Buffer.add_string buf "\", ";
        construct_argv buf t

  let flatten_filename fname = 
    let escaped = Str.global_replace (Str.regexp "_") "__"  fname in
      Str.global_replace (Str.regexp "/") "_" escaped

  (* try to open the directory named [dir] and look for old files.  We
     skip over the special entries "." and ".." which are present in
     every directory, but remove any others and abort if we encounter
     any error during removal. *)
  let clear_directory dir = 
    let dh = 
      try Unix.opendir dir
      with Unix.Unix_error _ -> Log.fatal Log.empty "clear_directory"
    in
      try while true do
        let dname =Unix.readdir dh in 
          if dname = "." || dname = ".." then ()
          else 
            try Sys.remove (Filename.concat dir dname)
            with e ->
              Log.fatal Log.empty
                "Unable to clean out temp directory %s: %s"
                dir (Printexc.to_string e)
      done
      with End_of_file -> Unix.closedir dh

  (* construct a directory in the temp directory based on our
     username.  This also ensures that we have write and execute
     access to this directory.  Returns the subdirectory created under
     temp, (.e.g., for /tmp/druby-furr/, it returns druby-furr/) *)
  let tmp_prefix () = 
    let user = (Unix.getpwuid(Unix.getuid())).Unix.pw_name in 
    let base = Filename.temp_dir_name in
    let rest = sprintf "druby-%s/" user in
    let full = base ^ "/" ^ rest in
      if Sys.file_exists full 
      then begin
        try
          Unix.access full [Unix.R_OK;Unix.W_OK;Unix.X_OK];
          clear_directory full;
          rest
        with Unix.Unix_error _  -> 
          Log.fatal Log.empty "can't setup temp directory: %s\n" full
      end else begin
        try
          Unix.mkdir full 0o700;
          rest
        with Unix.Unix_error _ ->
          Log.fatal Log.empty "can't setup temp directory: %s\n" full
      end

  class s0_visitor filename = 
  object(self)
    inherit Cfg.default_visitor
    method visit_expr e = match e with
      | `ID_Var(`Var_Builtin,"$0") -> Visitor.ChangeTo (`Lit_String filename)
      | _ -> Visitor.DoChildren
  end
    (* replace all instancs of $0 with the original filename.
       Assigning to $0 seems quite broken, and we assume here that the
       application does not do that either. *)
  let replace_string_0 t cfg = 
    let o = new s0_visitor t.orig_filename in
      Cfg.visit_stmt o cfg

  let append_require cfg file = 
    let code = Printf.sprintf "\nrequire('%s')\n" file in
    let ast = Parse_helper.parse_string code in
    let post = Cfg_refactor.refactor_ast ast in
    let node = Cfg.Seq [cfg;post] in
      Cfg.mkstmt node Lexing.dummy_pos

  let create_rlibrary_file tmp libs = 
    (* Create a file that holds the require calls to the libraries
       passed on the command line with -r.  We keep this is a separate
       file, so that it can be processed by the DynamicAnalysis
       modules just like a user-supplied source file (and correctly
       keeping track of the source locations) *)
    let req_cfg = List.fold_left append_require (Cfg.empty_stmt()) libs in
    let name_prefix = Filename.concat tmp "command_line_requires" in
    let name,oc = Filename.open_temp_file name_prefix ".rb" in
      CodePrinter.print_stmt oc req_cfg;
      close_out oc;
      File_loader.normalize_filename name


  (* intstrument all of the files that were required at runtime.
     Returns the name of the 'main' script file paired with the list
     of temporary file names holding the transformed code *)
  let instrument_and_save loader t req_info = try
    let tmp_tbl = Hashtbl.create 127 in
      (* for each file that was required at runtime, find the true
         location of that file *)
    let real_files = Require.all_required_files req_info in
      (* generate filename, temp_filename, output_channel for each
         file.  We do this separately from the next step because we
         need to preload the tmp_tbl hash with all of the temporary
         file names any of which may be used when instrumenting code
         below *)
    let tmps = List.map
      (fun (orig_file,file) -> 
         let flatname = Filename.concat t.tmp_dir (flatten_filename file) in
         let tmp, oc = Filename.open_temp_file flatname ".rb" in
           (*Printf.eprintf "adding %s => %s to tml_tbl\n" orig_file file;*)
           Hashtbl.add tmp_tbl file tmp;
           orig_file,file,tmp,oc
      ) real_files
    in
    let main_stub = "druby_main.rb" in
    let main_oc = open_out main_stub in
    let tmps = (t.norm_filename,t.norm_filename, main_stub, main_oc)::tmps in
    let () = Hashtbl.add tmp_tbl t.norm_filename main_stub in

      (* instrument these files and write them to temp, return the
         list of temp file names *)
    let tmp_files = List.map
      (fun (orig_file, real_file, tmp_file, oc) ->
         (*Printf.eprintf "outputting %s\n" real_file;*)
         let modf s = 
           let ast = Require.modify_requires tmp_tbl req_info s in
             DL.instrument_ast ast
         in
         let cfg = File_loader.load_file ~no_ext:true loader ~mod_ast_f:modf orig_file in
         let cfg = replace_string_0 t cfg in
         let cfg = DL.instrument_cfg cfg in
           (*Printf.eprintf "new cfg is %s\n" (format_to_string CodePrinter.format_stmt cfg);*)
           File_loader.update_file ~no_ext:true loader tmp_file cfg;
           CodePrinter.print_stmt oc cfg;
           close_out oc;
           tmp_file
      ) tmps
    in
      try Hashtbl.find tmp_tbl t.norm_filename, tmp_files
      with Not_found  -> 
        Log.fatal Log.empty "didn't find mainfile %s in tmp table?" t.norm_filename
  with Not_found -> 
    Log.fatal Log.empty "bug in instrument_and_save"

  let remove_temp_files t files () = 
    if not conf.save_temps 
    then try
      List.iter remove_if_exists files
    with e -> 
      Printf.eprintf "failed to remove the temp files: %s\n%!" 
        (Printexc.to_string e)
      
  let emit_initializer t cfg = 
    let file,oc = Filename.open_temp_file (Filename.concat t.tmp_dir "init") ".rb" in
    (*let file = "druby.init.rb" in*)
    let oc = open_out file in 
      CodePrinter.print_stmt oc cfg;
      close_out oc;
      file

  let append_safe_require cfg file = 
    let code = Printf.sprintf "\nDRuby::Profile::Runtime.safe_require('%s','%s')\n" file file in
    let ast = Parse_helper.parse_string code in
    let post = Cfg_refactor.refactor_ast ast in
    let node = Cfg.Seq [cfg;post] in
      Cfg.mkstmt node Lexing.dummy_pos

  let profile_init ?(append=false) t components = 
    let buf = Buffer.create 32 in
      Buffer.add_string buf "require('druby/profile')\n";
      Buffer.add_string 
        buf (sprintf "DRuby::Profile.new('%s', %B,[" conf.profile_db append);
      List.iter (fun x -> Buffer.add_string buf ("'"^x^"',")) components;
      Buffer.add_string buf "])\n";
      Cfg_refactor.refactor_ast (Parse_helper.parse_string (Buffer.contents buf))

  module YamlCollection = Yaml.YMap(Yaml.YString)(Yaml.YRaw)

  (* returns a function for loading data from the Yaml data file *)
  let yaml_load_func fname : string -> Yaml.yaml = 
    try let map = YamlCollection.of_yaml (Yaml.parse_file fname) in
      (fun s -> 
         try YamlCollection.Map.find s map
         with Not_found -> 
           Log.fatal Log.empty "unable to locate dynamic info for %s in yaml file" s
      )
    with Yaml.Parse_error s ->
      Log.fatal Log.empty "DRuby failed to parse %s: %s" fname s

  let gather_requires t = 
    let cfg = profile_init t Require.profile_libs in
    let cfg = append_require cfg t.rlibrary_file in
      (* add all of the -r libs to the end of this file *)
    (*let cfg = List.fold_left append_require cfg conf.rlibrary in*)
    let init_file = emit_initializer t cfg in
      at_exit (remove_temp_files t [init_file]);
      execute_ruby_file t [init_file] t.orig_filename false;
      Require.load_runtime_data (yaml_load_func conf.profile_db)

  let init loader fname = 
    let norm_file = File_loader.normalize_filename fname in
    let tmp_dir = tmp_prefix() in
      {tmp_dir = tmp_dir;
       rubylib = File_loader.mk_rubylib_env loader;
       args = conf.ruby_args;
       norm_filename = norm_file;
       orig_filename = fname;
       rlibrary_file = create_rlibrary_file tmp_dir conf.rlibrary;
      }

  let transform_files loader yaml_func req_t src_file =
    let norm_file = File_loader.normalize_filename src_file in
    let da_t = DL.load_runtime_data yaml_func in
    let all_files = Require.all_required_files req_t in
    let all_files = 
      if List.mem (src_file,norm_file) all_files then all_files
      else (src_file, norm_file)::all_files
    in
    let tmp_tbl = Hashtbl.create 127 in
      List.iter
        (fun (orig_file,file) -> Hashtbl.add tmp_tbl file file
        ) all_files;
      List.iter 
        (fun (orig_file,file) ->
           if not (File_loader.is_native file) then
             let modf s = 
               let ast = Require.modify_requires tmp_tbl req_t s in
                 DL.transform_ast da_t ast
             in
             let cfg = File_loader.load_file ~no_ext:true loader ~mod_ast_f:modf file in
             let cfg = DL.transform_cfg da_t cfg in
               File_loader.update_file ~no_ext:true loader file cfg
        ) all_files

  let tmpdir_rubylib t loader = 
    let orig = File_loader.lib_dirs_string loader in
    let tmp_dir = Filename.concat Filename.temp_dir_name t.tmp_dir in
    let rubylib = tmp_dir ^ ":" ^ orig in
      {t with rubylib = "RUBYLIB=" ^ rubylib}
      
  let profile loader src_fname = 
    (*Log.fixme "capture fork / system calls";*)

    (* reset mining *)
    let () = Mining.reset () in

    let t = init loader src_fname in
      
    (* require pass *)
    let req_t = gather_requires t in

    (* instrument pass *)
    let t = tmpdir_rubylib t loader in
    let fake_loader = File_loader.create File_loader.EmptyCfg [] in
    let main_tmp,tmps = instrument_and_save fake_loader t req_t in

    (* outputs the data into a yaml file *)
    let () = 
      let out_dir = Filename.concat Filename.temp_dir_name t.tmp_dir in
      (* let out_fname = (flatten_filename t.norm_filename) ^ ".mining.yml" in
      *)
      let out_fname = Filename.basename t.norm_filename in
      let out_fname = (Filename.concat out_dir out_fname) ^ ".mining.yml" in 
        (* print_endline out_fname; *)
        Mining.output_to_yaml out_fname
    in

    let init_cfg = profile_init ~append:true t DL.profile_libs in
    let init_tmp = emit_initializer t init_cfg in
    let preload = [init_tmp;"druby/profile/runtime"] in
      execute_ruby_file t preload main_tmp true;
      remove_temp_files t (init_tmp::tmps) ()

  let transform loader src_fname = 
    let req_t = Require.load_runtime_data (yaml_load_func conf.profile_db) in
    let f = yaml_load_func conf.profile_db in
    (* transform pass *)
    let () = transform_files loader f req_t src_fname in
    let () = assert(File_loader.already_loaded ~no_ext:true loader src_fname) in
    let cfg = File_loader.load_file ~no_ext:true loader src_fname in
      cfg

  let run loader src_fname = 
    profile loader src_fname;
    transform loader src_fname
    

end

module Cons(X:DynamicAnalysis)(XS:AnalysisList) : AnalysisList = struct

  open Yaml
  module XData = YMap(Y3Tuple(YString)(YInt)(X.Domain))(YList(X.CoDomain))

  type t = [`Cons of XData.t * XS.t]

  let profile_libs = X.name::XS.profile_libs
    
  let empty = `Cons(XData.Map.empty,XS.empty)

  let instrument_ast ast = XS.instrument_ast (X.instrument_ast ast)
  let instrument_cfg cfg = XS.instrument_cfg (X.instrument_cfg cfg)

  let load_runtime_data func = 
    let xdata = XData.of_yaml (func X.name) in
    `Cons(xdata,XS.load_runtime_data func)

  let lookup map key pos = 
    let fname = File_loader.normalize_filename pos.Lexing.pos_fname in
    let lnum = pos.Lexing.pos_lnum in
      try XData.Map.find (fname, lnum, key) map
      with Not_found -> []
        
  let transform_ast (`Cons(t,rest)) ast = 
    let func = lookup t in
      XS.transform_ast rest (X.transform_ast func ast)

  let transform_cfg (`Cons(t,rest)) cfg = 
    let func = lookup t in
      XS.transform_cfg rest (X.transform_cfg func cfg)

end

module Singleton(X:DynamicAnalysis) : AnalysisList = struct
  let profile_libs = [X.name]

  open Yaml
  module XData = YMap(Y3Tuple(YString)(YInt)(X.Domain))(YList(X.CoDomain))
  type t = XData.t
  let empty = XData.Map.empty

  let load_runtime_data func = XData.of_yaml (func X.name)

  let lookup map key pos = 
    let fname = File_loader.normalize_filename pos.Lexing.pos_fname in
    let lnum = pos.Lexing.pos_lnum in
      try XData.Map.find (fname, lnum, key) map
      with Not_found -> []

  let instrument_ast = X.instrument_ast
  let instrument_cfg = X.instrument_cfg
  let transform_ast t ast = X.transform_ast (lookup t) ast
  let transform_cfg t cfg = X.transform_cfg (lookup t) cfg

end

module ReqL = Singleton(Require)

module NoOpAnalysis : DynamicAnalysis = struct
  module Domain = Yaml.YUnit
  module CoDomain = Yaml.YUnit

  let name = ""

  let instrument_ast e = e
  let instrument_cfg s = s

  let transform_ast _ e = e 
  let transform_cfg _ s = s
end
