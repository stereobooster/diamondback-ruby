(** A file loader loads source files and keep the control flow graph (CFG) in
    the hashtable. The user may choose to cache them into .ast and .cache files 
    if neded. 'mod_ast' field of type 't' is used to modify ASTs before 
    refactoring into a CFG for handling dynamic features (evals). *)

open Config
open Utils

type src_file = 
  | RubyFile of string
  | InterfaceFile of string
  | NativeFile of string

type load_result = 
  | Ruby of Cfg.stmt
  | Interface of Cfg.stmt
  | Native of string

(** type of a file loader *)
type t = {
  path : string list;
  tbl : (src_file,load_result) Hashtbl.t;
  mutable cache_files : StrSet.t; (** the set of cached files for this 
    loader *)
}

(** converts the given path to a list of directories *)
let paths_from_rubylib lib_dirs = 
  let ruby_lib = 
    try Str.split (Str.regexp ":") (Sys.getenv "RUBYLIB") 
    with Not_found -> []
  in
    ruby_lib @ (lib_dirs @ ["."])
  
(** creates a file loader; mod_ast_f specifies an optional ast modifying
    function; lib_dirs specifies the list of paths to look *) 
let create lib_dirs = 
  { path = paths_from_rubylib lib_dirs;
    tbl = Hashtbl.create 127;
    cache_files = StrSet.empty;
  }

let native_extensions = [".so";".bundle";".o"]
let source_extensions = ".rb"::native_extensions
let all_extensions = ".rbi"::source_extensions
 
let has_native_ext f = 
  List.exists (Filename.check_suffix f) native_extensions

let is_native src = match src with
  | NativeFile(_) -> true
  | _ -> false

(** finds library directories for the given loader *)
let lib_dirs t = t.path
let lib_dirs_string t = 
  let buf = Buffer.create 256 in
    List.iter (fun d -> Buffer.add_string buf (d ^ ":")) t.path;
    Buffer.contents buf

let is_directory fname = 
  try
    Unix.closedir(Unix.opendir fname);
    true
  with _ -> false

let filename_to_src fname =
  match extension fname with
    | Some ".rbi" -> InterfaceFile(fname)
    | Some ".so"
    | Some ".bundle"
    | Some ".o"   -> NativeFile(fname)
    | _           -> RubyFile(fname)

let normalize_src_file s =
  match s with
    | RubyFile(name) -> RubyFile(normalize_filename name)
    | NativeFile(name)  -> NativeFile(normalize_filename name)
    | InterfaceFile(name)  -> InterfaceFile(normalize_filename name)

let src_name file = match file with
  | RubyFile(name) | NativeFile(name) | InterfaceFile(name) -> name

(** finds the file from the given list of library paths *)
let rec find_in_libpath ?(no_ext=false) ?(no_iface=false) f lst = 
  let find_with_ext fullname = 
    let exts =
      if no_iface then source_extensions else all_extensions in
    let exts = match extension fullname with
      | None -> if no_ext then ""::exts else exts 
      | Some e -> if (List.mem e exts) or no_ext then ""::exts else exts 
    in
    let exts =
      if not no_iface && extension fullname = Some ".rb" then
        "i"::exts else exts
    in
    let names_with_exts = List.map (fun e -> fullname ^ e) exts in
    List.find (fun f -> Sys.file_exists f && not (is_directory f)) names_with_exts
  in
  let rec work = function
    | [] -> 
        Log.note "Didn't find %s\n" f;
        raise Not_found
    | hd::tl -> 
        Log.note "looking for %s in %s" f hd;
        let fullname = Filename.concat hd f in
          try find_with_ext fullname
          with Not_found -> 
            work tl
  in
    if Filename.is_implicit f then filename_to_src (work lst)
    else filename_to_src (find_with_ext f)

let find_file ?no_ext ?(no_iface=false) t filename =
  find_in_libpath ?no_ext ~no_iface:no_iface filename t.path

let stale_cache file ast_file =
  if Sys.file_exists ast_file then
    let rb_tm = (Unix.stat file).Unix.st_mtime in
    let ast_tm = (Unix.stat ast_file).Unix.st_mtime in
    let druby_tm = (Unix.stat Sys.executable_name).Unix.st_mtime in
      rb_tm < ast_tm && druby_tm < ast_tm
  else false
        
(* caches the given cfg into a file *)
let cache_ast t fname ast =
  let fname = (normalize_filename fname) ^ ".ast" in
  let oc = open_out fname in
    Marshal.to_channel oc (ast:Ast.ast) [];
    close_out oc;
    t.cache_files <- StrSet.add fname t.cache_files

let load_cached_ast t fname = 
  let full_name = normalize_filename fname in
  let ast_name = full_name ^ ".ast" in
    if stale_cache full_name ast_name
    then
      let ic = open_in ast_name in 
      let (ast:Ast.ast) = Marshal.from_channel ic in
        close_in ic; 
        t.cache_files <- StrSet.add ast_name t.cache_files;
        ast
    else 
      let ast = Parse_helper.parse_ruby_file fname in
        cache_ast t fname ast;
        ast

let load_ast t fname = 
  if conf.print_filenames 
  then Printf.eprintf "parsing file %s\n%!" fname;
  if conf.cache 
  then load_cached_ast t fname
  else Parse_helper.parse_ruby_file fname

(** loads a file from the given file loader (and caches them if needed) *)
let load_file t ?(mod_ast_f=Utils.id) ?no_ext ?(no_iface=false) fname =
  Log.note "loading file %s" fname;
  try let src = find_file ?no_ext ~no_iface:no_iface t fname in
    match src with
      | NativeFile(found_name) -> 
        Log.note "skipping native file %s" fname;
        Native(found_name)
      | InterfaceFile(found_name) ->
        begin try Hashtbl.find t.tbl src
        with Not_found ->
          let iface = Parse_helper.parse_interface_file found_name in
          let cfg = Cfg.stmt_of_interface iface in
          Interface(cfg)
        end
      | RubyFile(found_name) ->
        let full_src = normalize_src_file src in
        (* look in the hash table and return it if already loaded *)
        try Hashtbl.find t.tbl src
          (* otherwise, load it; use cached if exists or cache if requested *)
        with Not_found ->
          let ast = load_ast t found_name in
          let ast = if conf.profile then mod_ast_f ast else ast in
          let cfg = Cfg_refactor.refactor_ast ast in
            Hashtbl.add t.tbl full_src (Ruby(cfg));
            Ruby(cfg)
  with Not_found -> 
    Log.err "*** unable to find file to load %s(%b)\n" 
      fname (default_opt false no_ext);
    Ruby(Cfg.empty_stmt ())

(** updates the cfg of the file in the given file loader with a new one *)
let update_file ?no_ext t fname cfg = 
  let src = find_file ?no_ext t fname in
  let full_name = normalize_src_file src in
    Hashtbl.replace t.tbl full_name cfg

(** checks whether the file has been previously loaded *)
let already_loaded ?no_ext t fname = 
  try let full_name = find_file ?no_ext t fname in
    if is_native full_name
    then true
    else Hashtbl.mem t.tbl (normalize_src_file full_name)
  with Not_found -> 
    Log.err "*** unable to find file to load %s\n" fname;
    true

(** makes RUBYLIB environment variable *)
let mk_rubylib_env loader = 
  let rec mk_helper = function
    | [] -> ""
    | h::t -> h ^ ":" ^ (mk_helper t)
  in
    "RUBYLIB=" ^ mk_helper loader.path

(** writes out the list of cache filenames to .cache file *)
let write_cache_file loader fname = 
  let fname = normalize_filename fname in
  let cache_fname = fname ^ ".cache" in
  let oc = open_out cache_fname in
    StrSet.iter (output_string oc) loader.cache_files;
    close_out oc

(** cleans up all the cached files (.ast files) and the list (.cache file) *)
let clean_cached fname =
  let () = print_endline "cleaning up cache files..." in
  let cache_fname = (normalize_filename fname) ^ ".cache" in
  let rm_file f = 
    print_endline ("removing " ^ f ^ "...");
    Sys.remove f
  in
  let ic = open_in cache_fname in
    try
      while true do
        let f = input_line ic in
          rm_file f
      done
    with End_of_file -> close_in ic;
    print_endline ("removing " ^ cache_fname ^ "...");
    Sys.remove cache_fname; (* don't forget to delete the '.cache' file *)
    print_endline "successfully removed all cache files "

let normalize_src_file = function
  | RubyFile s -> RubyFile (normalize_filename s)
  | InterfaceFile s -> InterfaceFile (normalize_filename s)
  | NativeFile s -> NativeFile (normalize_filename s)

(* TODO Dirty hack to get this to compile. *)
let load_result_to_cfg res =
  match res with
  | Ruby(stmt)
  | Interface(stmt) -> stmt
  | Native(_) -> Cfg.empty_stmt ()

let eql_load_result lr1 lr2 =
  match (lr1,lr2) with
    | Ruby cfg1 , Ruby cfg2 -> Cfg.stmt_eq cfg1 cfg2
    | Interface iface_stmt1, Interface iface_stmt2 ->
        Cfg.stmt_eq iface_stmt1 iface_stmt2
    | Native s1, Native s2 -> s1 == s2
    | _ -> false
