(** A file loader loads source files and keep the control flow graph (CFG) in
    the hashtable. The user may choose to cache them into .ast and .cache files 
    if neded. 'mod_ast' field of type 't' is used to modify ASTs before 
    refactoring into a CFG for handling dynamic features (evals). *)

open Config
open Utils

type c_handler = 
  | EmptyCfg
  | Rename of (string -> string)

(** type of a file loader *)
type t = {
  path : string list;
  tbl : (string,Cfg.stmt) Hashtbl.t;
  c_handler : c_handler;
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
let create handler lib_dirs = 
  { path = paths_from_rubylib lib_dirs;
    c_handler = handler;
    tbl = Hashtbl.create 127;
    cache_files = StrSet.empty;
  }

let update_c_handler t handler = {t with c_handler=handler}

let c_extensions = [".so";".bundle";".o"]

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
  
(** finds the file from the given list of library paths *)
let rec find_in_libpath ?(no_ext=false) f lst = 
  let find_with_ext fullname = 
    let exts = ".rb"::c_extensions in
    let exts = match extension fullname with
      | None -> if no_ext then ""::exts else exts 
      | Some e -> if (List.mem e exts) or no_ext then ""::exts else exts 
    in
      fullname ^ (List.find 
                    (fun e -> 
                       let f = fullname ^ e in
                         Sys.file_exists f && not (is_directory f)
                    ) exts)
  in
  let rec work = function
    | [] -> raise Not_found
    | hd::tl -> 
        Log.note "looking for %s in %s" f hd;
        let fullname = Filename.concat hd f in
          try find_with_ext fullname
          with Not_found -> work tl
  in
    if Filename.is_implicit f then work lst 
    else find_with_ext f

(** converts a relative path to an absolute path *)
let normalize_filename fname = 
  let dir = Filename.dirname fname in
  let file = Filename.basename fname in
  let orig_cwd = Sys.getcwd() in
  let () = Sys.chdir dir in
  let norm = Sys.getcwd() in
    Sys.chdir orig_cwd;
    Filename.concat norm file

let map_handler t file = match t.c_handler with
  | EmptyCfg -> file
  | Rename f -> f file

let is_native f = 
  List.exists (Filename.check_suffix f) c_extensions
  
(** finds a file in the given file loader *)
let find_file ?no_ext t f = 
  let file = find_in_libpath ?no_ext f t.path in
  let file = 
    if is_native file
    then map_handler t file 
    else file
  in
    file

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
      let ast = Parse_helper.parse_file fname in
        cache_ast t fname ast;
        ast

let load_ast t fname = 
  if conf.print_filenames 
  then Printf.eprintf "parsing file %s\n%!" fname;
  if conf.cache 
  then load_cached_ast t fname
  else Parse_helper.parse_file fname

(** loads a file from the given file loader (and caches them if needed) *)
let load_file t ?(mod_ast_f = Utils.id) ?no_ext fname = 
  Log.note "loading file %s" fname;
  try let found_name = find_file ?no_ext t fname in
  let full_name = normalize_filename found_name in
    if (is_native full_name)
    then (Log.note "skipping native file %s" fname; Cfg.empty_stmt ())
    else
      (* look in the hash table and return it if already loaded *)
      try Hashtbl.find t.tbl full_name
        (* otherwise, load it; use cached if exists or cache if requested *)
      with Not_found ->
        let ast = load_ast t found_name in
        let ast = if conf.transform || conf.profile then mod_ast_f ast else ast in
        let cfg = Cfg_refactor.refactor_ast ast in
          Hashtbl.add t.tbl full_name cfg;
          cfg
  with Not_found -> 
    Log.err "*** unable to find file to load %s(%b)\n" 
      fname (default_opt false no_ext);
    Cfg.empty_stmt ()
  
(** updates the cfg of the file in the given file loader with a new one *)
let update_file ?no_ext t fname cfg = 
  let name = find_file ?no_ext t fname in
  let full_name = normalize_filename name in
    Hashtbl.replace t.tbl full_name cfg


(** checks whether the file has been previously loaded *)
let already_loaded ?no_ext t fname = 
  try let full_name = find_file ?no_ext t fname in
    if is_native full_name
    then true
    else Hashtbl.mem t.tbl (normalize_filename full_name)
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

