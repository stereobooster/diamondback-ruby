
(** The abstract type for a file loader *)
type t

(** The type of a source file:  *)
type src_file =
  | RubyFile of string
  | InterfaceFile of string
  | NativeFile of string

type load_result = 
  | Ruby of Cfg.stmt
  | Interface of Cfg.stmt
  | Native of string

(** Create a new file loader with the specified list of library directories *)
val create : string list -> t

(** Returns true if the file given by string has a C extension *)
val has_native_ext : string -> bool

(** True if the file is a C extension *)
val is_native : src_file -> bool

(** Return the list of directories used to find a ruby file *)
val lib_dirs : t -> string list

(** Like [lib_dirs], but returns the list packed into a string with
    the ':' character as a separater *)
val lib_dirs_string : t -> string

(** [find_in_libpath file paths] Attempts to find a ruby file in the
    list of paths.  The search for the file adds suffixes such as .rb
    or .so if it does not already have a suffix.  If ~no_ext is set to
    true, then the file is searched for without a ruby compatible
    suffix (as is done by Kernel#load).  If set to false (the
    default), only files with suffixes are return (as is done by
    Kernel#require).  
    If ~no_iface_override is false and the ruby file begins with
    ".rb", will try to find a corresponding ".rbi" file.
    Raises [Not_found] if the file is not found. *)
val find_in_libpath : ?no_ext:bool -> ?no_iface:bool -> string -> string list -> src_file

(** Similar to find_in_libpath, but uses the path list stored
    internally in first argument. *)
val find_file : ?no_ext:bool -> ?no_iface:bool -> t -> string -> src_file

(** [load_file t ?modf fname] Attempt to load the file [fname] from
    disk or cache.  If the argument [modf] is present and the file has
    not already been loaded, it is applied to the intermediate AST
    before constructing the returned load result.  If the file is not found,
    an error is printed via Log.err and an empty cfg is returned.  *)
val load_file : t -> ?mod_ast_f:(Ast.expr list -> Ast.expr list) -> ?no_ext:bool
  -> ?no_iface:bool -> string -> load_result

(** [update_file t file load_result] Update the internal cache for [file] to
    point to [load_result].  Subsequent calls to [find_file t file] will
    return [load_result].  The argument [file] does not have to exist in the
    cache prior to this call.  Raises [Not_found] if the [file] can
    not be found on disk *)
val update_file : ?no_ext:bool -> t -> string -> load_result -> unit

(** [already_loaded t file] returns true if a previous call to
    [find_file t file] returned successfully *)
val already_loaded : ?no_ext:bool -> t -> string -> bool

(** Constructs a string of the form "RUBYLIB=paths" where [paths] is a
    colon (:) separated list of paths used by the file loader.
    *)
val mk_rubylib_env : t -> string

(** [write_cache_file t cfile] Writes out the list of cached Cfgs
    files to the file [cfile.cache]. *)
val write_cache_file : t -> string -> unit

(** [clean_cached cfile] Removes all cached files referenced by the
    file [cfile.cache] *)
val clean_cached : string -> unit

(** Same as {!Utils.normalize_filename}, lifted to the src_file type *)
val normalize_src_file : src_file -> src_file

(** Extract the name of a source file *)
val src_name : src_file -> string

(** Temporary; Replaces native and interface load_results with empty Cfgs *)
val load_result_to_cfg : load_result -> Cfg.stmt
                                          
(** Compare two load results. *)
val eql_load_result : load_result -> load_result -> bool
