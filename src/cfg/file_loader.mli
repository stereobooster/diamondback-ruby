
(** The abstract type for a file loader *)
type t

(** The list of file extensions for C objects (.so, .o, etc...) *)
val c_extensions : string list

(** Returns true if the file given by string has a C extension *)
val is_native : string -> bool

(** Determines the behavior of the loader with regard to loading
    native object files (.so, .o, etc..).  [EmptyCfg] causes the
    loader to return an empty Cfg for any file with a C filename
    extension.  [Rename f] first attempts to rename any file with a C
    filename extension by calling [f] on the absolute path of the
    file.  This mapping should produce an absolute file name which can
    be loaded directly (without recursively searching the library
    paths).  If the renamed file is itself a native object, an empty
    cfg is returned.
*)
type c_handler = 
  | EmptyCfg
  | Rename of (string -> string)

(** Create a new file loader with the specified c_handler and list of
    library directories *)
val create : c_handler -> string list -> t

(** Return a new file loader which looks up objct files according to
    the scheme [c_handler].  The new loader shares the same
    (imperative) cache and as the initial loader. *)
val update_c_handler : t -> c_handler -> t

(** Return the list of directories used to find a ruby file *)
val lib_dirs : t -> string list

(** Like [lib_dirs], but returns the list packed into a string with
    the ':' character as a separater *)
val lib_dirs_string : t -> string

(** Return an equivalent filename that contains no relative
     directories.  Does not treat symbolic links specially.  *)
val normalize_filename : string -> string

(** [find_in_libpath file paths] Attempts to find a ruby file in the
    list of paths.  The search for the file adds suffixes such as .rb
    or .so if it does not already have a suffix.  If ~no_ext is set to
    true, then the file is searched for without a ruby compatible
    suffix (as is done by Kernel#load).  If set to false (the
    default), only files with suffixes are return (as is done by
    Kernel#require).  Raises [Not_found] if the file is not found. *)
val find_in_libpath : ?no_ext:bool -> string -> string list -> string

(** Similar to find_in_libpath, but uses the path list stored
    internally in first argument.  The search may also be altered
    based on the current c_handler scheme. *)
val find_file : ?no_ext:bool -> t -> string -> string

(** [load_file t ?modf fname] Attempt to load the file [fname] from
    disk or cache.  If the argument [modf] is present and the file has
    not already been loaded, it is applied to the intermediate AST
    before constructing the returned Cfg.  If the file is not found,
    an error is printed via Log.err and the empty Cfg is returned.  *)
val load_file : t -> ?mod_ast_f:(Ast.expr list -> Ast.expr list) -> ?no_ext:bool
  -> string -> Cfg.stmt

(** [update_file t file cfg] Update the internal cache for [file] to
    point to [cfg].  Subsequent calls to [find_file t file] will
    return [cfg].  The argument [file] does not have to exist in the
    cache prior to this call.  Raises [Not_found] if the [file] can
    not be found on disk *)
val update_file : ?no_ext:bool -> t -> string -> Cfg.stmt -> unit

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

