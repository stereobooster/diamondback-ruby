
type env

val c_handler : File_loader.c_handler
  (** Rewriter that maps native files into the !{Config.stub_dir}
      directory *)

val type_cfg : File_loader.t -> string -> Cfg.stmt -> env
  (** [type_cfg loader name stmt] Generates typing contraints for the
      program named [name] whose body is [stmt] *)

val type_in_env : env -> string -> Cfg.stmt -> env
  (** Similar to {!type_cfg}, but takes an [env] type as an argument
      to facilitate the incremental generation of contraints *)

val solve_constraints : env -> unit
  (** Solve the current set of typing constraints *)
