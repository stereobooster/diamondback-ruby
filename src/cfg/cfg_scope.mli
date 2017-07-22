
type t

val create : File_loader.t -> t

val resolve_scopes : t -> Cfg.stmt -> Cfg.stmt
  (** Return a new Cfg where all constants have been resolved to an
      absolute scope *)

val special_case_yaml : string -> string
  (** Hack for working around the Yaml lib.  This function works as
      the identity function for all strings except those that include
      "/yaml.rb".  In this case, a stub filename is returned
      instead. *)
