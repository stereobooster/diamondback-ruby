
type t

val create : File_loader.t -> t

val special_case_yaml : string -> string
  (** Hack for working around the Yaml lib.  This function works as
      the identity function for all strings except those that include
      "/yaml.rb".  In this case, a stub filename is returned
      instead. *)

val resolve_scopes : t -> File_loader.load_result -> File_loader.load_result
  (** Return a new load_result where all constants have been resolved to an
      absolute scope *)
