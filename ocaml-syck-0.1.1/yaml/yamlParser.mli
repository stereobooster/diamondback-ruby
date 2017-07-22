(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(* Copyright (c) 2007 Christopher R. Waterson *)

(** A parser that will parse and compose YAML content. *)

type t
    (** The parser type. *)

exception Error of string
  (** [YamlParser.error msg] is raised when a problem occurs parsing
      YAML content. *)

val make : unit -> t
  (** [Yaml.make ()] creates a new parser. *)

val parse_string : t -> string -> YamlNode.t
  (** [Yaml.parse_string s] parses [s] as a single YAML document and
      returns the document's root node. *)


