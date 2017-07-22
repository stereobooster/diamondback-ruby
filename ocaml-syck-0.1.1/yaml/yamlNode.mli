(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(* Copyright (c) 2007 Christopher R. Waterson *)

(** A node in a YAML document. *)

type uri = string
    (** A fully-qualified URI that indicates the {i type} of the
        node. *)

type t =
    | SCALAR of uri * string
        (** [SCALAR (type, value)] is a scalar node. *)
    | SEQUENCE of uri * t list
        (** [SEQUENCE (type, nodes)] is a sequence of nodes. *)
    | MAPPING of uri * (t * t) list
        (** [MAPPING (type, map)] is a mapping of keys to values. *)
(** The three {i node kinds}, scalar, sequence, and mapping. *)

