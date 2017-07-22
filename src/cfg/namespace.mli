

type 'a t
  (** The type of a namespace hierachy. *)

type 'a cursor
  (** A cursor that points into the middle of a namespace hierachy *)
  
type scope = 
  | Absolute_Scope of string list
  | Relative_Scope of string list
      (** A description of the location of an element in the
	  hierarchy.  When parsing a Relative_Scope, the special
	  string ".." is interpreted as the parent scope. *)

val empty : 'a t
  (** An empty hierarchy *)

val root : scope
  (** The scope which points to the root of the tree *)

val to_cursor : 'a t -> 'a cursor
  (** Create a cursor rooted at the hierarchy argument*)

val from_cursor : 'a cursor -> 'a t
  (** Returns the root of the hierarchy in which cursor points *)

val at_root : 'a cursor -> bool

val current_scope : 'a cursor -> scope
  (** Return the current absolute scope *)

val append_scope : scope -> string -> scope
  (** [append_scope scope str] Create a new scope by appending [str]
      onto the end of [scope] *)

val absolute_scope : scope -> 'a cursor  -> scope
  (** Convert a scope to an absolute scope by prepending the current
      scope. *)

val lookup_scope : scope -> 'a cursor -> scope
  (** [lookup_scope scope curs] Convert a relative scope into an
      asbolute scope by trying to locate the scope in the current
      hierarchy. Raises [Not_found] if [scope] can not be located in
      the hierarchy. *)

val move_to_scope : scope -> 'a cursor -> 'a cursor
  (** [move_to_scope scope curs] Moves the cursor 'curs' to the scope
      'scope'. *)

val enter_scope : string -> 'a cursor -> 'a cursor
  (** [enter_scope s curs] Descend to the scope named 's' at the
      current cursor 'curs'.  Equivalent to
      "move_to_scope (Relative_Scope [s]) curs" 
  *)

val leave_scope : 'a cursor -> 'a cursor
  (** [leave_scope s curs] Leaves the current scope ascending to the
      parent scope of 'curs'.  Equivalent to "move_to_scope
      (Relative_Scope [".."]) curs" *)

val do_in_scope : ('a cursor -> 'a cursor) -> scope -> 'a cursor -> 'a cursor
  (** [do_in_scope f s c] Enters the scope 's', executes 'f', and
      returns to the original the scope.  'f' is free to return a
      cursor pointing to a different location, however, the original
      scope must still be reachable (not removed) by the returned
      cursor, otherwise the function will raise Failure *)

val fold_in_scope : ('a -> 'b cursor -> 'a * 'b cursor) -> 'a -> scope
  -> 'b cursor -> 'a * 'b cursor
  (** [fold_in_scope f acc s c] Enters the scope 's' yielding a new
      scope 'curs', executes 'f acc curs', and returns to the original
      the scope.  'f' is free to return a cursor pointing to a
      different location, however, the original scope must still be
      reachable (not removed) by the returned cursor, otherwise the
      function will raise Failure *)

(** construction *)

val add : string -> 'a -> 'a cursor -> 'a cursor
  (** [add b v c] Add the binding 'b' with value 'v' at the current
      point in the hierarchy.  If 'b' already exists, the previous
      bindings are kept. *)

val replace : string -> 'a -> 'a cursor -> 'a cursor
  (** [replace bind val curs] Replace the current value of the binding
      [bind] positioned at cursor [curs] with the value [val].   Does
      not require [bind] to be already present.
  *)

val add_link : scope -> 'a cursor -> 'a cursor
  (** [add_link s c] Add a link from the current scope to another
      position in the tree.  Any bindings in the scope s will be
      reachable from the resulting cursor provided they are not
      overriden by Ruby's scoping rules. *)

val mem : string -> 'a cursor -> bool
  (** [mem s curs] Check if the string [s] exists in the namespace
      hierarchy starting at the point [curs].  Returns true if [s] is
      a child of any node on the path from [curs] to the root of the
      hierarchy.*)

val find : string -> 'a cursor -> 'a option
  (** [find s curs] Lookup up the string [s] in the namespace
      hierarchy starting at the point [curs].  Returns the value of
      [s] if [s] is a child of any node on the path from [curs] to the
      root of the hierarchy.  If [s] is not in the hierarchy, returns
      [None].  *)

val mem_local : string -> 'a cursor -> bool
  (** [mem_local s curs] Checks to see if [s] is a child of the node
      pointed to by [curs].  Unlike {!Namespace.mem}, it does not
      traverse the hierarchy looking for a binding at other points in
      the tree. *)
  
val find_local : string -> 'a cursor -> 'a option
  (** [find_local s curs] Similar to {!Namespace.find} but does not
      recursively traverse the hierarchy.  Returns [None] if the [s]
      is not a child of the node pointed to by [curs].  *)

val current_val : 'a cursor -> 'a
  (** [current_val curs] Returns the value at position [curs].  If the
      cursor is at the root, raises [Failure "current_val"]. *)

(** Random helpers *)

val dump_children : 'a cursor -> string

val string_of_curs : 'a cursor -> string
  (** Returns the current scope of [curs] as a string. *)

val string_of_scope : scope -> string
  (** Formats the scope as a '::' delimited string.  Absolute scopes
      are prefixed by '::' *)

val scope_of_identifier : Cfg.identifier -> scope
  (** [scope_of_identifier e c] returns the scope of the identifier
      'e'.  For example, the identifier "A::B::c", would produce
      Relative_Scope ["A";"B";"c"] while the the absolute expression
      "::D::E::f" would produce Absolute_Scope ["D";"E";"f"].  *)

val scope_of_identifier_split : Cfg.identifier -> scope * string
  (** [scope_of_identifier_split e c] Returns the same list as
      scope_of_identifier except the last string is returned
      separately.  For example, the identifier "A::B::c", would
      produce (Relative_Scope ["A";"B"],"c").
  *)

