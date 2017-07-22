
type 'a t


val create : 'a -> ctx:Log.ctx -> 'a t

val vid : 'a t -> int
val vctx : 'a t -> Log.ctx

val compare : 'a t -> 'a t -> int

val deref : 'a t -> 'a

val same : 'a t -> 'a t -> bool

val union : 'a t -> 'a t -> unit

val format_var : (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a t -> unit


val status : string -> unit
