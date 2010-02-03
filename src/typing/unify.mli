

type 'a t

val create : 'a -> 'a t

val current : 'a t -> 'a

val value : 'a t -> 'a

val ecr : 'a t -> 'a t

val union : 'a t -> 'a t -> 'a t

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
