type 'a t

val make : (unit -> 'a) -> ('a -> unit) -> 'a t

val const : 'a -> 'a t

val ref : 'a -> 'a t

val get : 'a t -> 'a

val set : 'a t -> 'a -> unit

val combine : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val observe : ('a -> unit) -> 'a t -> 'a t
