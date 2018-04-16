type 'a t

val empty : 'a t
val add : Syntax.id -> 'a -> 'a t -> 'a t
val remove : Syntax.id -> 'a t -> 'a t

val find : Syntax.id -> 'a t -> 'a
val mem : Syntax.id -> 'a t -> bool
