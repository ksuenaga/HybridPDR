type 'a t [@@deriving show]
val add : 'a -> 'a t -> 'a t
val empty : 'a t
val find_exn : 'a t -> f:('a -> bool) -> 'a

val fold : init:('a) -> f:('a -> 'b -> 'a) -> 'b t -> 'a
val filter : f:('a -> bool) -> 'a t -> 'a t
