type 'a t [@@deriving show]
val add : 'a -> 'a t -> 'a t
val empty : 'a t
val find_exn : 'a t -> f:('a -> bool) -> 'a
