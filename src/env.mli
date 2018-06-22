type ('key,'value) t [@@deriving show]
val add : 'key -> 'value -> ('key,'value) t -> ('key,'value) t
val empty : ('key,'value) t
val find_exn : ('key,'value) t -> 'key -> 'value
val fold : init:'a -> f:('a -> 'key * 'value -> 'a) -> ('key,'value) t -> 'a
val map : f:('value -> 'value) -> ('key,'value) t -> ('key,'value) t
val fold2 : init:'a -> f:('a -> ('key * 'value) -> ('key * 'value) -> 'a) -> ('key,'value) t -> ('key,'value) t -> 'a
val domain : ('key,'value) t -> 'key list
val equal : ('key,'value) t -> ('key,'value) t -> bool
