type ('key,'value) t [@@deriving show]
val add : 'key -> 'value -> ('key,'value) t -> ('key,'value) t
val empty : ('key,'value) t
val find_exn : ('key,'value) t -> 'key -> 'value
val fold : init:'a -> f:('a -> 'key * 'value -> 'a) -> ('key,'value) t -> 'a
val map : f:('key * 'value1 -> 'value2) -> ('key,'value1) t -> ('key,'value2) t
val map2 : f:('value1 -> 'value2 -> 'value3) -> ('key,'value1) t -> ('key,'value2) t -> ('key,'value3) t
val fold : init:'a -> f:('a -> ('key * 'value) -> 'a) -> ('key,'value) t -> 'a  
val fold2 : init:'a -> f:('a -> ('key * 'value1) -> ('key * 'value2) -> 'a) -> ('key,'value1) t -> ('key,'value2) t -> 'a
val domain : ('key,'value) t -> 'key list
val equal : ('key,'value) t -> ('key,'value) t -> bool
