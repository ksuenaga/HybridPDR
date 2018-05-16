type ('key,'value) t [@@deriving show]
val add : 'key -> 'value -> ('key,'value) t -> ('key,'value) t
val empty : ('key,'value) t
val find_exn : ('key,'value) t -> 'key -> 'value
