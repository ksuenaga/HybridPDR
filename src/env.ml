open Core

type ('key,'value) t = ('key * 'value) list [@@deriving show]

let add k v l = (k,v)::l
let empty = []
let find_exn l k = List.Assoc.find_exn l ~equal:(=) k
