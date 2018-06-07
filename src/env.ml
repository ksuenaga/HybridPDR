open Core

type ('key,'value) t = ('key * 'value) list [@@deriving show]

let add k v l = (k,v)::l
let empty = []
let find_exn l k = List.Assoc.find_exn l ~equal:(=) k
let fold ~init ~f env = List.fold_left ~init:init ~f:f env
let map ~f env = List.map ~f:(fun (x,v) -> (x,f v)) env
let fold2 ~init ~f env1 env2 =
  List.fold2_exn env1 env2 ~init:init ~f:f

let domain t = List.map ~f:fst t
