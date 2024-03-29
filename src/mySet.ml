open Core_kernel

type 'a t = 'a list [@@deriving show]
let add x t = x::t
let empty = []
let find_exn t ~f =
  List.find_exn t ~f

let fold ~init ~f set =
  List.fold_left ~init ~f set

let filter ~f t =
  List.filter ~f:f t
