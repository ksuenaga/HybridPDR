type 'a t = 'a list [@@deriving show]
let add x t = x::t
let empty = []
let find_exn t ~f =
  List.find f t
