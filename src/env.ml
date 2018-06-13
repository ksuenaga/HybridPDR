open Core

type ('key,'value) t = ('key * 'value) list [@@deriving show]

let rec add k v l =
  (* (k,v)::l *)
  match l with
    [] -> [k,v]
  | (k',v')::tl ->
     if k = k' then
       (k,v)::tl
     else
       (k',v')::(add k v tl)
              
let empty = []
let find_exn l k = List.Assoc.find_exn l ~equal:(=) k
let fold ~init ~f env = List.fold_left ~init:init ~f:f env
let map ~f env = List.map ~f:(fun (x,v) -> (x,f v)) env

let sort_by_key env = List.sort ~compare:(fun (k1,_) (k2,_) -> compare k1 k2) env
               
let fold2 ~init ~f env1 env2 =
  let env1,env2 = sort_by_key env1, sort_by_key env2 in
  List.fold2_exn env1 env2 ~init:init ~f:(fun a (k1,v1) (k2,v2) -> assert(k1=k2); f a (k1,v1) (k2,v2))

let domain t = List.map ~f:fst t
