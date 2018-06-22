open Core_kernel
open Format

type frame = (SpaceexComponent.id,Cnf.t) Env.t [@@deriving show]

module E = Error
           
let pp_frame fmt frame =
  Env.fold
    ~init:()
    ~f:(fun _ (id,cnf) ->
      fprintf fmt "(%a -> %s) " SpaceexComponent.pp_id id (Z3.Expr.to_string (Cnf.to_z3 cnf)))
    frame

let extract_atomics (f:frame) =
  Env.fold
    ~init:[]
    ~f:(fun l (_,c) -> (Cnf.extract_atomics c) @ l)
    f
    
let frame_and_cnf frame d =
  Env.map ~f:(fun c -> Cnf.cnf_and c d) frame

let frame_lift (locs:SpaceexComponent.id list) (cnf : Cnf.t) =
  List.fold_left
    ~init:Env.empty
    ~f:(fun e (l:SpaceexComponent.id) -> Env.add l cnf e)
    locs

let frame_lift_given_id
      (locs:SpaceexComponent.id list)
      (id:SpaceexComponent.id)
      (cnf:Cnf.t) =
  let default = frame_lift locs Cnf.cnf_false in
  Env.add id cnf default

let find_exn ~loc ~frame =
  Env.find_exn frame loc


let equal f1 f2 = Env.equal f1 f2
  
