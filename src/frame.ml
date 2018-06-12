open Core

type frame = (SpaceexComponent.id,Cnf.t) Env.t

let extract_disjuncts (f:frame) =
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
  let default = frame_lift locs cnf in
  Env.add id cnf default

