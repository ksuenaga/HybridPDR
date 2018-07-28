open Core_kernel
open Format

type pre_frame = (SpaceexComponent.id,Cnf.t) Env.t [@@deriving show]
let pp_pre_frame fmt (pre:pre_frame) =
  Env.fold
    ~init:()
    ~f:(fun _ (id,cnf) ->
      fprintf fmt "(%a -> %a) " SpaceexComponent.pp_id id Cnf.pp cnf)
    pre
  
type frame = pre_frame [@@deriving show]
module E = Error
           
let extract_atomics (f:frame) =
  Env.fold
    ~init:[]
    ~f:(fun acc (loc,c) ->
      (List.map ~f:(fun c -> (loc,c)) (Cnf.extract_atomics c)) @ acc)
    f

let frame_and_cnf (frame:frame) d =
  Env.map ~f:(fun c -> Z3Intf.simplify (Z3Intf.mk_and c d)) frame

let frame_lift (locs:SpaceexComponent.id list) (cnf : Cnf.t) : frame =
  List.fold_left
    ~init:Env.empty
    ~f:(fun e (l:SpaceexComponent.id) -> Env.add l cnf e)
    locs
let frame_lift_given_id
      (locs:SpaceexComponent.id list)
      (id:SpaceexComponent.id)
      (cnf:Cnf.t)
    : frame =
  let default = frame_lift locs Z3Intf.mk_false in
  Env.add id cnf default
let equal f1 f2 = Env.equal f1 f2

let pp_locfml fmt (id,e) =
  fprintf fmt "loc %a: %s" SpaceexComponent.pp_id id (Z3.Expr.to_string e)
let pp_locfmls fmt l =
  fprintf fmt "%a" (Util.pp_list pp_locfml) l
                
let strengthen ~(locfmls:(SpaceexComponent.id*Z3.Expr.expr) list) ~(t:frame) : frame =
  let ret =
    List.fold_left
      ~init:t
      ~f:(fun frame (loc,fml) ->
        (*
        let _ = printf "before frame: %a@." pp_frame frame in
        let _ = printf "loc: %a@." SpaceexComponent.pp_id loc in
         *)
        let p = Env.find_exn t loc in
        (* let fml = Cnf.cnf_lift_atomic (Cnf.z3_to_atomic fml) in *)
        (*
        let _ = printf "fml: %a@." Cnf.pp fml in
        let _ = printf "p(before): %a@." Cnf.pp p in
         *)
        let p = Z3Intf.simplify (Z3Intf.mk_and p fml) in
        (* let _ = printf "p(after): %a@." Cnf.pp p in *)
        let frame = Env.add loc p frame in
        (* let _ = printf "after frame: %a@." pp_frame frame in *)
        frame
      )
      locfmls
  in
  (* let _ = printf "Returned frame: %a@." pp_frame ret in *)
  ret

let mk_pf_from_assoclist l : pre_frame =
  List.fold_left
    ~init:Env.empty
    ~f:(fun env (l,cnf) ->
      Env.add l cnf env
    )
    l
  
let%test _ =
  let open Z3Intf in
  let open SpaceexComponent in
  let pf =
    mk_pf_from_assoclist
      [(id_of_string "1", Z3Intf.mk_true);
       (id_of_string "2", Z3Intf.mk_false)]
  in
  let e1 = mk_not (mk_eq (mk_add (mk_real_var "y") (mk_neg (mk_real_var "x"))) (mk_real_numeral_float (-.2.0))) in
  let locfmls = 
    [(id_of_string "1", e1);
     (id_of_string "2", mk_false)]
  in
  (* let frame = pf in *)
  let frame = strengthen ~locfmls:locfmls ~t:pf in
  let e1' = simplify e1 in
  let e2cnf = Env.find_exn frame (id_of_string "1") in
  let e2' = simplify e2cnf in
  let _ =
    printf "e1': %s@." (Z3.Expr.to_string e1');
    printf "e2cnf: %a@." Cnf.pp e2cnf;
    printf "e2': %s@." (Z3.Expr.to_string e2')
  in
  Z3Intf.expr_equal e1' e2'

let find_exn frame loc = Env.find_exn frame loc
                                          
let is_valid_implication_frame (e1:frame) (e2:frame) =
  Env.fold2
    e1 e2
    ~init:`Valid
    ~f:(fun res (loc1,c1) (loc2,c2) ->
      assert(loc1=loc2);
      match res with
      | `Valid | `NotValidNoModel ->
         Cnf.is_valid_implication loc1 c1 c2
      | `NotValid (loc,m) -> `NotValid (loc,m))

let is_valid_implication_cnf (e:frame) (c:Cnf.t) =
  let locs = Env.domain e in
  is_valid_implication_frame e (frame_lift locs c)

