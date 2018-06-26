open Core_kernel
open Format

type pre_frame = (SpaceexComponent.id,Cnf.t) Env.t [@@deriving show]
let pp_pre_frame fmt (pre:pre_frame) =
  Env.fold
    ~init:()
    ~f:(fun _ (id,cnf) ->
    (* fprintf fmt "(%a -> %s) " SpaceexComponent.pp_id id (Z3.Expr.to_string (Cnf.to_z3 cnf)) *)
      fprintf fmt "(%a -> %a) " SpaceexComponent.pp_id id Cnf.pp cnf
    )
    pre
  
type frame =
  (* Involve only purely continous transition. *)
  | Continuous of pre_frame
  (* May involve continous and discrete transition. *)
  | Hybrid of pre_frame
                [@@deriving show]
(*
let pp_frame fmt (f:frame) =
  match f with
  | Continuous pf -> fprintf fmt "Continuous %a" pp_pre_frame pf
  | Hybrid pf -> fprintf fmt "Hybrid %a" pp_pre_frame pf
 *)

module E = Error
           

let extract_atomics (f:frame) =
  match f with
  | (Continuous pre | Hybrid pre) ->
     Env.fold
      ~init:[]
      ~f:(fun l (_,c) -> (Cnf.extract_atomics c) @ l)
      pre

let lift_frame_pf (f:pre_frame -> pre_frame) (frame:frame) : frame =
  match frame with
  | Continuous pf -> Continuous (f pf)
  | Hybrid pf -> Hybrid (f pf)
;;
    
let frame_and_cnf (frame:frame) d =
  lift_frame_pf (Env.map ~f:(fun c -> Cnf.cnf_and c d)) frame

let frame_lift (locs:SpaceexComponent.id list) (cnf : Cnf.t) (post : pre_frame -> frame) : frame =
  post
    (List.fold_left
       ~init:Env.empty
       ~f:(fun e (l:SpaceexComponent.id) -> Env.add l cnf e)
       locs)
let continuous_frame_lift locs cnf = frame_lift locs cnf (fun pf -> Continuous pf)
let hybrid_frame_lift locs cnf = frame_lift locs cnf (fun pf -> Hybrid pf)

let frame_lift_given_id
      (locs:SpaceexComponent.id list)
      (id:SpaceexComponent.id)
      (cnf:Cnf.t)
      (post : pre_frame -> frame)
    : frame =
  let default = frame_lift locs Cnf.cnf_false post in
  lift_frame_pf (Env.add id cnf) default
let hybrid_frame_lift_given_id locs id cnf = frame_lift_given_id locs id cnf (fun pf -> Hybrid pf)
                               

let equal f1 f2 = Env.equal f1 f2

let pp_locfml fmt (id,e) =
  fprintf fmt "loc %a: %s" SpaceexComponent.pp_id id (Z3.Expr.to_string e)
let pp_locfmls fmt l =
  fprintf fmt "%a" (Util.pp_list pp_locfml) l
                
let strengthen ~(locfmls:(SpaceexComponent.id*Z3.Expr.expr) list) ~(t:frame) : frame =
  let _ = printf "(** strengthen **)@." in
  let _ = printf "Passed frame: %a@." pp_frame t in
  let _ = printf "locfmls: %a@." pp_locfmls locfmls in
  let ret =
    List.fold_left
      ~init:t
      ~f:(fun frame (loc,fml) ->
        let _ = printf "before frame: %a@." pp_frame frame in
        let _ = printf "loc: %a@." SpaceexComponent.pp_id loc in
        let p =
          match t with
          | (Continuous pf | Hybrid pf) -> Env.find_exn pf loc
        in
        let fml = Cnf.cnf_lift_atomic (Cnf.z3_to_atomic fml) in
        let _ = printf "fml: %a@." Cnf.pp fml in
        let _ = printf "p(before): %a@." Cnf.pp p in
        let p = Cnf.cnf_and p fml in
        let _ = printf "p(after): %a@." Cnf.pp p in
        let frame = lift_frame_pf (Env.add loc p) frame in
        let _ = printf "after frame: %a@." pp_frame frame in
        frame
      )
      locfmls
  in
  let _ = printf "Returned frame: %a@." pp_frame ret in
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
      [(id_of_string "1", Cnf.cnf_true);
       (id_of_string "2", Cnf.cnf_false)]
  in
  let e1 = mk_not (mk_eq (mk_add (mk_real_var "y") (mk_neg (mk_real_var "x"))) (mk_real_numeral_float (-.2.0))) in
  let locfmls = 
    [(id_of_string "1", e1);
     (id_of_string "2", mk_false)]
  in
  let frame = Hybrid pf in
  let frame = strengthen ~locfmls:locfmls ~t:frame in
  match frame with
    Hybrid pf ->
     let e1' = simplify e1 in
     let e2cnf = Env.find_exn pf (id_of_string "1") in
     let e2' = simplify (Cnf.to_z3 e2cnf) in
     let _ =
       printf "e1': %s@." (Z3.Expr.to_string e1');
       printf "e2cnf: %a@." Cnf.pp e2cnf;
       printf "e2': %s@." (Z3.Expr.to_string e2')
     in
     Z3Intf.expr_equal e1' e2'
  | _ -> false
  
let mk_lifter f =
  match f with Hybrid _ -> hybrid_frame_lift
             | Continuous _ -> continuous_frame_lift
;;

let preframe_of_frame f =
  match f with Continuous pf | Hybrid pf -> pf

let find_exn frame loc =
  Env.find_exn (preframe_of_frame frame) loc
                                          
let is_valid_implication_frame (e1:frame) (e2:frame) =
  Env.fold2
    (preframe_of_frame e1) (preframe_of_frame e2)
    ~init:`Valid
    ~f:(fun res (loc1,c1) (loc2,c2) ->
      assert(loc1=loc2);
      match res with
      | `Valid | `NotValidNoModel ->
         Cnf.is_valid_implication loc1 c1 c2
      | `NotValid (loc,m) -> `NotValid (loc,m))

let is_valid_implication_cnf (e:frame) (c:Cnf.t) =
  let locs = Env.domain (preframe_of_frame e) in
  is_valid_implication_frame e (hybrid_frame_lift locs c)

let is_continuous_frame f = match f with Continuous _ -> true | Hybrid _ -> false
