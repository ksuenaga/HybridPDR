open Core_kernel
open Format

module E = Error

type cont_triple_partial =
  { pre_loc_partial : SpaceexComponent.id;
    post_loc_partial : SpaceexComponent.id;
    pre_partial : Z3.Expr.expr;
    post_partial : Z3.Expr.expr;
    dynamics_partial : SpaceexComponent.flow;
    inv_partial : Z3.Expr.expr }

type cont_triple_total =
  { pre_loc_total : SpaceexComponent.id;
    post_loc_total : SpaceexComponent.id;
    pre_total : Z3.Expr.expr;
    post_total : Z3.Expr.expr;
    dynamics_total : SpaceexComponent.flow;
    inv_total : Z3.Expr.expr }

let pp_cont_triple_partial fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %s }"
    SpaceexComponent.pp_id crp.pre_loc_partial
    SpaceexComponent.pp_id crp.post_loc_partial
    (Z3.Expr.to_string crp.pre_partial)
    (Z3.Expr.to_string crp.post_partial)
    SpaceexComponent.pp_flow crp.dynamics_partial
    (Z3.Expr.to_string crp.inv_partial)
let pp_cont_triple_total fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %s }"
    SpaceexComponent.pp_id crp.pre_loc_total
    SpaceexComponent.pp_id crp.post_loc_total
    (Z3.Expr.to_string crp.pre_total)
    (Z3.Expr.to_string crp.post_total)
    (* SpaceexComponent.pp_id crp.pre_loc *)
    SpaceexComponent.pp_flow crp.dynamics_total
    (Z3.Expr.to_string crp.inv_total)
  
let rec backward_simulation
          ?(discretization_rate = 0.01)
          ~(pre_loc : SpaceexComponent.id)
          ~(post : Z3.Expr.expr)
          ~(flow : SpaceexComponent.flow)
          ~(inv : Z3.Expr.expr)
          ~(pre : Z3.Expr.expr)
          ~(history : Z3.Expr.expr list) =
  let open Z3Intf in
  let checkCE = callZ3 (mk_and post pre) in
  match checkCE with
    `Sat m -> `Propagated(pre_loc, m)
  | (`Unsat | `Unknown) ->
     let newpost = mk_and inv (SpaceexComponent.prev_time ~discretization_rate ~flow ~post) in
     let checkConflict = callZ3 newpost in
     match checkConflict with
     | `Unsat ->
       (* Post is empty.  We found a conflict.  Compute an interpoalnt and return it. *)
       let e1 = simplify pre in
       let e2 = simplify (List.fold_left ~init:mk_false ~f:mk_or (newpost::history)) in
       let _ = printf "e1:%s@." (Z3.Expr.to_string e1) in
       let _ = printf "e2:%s@." (Z3.Expr.to_string e2) in                            
       let intp = interpolant e1 e2 in
       let res =
         begin
           match intp with
           | `InterpolantFound intp ->
              let intp = simplify intp in
              let _ = printf "Obtained interpolant (at %a): %s@." SpaceexComponent.pp_id pre_loc (Z3.Expr.to_string intp) in
              `Conflict(pre_loc,intp)
           | `InterpolantNotFound ->
              Util.not_implemented "intp not found"
           | `NotUnsatisfiable ->
              assert(false)
         end
       in
       res
     | (`Unknown | `Sat _) ->
        (* Post may be still nonempty but CE is not found.  Go further. *)
        backward_simulation
          ~discretization_rate ~pre_loc ~post:newpost ~flow
          ~inv ~pre ~history:(post::history)
  
  (*
  (* Check whether post is already empty. *)
  let checkPostEmpty = callZ3 (mk_and inv post) in
  (* Check whether there is a CE. *)
  let checkCE = callZ3 (mk_and (mk_and pre inv) post) in
  match checkCE, checkPostEmpty with
  | `Sat m, _ ->
     (* Counterexample found *)
     `Propagated(pre_loc, m)
  | (`Unsat | `Unknown), (`Sat _ | `Unknown) ->
     (* Post may be still nonempty but CE is not found.  Go further. *)
     let newpost = SpaceexComponent.prev_time ~discretization_rate ~flow ~post in
     backward_simulation
       ~discretization_rate ~pre_loc ~post:newpost ~flow
       ~inv ~pre ~history:(post::history)
  | (`Unsat | `Unknown), `Unsat ->
   *)
     
let%test _ =
  let open Z3Intf in
  let pre = (mk_le (mk_real_var "x") (mk_real_numeral_float 1.0)) in
  let post = (mk_and
               (mk_le (mk_real_var "y") (mk_real_numeral_float 0.0))
               (mk_eq (mk_real_var "x") (mk_real_numeral_float 2.0))) in
  let res =
    backward_simulation
      ~discretization_rate:0.01
      ~pre_loc:(SpaceexComponent.id_of_string "1")
      ~post:post
      ~flow:(SpaceexComponent.parse_flow "y'==x & x'==-y")
      ~inv:(mk_ge (mk_real_var "y") (mk_real_numeral_float 0.0))
      ~pre:pre
      ~history:[]
  in
  match res with
  | `Conflict(pre_loc,intp) ->
     let vc1 = callZ3 (mk_and pre (mk_not intp)) in
     let vc2 = callZ3 (mk_and intp post) in
     let cond = (pre_loc = (SpaceexComponent.id_of_string "1")) in
     begin
       match vc1,vc2,cond with
         `Unsat,`Unsat,true -> true
       | _ -> false
     end
  | _ -> false
                  
(*  Util.not_implemented "backward_simulation" *)
  

(* [XXX] Premature rough implementation *)
let discharge_vc_total ~(triple:cont_triple_total) =
  let open Z3Intf in
  (* let _ = printf "discharge_vc_total: %a@." pp_cont_triple_total triple in *)
  (*  in *)
  let _ =
    printf "Post: %s@." (Z3.Expr.to_string triple.post_total);
    printf "Pre loc: %a@." SpaceexComponent.pp_id triple.pre_loc_total;
    printf "Pre: %s@." (Z3.Expr.to_string triple.pre_total);
    printf "Inv: %s@." (Z3.Expr.to_string triple.inv_total);
    printf "Flow: %a@." SpaceexComponent.pp_flow triple.dynamics_total
  in
  let res =
    backward_simulation
      ~discretization_rate:1.0
      ~pre_loc:triple.pre_loc_total
      ~post:triple.post_total
      ~flow:triple.dynamics_total
      ~inv:triple.inv_total
      ~pre:triple.pre_total
      ~history:[]
  in
  res
(* E.raise (E.of_string "discharge_vc_total: not implemented.") *)

(* [XXX] to be implememnted. *)
let discharge_vc_partial vc =
  false
    (* E.raise (E.of_string "discharge_vc_partial: not implemented.") *)
