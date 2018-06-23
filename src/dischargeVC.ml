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
  (* Check whether post is already empty. *)
  let checkPostEmpty = callZ3 post in
  (* Check whether there is a CE. *)
  let checkCE = callZ3 (mk_and pre post) in
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
     (* Post is empty.  We found a conflict.  Compute an interpoalnt and return it. *)
     Util.not_implemented "discharge_vc_total(interpolant)"
                  
(*  Util.not_implemented "backward_simulation" *)
  

(* [XXX] Premature rough implementation *)
let discharge_vc_total ~(triple:cont_triple_total) =
  let open Z3Intf in
  (* let _ = printf "discharge_vc_total: %a@." pp_cont_triple_total triple in *)
  (*  in *)
  let res =
    backward_simulation
      ~discretization_rate:0.1
      ~pre_loc:triple.pre_loc_total
      ~post:triple.post_total
      ~flow:triple.dynamics_total
      ~inv:triple.inv_total
      ~pre:triple.pre_total
      ~history:[]
  in
  res
  (*
   *)
(* E.raise (E.of_string "discharge_vc_total: not implemented.") *)

(* [XXX] to be implememnted. *)
let discharge_vc_partial vc =
  false
    (* E.raise (E.of_string "discharge_vc_partial: not implemented.") *)
