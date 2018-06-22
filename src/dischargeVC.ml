open Core_kernel
open Format

module E = Error

type cont_triple_partial =
  { pre_loc_partial : SpaceexComponent.id;
    post_loc_partial : SpaceexComponent.id;
    pre_partial : Z3.Expr.expr;
    post_partial : Z3.Expr.expr;
    dynamics_partial : SpaceexComponent.flow;
    inv_partial : Cnf.t }

type cont_triple_total =
  { pre_loc_total : SpaceexComponent.id;
    post_loc_total : SpaceexComponent.id;
    pre_total : Z3.Expr.expr;
    post_total : Z3.Expr.expr;
    dynamics_total : SpaceexComponent.flow;
    inv_total : Cnf.t }

let pp_cont_triple_partial fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %a }"
    SpaceexComponent.pp_id crp.pre_loc_partial
    SpaceexComponent.pp_id crp.post_loc_partial
    (Z3.Expr.to_string crp.pre_partial)
    (Z3.Expr.to_string crp.post_partial)
    SpaceexComponent.pp_flow crp.dynamics_partial
    Cnf.pp crp.inv_partial
let pp_cont_triple_total fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %a }"
    SpaceexComponent.pp_id crp.pre_loc_total
    SpaceexComponent.pp_id crp.post_loc_total
    (Z3.Expr.to_string crp.pre_total)
    (Z3.Expr.to_string crp.post_total)
    (* SpaceexComponent.pp_id crp.pre_loc *)
    SpaceexComponent.pp_flow crp.dynamics_total
    Cnf.pp crp.inv_total


(* [XXX] Premature rough implementation *)
let discharge_vc_total ~(triple:cont_triple_total) : (SpaceexComponent.id * Z3.Model.model) option =
  let open Z3Intf in
  let _ = printf "discharge_vc_total: %a@." pp_cont_triple_total triple in
  let res = callZ3 (mk_and triple.pre_total triple.post_total) in
  match res with
  | `Unsat | `Unknown -> None
  | `Sat m -> Some(triple.pre_loc_total, m)
(* E.raise (E.of_string "discharge_vc_total: not implemented.") *)

(* [XXX] to be implememnted. *)
let discharge_vc_partial vc =
  false
    (* E.raise (E.of_string "discharge_vc_partial: not implemented.") *)
