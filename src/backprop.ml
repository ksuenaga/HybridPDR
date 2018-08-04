
(* Louis is in charge of this file! *)

(*****************************************************************************
	This module is used for backpropagation.
	It determines whether or not a state n can be reached from a state n+1	 
	using the reversed dynamics that led the state n to the state n+1.
	SUCCESS : n+1 	reached 		n 	using 	reversed dynamics
	FAILURE : n+1 	did NOT reach 	n 	using 	reversed dynamics
*****************************************************************************)

open Core_kernel
module E = Error
open Format

(* expected result for the function backprop *)
type result =
  | Succeed of SpaceexComponent.id * Z3.Expr.expr   	(* ID AND formula of the reached state *)
  | Unsuccessful				      	(* might as well send more information *)

(* definition of utility function: get_inv_dynam *)
let get_inv_dynam (locs:SpaceexComponent.id list) (orig_dynam:SpaceexComponent.flow) : SpaceexComponent.flow = 
  (* determine the inverted dynamics from the original dynamics orig_dynam *)
  let open Env in
  let open Z3Intf in
  let flow_exp_list = List.map ~f:(fun x -> Env.find_exn orig_dynam (SpaceexComponent.string_of_id x)) locs in  (* x or x' ?  *)
  let flow_exp_list = List.map ~f:(fun x -> Z3Intf.mk_neg x) flow_exp_list in
  let inv_dynam =
    List.fold2_exn
      locs
      flow_exp_list
      ~init:Env.empty
      ~f:(fun env k z3 -> Env.add (SpaceexComponent.string_of_id k) z3 env)
  in
  inv_dynam
(* E.raise (E.of_string "get_inv_dynam: not implemented.") *)

(* definition of utility function: random_point *)
let get_unsafe_fml (safe_fml:Z3.Expr.expr) : Z3.Expr.expr =
  (* find the formula corresponding to an unsafe state *)
  E.raise (E.of_string "unsafe_fml: not implemented.")

let get_random_unsafe_state (unsafe_fml:Z3.Expr.expr) : Z3.Expr.expr =
  E.raise (E.of_string "get_random_unsafe_state: not implemented.")

(* definition of utility function: simul_backprop *)
let simul_backprop (inv_dynam:SpaceexComponent.flow) (unsafe_state:Z3.Expr.expr) : result =
  (* simulate backpropagation for a given state and dynamics *)
  E.raise (E.of_string "simul_backprop: not implemented.")

(*)
(* definition of utility function: loop_simul *)
let rec loop_simul (dynam:SpaceexComponent.flow) unsafe_states : result =
(* simulate backpropagation for a list of states *)
  match unsafe_states with
  | [] -> Unsuccessful
  | x::l -> 
    let backprop_result = simul_backprop dynam x in
    if backprop_result = Succeed then backprop_result else loop_simul dynam l 

  E.raise (E.of_string "simul_backprop: not implemented.")*)

(* definition of main function: backprop *)
let backprop ~(locs:SpaceexComponent.id list) ~(pre:SpaceexComponent.id) ~(post:SpaceexComponent.id) ~(pre_fml:Z3.Expr.expr) ~(post_fml:Z3.Expr.expr) ~(dynamics:SpaceexComponent.flow) ~(inv:Z3.Expr.expr) : result =		        
      (* ID of state n *)  (* ID of state n+1 *)  (* formula for state n *)  (* formula for state n+1 *)  (* dynamics used to go from n to n+1 *)  (* invariant *)
  let open SpaceexComponent in
  let open Z3Intf in
  let inv_dynam = get_inv_dynam locs dynamics in
  let unsafe_fml = get_unsafe_fml post_fml in
  let backprop_result = simul_backprop inv_dynam (get_random_unsafe_state unsafe_fml) in
  backprop_result
    (* E.raise (E.of_string "backprop: not implemented.") *)
    

