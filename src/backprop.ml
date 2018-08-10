
(* Louis is in charge of this file! *)

(*
  This module is used for backpropagation.

  It determines whether or not a state n (supposedly safe) can be reached from a state n+1 (unsafe)	 
  To do so, we generate random unsafe final states:     get_random_unsafe_state
  Then, we simulate its backpropagation:                simul_backprop
  We can also check if the invariant is respected:      discretize

  We finally return the result:
  SUCCESS : n+1 (unsafe)     reached            n       ->      n is actually not safe       
  FAILURE : n+1 (unsafe)     did NOT reach 	n       ->      n can be safe
 *)

open Core_kernel
module E = Error
open Format

(* expected result for the main function backprop *)
type result =
  | Succeed of SpaceexComponent.id * Z3.Expr.expr   	(* ID AND state of the reached unsafe state *)
  | Unsuccessful				      	(* might as well send more information *)

(* return type for the methods discretize and simul_backprop *)
type resultSimul =
  | Possible of Z3.Expr.expr
  | Unpossible

(* 
   get_inv_dynam 
   determine the inverted dynamics from the original dynamics orig_dynam
   EDIT: not used for now, might be usefull later
 *)
(*
let get_inv_dynam (locs:SpaceexComponent.id list) (orig_dynam:SpaceexComponent.flow) : SpaceexComponent.flow = 
  let open Env in
  let open Z3Intf in
  let flow_exp_list = List.map ~f:(fun x -> Env.find_exn orig_dynam (SpaceexComponent.string_of_id x)) locs in
  let flow_exp_list = List.map ~f:(fun x -> Z3Intf.mk_neg x) flow_exp_list in
  let inv_dynam =
    List.fold2_exn
      locs
      flow_exp_list
      ~init:Env.empty
      ~f:(fun env k z3 -> Env.add (SpaceexComponent.string_of_id k) z3 env)
  in
  inv_dynam
 *)

(* exception defined in case of issue with the unsafe_fml *)
exception CannotSample of Z3.Expr.expr

(* 
   get_random_unsafe_state
   generates an unsafe state according to the unsafe formula
 *)
let get_random_unsafe_state (unsafe_fml:Z3.Expr.expr) : Z3.Expr.expr =
  let open Z3Intf in
  let p = callZ3 unsafe_fml in
  match p with
  | `Sat m -> expr_of_model m
  | `Unsat | `Unknown -> raise (CannotSample unsafe_fml)

(* 
   discretize
   checks whether or not the invariant is respected all allong the backpropagation process
 *)
let rec discretize (dynamics:SpaceexComponent.flow) (unsafe_state:Z3.Expr.expr) (inv:Z3.Expr.expr) (discretization_it:int) (discretization_rate:float) : resultSimul =
  let open Z3Intf in
  let discret = ref (SpaceexComponent.prev_time discretization_rate dynamics unsafe_state) in
  let checkConflict = callZ3 (mk_and inv !discret) in
   match checkConflict with
   | `Sat m -> if (discretization_it > 0) then
                    discretize dynamics !discret inv (discretization_it - 1) discretization_rate
                  else
                    Possible(!discret)
   | `Unsat | `Unknown -> Unpossible   
                       
(* 
   simul_backprop 
   tries to back propagate a final state from an unsafe formula to an initial state from a safe formula
 *)
let rec simul_backprop (dynamics:SpaceexComponent.flow) (unsafe_fml:Z3.Expr.expr) (pre_fml:Z3.Expr.expr) (inv:Z3.Expr.expr) (tryTimes:int) (discretization_rate:float) : resultSimul =
  let open Z3Intf in
  let unsafe_state = get_random_unsafe_state unsafe_fml in
  let simul = discretize dynamics unsafe_state inv (1/(int_of_float discretization_rate)) discretization_rate in
  match simul with
  | Possible backpropagated ->
                begin 
                  let compare = callZ3 (mk_and pre_fml backpropagated) in
                  match compare with
                  | `Sat m -> Possible(expr_of_model m)
                  | `Unsat | `Unknown -> if (tryTimes > 0)
                              then simul_backprop dynamics (mk_and (mk_not unsafe_state) unsafe_fml) pre_fml inv (tryTimes-1) discretization_rate
                              else Unpossible
                end
  | Unpossible -> if (tryTimes > 0)
                  then simul_backprop dynamics (mk_and (mk_not unsafe_state) unsafe_fml) pre_fml inv (tryTimes-1) discretization_rate
                  else Unpossible               
  
(* 
   definition of main function: backprop 
 *)
let backprop ~(pre:SpaceexComponent.id) ~(pre_fml:Z3.Expr.expr) ~(post_fml:Z3.Expr.expr) ~(dynamics:SpaceexComponent.flow) ~(inv:Z3.Expr.expr) ~(safe:Z3.Expr.expr) ~(tryTimes:int) ~(discretization_rate:float) : result =		        
  let open SpaceexComponent in
  let open Z3Intf in
  let unsafe_fml = mk_and (mk_not safe) post_fml in
  let backprop_result = simul_backprop dynamics unsafe_fml pre_fml inv tryTimes discretization_rate  in
  match backprop_result with
  | Possible p -> Succeed(pre,p)
  | Unpossible -> Unsuccessful


(* 
   definition of main function: backprop 
   EDIT: this version is using the get_inv_dynam method and is therefore NOT VALID currently 
 *)
                    (*
let backprop ~(locs:SpaceexComponent.id list) ~(pre:SpaceexComponent.id) ~(post:SpaceexComponent.id) ~(pre_fml:Z3.Expr.expr) ~(post_fml:Z3.Expr.expr) ~(dynamics:SpaceexComponent.flow) ~(inv:Z3.Expr.expr) ~(safe:Z3.Expr.expr) ~(tryTimes:int) (discretization_rate:float) : result =		        
  let open SpaceexComponent in
  let open Z3Intf in
  let inv_dynam = get_inv_dynam locs dynamics in
  let unsafe_fml = mk_and (mk_not safe) post_fml in
  let backprop_result = simul_backprop inv_dynam unsafe_fml pre_fml inv tryTimes discretization_rate  in
  match backprop_result with
  | Possible p -> Succeed(pre,p)
  | Unpossible -> Unsuccessfu
                     *)
    

