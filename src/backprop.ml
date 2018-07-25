
(* Louis is in charge of this file! *)

open Core_kernel
module E = Error
open Format

type result =
  | Succeed of SpaceexComponent.id * Z3.Expr.expr
  | Unsuccessful

let backprop ~(pre:SpaceexComponent.id) ~(post:SpaceexComponent.id) ~(pre_fml:Z3.Expr.expr) ~(post_fml:Z3.Expr.expr) ~(dynamics:SpaceexComponent.flow) ~(inv:Z3.Expr.expr) =
  E.raise (E.of_string "backprop: not implemented.")
    
