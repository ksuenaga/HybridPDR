open Core_kernel
open Format

module S = SpaceexComponent

type t =
  | Prim of Z3.Expr.expr
  | Dyn of S.flow * Z3.Expr.expr * t
