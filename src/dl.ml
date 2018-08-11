open Core_kernel
open Format

module S = SpaceexComponent

module U = Unit

type t =
  | Prim of Z3.Expr.expr
  | Dyn of S.flow * Z3.Expr.expr * t
  | And of t list

let mk_dl_prim e = Prim e

let mk_dl_dyn f e t = Dyn(f,e,t)
         
let mk_dl_and e1 e2 =
  match e1,e2 with
  | And l1, And l2 -> And (l1 @ l2)
  | And l, e | e, And l -> And (e::l)
  | e1,e2 -> And [e1;e2]

let dl_discharge dl =
  Util.not_implemented "dl_discharge"
