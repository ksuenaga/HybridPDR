
open Format

type t
val pp : formatter -> t -> unit
   
module S = SpaceexComponent

val mk_dl_prim : Z3.Expr.expr -> t
val mk_dl_dyn : S.flow -> Z3.Expr.expr -> t -> t
val mk_dl_and : t -> t -> t
val mk_dl_or : t -> t -> t

(* val dl_discharge : t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ] *)

                          (* val dl_elim_dyn : t -> Z3.Expr.expr *)

val is_valid_implication : t -> t -> [> `Valid | `NotValid of Z3.Model.model | `Unknown ]
val interpolant : t -> t -> [> `InterpolantFound of Z3.Expr.expr | `InterpolantNotFound | `NotUnsatisfiable ]

