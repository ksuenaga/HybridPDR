
type t
module S = SpaceexComponent

val mk_dl_prim : Z3.Expr.expr -> t
val mk_dl_dyn : S.flow -> Z3.Expr.expr -> t -> t
val mk_dl_and : t -> t -> t

val dl_discharge : t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
