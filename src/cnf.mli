
(* type atomic *)
type t = Z3.Expr.expr
val pp : Format.formatter -> t -> unit
val parse : string -> t
(* val extract_atomics : t -> t list *)
val is_valid_implication : 'a -> t -> t -> [> `NotValid of 'a * Z3.Model.model | `NotValidNoModel | `Valid ]
                                             
  (*
val set_context : (string * string) list -> unit
val cnf_true : t
val cnf_false : t
val parse : string -> t

val sat_andneg : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
val sat_implication : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]



val cnf_and : t -> t -> t
val cnf_implies : t -> t -> Z3.Expr.expr
(* val cnf_lift_disj : disj -> t *)
  (*
val cnf_lift_atomic : atomic -> t
val z3_to_atomic : Z3.Expr.expr -> atomic
   *)

val listlist_to_cnf : Z3.Expr.expr list list -> t

val substitute_one : string -> Z3.Expr.expr -> t -> t

val to_z3 : t -> Z3.Expr.expr

   *)
