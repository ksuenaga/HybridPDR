
type atomic
type t
(* type disj [@@deriving show] *)

val pp : Format.formatter -> t -> unit
   
val set_context : (string * string) list -> unit
val cnf_true : t
val cnf_false : t
val parse : string -> t

val sat_andneg : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
val sat_implication : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
val extract_atomics : t -> atomic list
val cnf_and : t -> t -> t
                          (* val cnf_lift_disj : disj -> t *)
val cnf_lift_atomic : atomic -> t
val z3_to_atomic : Z3.Expr.expr -> atomic

val listlist_to_cnf : Z3.Expr.expr list list -> t

val substitute_one : string -> Z3.Expr.expr -> t -> t
