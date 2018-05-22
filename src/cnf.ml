
open Core
module E = Error

type atomic = Z3.Expr.expr
type disj = atomic list
type cnf = disj list
type t = cnf
(* [] is true *)
(* [[]] is false *)

let ctx = ref (Z3.mk_context [])
let set_context param = ctx := Z3.mk_context param

(* [] is true *)
let cnf_true = []
(* [[]] is false *)
let cnf_false = [[]]

let parse s =
  E.raise (Error.of_string "parse: not implemented.")

(* Check satisfiability of (and i (neg s)). *)
let sat_andneg i s =
  E.raise (Error.of_string "sat_andneg: not implemented.")
