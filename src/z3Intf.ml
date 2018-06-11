open Core
open Format

module E = Error

let ctx = ref (Z3.mk_context [])
let solver = ref (Z3.Solver.mk_simple_solver !ctx)
let set_context param = ctx := Z3.mk_context param
                      
let callZ3 z3expr =
  Z3.Solver.push !solver;
      Z3.Solver.add !solver [z3expr];
      let st = Z3.Solver.check !solver [] in
      let res =
        match st with
        | Z3.Solver.UNSATISFIABLE ->
           `Unsat
        | Z3.Solver.SATISFIABLE ->
           let m = Z3.Solver.get_model !solver in
           begin
             match m with
               Some m' -> `Sat m'
             | None ->
                E.raise (E.of_string "sat_andneg: cannot happen")
           end
        | Z3.Solver.UNKNOWN ->
           `Unknown
      in
      Z3.Solver.pop !solver 1;
      res
      
let parse_smtlib2_expr s =
  let open Z3.SMT in
  parse_smtlib2_string !ctx s [] [] [] []
  
let make_expr sexp =
  E.raise (E.of_string "make_expr: not implemented.")
  
let%test_module _ =
  (module struct
     open Sexp
     open Z3.Arithmetic.Real
     open Z3.Expr
     let _ = equal (make_expr (of_string "(int 3)")) (mk_numeral_i !ctx 3)
   end)
  
