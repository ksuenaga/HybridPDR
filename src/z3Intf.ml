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

let simplify e =
  let module Expr = Z3.Expr in
  Expr.simplify e None

let expr_equal e1 e2 = Z3.Expr.equal e1 e2
  
let mk_real_var s =
  let module Sym = Z3.Symbol in
  let module R = Z3.Arithmetic.Real in
  R.mk_const !ctx (Sym.mk_string !ctx s)

let%test _ =
  let module Expr = Z3.Expr in
  let module R = Z3.Arithmetic.Real in
  Expr.equal (mk_real_var "x") (R.mk_const_s !ctx "x")

let mk_real_numeral_s s =
  let module R = Z3.Arithmetic.Real in
  R.mk_numeral_s !ctx s

let%test _ =
  let module Expr = Z3.Expr in
  let module R = Z3.Arithmetic.Real in
  Expr.equal (mk_real_numeral_s "3.4") (R.mk_numeral_nd !ctx 34 10)

let mk_neg e =
  let module A = Z3.Arithmetic in
  let module R = A.Real in
  A.mk_mul !ctx [e; R.mk_numeral_nd !ctx (-1) 1]

let%test _ =
  let module Expr = Z3.Expr in
  let module R = Z3.Arithmetic.Real in
  Expr.equal (simplify (mk_neg (mk_real_numeral_s "3"))) (mk_real_numeral_s "-3")

let mk_true = Z3.Boolean.mk_true !ctx
let mk_false = Z3.Boolean.mk_false !ctx
            
let mk_add e1 e2 = Z3.Arithmetic.mk_add !ctx [e1; e2]
let mk_sub e1 e2 = Z3.Arithmetic.mk_sub !ctx [e1; e2]
let mk_mul e1 e2 = Z3.Arithmetic.mk_mul !ctx [e1; e2]
let mk_div e1 e2 = Z3.Arithmetic.mk_div !ctx e1 e2
let mk_gt e1 e2 = Z3.Arithmetic.mk_gt !ctx e1 e2
let mk_ge e1 e2 = Z3.Arithmetic.mk_ge !ctx e1 e2
let mk_lt e1 e2 = Z3.Arithmetic.mk_lt !ctx e1 e2
let mk_le e1 e2 = Z3.Arithmetic.mk_le !ctx e1 e2
let mk_eq e1 e2 = Z3.Boolean.mk_eq !ctx e1 e2
let mk_and e1 e2 = Z3.Boolean.mk_and !ctx [e1; e2]
let mk_or e1 e2 = Z3.Boolean.mk_or !ctx [e1; e2]
  
let%test_module _ =
  (module struct
     module Expr = Z3.Expr
     module R = Z3.Arithmetic.Real
     module B = Z3.Boolean

     let%test _ =
       Expr.equal
         (simplify (mk_add (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         (mk_real_numeral_s "7")
       
     let%test _ =
       Expr.equal
         (simplify (mk_add (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         (mk_real_numeral_s "7")

     let%test _ =
       Expr.equal
         (simplify (mk_mul (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         (mk_real_numeral_s "12")

     let%test _ =
       Expr.equal
         (simplify (mk_div (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         (mk_real_numeral_s "3/4")

     let%test _ =
       Expr.equal
         (simplify (mk_gt (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         mk_false

     let%test _ =
       Expr.equal
         (simplify (mk_ge (mk_real_numeral_s "4") (mk_real_numeral_s "4")))
         mk_true

     let%test _ =
       Expr.equal
         (simplify (mk_lt (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         mk_true
       
     let%test _ =
       Expr.equal
         (simplify (mk_le (mk_real_numeral_s "3") (mk_real_numeral_s "3")))
         mk_true
       
     let%test _ =
       Expr.equal
         (simplify (mk_eq (mk_real_numeral_s "3") (mk_real_numeral_s "4")))
         mk_false

     let%test _ =
       Expr.equal
         (simplify (mk_and mk_true mk_false))
         mk_false

     let%test _ =
       Expr.equal
         (simplify (mk_or mk_true mk_false))
         mk_true
       
end)

(* let make_expr sexp =
 *   E.raise (E.of_string "make_expr: not implemented.") *)
  
(* let%test_module _ =
 *   (module struct
 *      open Sexp
 *      open Z3.Arithmetic.Real
 *      open Z3.Expr
 *      let _ = equal (make_expr (of_string "(int 3)")) (mk_numeral_i !ctx 3)
 *    end) *)
