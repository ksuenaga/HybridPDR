open Core_kernel
open Format

module E = Error

let ctx = ref (Z3.mk_context [])
let param = ref (Z3.Params.mk_params !ctx)
let solver =
  Z3.Params.add_bool !param (Z3.Symbol.mk_string !ctx "proof") true;
  Z3.Params.add_bool !param (Z3.Symbol.mk_string !ctx "produce-interpolants") true;  
  let solver = Z3.Solver.mk_simple_solver !ctx in
  (*
  let _ = printf "Help:%s@." (Z3.Solver.get_help solver) in
  let _ = printf "Simplify Help:%s@." (Z3.Expr.get_simplify_help !ctx) in
   *)
  (* Z3.enable_trace "hpdr"; *)
  ref solver

let pp_expr fmt e =
  fprintf fmt "%s" (Z3.Expr.to_string e)
let pp_symbol fmt e =
  fprintf fmt "%s" (Z3.Symbol.to_string e)
  
let set_context param = ctx := Z3.mk_context param
                      
let callZ3 z3expr =
  let open Format in
  (* let _ = printf "callZ3: %s@." (Z3.Expr.to_string z3expr) in *)
  Z3.Solver.push !solver;
  Z3.Solver.add !solver [z3expr];
  let st = Z3.Solver.check !solver [] in
  (* let _ = printf "reply:%s@." (Z3.Solver.string_of_status st) in *)
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

  
let pp_result_callZ3 fmt res =
  match res with
    `Unsat -> fprintf fmt "Unsat"
  | `Sat m -> fprintf fmt "Sat %s" (Z3.Model.to_string m)
  | `Unknown -> fprintf fmt "Unknown"
      
let parse_smtlib2_expr s =
  let open Z3.SMT in
  parse_smtlib2_string !ctx s [] [] [] []

let symbol s =
  Z3.Symbol.mk_string !ctx s
  
let simplify e =
  let module Expr = Z3.Expr in
  let module P = Z3.Params in
  let param = P.mk_params !ctx in
  let () = P.add_bool param (symbol "eq2ineq") true in
  Expr.simplify e (Some(param))

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

let mk_real_numeral_float f =
  let module R = Z3.Arithmetic.Real in
  R.mk_numeral_s !ctx (sprintf "%f" f)

let%test _ =
  let module Expr = Z3.Expr in
  let module R = Z3.Arithmetic.Real in
  Expr.equal (mk_real_numeral_float 3.4) (R.mk_numeral_nd !ctx 34 10)


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
let mk_le e1 e2 =
  (*
  let open Z3.Expr in
  let _ = printf "le@." in
  let _ = printf "e1: %s@." (to_string e1) in
  let _ = printf "e2: %s@." (to_string e2) in
   *)
  Z3.Arithmetic.mk_le !ctx e1 e2
let mk_eq e1 e2 = Z3.Boolean.mk_eq !ctx e1 e2
let mk_and e1 e2 = Z3.Boolean.mk_and !ctx [e1; e2]
let mk_or e1 e2 = Z3.Boolean.mk_or !ctx [e1; e2]
let mk_implies e1 e2 = Z3.Boolean.mk_implies !ctx e1 e2
let mk_not e = Z3.Boolean.mk_not !ctx e
  
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

  (*
let expr_of_model (m:Z3.Model.model) : Z3.Expr.expr =
   *)

    
    (*
let tseitin_cnf
tseitin-cnf 
   *)  
    
(* let make_expr sexp =
 *   E.raise (E.of_string "make_expr: not implemented.") *)
  
(* let%test_module _ =
 *   (module struct
 *      open Sexp
 *      open Z3.Arithmetic.Real
 *      open Z3.Expr
 *      let _ = equal (make_expr (of_string "(int 3)")) (mk_numeral_i !ctx 3)
 *    end) *)

  
let expr_of_model ~(model:Z3.Model.model) : Z3.Expr.expr =
  (* let open Z3Intf in *)
  let module M = Z3.Model in
  let module FD = Z3.FuncDecl in
  let module R = Z3.Arithmetic.Real in
  (* let _ = printf "model:%s@." (M.to_string model) in *)
  let fds = M.get_const_decls model in
  (* let _ = printf "fds:%a" (Util.pp_list pp_print_string) (List.map ~f:FD.to_string fds) in *)
  let interps = List.map ~f:(fun fd -> M.get_const_interp model fd) fds in
  (* let _ = printf "interps:%a" (Util.pp_list (Util.pp_option pp_expr)) interps in *)
  let ids = List.map ~f:FD.get_name fds in
  (* let _ = printf "ids:%a" (Util.pp_list pp_symbol) ids in *)
  let exprs = List.map ~f:(fun interp -> match interp with None -> E.raise (E.of_string "expr_of_model: cannot happen") | Some e -> e) interps in
  (* let _ = printf "exprs:%a" (Util.pp_list pp_expr) exprs in *)
  let mapped = List.map2_exn ~f:(fun id exp -> mk_eq (R.mk_const !ctx id) exp) ids exprs in
  let ret =
    List.fold_left
      ~init:mk_true
      ~f:mk_and
      mapped
  in
  simplify ret
  (* frame_lift_given_id ~locs ~loc (simplify ret); *)
(* E.raise (Error.of_string "frame_of_model: not implemented.") *)

let%test _ =
  (* let open Z3Intf in *)
  (*
  let l1 = id_of_string "1" in
  let l2 = id_of_string "1" in
  let locs = [l1;l2] in
  let cnf1 = Cnf.parse "x==0.0" in
   *)
  let e1 = mk_eq (mk_real_var "x") (mk_real_numeral_s "0.0") in
  (* let _ = printf "e1:%s@." (Z3.Expr.to_string e1) in *)
  let res = callZ3 e1 in
  match res with
  | `Sat model ->
     let e2 = expr_of_model model in
     (* let _ = printf "e2:%s@." (Z3.Expr.to_string e2) in *)
     Z3.Expr.equal e1 e2
  | _ -> false

(* Compute [es/xs]e *)
let substitute (xs:string list) (es:Z3.Expr.expr list) (e:Z3.Expr.expr) : Z3.Expr.expr =
  let syms : Z3.Symbol.symbol list = Z3.Symbol.mk_strings !ctx xs in
  let xs = List.map ~f:(Z3.Arithmetic.Real.mk_const !ctx) syms in
  (simplify (Z3.Expr.substitute e xs es))

(* x + x * y *)
let%test _ =
  let x = mk_real_var "x" in
  let y = mk_real_var "y" in
  let e = mk_eq (mk_add x (mk_mul x y)) (mk_real_numeral_s "8.0") in
  let e1 = substitute ["x";"y"] [(mk_real_numeral_s "2.0");(mk_real_numeral_s "3.0")] e in
  let e2 = substitute ["y";"x"] [(mk_real_numeral_s "3.0");(mk_real_numeral_s "2.0")] e in
  let e3 = substitute ["x";"y"] [(mk_real_numeral_s "1.0");(mk_real_numeral_s "1.0")] e in
  let res1 = callZ3 e1 in
  let res2 = callZ3 e2 in
  let res3 = callZ3 e3 in
  match res1,res2,res3 with
  | `Sat _, `Sat _, `Unsat -> true
  | _ -> false
       
(* Compute [e1/x]e *)
let substitute_one (x:string) (e1:Z3.Expr.expr) (e:Z3.Expr.expr) : Z3.Expr.expr =
  substitute [x] [e1] e
  (*
  let sym = Z3.Symbol.mk_string !ctx x in
  let x = Z3.Arithmetic.Real.mk_const !ctx sym in
  (simplify (Z3.Expr.substitute_one e x e1))
   *)

(* x + x * x *)
let%test _ =
  let x = mk_real_var "x" in
  let e = mk_eq (mk_add x (mk_mul x x)) (mk_real_numeral_s "6.0") in
  let e1 = substitute_one "x" (mk_real_numeral_s "0.0") e in
  let e2 = substitute_one "x" (mk_real_numeral_s "2.0") e in
  let res1 = callZ3 e1 in
  let res2 = callZ3 e2 in
  match res1,res2 with
  | `Unsat, `Sat _ -> true
  | _ -> false
 
let interpolant (e1:Z3.Expr.expr) (e2:Z3.Expr.expr) =
  let open Z3.Interpolation in
  (*
  let _ = printf "Taking interpolant of:@." in
  let _ = printf "e1:%s@." (Z3.Expr.to_string e1) in
  let _ = printf "e2:%s@." (Z3.Expr.to_string e2) in
   *)
  match callZ3 (mk_and e1 e2) with
  | `Unsat ->
     begin
       let e1intp = Z3.Interpolation.mk_interpolant !ctx e1 in
       let params = Z3.Params.mk_params !ctx in
       (*
       let _ =
         printf "e1: %s@." (Z3.Expr.to_string e1);
         printf "e1intp: %s@." (Z3.Expr.to_string e1intp);
         printf "e2: %s@." (Z3.Expr.to_string e2);
         printf "params: %s@." (Z3.Params.to_string params)
       in
        *)
       (* let es = get_interpolant !ctx e1 e2 params in *)
       let _, res, _ = compute_interpolant !ctx (mk_and e1intp e2) params in
       (*
       let _ = printf "Obtained interpolant: %a@."
                 (Util.pp_option (Util.pp_list pp_expr)) res
       in
        *)
       (* Util.not_implemented "interpolant" *)
       match res with
       | None -> `InterpolantNotFound
       | Some (hd::_) ->
          let () =
            let vc1 = callZ3 (mk_and e1 (mk_not hd)) in
            let vc2 = callZ3 (mk_and hd e2) in
            (*
            printf "vc1:%a@." pp_result_callZ3 vc1;
            printf "vc2:%a@." pp_result_callZ3 vc2;
             *)
            assert(vc1 = `Unsat);
            assert(vc2 = `Unsat);
            ()
          in
          `InterpolantFound hd
       | Some [] -> assert(false)
     end
  | _ -> `NotUnsatisfiable

let%test _ =
  let x = mk_real_var "x" in
  (* let y = mk_real_var "y" in *)
  (* let z = mk_real_var "z" in   *)
  let e1 = mk_gt x (mk_real_numeral_s "1.0") in
  let e2 = mk_lt x (mk_real_numeral_s "0.0") in
  let res3 = callZ3 (mk_and e1 e2) in
  (*
  let _ =
    match res3 with
      `Unsat -> printf "OK@."
    | _ -> Util.not_implemented "hoge"
  in
   *)
  let intp = interpolant e1 e2 in
  match intp with
  | `InterpolantFound intp ->
     begin
       let res1 = callZ3 (simplify (mk_not (mk_implies e1 intp))) in
       let res2 = callZ3 (simplify (mk_and intp e2)) in
       match res1,res2,res3 with
       | `Unsat,`Unsat,`Unsat -> true
       | _,_,_ -> false
     end
  | _ -> false

let%test _ =
  let e1 = mk_le (mk_real_var "x") (mk_real_numeral_float 1.0) in
  (* let e1intp = Z3.Interpolation.mk_interpolant !ctx e1 in *)
  let e2 = mk_not (mk_le (mk_real_var "x") (mk_real_numeral_float 1.0)) in
  let res =
    callZ3 (mk_and e1 e2)
  in
  res = `Unsat

let pp_model fmt m =
  (* fprintf fmt "%s" (Z3.Model.to_string m) *)
  fprintf fmt "%s" (Z3.Expr.to_string (expr_of_model m))
