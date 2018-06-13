
(* [XXX] We currently do not use CNF. *)

open Core
open Format
module E = Error
module U = Util

type atomic = Z3.Expr.expr

let pp_atomic fmt a = Format.fprintf fmt "%s" (Z3.Expr.to_string a)
(* let pp_expr fmt e = Format.fprintf fmt "%s" (Z3.Expr.to_string e) *)

type disj = atomic list [@@deriving show]
type cnf = disj list [@@deriving show]
type t = cnf [@@deriving show]

(* [] is true *)
(* [[]] is false *)

(* type t = Z3.Expr.expr *)
       
let set_context = Z3Intf.set_context
let ctx = Z3Intf.ctx

(* let pp fmt t = Format.fprintf fmt "%s" (Z3.Expr.to_string t) *)


let parse s = ParseFml.parse_to_cnf s

(* [] is true *)
let cnf_true : t = []
(* [[]] is false *)
let cnf_false : t = [[]]
            
(* [XXX] not tested *)
let disj_to_z3 (t:disj) : Z3.Expr.expr =
  List.fold_left
    ~init:(Z3.Boolean.mk_false !ctx)
    ~f:(fun z3expr atomic ->
      Z3.Boolean.mk_or !ctx [z3expr; atomic]) t

(* [XXX] not tested *)
let conj_to_z3 (t:cnf) : Z3.Expr.expr =
  List.fold_left
    ~init:(Z3.Boolean.mk_true !ctx)
    ~f:(fun z3expr d ->
      Z3.Boolean.mk_and !ctx [z3expr; (disj_to_z3 d)]) t


let%test _ =
  let open Z3Intf in
  expr_equal (conj_to_z3 cnf_true) mk_true
let%test _ =
  let open Z3Intf in
  let p = simplify (conj_to_z3 cnf_false) in
  (* let _ = printf "s:%s@." (Z3.Expr.to_string p) in *)
  expr_equal p mk_false
  
let%test _ =
  let open Z3Intf in
  let q = conj_to_z3 cnf_true in
  (* let _ = printf "cnf_true: %a@." pp_expr q in *)
  let st = callZ3 q in
  let res =
    match st with
    | `Unsat | `Unknown -> false
    | `Sat _ -> true
  in
  res

let%test _ =
  let open Z3Intf in
  let q = conj_to_z3 cnf_false in
  let st = callZ3 q in
  let res =
    match st with
    | `Unsat -> true
    | `Sat _ | `Unknown  -> false
  in
  res
  
(* Check satisfiability of (and i (neg s)). *)
(* [XXX] not tested *)
let sat_andneg (i:t) (s:t) =
  let open Z3Intf in
  let iz3 = conj_to_z3 i in
  let sz3 = conj_to_z3 s in
  let q = Z3.Boolean.mk_and !ctx [iz3; (Z3.Boolean.mk_not !ctx sz3)] in
  let st = callZ3 q in
  st

let%test _ =
  sat_andneg cnf_true cnf_true = `Unsat

let%test _ =
  match sat_andneg cnf_true cnf_false with
  | `Sat _ ->
     true
  | `Unknown | `Unsat ->
     false

let%test _ =
  sat_andneg cnf_false cnf_true = `Unsat

let%test _ =
  sat_andneg cnf_false cnf_false = `Unsat

(* [XXX] not tested *)  
let sat_implication (conj1:t) (conj2:t) =
  let open Z3Intf in
  let zexp1 = conj_to_z3 conj1 in
  let zexp2 = conj_to_z3 conj2 in
  let q = mk_implies zexp1 zexp2 in
  let st = callZ3 q in
  st

let rec extract_atomics (hd:t) : atomic list =
  List.dedup_and_sort ~compare:Z3.Expr.compare (List.concat hd)
  (*
  let module Expr =  Z3.Expr in
  let module A =  Z3.AST in
  let open Sexp in
  let open Format in
  let sexp = Sexp.of_string (Expr.to_string hd) in
  let _ = printf "sexp: %a@." Sexp.pp sexp in
  let rec sexp_to_atomics s =
    match s with
      Atom "true" -> []
    | Atom "false" -> []
    | List ((Atom s)::tl) ->
       begin
         E.raise (E.of_string "extract_atomics: not implemented.")         
       end
    | Atom _ -> E.raise (E.of_string "extract_atomics: atom should not appear here.")
    | List _ -> E.raise (E.of_string "extract_atomics: list malformed.")
  in
  sexp_to_atomics sexp
   *)
  
(* [XXX] not tested *)
let cnf_and hd1 hd2 = hd1 @ hd2

(* [XXX] not tested *)
                                    (* let cnf_lift_disj d = [d] *)

let cnf_lift_atomic a = [[a]]
let z3_to_atomic e = e

let listlist_to_cnf ll = ll

let substitute_one x e cnf =
  let open Z3Intf in
  List.map
    ~f:(fun d ->
      List.map
        ~f:(fun a ->
          let x = mk_real_var x in
          Z3.Expr.substitute_one x e a)
        d)
    cnf
