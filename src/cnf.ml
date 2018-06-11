
open Core
module E = Error
module U = Util

type atomic = Z3.Expr.expr
let pp_atomic fmt a = Format.fprintf fmt "%s" (Z3.Expr.to_string a)
                    
type disj = atomic list [@@deriving show]
type cnf = disj list [@@deriving show]
type t = cnf [@@deriving show]
(* [] is true *)
(* [[]] is false *)
       
let parse s =
  ParseFml.parse s;
  E.raise (E.of_string "not implemented: parse")

let set_context = Z3Intf.set_context
let ctx = Z3Intf.ctx

(* [] is true *)
let cnf_true = []
(* [[]] is false *)
let cnf_false = [[]]


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
  let q = (conj_to_z3 cnf_true) in
  let st = callZ3 q in
  let res =
    match st with
    | `Unsat | `Unknown -> false
    | `Sat _ -> true
  in
  res

let%test _ =
  let open Z3Intf in
  let q = (conj_to_z3 cnf_false) in
  let st = callZ3 q in
  let res =
    match st with
    | `Unsat -> true
    | `Sat _ | `Unknown  -> false
  in
  res
  
(* Check satisfiability of (and i (neg s)). *)
(* [XXX] not tested *)
let sat_andneg (i:cnf) (s:cnf) =
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
let sat_implication conj1 conj2 =
  let open Z3Intf in
  let zexp1 = conj_to_z3 conj1 in
  let zexp2 = conj_to_z3 conj2 in
  let q = Z3.Boolean.mk_and !ctx [zexp1; zexp2] in
  let st = callZ3 q in
  st

let rec extract_disjuncts (hd:t) : disj list =
  E.raise (E.of_string "extract_disjuncts: not implemented.")

(* [XXX] not tested *)
let cnf_and hd1 hd2 = hd1 @ hd2

(* [XXX] not tested *)
let cnf_lift_disj d = [d]
