(* [XXX] We currently do not use CNF. *)

open Core_kernel
open Format

module E = Error
module U = Util

type atomic = Z3.Expr.expr

let pp_atomic fmt a = fprintf fmt "%s" (Z3.Expr.to_string a)
(* let pp_expr fmt e = fprintf fmt "%s" (Z3.Expr.to_string e) *)

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
  let ret =
    List.fold_left
      ~init:(Z3.Boolean.mk_true !ctx)
      ~f:(fun z3expr d ->
        Z3.Boolean.mk_and !ctx [z3expr; (disj_to_z3 d)]) t
  in
  Z3Intf.simplify ret

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
  match sat_andneg cnf_true cnf_true with
    `Unsat -> true
  | _ -> false

let%test _ =
  match sat_andneg cnf_true cnf_false with
  | `Sat _ ->
     true
  | `Unknown | `Unsat ->
     false

let%test _ =
  match sat_andneg cnf_false cnf_true with
    `Unsat -> true
  | _ -> false

let%test _ =
  match sat_andneg cnf_false cnf_false with
    `Unsat -> true
  | _ -> false

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

let rec choose (l : 'a list list) : 'a list list =
  match l with
  | [] -> []
  | [hd] -> List.map ~f:(fun elm -> [elm]) hd
  | hd::tl ->
     let l = choose tl in
     (* [[3];[4]] *)
     List.fold_left ~init:[]
       ~f:(fun res comb ->
         res @ 
           (List.map
              ~f:(fun elm -> elm::comb)
              hd))
       l

     (*
let print_int_list fmt l =
  fprintf fmt "[%a]" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";") (fun fmt n -> fprintf fmt "%d" n)) l
let print_int_list_list fmt l =
  fprintf fmt "[%a]" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";") (fun fmt l -> fprintf fmt "%a" print_int_list l)) l  
     
let%test_module _ =
  (module struct
     let%test _ = choose [] = []
     let%test _ = choose [[]] = []
     let%test _ = choose [[1]] = [[1]]
     let%test _ = choose [[1];[2]] = [[1;2]]
     let%test _ = choose [[1];[2;3]] = [[1;2];[1;3]]
     let%test _ =
       let res = choose [[1;2];[3;4]] in
       let expected = [[1;3];[1;4];[2;3];[2;4]] in
       (*
       printf "res:%a@\n" print_int_list_list res;
       printf "expected:%a@\n" print_int_list_list expected;
        *)
       res=expected
     let%test _ = (List.sort compare (choose [[1;2];[3;4];[5;6]])) = (List.sort compare [[1;3;5];[1;3;6];[1;4;5];[1;4;6];[2;3;5];[2;3;6];[2;4;5];[2;4;6]])
   end)
      *)             
(*
  Example:
  hd1 = (A \/ B) /\ C
  hd2 = (D \/ E) /\ F
  hd1 => hd2 is equivalent to
  (~A /\ ~B) \/ ~C \/ ((D \/ E) /\ F), which is equivalent to
  (~A \/ ~C \/ D \/ E) /\ (~A \/ ~C \/ F) /\ (~B \/ ~C \/ D \/ E) /\ (~B \/ ~C \/ F)
  
 *)
let cnf_implies (hd1:t) (hd2:t) : Z3.Expr.expr =
  let open Z3Intf in
  mk_implies (conj_to_z3 hd1) (conj_to_z3 hd2)
  (*
  let open List in
  let open Z3Intf in
  let hd1 = List.map ~f:(fun l -> List.map ~f:(fun p -> mk_not p) l) hd1 in
  let chosen = choose hd1 in
  let _ = printf "hd1:%a@." pp_cnf hd1 in
  let _ = printf "chosen:%a@." pp_cnf chosen in
  let _ = printf "hd2:%a@." pp_cnf hd2 in  
  List.fold_left ~init:[]
    ~f:(fun res p1 ->
      let l =
        List.map
          ~f:(fun p2 ->
            p1 @ p2
          )
          hd2
      in
      l @ res
    )
  chosen
   *)
let%test _ =
  let open Z3Intf in
  expr_equal (simplify (cnf_implies cnf_false cnf_true)) mk_true

let%test _ =
  let open Z3Intf in
  let implies = simplify (cnf_implies cnf_true cnf_false) in
  let expected = mk_false in
  (*
  let _ = printf "implies:%s@." (Z3.Expr.to_string implies) in
  let _ = printf "expected:%s@." (Z3.Expr.to_string expected) in
   *)
  expr_equal implies mk_false
  
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

let to_z3 = conj_to_z3
