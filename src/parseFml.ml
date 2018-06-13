open Core
open Format

open MParser
module Regex = MakeRegexp(MParser_RE.Regexp)
open Regex
open Tokens

module E = Error
   
type unop = Neg
[@@deriving show]

type binop = Mul | Div | Add | Sub | Gt | Ge | Lt | Le | Eq | And | Or
[@@deriving show]                                                                  
                                                                  
type expr =
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Ident of string
  | Float of float
[@@deriving show]           

let infix assoc p op =
  Infix (p |>> (fun _ a b -> (Binop (op, a, b))), assoc)
let prefix p op =
  Prefix (p |>> (fun _ a -> (Unop (op, a))))

let operators =
  [
    [ prefix (symbol "-") Neg ];
    [
      infix Assoc_left (symbol "*") Mul;
      infix Assoc_left (symbol "/") Div;
    ];
    [
      infix Assoc_left (symbol "+") Add;
      infix Assoc_left (symbol "-") Sub;
    ];
    [
      infix Assoc_left (symbol ">") Gt;
      infix Assoc_left (symbol ">=") Ge;
      infix Assoc_left (symbol "<")  Lt;
      infix Assoc_left (symbol "<=") Le;
      infix Assoc_left (symbol "==") Eq;
    ];
    [
      infix Assoc_left (symbol "&") And;
      infix Assoc_left (symbol "|") Or;
    ];
  ]

let parse_ident = regexp (make_regexp "[a-zA-Z][a-zA-Z0-9_]*") |>> (fun x -> Ident x)
let parse_float = float |>> (fun f -> Float f)
let parse_integer = integer |>> (fun i -> Float (float_of_int i))

let rec basic =
  spaces >> (parse_ident <|> parse_float <|> parse_integer) << spaces

(* [XXX] Support of parenthesis is buggy. *)
let rec expr s =
  ((expression operators (basic <|> parens expr)) << eof) s

  (*
let rec expr s =
  ((expression operators (basic <|> term)) << eof) s
and term s = (parens expr) s
   *)

  (*
let rec expr_to_cnf res : Z3.Expr.expr list list =
  let open Z3Intf in
  let float f = mk_real_numeral_s (string_of_float f) in
  (* [XXX] Consider the type of x *)
  let ident x = mk_real_var x in
  let neg a = mk_neg a in
  (* let binop f a1 a2 = mk_binop f a1 a2 in *)
  match res with
  (* | `Int i -> float i *)
  | Unop(op, a) ->
     begin
       match op with
       | Neg ->  [[(neg (expr_to_z3 a))]]
     end
  | Binop (op, a, b) ->
     let op_f = 
       match op with
       | Add -> mk_add
       | Sub -> mk_sub
       | Mul -> mk_mul
       | Div -> mk_div
       | Gt -> mk_gt
       | Ge -> mk_ge
       | Lt -> mk_lt
       | Le -> mk_le
       | Eq -> mk_eq
       | And -> mk_and
       | Or -> mk_or
     in
     op_f (expr_to_z3 a) (expr_to_z3 b)
  | Float f -> float f
  | Ident x ->  ident x
   *)
  
let rec expr_to_z3 (res:expr) : Z3.Expr.expr =
  let open Z3Intf in
  let float f = mk_real_numeral_s (string_of_float f) in
  (* [XXX] Consider the type of x *)
  let ident x = mk_real_var x in
  let neg a = mk_neg a in
  match res with
  | Unop(op,a) ->
    begin
      match op with
        Neg -> neg (expr_to_z3 a)
    end
  | Float f -> float f
  | Ident x -> ident x
  | Binop(op,a,b) ->
     begin
       let op_f =
         match op with
         | Add -> mk_add
         | Sub -> mk_sub
         | Mul -> mk_mul
         | Div -> mk_div
         | _ -> E.raise (E.of_string "expr_to_z3: binop: should not occur")
       in
       op_f (expr_to_z3 a) (expr_to_z3 b)
     end

(* E.raise (E.of_string "expr_to_z3: not implemented.") *)
       (*
     begin
       match op with
       | Neg ->  [[(neg (expr_to_z3 a))]]
     end
      *)
         (*
          *)
  (*
  | Float f -> float f
  | Ident x ->  ident x
   *)
              
let rec predexpr_to_cnf (res:expr) : Z3.Expr.expr list list =
  let open Z3Intf in
  match res with
  | Unop(op, a) ->
     begin
       match op with
       | Neg -> E.raise (E.of_string "predexpr_to_cnf: unop: should not occur")
     end
  | Binop (op, a, b) ->
     begin
       match op with
       | Add | Sub | Mul | Div -> E.raise (E.of_string "predexpr_to_cnf: binop: should not occur")
       | Gt -> [[mk_gt (expr_to_z3 a) (expr_to_z3 b)]]
       | Ge -> [[mk_ge (expr_to_z3 a) (expr_to_z3 b)]]
       | Lt -> [[mk_lt (expr_to_z3 a) (expr_to_z3 b)]]
       | Le -> [[mk_le (expr_to_z3 a) (expr_to_z3 b)]]
       | Eq -> [[mk_eq (expr_to_z3 a) (expr_to_z3 b)]]
       | And -> (predexpr_to_cnf a) @ (predexpr_to_cnf b)
       | Or ->
          let cnf_a, cnf_b = (predexpr_to_cnf a), (predexpr_to_cnf b) in
          List.rev
            (List.fold_left
               ~init:[]
               ~f:(fun cnf disja ->
                 let res =
                   List.fold_left
                     ~init:cnf
                     ~f:(fun cnf disjb -> (disja @ disjb)::cnf)
                     cnf_b
                 in
                 res @ cnf)
               cnf_a)
     end
  | Float _ | Ident _ ->
     printf "res:%a@." pp_expr res;
     E.raise (E.of_string "predexpr_to_cnf: value: should not occur")

let parse_to_cnf s =
  match MParser.parse_string expr s () with
  | Success e ->
  (* expr_to_z3 e *)
     predexpr_to_cnf e
  (* e *)
  | Failed (msg, e) ->
     failwith msg

let%test_module _ =
  (module struct
     open Z3Intf

     let disj_to_z3 disj : Z3.Expr.expr = Z3.Boolean.mk_or !ctx disj
     let cnf_to_z3 cnf : Z3.Expr.expr = Z3.Boolean.mk_and !ctx (List.map ~f:disj_to_z3 cnf)
       (*
       List.fold_left
         ~f:(fun z3 d -> mk_and z3 (disj_to_z3 d))
         ~init:mk_true
         cnf
        *)

                                      (*
     let%test _ =
       let s = parse_to_cnf " x " in
       let s = cnf_to_z3 s in
       expr_equal s (mk_real_var "x")
                                       *)
                                      
     let%test _ =
       let s = parse_to_cnf "x >= y" in
       let s = cnf_to_z3 s in
       let expected = cnf_to_z3 [[(mk_ge (mk_real_var "x") (mk_real_var "y"))]] in
       (*
       let _ = printf "s:%s@." (Z3.Expr.to_string s) in
       let _ = printf "expected:%s@." (Z3.Expr.to_string expected) in
        *)
       expr_equal s expected
       
     let%test _ =
       let s = parse_to_cnf "4*x+10*x >= y & z >= 5 | z >= 6" in
       let s = Z3Intf.simplify (cnf_to_z3 s) in
       (* let _ = printf "s:%s@." (Z3.Expr.to_string s) in *)
       let expected =
         Z3Intf.simplify
           (cnf_to_z3
              [[(mk_ge
                   (mk_add
                      (mk_mul
                         (mk_real_numeral_s "4.0")
                         (mk_real_var "x"))
                      (mk_mul
                         (mk_real_numeral_s "10.0")
                         (mk_real_var "x")))
                   (mk_real_var "y"));
                (mk_ge
                   (mk_real_var "z")
                   (mk_real_numeral_s "6.0"))];
               [(mk_ge
                   (mk_real_var "z")
                   (mk_real_numeral_s "5.0"));
                (mk_ge
                   (mk_real_var "z")
                   (mk_real_numeral_s "6.0"))]])
       in
       (* let _ = printf "expected:%s@." (Z3.Expr.to_string expected) in *)
       expr_equal s expected
   end)

let assignment_parser s =
  ((spaces >> parse_ident << spaces) >>=
     (fun x ->
       match x with
         Ident x' -> 
          (spaces >> regexp (make_regexp ":=") >> spaces) >> expr >>= (fun e -> return (x',expr_to_z3 e))
       | _ ->
          E.raise (E.of_string "parse_assignment: malformed assignment."))) s

let parse_assignment s =
  match MParser.parse_string assignment_parser s () with
  | Success e ->
     e     
  | Failed (msg, e) ->
     failwith msg
  
