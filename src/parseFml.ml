open Core
open Format

open MParser
module Regex = MakeRegexp(MParser_RE.Regexp)
open Regex
open Tokens

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


let rec expr_to_z3 res : Z3.Expr.expr =
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
       | Neg ->  neg (expr_to_z3 a)
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

let parse s =
  match MParser.parse_string expr s () with
  | Success e ->
     expr_to_z3 e
  (* e *)
  | Failed (msg, e) ->
     failwith msg

let%test_module _ =
  (module struct
     open Z3Intf
     
     let%test _ =
       let s = parse " x " in
       expr_equal s (mk_real_var "x")

     let%test _ =
       let s = parse "x >= y" in
       expr_equal s (mk_ge (mk_real_var "x") (mk_real_var "y"))
       
     let%test _ =
       let s = parse "4*x+10*x >= y & z >= 5" in
       expr_equal s 
         (mk_and
            (mk_ge
               (mk_add
                  (mk_mul
                     (mk_real_numeral_s "4.0")
                     (mk_real_var "x"))
                  (mk_mul
                     (mk_real_numeral_s "10.0")
                     (mk_real_var "x")))
               (mk_real_var "y"))
            (mk_ge
               (mk_real_var "z")
               (mk_real_numeral_s "5.0")))
   end)
    
