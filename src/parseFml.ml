open Core
open Format

open MParser
module Regex = MakeRegexp(MParser_RE.Regexp)
open Regex
open Tokens

type unop = Neg
type binop = Mul | Div | Add | Sub | Gt | Ge | Lt | Le | Eq | And | Or
                                                                  
type expr =
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Ident of string
  | Float of float

let infix assoc p op =
  Infix (p |>> (fun _ a b -> (Binop (op, a, b))), assoc (*Assoc_left*))
let prefix p op =
  Prefix (p |>> (fun _ a -> (Unop (op, a))))

let operators =
  [
    [ prefix (string "-") Neg ];
    [
      infix Assoc_left (string "*") Mul;
      infix Assoc_left (string "/") Div;
    ];
    [
      infix Assoc_left (string "+") Add;
      infix Assoc_left (string "-") Sub;
    ];
    [
      infix Assoc_left (string ">") Gt;
      infix Assoc_left (string ">=")Ge;
      infix Assoc_left (string "<")  Lt;
      infix Assoc_left (string "<=") Le;
      infix Assoc_left (string "==") Eq;
    ];
    [
      infix Assoc_left (string "&&") And;
      infix Assoc_left (string "||") Or;
    ];
  ]

let basic =
  (regexp (make_regexp "[a-zA-Z][a-zA-Z0-9_]*") |>> (fun x -> Ident x)) <|>
    (integer |>> (fun i -> Float (float_of_int i))) <|>
    (float |>> (fun f -> Float f))
  
let rec expr s =
  expression operators term s
and term s =
  (parens expr <|> basic) s

let rec expr_to_sexp res =
  let open Sexp in
  let float f = List [Atom "float"; Atom (string_of_float f)] in
  let ident x = List [Atom "ident"; Atom x] in
  let neg a = List [Atom "neg"; a] in
  let binop s a1 a2 = List [Atom s; a1; a2] in
  match res with
  (* | `Int i -> float i *)
  | Unop(op, a) ->
     begin
       match op with
       | Neg ->  neg (expr_to_sexp a)
     end
  | Binop (op, a, b) ->
     let op_s = 
       match op with
       | Add -> "add"
       | Sub -> "sub"
       | Mul -> "mul"
       | Div -> "div"
       | Gt -> "gt"
       | Ge -> "ge"
       | Lt -> "lt"
       | Le -> "le"
       | Eq -> "eq"
       | And -> "and"
       | Or -> "or"
     in
     binop op_s (expr_to_sexp a) (expr_to_sexp b)
  | Float f -> float f
  | Ident x ->  ident x

let parse s =
  match MParser.parse_string expr s () with
  | Success e ->
     expr_to_sexp e
  | Failed (msg, e) ->
     failwith msg

let%test_module _ =
  (module struct
     module S = Sexp
     let%test _ =
       let s = parse "4*4+10/2" in
       let _ = printf "ans:%a@." S.pp s in
       S.equal (parse "4*4+10/2") (List [Atom "int"; Atom "21.0"])
     let%test _ =
       S.equal (parse "x>=0") (S.of_string "(>= x 0)")
     let%test _ =
       S.equal (parse "x >= 0") (S.of_string "(>= x 0)")
     let%test _ =
       S.equal (parse "x > 0") (S.of_string "(> x 0)")
     let%test _ =
       S.equal (parse "x > 1.0") (S.of_string "(> x 1.0)")
     let%test _ =
       S.equal (parse "x <= 0.05") (Sexplib__Sexp.of_string "(<= x 0.05)")
   end)
    
