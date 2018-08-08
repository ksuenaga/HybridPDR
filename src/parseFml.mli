
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

(* type expr [@@deriving show]
 * type unop [@@deriving show]
 * type binop [@@deriving show]                                                                   *)

val parse_to_cnf : string -> Z3.Expr.expr
val parse_assignment : string -> string * Z3.Expr.expr
val parse_flow : Core.String.t -> (string * Z3.Expr.expr) list

val parse_basic : string -> expr
val extract_atomics : Z3.Expr.expr -> Z3.Expr.expr list
