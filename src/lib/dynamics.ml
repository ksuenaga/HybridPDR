
type 'a equation =
  'a * 'a Expr.t list

type 'a t =
  { vars : 'a list;
    equations : 'a Expr.t list }

let empty_dynamics vars =
  { vars = vars;
    equations = [] }
