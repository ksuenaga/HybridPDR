
type result =
  | Succeed of SpaceexComponent.id * Z3.Expr.expr
  | Unsuccessful

val backprop :
  pre:SpaceexComponent.id ->
  post:SpaceexComponent.id ->
  pre_fml:Z3.Expr.expr ->
  post_fml:Z3.Expr.expr ->
  dynamics:SpaceexComponent.flow ->
  inv:Z3.Expr.expr ->
  result
