
type result =
  | Succeed of SpaceexComponent.id * Z3.Expr.expr
  | Unsuccessful

val backprop :
  pre:SpaceexComponent.id ->
  pre_fml:Z3.Expr.expr ->
  post_fml:Z3.Expr.expr ->
  dynamics:SpaceexComponent.flow ->
  inv:Z3.Expr.expr ->
  safe:Z3.Expr.expr ->
  tryTimes:int ->
  discretization_rate:float ->
  result
