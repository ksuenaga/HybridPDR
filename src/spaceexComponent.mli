type id [@@deriving show]
type typ = Int | Real | Label [@@deriving show]
type fml = Cnf.t [@@deriving show]
type flow = (string,Z3.Expr.expr) Env.t
type command
type loc =
  { id : id;
    name : id;
    inv : fml;
    flow : flow }
[@@deriving show]
(* [XXX] Ignoring asap, timedriven, and priority *)
type trans =
  { source : id;
    target : id;
    guard : fml;
    label : id;
    command : command }
[@@deriving show]
type param =
  { typ : typ;
    local : bool }
[@@deriving show]
type t =
  { id : id;
    params : (id,param) Env.t; (* bool is false if it is a local param *)
    locations : (id,loc) Env.t;
    transitions : trans MySet.t }
[@@deriving show]

val parse_from_channel : Core.In_channel.t -> t list

val command_is_empty : command -> bool
  
val wp_command : command -> Cnf.t -> Cnf.t
val wp_command_z3 : command -> Z3.Expr.expr -> Z3.Expr.expr

val locations : t -> id list

val id_of_string : string -> id
val string_of_id : id -> string
val pp_flow : Format.formatter -> flow -> unit
val pp_command : Format.formatter -> command -> unit

val prev_time : discretization_rate:float -> flow:flow -> post:Z3.Expr.expr -> Z3.Expr.expr

val parse_flow : string -> flow

val invert_flow : flow -> flow

val find_trans : src:id -> tgt:id -> t -> trans
