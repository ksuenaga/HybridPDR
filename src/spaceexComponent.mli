type id [@@deriving show]
type typ = Int | Real | Label [@@deriving show]
type fml = Cnf.t [@@deriving show]
type flow
type command
type loc =
  { name : id;
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

val wp_command : command -> Cnf.t -> Cnf.t
val wp_command_z3 : command -> Z3.Expr.expr -> Z3.Expr.expr

val locations : t -> id list

val id_of_string : string -> id
val string_of_id : string -> id
val pp_flow : Format.formatter -> flow -> unit
