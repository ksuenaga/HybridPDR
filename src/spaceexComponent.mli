type id [@@deriving show]
type typ = Int | Real | Label [@@deriving show]
type fml [@@deriving show]
type flow [@@deriving show]
type command [@@deriving show]
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

