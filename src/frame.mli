
type pre_frame
type frame [@@deriving show]

(* val continuous_frame_lift : SpaceexComponent.id list -> Cnf.t -> frame *)
  (*
val hybrid_frame_lift : SpaceexComponent.id list -> Cnf.t -> frame
val hybrid_frame_lift_given_id : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> frame
   *)
val frame_lift : SpaceexComponent.id list -> Cnf.t -> frame
val extract_atomics : frame -> Cnf.t list
val frame_and_cnf : frame -> Cnf.t -> frame
val frame_lift_given_id : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> frame

(* val mk_lifter : frame -> (SpaceexComponent.id list -> Cnf.t -> frame) *)

val is_valid_implication_frame : frame -> frame -> [`NotValid of Hpdr__.SpaceexComponent.id * Z3.Model.model | `NotValidNoModel | `Valid ]
val is_valid_implication_cnf : frame -> Cnf.t -> [`NotValid of Hpdr__.SpaceexComponent.id * Z3.Model.model | `NotValidNoModel | `Valid ]
(* val is_continuous_frame : frame -> bool *)

(* val strengthen : loc:SpaceexComponent.id -> fmls:Cnf.atomic list -> t:frame -> frame *)
val strengthen : locfmls:(SpaceexComponent.id*Z3.Expr.expr) list -> t:frame -> frame

val find_exn : frame -> SpaceexComponent.id -> Cnf.t

val pp_locfmls : Format.formatter -> (SpaceexComponent.id*Z3.Expr.expr) list -> unit
