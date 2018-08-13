
open Format
module S = SpaceexComponent

type 'a frame

val lift : S.id list -> 'a -> 'a frame
val find : 'a frame -> S.id -> 'a
val pp_frame : (formatter -> 'a -> unit) -> formatter -> 'a frame -> unit
val apply : ('a -> 'b) -> 'a frame -> 'b frame
val apply_loc : (S.id * 'a -> 'b) -> 'a frame -> 'b frame
val apply2 : ('a -> 'b -> 'c) -> 'a frame -> 'b frame -> 'c frame
val apply_on_id : ('a -> 'a) -> S.id -> 'a frame -> 'a frame
val fold : init:'b -> f:('b -> S.id * 'a -> 'b) -> 'a frame -> 'b
val fold2 : init:'b -> f:('b -> S.id * 'a -> S.id * 'a -> 'b) -> 'a frame -> 'a frame -> 'b
                                                                       
(*
val extract_atomics : frame -> (SpaceexComponent.id * Cnf.t) list
val frame_and_cnf : frame -> Cnf.t -> frame
val frame_lift_given_id : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> frame
val is_valid_implication_frame : frame -> frame -> [`NotValid of Hpdr__.SpaceexComponent.id * Z3.Model.model | `NotValidNoModel | `Valid ]
val is_valid_implication_cnf : frame -> Cnf.t -> [`NotValid of Hpdr__.SpaceexComponent.id * Z3.Model.model | `NotValidNoModel | `Valid ]
val strengthen : locfmls:(SpaceexComponent.id*Z3.Expr.expr) list -> t:frame -> frame

val pp_locfmls : Format.formatter -> (SpaceexComponent.id*Z3.Expr.expr) list -> unit
*)
