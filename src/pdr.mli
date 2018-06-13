type vcgen [@@deriving show]
type frames [@@deriving show]
type result =
  | Ok of frames
  | Ng of (SpaceexComponent.id,Z3.Model.model) Env.t

val to_vcgen : SpaceexComponent.t -> vcgen
(* init l initfml safefml returns the initial frames.  l is the id of the initial state. *)
val init : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> Cnf.t -> frames
val verify : locs:SpaceexComponent.id list -> hs:SpaceexComponent.t -> vcgen:vcgen -> safe:Cnf.t -> candidates:(SpaceexComponent.id*Z3.Model.model) list -> frames:frames -> result
val pp_result : Format.formatter -> result -> unit
