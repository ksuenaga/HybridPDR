
type vcgen [@@deriving show]
type frames [@@deriving show]
type result =
  | Ok of frames
  | Ng of Z3.Model.model list

val to_vcgen : SpaceexComponent.t -> vcgen
(* init l initfml safefml returns the initial frames.  l is the id of the initial state. *)
val init : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> Cnf.t -> frames
val verify : SpaceexComponent.id list -> vcgen -> Cnf.t -> Z3.Model.model list -> frames -> result
