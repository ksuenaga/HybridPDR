(*
type vcgen_partial (* [@@deriving show] *)
type vcgen_total (* [@@deriving show] *)
 *)
type frames
type index
type ce
type result =
  | Ok of frames
  | Ng of ce

        (*
val to_vcgen_partial : SpaceexComponent.t -> vcgen_partial
val to_vcgen_total : SpaceexComponent.t -> vcgen_total
         *)
(* init l initfml safefml returns the initial frames.  l is the id of the initial state. *)
val init : SpaceexComponent.id list -> SpaceexComponent.id -> Cnf.t -> Cnf.t -> frames

val verify : locs:SpaceexComponent.id list ->
             hs:SpaceexComponent.t ->
             vcgen_partial:DischargeVC.vcgen_partial ->
             vcgen_total:DischargeVC.vcgen_total ->
             safe:Cnf.t ->
             candidates:(SpaceexComponent.id*Z3.Model.model) list ->
             frames:frames ->
             result
  
val pp_result : Format.formatter -> result -> unit
                                                
