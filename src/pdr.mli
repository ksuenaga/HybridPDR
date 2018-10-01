open Format

module S = SpaceexComponent

type frames
type result =
  | Ok of int * frames
  | Ng of DischargeVC.ce
val pp_result : formatter -> result -> unit

(* init l initfml safefml returns the initial frames.  l is the id of the initial state. *)
(* val init : hs:S.t -> initloc:S.id -> init:Z3.Expr.expr -> safe:Z3.Expr.expr -> frames *)

val verify : tactic_in:Core_kernel.In_channel.t -> hs:S.t -> initloc:S.id -> init:Z3.Expr.expr -> safe:Z3.Expr.expr -> result
                                                            (*
type vcgen_partial (* [@@deriving show] *)
type vcgen_total (* [@@deriving show] *)
val to_vcgen_partial : SpaceexComponent.t -> vcgen_partial
val to_vcgen_total : SpaceexComponent.t -> vcgen_total

val verify : locs:SpaceexComponent.id list ->
             hs:SpaceexComponent.t ->
             vcgen_partial:DischargeVC.vcgen_partial ->
             vcgen_total:DischargeVC.vcgen_total ->
             safe:Cnf.t ->
             candidates:(SpaceexComponent.id*Z3.Model.model) list ->
             frames:frames ->
             iteration_num:int ->
             result
  
val pp_result : Format.formatter -> result -> unit
         *)

