type cont_triple_partial
type cont_triple_total

type index = int (* Index for the frames. *) [@@deriving show]
type ce = SpaceexComponent.id * Z3.Expr.expr * index
val pp_ce : Format.formatter -> ce -> unit

type vcgen_partial = is_continuous:bool ->
                     pre_loc:SpaceexComponent.id ->
                     pre_fml:Z3.Expr.expr ->
                     atomic:Z3.Expr.expr ->
                     (cont_triple_partial * SpaceexComponent.id * Z3.Expr.expr) list
  (*
type vcgen_partial =
  is_continuous:bool ->
  pre:Hpdr__.Frame.frame ->
  post:Hpdr__.Frame.frame -> cont_triple_partial list
   *)
  
type vcgen_total = is_continuous:bool -> pre:Frame.frame -> post:Frame.frame -> candidate:ce -> cont_triple_total list

type result =
  Conflict of ce
| Propagated of ce

val discharge_vc_partial : cont_triple_partial -> bool
val discharge_vc_total : triple:cont_triple_total -> idx_pre:int -> result

val to_vcgen_partial : SpaceexComponent.t -> vcgen_partial
val to_vcgen_total : SpaceexComponent.t -> vcgen_total

val pp_propagated_conflict : Format.formatter -> result -> unit

val pp_cont_triple_partial : Format.formatter -> cont_triple_partial -> unit
