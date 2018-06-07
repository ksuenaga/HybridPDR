type t [@@deriving show]
type disj [@@deriving show]

val pp : Format.formatter -> t -> unit
   
val set_context : (string * string) list -> unit
val cnf_true : t
val parse : string -> t

val sat_andneg : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
val sat_implication : t -> t -> [> `Sat of Z3.Model.model | `Unknown | `Unsat ]
val extract_disjuncts : t -> disj list
val cnf_and : t -> t -> t
val cnf_lift_disj : disj -> t
