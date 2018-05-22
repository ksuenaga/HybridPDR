type t
   
val set_context : (string * string) list -> unit
val cnf_true : t
val parse : string -> t

val sat_andneg : t -> t -> Z3.Model.model option
