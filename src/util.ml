open Core_kernel
open Format

let debug pref s =
  printf "Debug(%s):@[%s@]@\n@." pref s

(* lightweight pp_print_list *)
let rec pp_list pp_elm ?(sep=format_of_string "@.") () fmt l =
  match l with
    [] -> ()
  | [hd] -> fprintf fmt "%a" pp_elm hd
  | hd::tl ->
     fprintf fmt "%a" pp_elm hd;
     fprintf fmt sep;
     fprintf fmt "%a" (pp_list pp_elm ~sep ()) tl

let pp_option pp_elm fmt opt =
  match opt with
    None -> fprintf fmt "None"
  | Some x -> fprintf fmt "Some %a" pp_elm x

let not_implemented msg =
  let module E = Error in
  E.raise (E.of_string ("not implemented:" ^ msg))

let error msg =
  let module E = Error in
  E.raise (E.of_string ("error:" ^ msg))

let pp_array pp_elm ?(sep=format_of_string "@.") () fmt a =
  let len = Array.length a in
  for i = 0 to len - 1 do
    fprintf fmt "%d:%a" i pp_elm a.(i);
    fprintf fmt sep
  done

let pp_bigarray_fortran_array1 pp_elm fmt (a:('a,'b, Bigarray.fortran_layout) Bigarray.Array1.t) =
  let len = Bigarray.Array1.dim a in
  fprintf fmt "[|";
  for i = 1 to len do
    fprintf fmt "%a;" pp_elm a.{i}
  done;
  fprintf fmt "|]"

let pp_pair pp1 pp2 fmt p =
  match p with
  | (x,y) ->
     fprintf fmt "(%a,%a)" pp1 x pp2 y

let pp_triple pp1 pp2 pp3 fmt p =
  match p with
  | (x,y,z) ->
     fprintf fmt "(%a,%a,%a)" pp1 x pp2 y pp3 z

type style =
  Green
let style_to_int = function
    Green -> 32
let pp_start_style fmt style =
  fprintf fmt "\027[%dm" (style_to_int style)
let pp_end_style fmt () =
  fprintf fmt "\027[0m"

let default_randomization_seed = 283903
let default_trial_number = 1
let default_trial_number_for_ce = 20
let default_randomization_factor = 0.5
let default_drift_factor = 1.0

(* let debug_flag = ref false *)
let debug_events = ref false
let debug_sample = ref false
let debug_validity_check = ref (*true*) false
let debug_conjunction_satisfiability = ref false
let debug_check_satisfiability = ref false
let debug_interpolation = ref false
let debug_propagate_clauses = ref false
let debug_resolve_conflict = ref false
let debug_remove_cti = ref false
let debug_propagate_one_step = ref false
let debug_frontier_iter = ref false
let debug_extend_frontier = ref false
let debug_verify_iter = ref false
let debug_verify = ref false
let debug_z3 = ref false

let debug_flags = [
    debug_events;
    debug_sample;
    debug_validity_check;
    debug_conjunction_satisfiability;
    debug_check_satisfiability;
    debug_interpolation;
    debug_propagate_clauses;
    debug_resolve_conflict;
    debug_remove_cti;
    debug_propagate_one_step;
    debug_frontier_iter;
    debug_extend_frontier;
    debug_verify_iter;
    debug_verify;
    debug_z3;
  ]

let query_for_reolsve_conflict = ref true

let debug_all_off () = List.iter debug_flags ~f:(fun r -> r := false)
let debug_all_on () = List.iter debug_flags ~f:(fun r -> r := true)

let debug flag promise =
  if flag then
    Lazy.force promise

  (*
let fdebug fmt s =
  if !debug_flag then
    Format.fprintf fmt s
   *)

let measure_time promise =
  let s = Unix.gettimeofday () in
  try
    let ret = Lazy.force promise in
    let e = Unix.gettimeofday () in
    let () = printf "Time: %f@." (e-.s) in
    ret
  with
  | exn ->
     let e = Unix.gettimeofday () in
     let () = printf "Time: %f@." (e-.s) in
     raise exn
