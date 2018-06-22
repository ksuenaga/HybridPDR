(* Memo:
   find . -name '*.xml' -exec wc \{\} \; | sort -n
 *)

open Core_kernel
open Format
module E = Error

(**************** Commandline args ****************)
         
(**************** Main ****************)
   
(* Parse command-line arguments
   [XXX] Not tested
 *)
let parse_verif_task_from_file ?(initial_condition=Cnf.cnf_true) ?(safety_region=Cnf.cnf_true) input_file =
  let input_file = input_file in
  (* let input_stream = ref In_channel.stdin in *)
  (*
  let initial_condition = ref initial_condition in
  let safety_region = ref safety_region in
   *)
  let input_stream =
    try In_channel.create input_file
    with Not_found_s _ -> E.raise (E.of_string "Input model not found.")
  in
  let model = SpaceexComponent.parse_from_channel input_stream in
  (* (model, !initial_condition, !safety_region) *)
  model
  
(* Verifier core *)
let verify ~model ?(init_id=SpaceexComponent.id_of_string "1") ~init ~safe : Pdr.result =
  let open SpaceexComponent in
  let _ = printf "(******* Starting verification *******)@." in
  let _ = printf "(******* System to be verified *******)@." in
  let _ = printf "%a@." pp model in
  let _ = printf "Initial location: %s@." (SpaceexComponent.string_of_id init_id) in
  let _ = printf "Initial condition: %s@." (Z3.Expr.to_string (Cnf.to_z3 init)) in
  let _ = printf "Safe region: %s@." (Z3.Expr.to_string (Cnf.to_z3 safe)) in
  (* Setup frames *)
  let locs = locations model in
  let t = Pdr.init locs init_id init safe in
  (* let _ = printf "verify: %a@." Pdr.pp_frames t in *)
  let result = Pdr.verify ~hs:model ~locs:locs ~vcgen_partial:(Pdr.to_vcgen_partial model) ~vcgen_total:(Pdr.to_vcgen_total model) ~safe:safe ~candidates:[] ~frames:t in
  (* let result = printf "result:%a@." Pdr.pp_result result in *)
  result

(* Output the result *)
let printResult result =
  printf "result:%a@." Pdr.pp_result result
  
(* Tests *)
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) in
  let model = List.hd_exn models in
  let res = verify ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(listlist_to_cnf (parse_to_cnf "x == 0.5 & y == 0.0")) (* Cnf.cnf_true *) ~safe:(listlist_to_cnf (parse_to_cnf "x <= 1.0")) in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball/bball.xml")) in
  let model = List.hd_exn models in
  let res = verify ~init_id:(SpaceexComponent.id_of_string "1") ~model ~init:Cnf.cnf_true ~safe:Cnf.cnf_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_nondet/bball_nondet.xml")) in
  let model = List.hd_exn models in
  let res = verify model Cnf.cnf_true Cnf.cnf_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_timed/bball_timed.xml")) in
  let model = List.hd_exn models in
  let res = verify model Cnf.cnf_true Cnf.cnf_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator/filtered_oscillator.xml")) in
  let model = List.hd_exn models in
  let res = verify model Cnf.cnf_true Cnf.cnf_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_16/filtered_oscillator_16.xml")) in
  let model = List.hd_exn models in
  let res = verify model Cnf.cnf_true Cnf.cnf_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_32/filtered_oscillator_32.xml")) in
  let model = List.hd_exn models in
  let res = verify model Cnf.cnf_true Cnf.cnf_true in
  let _ = printResult res in
  true

let _ =
  let open SpaceexComponent in
  let input_file = ref None in
  let init_cond = ref None in
  let safety_region = ref None in
  let init_id = ref None in
  let _ =
    Arg.parse
      ["model", String (fun s -> input_file := Some s), "Model file in SpaceEx format";
       "init", String (fun s -> init_cond := Some s), "Initial condition in SpaceEx format. (Whole space if omitted.)";
       "safe", String (fun s -> safety_region := Some s), "Safety region in SpaceEx format. (Whole space if omitted.)";
       "initid", String (fun s -> init_id := Some s), "ID of the initial location.";
      ]
      (fun s -> E.raise (E.of_string "Anonymous argument is not allowed"))
  in
  let ts =
    match !input_file with
      None -> E.raise (E.of_string "Model file not specified")
    | Some s -> parse_verif_task_from_file s
  in
  let t = List.hd_exn ts in
  let init_cond =
    match !init_cond with
      None -> Cnf.cnf_true
    | Some s -> Cnf.parse s
  in
  let safety_region =
    match !safety_region with
      None -> Cnf.cnf_true
    | Some s -> Cnf.parse s
  in
  let init_id =
    match !init_id with
      None -> id_of_string "1"
    | Some s -> id_of_string s
  in
  let result = verify ~model:t ~init_id:init_id ~init:init_cond ~safe:safety_region in
  let _ = printResult result in
  ()

(**************** Commandline args ****************)
  
