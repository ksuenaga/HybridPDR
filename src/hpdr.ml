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
let parse_verif_task_from_file ?(initial_condition=Z3Intf.mk_true) ?(safety_region=Z3Intf.mk_true) input_file =
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
let verify ?(tactic_in=In_channel.stdin) ~model ?(init_id=SpaceexComponent.id_of_string "1") ~init ~safe : Pdr.result =
  let open SpaceexComponent in
  let () =
    lazy (printf "(******* Starting verification *******)@.";
          printf "(******* System to be verified *******)@.";
          printf "%a@." pp model;
          printf "Initial location: %s@." (SpaceexComponent.string_of_id init_id);
          printf "Initial condition: %s@." (Z3.Expr.to_string init);
          printf "Safe region: %s@." (Z3.Expr.to_string safe))
    |> Util.debug !Util.debug_verify
  in
  (* Setup frames *)
  (* let locs = locations model in *)
  (* let t = Pdr.init model init_id init safe in *)
  (* let _ = printf "verify: %a@." Pdr.pp_frames t in *)
  (* let result = Pdr.verify ~hs:model ~locs:locs ~vcgen_partial:(DischargeVC.to_vcgen_partial model) ~vcgen_total:(DischargeVC.to_vcgen_total model) ~safe:safe ~candidates:[] ~frames:t ~iteration_num:0 in *)
  (* let result = printf "result:%a@." Pdr.pp_result result in *)
  let result = Pdr.verify ~tactic_in ~hs:model ~initloc:init_id ~safe:safe ~init:init in
  result

(* Output the result *)
let printResult result =
  printf "result:%a@." Pdr.pp_result result
  
(* Tests *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/line/line.xml")) in
  let model = List.hd_exn models in
  let () = Util.debug_all_off (); Util.debug_verify := true in
  let res =
    lazy (verify ~tactic_in:stdin ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x == 0.5 & y == 0.0")  ~safe:(parse_to_cnf "x <= 1.0"))
    |> Util.measure_time
  in
  let _ = printResult res in
  match res with
  | Ok _ -> true
  | _ -> false
   *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/line2/line.xml")) in
  let model = List.hd_exn models in
  let () = Util.debug_all_off (); Util.debug_verify := true in
  let res =
    lazy (verify ~tactic_in:stdin ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x <= 0") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 1.5"))
    |> Util.measure_time
  in
  let () =
    printResult res
  in
  match res with
  | Ok _ -> true
  | _ -> false
   *)
  
  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/line2/line.xml")) in
  let model = List.hd_exn models in
  let () = Util.debug_all_off (); Util.debug_verify := true in
  let res =
    lazy (verify ~tactic_in:stdin ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x <= 0") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 0.5"))
    |> Util.measure_time
  in
  let _ = printResult res in
  match res with
  | Ng _ -> true
  | _ -> false
   *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle_tactic2.smt2") in
  let model = List.hd_exn models in
  let () = Util.debug_all_off (); Util.debug_verify := true; Util.debug_verify_iter := true in
  let res =
    lazy (verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x == 0.0 & y == 0.0") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 1.0"))
    |> Util.measure_time
  in
  let _ = printResult res in
  match res with
  | Ok _ -> true
  | Ng _ -> false
   *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle_tactic1.smt2") in
  let model = List.hd_exn models in
  let () = Util.debug_all_off (); Util.debug_verify := true; Util.debug_verify_iter := true in
  (* let () = Util.debug_all_on () in *)
  let res = verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x <= 0.5") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 1.0") in
  let _ = printResult res in
  true
   *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) in
  let model = List.hd_exn models in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle_tactic5.smt2") in
  let () = Util.debug_all_off (); Util.debug_verify := true; Util.debug_verify_iter := true in
  let res =
    lazy (verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x >= 0.0 & x <= 0.5 & y >= 0.0 & y <= 0.5") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 1.0"))
    |> Util.measure_time
  in
  let _ = printResult res in
  true
   *)
  
  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) in
  let model = List.hd_exn models in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle_tactic4.smt2") in
  let () = Util.debug_all_off (); Util.debug_resolve_conflict := true; Util.debug_remove_cti := true in
  let res =
    lazy (verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "x >= 0.0 & x <= 0.5 & y >= 0.0 & y <= 0.5") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 1.0"))
    |> Util.measure_time
  in
  let _ = printResult res in
  true
   *)
  
  (*
let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball/bball.xml")) in
  let model = List.hd_exn models in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/bball/bball_tactic1.smt2") in
  let res = verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model ~init:Z3Intf.mk_true ~safe:Z3Intf.mk_true in
  let _ = printResult res in
  true
   *)
  
(*
let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_nondet/bball_nondet.xml")) in
  let model = List.hd_exn models in
  let res = verify model Z3Intf.mk_true Z3Intf.mk_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_timed/bball_timed.xml")) in
  let model = List.hd_exn models in
  let res = verify model Z3Intf.mk_true Z3Intf.mk_true in
  let _ = printResult res in
  true
 *)

  (*
let%test _ =
  let open Z3Intf in
  let open Cnf in
  let open ParseFml in
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator/filtered_oscillator.xml")) in
  let model = List.hd_exn models in
  let tactic_in = In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator/filtered_oscillator_tactic1.smt") in
  let () = Util.debug_all_off () (*; Util.debug_check_satisfiability := false *) in
  let res =
    lazy (verify ~tactic_in:tactic_in ~init_id:(SpaceexComponent.id_of_string "1") ~model:model ~init:(parse_to_cnf "a1==-2 & x0==-0.7 & a2==-1 & y0==0.7 & c==0.5 & x==0.2 & y==0") (* Cnf.cnf_true *) ~safe:(parse_to_cnf "x <= 0.6"))
    |> Util.measure_time
  in
  let _ = printResult res in
  true
   *)
  
(*
let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_16/filtered_oscillator_16.xml")) in
  let model = List.hd_exn models in
  let res = verify model Z3Intf.mk_true Z3Intf.mk_true in
  let _ = printResult res in
  true

let%test _ =
  let models = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_32/filtered_oscillator_32.xml")) in
  let model = List.hd_exn models in
  let res = verify model Z3Intf.mk_true Z3Intf.mk_true in
  let _ = printResult res in
  true
 *)
  
(*let%expect_test "expect_test test" =
  printf "src_root path edited";
  [%expect {|
    src_root path edited
  |}]*)

let main () =
  let () = Random.init Util.default_randomization_seed in
  let open SpaceexComponent in
  let input_file = ref None in
  let init_cond = ref None in
  let safety_region = ref None in
  let init_id = ref None in
  (*let srcroot_arg = ref None in*)
  (*let set_srcroot_dir dir = Config.srcroot := dir (*); printf "src_root path edited" Config.srcroot*) in*)
  let _ =
    Arg.parse
      ["model", String (fun s -> input_file := Some s), "Model file in SpaceEx format";
       "init", String (fun s -> init_cond := Some s), "Initial condition in SpaceEx format. (Whole space if omitted.)";
       "safe", String (fun s -> safety_region := Some s), "Safety region in SpaceEx format. (Whole space if omitted.)";
       "initid", String (fun s -> init_id := Some s), "ID of the initial location.";
       (*"srcroot", String (fun s -> srcroot_arg := Some s), "Edit srcroot folder path";*)
       (*"srcroot", Arg.String (set_srcroot_dir), "Edit srcroot folder path";*)
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
      None -> Z3Intf.mk_true
    | Some s -> Cnf.parse s
  in
  let safety_region =
    match !safety_region with
      None -> Z3Intf.mk_true
    | Some s -> Cnf.parse s
  in
  let init_id =
    match !init_id with
      None -> id_of_string "1"
    | Some s -> id_of_string s
  in
  (*let srcroot_arg =
    match !srcroot_arg with
      None -> printf "No new location for srcroot"  (*None*)
    | Some s -> printf "New location for srcroot"   (*Sys.command "srcroot_newLocation_script"*)
  in*)
  let result = verify ~tactic_in:stdin ~model:t ~init_id:init_id ~init:init_cond ~safe:safety_region in
  let _ = printResult result in
  ()

(**************** Commandline args ****************)
  
