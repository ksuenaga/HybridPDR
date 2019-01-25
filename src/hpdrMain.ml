open Core_kernel
open Format

let parse_verif_task_from_file ?(initial_condition=Z3Intf.mk_true) ?(safety_region=Z3Intf.mk_true) input_file =
  let module E = Error in
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

let () =
  try
    let module E = Error in
    let open SpaceexComponent in
    let input_file = ref None in
    let init_cond = ref None in
    let safety_region = ref None in
    let init_id = ref None in
    (*let srcroot_arg = ref None in*)
    (*let set_srcroot_dir dir = Config.srcroot := dir (*); printf "src_root path edited" Config.srcroot*) in*)
    let () =
      Arg.parse
        ["-model", String (fun s -> input_file := Some s), "Model file in SpaceEx format";
         "-init", String (fun s -> init_cond := Some s), "Initial condition in SpaceEx format. (Whole space if omitted.)";
         "-safe", String (fun s -> safety_region := Some s), "Safety region in SpaceEx format. (Whole space if omitted.)";
         "-initid", String (fun s -> init_id := Some s), "ID of the initial location.";
         (*"srcroot", String (fun s -> srcroot_arg := Some s), "Edit srcroot folder path";*)
         (*"srcroot", Arg.String (set_srcroot_dir), "Edit srcroot folder path";*)
        ]
        (fun s -> E.raise (E.of_string "Anonymous argument is not allowed"))
        ""
    in
    let ts =
      match !input_file with
        None -> E.raise (E.of_string "Model file not specified")
      | Some s -> parse_verif_task_from_file s
    in
    let model = List.hd_exn ts in
    let init =
      match !init_cond with
        None -> Z3Intf.mk_true
      | Some s -> Cnf.parse s
    in
    let safe =
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
    let () = Hpdr.main ~model ~init ~safe ~init_id in
    exit 0
  with
    _ -> exit 2
;;

