
(* Memo:
   find . -name '*.xml' -exec wc \{\} \; | sort -n
 *)

open Core
open Format
module E = Error

(**************** Commandline args ****************)
let input_file = ref ""
(* let config_file = ref "" *)
let input_stream = ref In_channel.stdin
(* let config_stream = ref In_channel.stdin *)
let initial_condition = ref Fml.fml_true
let safety_region = ref Fml.fml_true
   
(**************** Main ****************)
   
(* Parse command-line arguments
   [XXX] Not tested
 *)
let parse_commandline_arg () : unit =
  let open Arg in
  parse
    [("model", String (fun s -> input_file := s), "SpaceEx xml file");
     ("init", String (fun s -> initial_condition := Fml.parse s), "Initial conditions");
     ("safereg", String (fun s -> safety_region := Fml.parse s), "Safety region")]
    (fun s -> raise (Bad "Anonymous argument is not allowed."))
    "";
  let _ =
  input_stream :=
    try In_channel.create !input_file
    with Not_found_s _ -> E.raise (E.of_string "Input model not found.")
  in
  (*
  let _ =
    config_stream :=
      try In_channel.create !config_file
      with Not_found_s _ -> E.raise (E.of_string "Config file not found")
  in
   *)
  ()

  (*
let parse_config_from_channel filename =
  let open Config_file in
  let g = new group in
  let _ = g#read filename in
  let _ = g#write "hoge" in
  ()
   *)
  
(* Verifier core *)
let verify model init safe =
  (* Setup frames *)
  let frames = Pdr.init init safe in
  (* *)
  let result = Pdr.verify frames in 
  result
  
(* Output the result *)
let printResult result =
  assert false
  
let _ =
  let open SpaceexComponent in
  (* Parse command-line arguments *)
  let _ = parse_commandline_arg () in
  (* Parse the input *)
  let t = SpaceexComponent.parse_from_channel !input_stream in
  (* Parse the config file *)
  (* let cfg = parse_config_from_channel !config_file in *)
  (* Pass the model to the verifier core routine *)
  let result = verify t !initial_condition !safety_region in
  (* Output the result *)
  let _ = printResult result in
  ()

(**************** Commandline args ****************)
  
(* Tests *)
let%test_module _ =
  (module struct
     let circleComponentTest =
       SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml"))
   end)
