
open Core
open Format

(**************** Commandline args ****************)
let input_stream = ref stdin
   
(**************** Commandline args ****************)
   
(* Parse command-line arguments
   [XXX] Not tested
 *)
let parse_commandline_arg () : in_channel =
  Arg.parse
    []
    (fun s ->
      try
        input_stream := open_in s
      with
        Not_found ->
        assert false)
    "";
  !input_stream

(* Parse the input into an S-expr *)
let parse_input_to_sexpr inchan =
  (*
  let open XmlParser in
  let parser = make () in
  let _ = prove parser true in
   *)
  let open Xml in
  let xml =
    try
      parse_in inchan
    with
    | Error e -> failwith (error e)
    | File_not_found s -> failwith ("File not found" ^ s)
  in
  printf "Output: @[%s@]@\n" (to_string xml);
  assert false

let%test _ =
  (* [XXX] parametrize *)
  parse_input_to_sexpr (open_in "/Users/ksuenaga/work/HybridPDR/src/examples/filtered_oscillator/filtered_oscillator.xml")
;;

(* Convert the S-expr to a datatype for verification task *)
let convert_to_verification_task_from_sexpr sexpr =
  assert false

(* Verifier core *)
let verify verifTask =
  assert false

(* Output the result *)
let printResult result =
  assert false
  
let _ =
  (* Parse command-line arguments *)
  let inchan = parse_commandline_arg () in
  (* Parse the input into an S-expr *)
  let sexpr = parse_input_to_sexpr inchan in
  (* Convert the S-expr to a datatype for verification task *)
  let verifTask = convert_to_verification_task_from_sexpr sexpr in
  (* Pass the model to the verifier core routine *)
  let result = verify verifTask in
  (* Output the result *)
  let _ = printResult result in
  ()
