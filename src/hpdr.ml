
open Core

(**************** Commandline args ****************)
let input_stream = ref stdin
   
(**************** Commandline args ****************)
   
(* Parse command-line arguments *)
let parse_commandline_arg () =
  Arg.parse
    []
    (fun s ->
      try
        input_stream := open_in s
      with
        Not_found ->
        failwith ("File" ^ s ^ " not found"))
    ""

(* Parse the input into an S-expr *)
let parse_input_to_sexpr () =
  assert false

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
  let open Mock in
  (* Parse command-line arguments *)
  let _ = parse_commandline_arg () in
  (* Parse the input into an S-expr *)
  let sexpr = parse_input_to_sexpr () in
  (* Convert the S-expr to a datatype for verification task *)
  let verifTask = convert_to_verification_task_from_sexpr sexpr in
  (* Pass the model to the verifier core routine *)
  let result = verify verifTask in
  (* Output the result *)
  let _ = printResult result in
  ()
