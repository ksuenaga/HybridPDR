
(* Memo:
   find . -name '*.xml' -exec wc \{\} \; | sort -n
 *)

open Core
open Format
module E = Error

(**************** Commandline args ****************)
let input_stream = ref In_channel.stdin
   
(**************** Main ****************)
   
(* Parse command-line arguments
   [XXX] Not tested
 *)
let parse_commandline_arg () : In_channel.t =
  Arg.parse
    []
    (fun s ->
      try
        input_stream := In_channel.create s
      with
        Not_found_s _ ->
        E.raise (E.of_string "File not found"))
    "";
  !input_stream

(* Verifier core *)
let verify verifTask =
  assert false

(* Output the result *)
let printResult result =
  assert false
  
let _ =
  let open SpaceexComponent in
  (* Parse command-line arguments *)
  let inchan = parse_commandline_arg () in
  (* Parse the input *)
  let t = SpaceexComponent.parse_from_channel inchan in
  (* Pass the model to the verifier core routine *)
  let result = verify t in
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
