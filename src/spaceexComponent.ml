
open Core
open Format

module E = Error

(* [XXX] Use conv{ocaml} (Camlon) instead of deriving show *)
         
type id = string [@@deriving show]
type exp = string [@@deriving show]
type typ = Int | Real | Label [@@deriving show]
type fml = Cnf.t [@@deriving show]
type flow = string [@@deriving show]
type command = string [@@deriving show]
type loc =
  { name : id;
    inv : fml;
    flow : flow }
[@@deriving show]
(* [XXX] Ignoring asap, timedriven, and priority *)
type trans =
  { source : id;
    target : id;
    guard : fml;
    label : id;
    command : command }
[@@deriving show]
type param =
  { typ : typ;
    local : bool }
    [@@deriving show]
type t =
  { id : id;
    params : (id,param) Env.t; (* bool is false if it is a local param *)
    locations : (id,loc) Env.t;
    transitions : trans MySet.t }
[@@deriving show]

let make_param typ local = { typ; local }
let add_param id param t =
  { t with params = Env.add id param t.params }
let add_locations id loc t =
  { t with locations = Env.add id loc t.locations }
let add_transitions trans t =
  { t with transitions = MySet.add trans t.transitions }
let add_transition trans t =
  { t with transitions = MySet.add trans t.transitions }
let add_invariant inv loc = { loc with inv = inv }
let add_flow flow loc = { loc with flow = flow }
let add_label label trans = { trans with label = label }
let add_guard guard trans = { trans with guard = guard }
let add_command command trans = { trans with command = command }

(**************** Aux ****************)

(* Parsing according to the grammar (RelaxNG def.): Appendix A in http://spaceex.imag.fr/sites/default/files/spaceex_modeling_language_0.pdf *)

let malformed_error_xml id x =
  let s = Xml.to_string_fmt x in
  E.raise (E.of_string (sprintf "%s: Malformed:@\n%s@\n" id s))
let malformed_error_xmls id xs =
  let s = List.fold_left ~init:"" ~f:(fun s x -> s ^ (Xml.to_string_fmt x)) xs in
  E.raise (E.of_string (sprintf "%s: Malformed:@\n%s@\n" id s))

let get_child_pcdata (x:Xml.xml) : string =
  match Xml.children x with
    [pcdata] -> Xml.pcdata pcdata
  | _ -> malformed_error_xml "notpcdata" x
  
let rec top (xml : Xml.xml) : t list =
  if Xml.tag xml = "sspaceex" then
    (* List.rev (List.fold_left ~init:[] ~f:(fun l x -> (component x)::l) (Xml.children xml)) *)
    List.map ~f:component (Xml.children xml)
  else
    malformed_error_xml "top" xml
(* [XXX] Not tested *)
and param (xml : Xml.xml) (t : t) : t =
  let name = Xml.attrib xml "name" in
  (* [XXX] Look at ignored attributes. *)
  let local =
    match Xml.attrib xml "local" with
    | "true" -> true | "false" -> false
    | _ -> malformed_error_xml "param.local" xml in
  let typ =
    match Xml.attrib xml "type" with
    | "int" -> Int | "real" -> Real | "label" -> Label
    | _ -> malformed_error_xml "param.type" xml in
  add_param name (make_param typ local) t
and location (xml : Xml.xml) (t : t) : t =
  let id = Xml.attrib xml "id" in
  let name = Xml.attrib xml "name" in
  let loc = { name; inv = Cnf.cnf_true; flow = "" } in
  let loc =
    List.fold_left ~init:loc
      ~f:(fun (t : loc) x ->
        match Xml.tag x with
        | "invariant" ->
           let s = get_child_pcdata x in
           let r = add_invariant (Cnf.parse s) t in
           r
        | "flow" ->
           let s = get_child_pcdata x in
           let r = add_flow s t in
           r
        | _ ->
           malformed_error_xml "location" x)
      (Xml.children xml) in
  {t with locations = Env.add id loc t.locations }
  
and transition (xml : Xml.xml) (t : t) : t =
  let source = Xml.attrib xml "source" in
  let target = Xml.attrib xml "target" in
  let trans = { source; target; guard = Cnf.cnf_true; label = ""; command = "" } in
  let trans =
    List.fold_left
      ~init:trans
      ~f:(fun t x ->
        match Xml.tag x with
        | "label" -> add_label (get_child_pcdata x) t
        | "guard" -> add_guard (Cnf.parse (get_child_pcdata x)) t
        | "assignment" -> add_command (get_child_pcdata x) t
        | "labelposition" -> t (* ignored *)
        | _ -> malformed_error_xml "transition" x
      )
      (Xml.children xml)
  in
  add_transition trans t
and component (xml : Xml.xml) : t =
  if Xml.tag xml = "component" then
    let children = Xml.children xml in
    let id = Xml.attrib xml "id" in
    let t = { id; params = Env.empty; locations = Env.empty; transitions = MySet.empty } in
    List.fold_left ~init:t
      ~f:(fun t x ->
        match Xml.tag x with
        | "param" -> param x t
        | "location" -> location x t
        | "transition" -> transition x t
        | "note" -> t (* ignored *)
        | "bind" -> t (* [XXX] Bindings are currently ignored *)
        | _ ->
           malformed_error_xml "component" x)
    children
  else
    malformed_error_xml "component" xml

(**************** Operators ****************)
  
let locations (t:t) = Env.domain t.locations

(**************** Main ****************)
  
let rec parse_from_channel (inchan : In_channel.t) : t list =
  let open Xml in
  let xml =
    try
      parse_in inchan
    with
    | Error e -> failwith (error e)
    | File_not_found s -> failwith ("File not found" ^ s)
  in
  top xml

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball/bball.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_nondet/bball_nondet.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/bball_timed/bball_timed.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator/filtered_oscillator.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_16/filtered_oscillator_16.xml"))
   end)

let%test_module _ =
  (module struct
     let circleComponentTest =
       parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/filtered_oscillator_32/filtered_oscillator_32.xml"))
   end)
  
let%test_module _ =
  (module struct
     let circleXml =
       Xml.parse_in (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml"))
     let circleT =
       top circleXml
     let circleTComp =
       match circleT with
         [comp] -> comp
       | _ -> E.raise (E.of_string "Malformed circle.xml")
     let%test _ =
       circleTComp.id = "circle"
     let circleTParams = circleTComp.params 
     let%test _ =
       let param = Env.find_exn circleTParams "x" in
       param.typ = Real && param.local = false
     let%test _ =
       let param = Env.find_exn circleTParams "y" in
       param.typ = Real && param.local = false
     let%test _ =
       let param = Env.find_exn circleTParams "hop" in
       param.typ = Label && param.local = false
     let circleTLocs = circleTComp.locations
     let%test _ =
       let loc1 = Env.find_exn circleTLocs "1" in
       (loc1.name = "p" &&
          loc1.inv = Cnf.parse "y>=0" &&
            loc1.flow = "x'==-y & y'==x")
     let%test _ =
       let loc2 = Env.find_exn circleTLocs "2" in
       (loc2.name = "n" && loc2.inv = Cnf.parse "y<=0" && loc2.flow = "x'==-y & y'==x")
     (* [XXX] Complete the test *)
     let circleTTrans = circleTComp.transitions
     let%test _ =
       let trans12 = MySet.find_exn circleTTrans ~f:(fun t -> t.source = "1" && t.target = "2") in
       trans12.label = "hop" && trans12.guard = Cnf.parse "y<=0" && trans12.command = ""
     let%test _ =
       let trans21 = MySet.find_exn circleTTrans ~f:(fun t -> t.source = "2" && t.target = "1") in
       trans21.label = "hop" && trans21.guard = Cnf.parse "y>=0" && trans21.command = ""
   end)

(**************** WP computation ****************)

let wp_command cmd cnf =
  Error.raise (Error.of_string "wp_command: not implemented")

let id_of_string s = s