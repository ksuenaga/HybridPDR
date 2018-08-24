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

let pp_array pp_elm fmt a =
  fprintf fmt "%a" (pp_list pp_elm ()) (Array.to_list a)

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
