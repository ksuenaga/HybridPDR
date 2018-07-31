open Core_kernel
open Format

let debug pref s =
  printf "Debug(%s):@[%s@]@\n@." pref s

(* lightweight pp_print_list *)
let rec pp_list pp_elm fmt l =
  match l with
    [] -> ()
  | [hd] -> fprintf fmt "%a@." pp_elm hd
  | hd::tl -> fprintf fmt "%a@.%a@." pp_elm hd (pp_list pp_elm) tl

let pp_option pp_elm fmt opt =
  match opt with
    None -> fprintf fmt "None"
  | Some x -> fprintf fmt "Some %a" pp_elm x

let not_implemented msg =
  let module E = Error in
  E.raise (E.of_string ("not implemented:" ^ msg))
