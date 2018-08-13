open Core_kernel
open Format

module S = SpaceexComponent

module U = Util

type t =
  | Prim of Z3.Expr.expr
  | Dyn of S.flow * Z3.Expr.expr * t
  | And of t list
  | Or of t list

let rec pp fmt t =
  let open Z3Intf in
  match t with
  | Prim z -> fprintf fmt "%a" pp_expr z
  | Dyn(flow,inv,t) -> fprintf fmt "[%a & %a]%a" S.pp_flow flow pp_expr inv pp t
  | And ts ->
     fprintf fmt "(%a)" (U.pp_list pp ~sep:" && " ()) ts
  | Or ts ->
     fprintf fmt "(%a)" (U.pp_list pp ~sep:" || " ()) ts
         
let mk_dl_prim e = Prim e

let mk_dl_dyn f e t = Dyn(f,e,t)
         
let mk_dl_and e1 e2 =
  match e1,e2 with
  | And l1, And l2 -> And (l1 @ l2)
  | And l, e | e, And l -> And (e::l)
  | e1,e2 -> And [e1;e2]

let mk_dl_or e1 e2 =
  match e1,e2 with
  | Or l1, Or l2 -> Or (l1 @ l2)
  | Or l, e | e, Or l -> Or (e::l)
  | e1,e2 -> Or [e1;e2]
           
let rec elim_dyn_iter ~acc flow inv (t:Z3.Expr.expr) =
  let open Z3Intf in
  let module S = SpaceexComponent in
  let tprev = mk_and inv (S.prev_time ~discretization_rate:1.0 ~flow:flow ~post:t) in
  if is_valid (mk_implies tprev acc) then acc
  else
    (* let res = callZ3 tprev in *)
    elim_dyn_iter ~acc:(mk_or acc tprev) flow inv tprev
      (*
    match res with
    | `Sat _ -> elim_dyn_iter ~acc:(mk_or acc tprev) flow inv tprev
    | `Unsat -> acc
    | `Unknown ->
       E.raise (E.of_string "elim_dyn_iter: got stuck")
       *)
let rec dl_elim_dyn t =
  (* let () = printf "elim_dyn: %a@." pp t in *)
  let open Z3Intf in
  match t with
  | Prim z -> z |> simplify
  | And ts ->
     List.fold_left
       ~init:mk_true
       ~f:(fun z t ->
         mk_and z (dl_elim_dyn t))
       ts |> simplify
  | Or ts ->
     List.fold_left
       ~init:mk_false
       ~f:(fun z t ->
         mk_or z (dl_elim_dyn t))
       ts |> simplify
  | Dyn(flow,inv,t') ->
     let () = printf "Eliminating: %a@." pp t in
     let t' = dl_elim_dyn t' in
     let res = simplify (elim_dyn_iter ~acc:t' flow inv t') in
     let () = printf "Elimed: %a@." pp_expr res in
     res
           
let dl_discharge t = 
  let open Z3Intf in
  dl_elim_dyn t |> callZ3
