open Core_kernel
open Format

module S = SpaceexComponent

module U = Util

type t =
  | Prim of Z3.Expr.expr
  | Dyn of S.flow * Z3.Expr.expr * t
  | And of t list
  | Or of t list

let rec simplify t =
  let module Z = Z3Intf in
  match t with
  | Prim t' -> Prim (Z.simplify t')
  | Dyn(f, inv, Prim z3) ->
     let inv, z3 = Z.simplify inv, Z.simplify z3 in
     let invres, z3res = Z.callZ3 inv, Z.callZ3 z3 in
     begin
       match z3res with
       | `Unsat -> Prim (Z.mk_false)
       | _ ->
          begin
            match invres with
            | `Unsat -> Prim z3
            | _ -> Dyn(f, inv, Prim z3)
          end
     end
  | Dyn(f, inv, t') -> Dyn(f, Z.simplify inv, simplify t')
  | And ts ->
     let ts = List.map ~f:simplify ts in
     And (ts)
  | Or ts ->
     let ts = List.map ~f:simplify ts in
     Or (ts)
        
let rec pp fmt t =
  let open Z3Intf in
  match t with
  | Prim z -> fprintf fmt "prim(%a)" pp_expr z
  | Dyn(flow,inv,t) -> fprintf fmt "[%a & %a]%a" S.pp_flow flow pp_expr inv pp t
  | And ts ->
     fprintf fmt "(%a)" (U.pp_list pp ~sep:" && " ()) ts
  | Or ts ->
     fprintf fmt "(%a)" (U.pp_list pp ~sep:" || " ()) ts

let rec dl_is_valid e =
  match e with
  | Prim z -> Z3Intf.is_valid z
  | Dyn _ -> false (* Unknown *)
  | Or es -> List.exists ~f:dl_is_valid es
  | And es -> List.for_all ~f:dl_is_valid es

let rec dl_is_unsat e =
  match e with
  | Prim z -> Z3Intf.is_unsat z
  | Dyn _ -> false (* Unknown *)
  | Or es -> List.for_all ~f:dl_is_unsat es
  | And es -> List.exists ~f:dl_is_unsat es
            
let mk_dl_prim e = Prim e

let mk_dl_dyn f e t = Dyn(f,e,t)
         
let mk_dl_and e1 e2 =
  let open Z3Intf in
  match e1,e2 with
  | And l1, And l2 -> And (l1 @ l2)
  | And l, e | e, And l -> And (e::l)
  | Prim z1, Prim z2 -> Prim (mk_and z1 z2)
  | e1,e2 -> And [e1;e2]

          
let mk_dl_or e1 e2 =
  let open Z3Intf in
  if dl_is_valid e1 || dl_is_valid e2 then
    Prim mk_true
  else if dl_is_unsat e1 then
    e2
  else if dl_is_unsat e2 then
    e1
  else
    match e1,e2 with
    | Or l1, Or l2 -> Or (l1 @ l2)
    | Or l, e | e, Or l -> Or (e::l)
    | Prim z1, Prim z2 -> Prim (mk_or z1 z2)
    | e1,e2 -> Or [e1;e2]
           
let rec elim_dyn_iter ~acc flow inv (t:Z3.Expr.expr) =
  let open Z3Intf in
  let module S = SpaceexComponent in
  let tprev = mk_and inv (S.prev_time ~discretization_rate:1.0 ~flow:flow ~post:t) in
  if is_valid (mk_implies tprev acc) then
    simplify acc
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

let rec is_valid_implication t1 t2 =
  let module Z = Z3Intf in
  let z3res_to_res r =
    match r with
    | `Sat m -> `NotValid m
    | `Unsat -> `Valid
    | `Unknown -> `Unknown
  in
  let t1, t2 = simplify t1, simplify t2 in
  printf "t1simpl:%a@." pp t1;
  printf "t2simpl:%a@." pp t2;
  match t1,t2 with
  | Prim e1, Prim e2 ->
     Z.callZ3 (Z.mk_implies e1 e2) |> z3res_to_res
  | Prim e1, Dyn(f,inv,post) ->
  (* Check whether "e1 implies post is valid"; if so, the entire formula is valid *)
     let r = is_valid_implication t1 post in
     begin
       match r with
       | `Valid -> `Valid
       | _ ->
            printf "t1simpl:%a@." pp t1;
            printf "t2simpl:%a@." pp t2;
            Util.not_implemented "Dl.is_valid_implication: primdyn"
     end
  | _,_ -> Util.not_implemented "Dl.is_valid_implication"
                     
let interpolant t1 t2 =
  let module Z = Z3Intf in
  printf "t1:%a@." pp t1;
  printf "t2:%a@." pp t2;
  let t1, t2 = simplify t1, simplify t2 in
  printf "t1simpl:%a@." pp t1;
  printf "t2simpl:%a@." pp t2;
  match t1,t2 with
  | Prim t1', Prim t2' -> Z.interpolant t1' t2'
  | _, _ ->
     Util.not_implemented "Dl.interpolant"  
