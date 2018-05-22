
open Core
module E = Error

type frame = Cnf.t
type frames = int * frame list
type result =
  | Ok of frames
  | Ng of Z3.Model.model list

exception Unsafe of Z3.Model.model list
        
let init i s =
  let model = Cnf.sat_andneg i s  in
  match model with
  | None -> 
     2, [i; Cnf.cnf_true]
  | Some m ->
     raise (Unsafe [m])

let rec induction (t : frames) =
  E.raise (E.of_string "induction: not implemneted")

let is_valid (t : frames) =
  E.raise (E.of_string "is_valid: not implemneted")

let expand (t : frames) =
  E.raise (E.of_string "expand: not implemneted")

let rec exploreCE (candidates : Z3.Model.model list) (t : frames) =
  E.raise (E.of_string "exploreCE: not implemented")  

(* [XXX] Not tested *)
let rec verify
          (hybridSystem : SpaceexComponent.t)
          (safe : Cnf.t)
          (candidates : Z3.Model.model list)
          ((n,frames) as t) =
  let t = induction t in
  if is_valid t then
    Ok t
  else
    match candidates with
    | [] ->
       let res = expand t in
       begin
         match res with
         | `Expandable (n',frame') ->
            assert(n' = n + 1);
            assert(List.length frame' = n');
            verify hybridSystem safe candidates (n',frame')
         | `NonExpandable model ->
            let res = exploreCE candidates t in
            begin
              match res with
              | `CEFound trace -> Ng trace
              | `Conflict t -> verify model safe [] t
            end
       end
    | _ ->
       (* exploreCE should fully explore the candidaets *)
       E.raise (E.of_string "verify: should not happen")
