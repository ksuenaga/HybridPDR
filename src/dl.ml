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
  if dl_is_unsat e1 || dl_is_unsat e2 then
    Prim mk_false
  else if dl_is_valid e1 then
    e2
  else if dl_is_valid e2 then
    e1
  else
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
  let tprev = mk_and inv (S.prev_time ~discretization_rate:0.05 ~flow:flow ~post:t) in
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
  (* U.not_implemented "dl_elim_dyn" *)
  (*
  let () = printf "elim_dyn: %a@." pp t in
  let open Z3Intf in
  match t with
  | Prim z -> z |> simplify
  | _ ->   U.not_implemented "dl_elim_dyn"
*)
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

let array_to_model ~vars ~array =
  let open Z3Intf in
  let open Bigarray in
  List.fold_left
    vars
    ~init:(mk_true,1)
    ~f:(fun (e,idx) x -> (mk_and e (mk_eq (mk_real_var x) (mk_real_numeral_float array.{idx})),idx+1))
  |> fst
  |> callZ3
  |> function `Sat m -> m | _ -> U.error "array_to_model: callZ3 error"

let%test _ =
  let open Z3Intf in
  let open Bigarray in
  let vars = ["x"; "y"] in
  let array = Array1.of_array float64 fortran_layout [|1.0; 0.5|] in
  (* let () = printf "array:%a@." (Util.pp_bigarray_fortran_array1 Float.pp) array in *)
  let assoc =
    array_to_model ~vars:vars ~array:array
    |> assoc_of_model
    |> List.map ~f:(fun (x,e) -> (x,get_float e))
  in
  (* let () = printf "assoc:%a@." (Util.pp_list (Util.pp_pair String.pp Float.pp) ()) assoc in *)
  assoc = [("y", 0.5); ("x", 1.0)]

let model_to_array ~vars ~model =
  let module Z = Z3Intf in
  let fs = List.map vars
      ~f:(fun s ->
          (* printf "s:%a@." String.pp s; *)
          Z.mk_real_var s
          |> Z.eval model
          (* Substitute zero to every vars.  If model does not
               substitute all the variables, then `Z.eval model` may
               not be a float constant.  In this case, it is OK to
               substitute zero to the all the variable in vars because
               they can be arbitrary.  (Notice that the variables in
               model does not appear in `Z.eval model`.) *)
          |> (fun e -> Z.substitute vars (List.map vars ~f:(fun _ -> Z.mk_real_numeral_float 0.0)) e)
          |> Z.get_float)
  in
  let open Bigarray in
  let open Array1 in
  let open Odepack in
  Array.of_list fs |> of_array float64 fortran_layout

let%test _ =
  let open Z3Intf in
  let open Bigarray in
  let vars = ["x"; "y"] in
  let exp =
    mk_and
      (mk_eq (mk_real_var "y") (mk_real_numeral_float 0.5))
      (mk_eq (mk_real_var "x") (mk_real_numeral_float 1.0))
  in
  callZ3 exp |>
  (function
      `Sat m -> model_to_array ~vars:vars ~model:m = Array1.of_array float64 fortran_layout [|1.0; 0.5|]
    | _ -> false)

let%test _ =
  let open Z3Intf in
  let open Bigarray in
  let vars = ["x"; "y"] in
  let exp =
    mk_eq (mk_real_var "y") (mk_real_numeral_float 0.5)
  in
  callZ3 exp |>
  (function
      `Sat m -> model_to_array ~vars:vars ~model:m = Array1.of_array float64 fortran_layout [|0.0; 0.5|]
    | _ -> false)

let rec check_satisfiability ~pre ~flow ~inv ~(post:Z3.Model.model) =
  let module S = SpaceexComponent in
  let module E = Env in
  let module Z = Z3Intf in
  let open Bigarray in
  let open Array1 in
  let open Odepack in
  let flow = S.invert_flow flow in
  let vars = E.domain flow in
  (* Function used by the ODE solver. *)
  let f _ (y:vec) (y':vec) =
    let model_y = array_to_model ~vars:vars ~array:y in
    List.iteri
      vars
      ~f:(fun i x ->
          E.find_exn flow x
          |> Z.eval model_y
          |> Z.get_float
          (* vars is 0-origin while y' is 1-origin.  Therefore, i+1. *)
          |> (fun f -> y'.{i+1} <- f))
  in
  (*
  let post_array =
    Z.callZ3 post |>
    (function
      | `Sat m ->
          (* printf "m:%a@." Z.pp_model m; *)
          model_to_array ~vars:vars ~model:m
      | _ -> Util.error "should not happen")
  in
*)
  let post_array = model_to_array ~vars:vars ~model:post in
  let t = 10. in
  let n = 1000 in
  let dt = t /. float (n-1) in
  let ode = Odepack.lsoda f post_array 0. 0. in
  let exception Result in
  let res = ref `Unknown in
  (* let () = printf "vars: %a@." (Util.pp_list String.pp ~sep:";" ()) vars in *)
  try
    for i = 0 to n - 1 do
      let vec =
        float i *. dt
        |> Odepack.sol ode
      in
      (* let () = printf "%a@." (Util.pp_bigarray_fortran_array1 Float.pp) vec in *)
      let m = array_to_model ~vars:vars ~array:vec in
      match Z.eval m pre |> Z.callZ3 with
      | `Sat m' -> res := `Sat m; raise Result
      | _ ->
          begin
            match Z.eval m inv |> Z.callZ3 with
            | `Unsat -> res := `Unsat; raise Result
            | `Unknown -> res := `Unknown; raise Result
            | `Sat _ -> ()
          end
    done;
    `Unknown
  with
  | Result -> !res

let%test _ =
  let open Z3Intf in
  let exception Unsat in
  let pre = mk_le (mk_real_var "x") (mk_real_numeral_float 1.0) in
  let flow = Env.from_list ["y", mk_real_var "x"; "x", mk_mul (mk_real_var "y") (mk_real_numeral_float (-.1.0))] in
  let inv = mk_ge (mk_real_var "y") (mk_real_numeral_float 0.0) in
  (* let post = mk_not (mk_le (mk_real_var "x") (mk_real_numeral_float 1.0)) in *)
  try
    let post =
      mk_and (mk_eq (mk_real_var "x") (mk_real_numeral_float 1.25))
        (mk_eq (mk_real_var "y") (mk_real_numeral_float 1.25))
      |> callZ3
    |> function `Sat m -> m | _ -> raise Unsat in
    let res = check_satisfiability ~pre:pre ~flow:flow ~inv:inv ~post:post in
    match res with `Unsat -> true | _ -> false
  with
    Unsat -> false


(*
let dl_discharge t = 
  let open Z3Intf in
  dl_elim_dyn t |> callZ3
*)

let rec is_valid_implication ?(nsamples=10) t1 t2 =
  let module Z = Z3Intf in
  let z3res_to_res r =
    match r with
    | `Sat m -> `NotValid [m]
    | `Unsat -> `Valid
    | `Unknown -> `Unknown
  in
  let pp_res fmt = function
    | `NotValid ms -> fprintf fmt "NotValid with %a" (Util.pp_list Z.pp_model ()) ms
    | `Valid -> fprintf fmt "Valid"
    | `Unknown -> fprintf fmt "Unknown"
  in
  (*
  let () =
    printf "t1:%a@." pp t1;
    printf "t2:%a@." pp t2
  in
*)
  let t1, t2 = simplify t1, simplify t2 in
  let () =
    printf "%aCHECKING THE VALIDITY OF:%a@." U.pp_start_style U.Green U.pp_end_style ();
    printf "%a@." pp t1;
    printf "%aIMPLIES%a@." U.pp_start_style U.Green U.pp_end_style ();
    printf "%a@." pp t2
  in
  (*
  printf "t1simpl:%a@." pp t1;
  printf "t2simpl:%a@." pp t2;
   *)
  let ret =
    match t1,t2 with
    | Prim e, _ when Z.is_unsat e -> `Valid
    | _, Prim e when Z.is_unsat (Z.mk_not e) -> `Valid
    (* | _, Prim e when Z.is_unsat (Z.mk_not e) -> `Valid *)
    | Prim e1, Prim e2 ->
        Z.callZ3 (Z.mk_and e1 (Z.mk_not e2)) |> z3res_to_res
    | _, Dyn(_,_,Prim post) when Z.is_unsat (Z.mk_not post) -> `Valid
    | Prim e1, Dyn(f,inv,Prim post) ->
        (* Check whether "e1 implies post is valid"; if not, the entire formula is not valid *)
        (* let r = is_valid_implication t1 post in *)
        let r = `Valid in
        begin
          match r with
          | `NotValid m -> `NotValid [m]
          | _ ->
              printf "is_valid_implication: primdyn: eliminating@.";
              (* Util.not_implemented "Dl.is_valid_implication: primdyn" *)
              let samples : Z3.Model.model list = Z.sample ~n:nsamples post in
              let results = List.map ~f:(fun m -> check_satisfiability ~pre:e1 ~flow:f ~inv:inv ~post:m) samples in
              (* Z.mk_and e1 (Z.mk_not (dl_elim_dyn t2)) |> Z.callZ3 |> z3res_to_res *)
              let res =
                List.fold_left results
                  ~init:[]
                  ~f:(fun acc r ->
                      match r with
                      | `Sat m -> m::acc
                      | `Unsat -> acc
                      | `Unknown -> U.not_implemented "is_valid_implication: unknown")
              in
              match res with
              | [] -> `Valid
              | _ -> `NotValid res
        end
    | _,_ -> Util.not_implemented "Dl.is_valid_implication"
  in
  printf "%aResult: %a%a@." U.pp_start_style U.Green pp_res ret U.pp_end_style ();
  ret




(*
let%test _ =
  let open Printf in
  let open Bigarray in
  let fname = Filename.temp_file "odepack" ".dat" in
  let () =
    let f _ y y' =
      y'.{1} <- y.{2};
      y'.{2} <- -. y.{1} in
    let y = Array1.of_array float64 fortran_layout [| 0.; 1. |] in
    let exact_sol t = (sin t, cos t) in
    let t = 10. in
    let n = 100 in
    let dt = t /. float(n-1) in
    let ode = Odepack.lsoda f y 0. 0. in
    let fh = open_out fname in
    let err = ref 0. in
    for i = 0 to n - 1 do
      let t = float i *. dt in
      let y = Odepack.sol ode t in
      let y1, y2 = exact_sol t in
      let dy1 = y.{1} -. y1 and dy2 = y.{2} -. y2 in
      fprintf fh "%f %f %f %g %g\n" t y.{1} y.{2} dy1 dy2;
      err := max !err (max (abs_float dy1) (abs_float dy2));
    done;
    close_out fh;
    printf "Computed max abs error = %g\n" !err;
    printf "Wrote %S.\n" fname
  in
  true
*)

let rec is_satisfiable_conjunction t1 t2 : [> `Sat of Z3.Model.model | `Unknown ] =
  let module Z = Z3Intf in
  let z3res_to_res r =
    match r with
    | `Sat m -> `Sat m
    (*     | `Unsat -> `Unsat *)
    | _ -> `Unknown
  in
  let pp_res fmt = function
    | `Sat m -> fprintf fmt "Sat with %a" Z.pp_model m
    | `Unsat -> fprintf fmt "Unsat"
    | `Unknown -> fprintf fmt "Unknown"
  in
  let () =
    printf "t1:%a@." pp t1;
    printf "t2:%a@." pp t2
  in
  let t1, t2 = simplify t1, simplify t2 in
  let () =
    printf "%aCHECKING WHETHER THE FOLLOWING IS UNSAT:%a@." U.pp_start_style U.Green U.pp_end_style ();
    printf "%a@." pp t1;
    printf "%aAND%a@." U.pp_start_style U.Green U.pp_end_style ();
    printf "%a@." pp t2
  in
  let ret =
    match t1,t2 with
    | Prim e, _ when Z.is_unsat e -> `Unsat
    | _, Prim e when Z.is_unsat e -> `Unsat
    | Prim e1, Prim e2 ->
        Z.callZ3 (Z.mk_and e1 e2) |> z3res_to_res
    | Prim e, Dyn(f,inv,Prim e_post) | Dyn(f,inv,Prim e_post), Prim e ->
        (* Check whether "e and post is sat"; if it is, the entire formula is sat *)
        (* let r = is_satisfiable_conjunction (Prim e) post in *)
        let r = Z.mk_and e e_post |> Z.callZ3 in
        begin
          match r with
          | `Sat m -> `Sat m
          | _ ->
              let exception Unsat in
              let exception Unknown in
              printf "is_satisfiable_conjunction: primdyn: eliminating@.";
              try
                let e_post = Z.callZ3 e_post |> function `Sat m -> m | `Unsat -> raise Unsat | `Unknown -> raise Unknown in
                check_satisfiability ~pre:e ~flow:f ~inv:inv ~post:e_post
              with
              | Unsat -> `Unsat
              | Unknown -> `Unknown
        end
    | _,_ -> Util.not_implemented "Dl.is_satisfiable_conjunction"
  in
  printf "%aResult: %a%a@." U.pp_start_style U.Green pp_res ret U.pp_end_style ();
  ret

let%test _ =
  let open Z3Intf in
  let open ParseFml in
  let open SpaceexComponent in
  let x = mk_real_var "x" in
  let y = mk_real_var "y" in
  let fl = mk_real_numeral_float in
  let z3v = mk_real_var in
  (* prim((and (<= x 1.0) (>= x (/ 1.0 2.0)))) *)
  (* let t1 = Prim(mk_and (mk_le x (fl 1.0)) (mk_ge x (mk_div (fl 1.0) (fl 2.0)))) in *)
  let t1 = mk_and (mk_le x (fl 1.0)) (mk_ge x (mk_div (fl 1.0) (fl 2.0))) in
  (* [[(y, x); (x, ( * y (- 1.0)))] & (>= y 0.0)]prim((and (<= y 0.0) (>= y 0.0) (<= x (- (/ 5.0 4.0))) (>= x (- (/ 5.0 4.0))))) *)
  let flow = ["y", z3v "x"; "x", (mk_mul y (fl (-1.0)))] |> Env.from_list in
  let inv = mk_ge y (fl 0.0) in
  let post =
    (mk_and_l
       [mk_le y (fl 0.0);
        mk_ge y (fl 0.0);
        mk_le x (mk_div (fl (-.5.0)) (fl 4.0));
        mk_ge x (mk_div (fl (-.5.0)) (fl 4.0))])
  in
  let exception Unsat in
  try
    let post_m = callZ3 post |> function `Sat m -> m | _ -> raise Unsat in
    (* let t2 = Dyn(flow,inv,Prim post) in *)
    (* let res = is_satisfiable_conjunction t1 t2 in *)
    let res = check_satisfiability ~pre:t1 ~flow:flow ~inv:inv ~post:post_m in
    match res with
    | `Sat _ -> true
    | _ -> false
  with
    Unsat -> false
;;

let interpolant ?(nsamples=10) t1 t2 =
  let module Z = Z3Intf in
  (*
  printf "t1:%a@." pp t1;
  printf "t2:%a@." pp t2;
   *)
  let t1, t2 = simplify t1, simplify t2 in
  printf "t1simpl:%a@." pp t1;
  printf "t2simpl:%a@." pp t2;
  match t1,t2 with
  | Prim t1', Prim t2' -> Z.interpolant t1' t2'
  | _, Prim t when Z.callZ3 t = `Unsat -> `InterpolantFound Z.mk_true
  | And [Prim guard; Dyn(f,inv,Prim e1)], Prim e2 ->
      let samples1 = Z.sample ~n:nsamples e1 in
      let samples1 =
        List.map samples1 ~f:(fun m -> check_satisfiability ~pre:guard ~flow:f ~inv:inv ~post:m)
        |> List.filter ~f:(function `Sat _ -> true | _ -> false)
        |> List.map ~f:(function `Sat m -> m | _ -> U.error "hoge")
      in
      let e1 =
        List.fold_left samples1 ~init:Z.mk_false ~f:(fun e m -> Z.mk_or e (Z.expr_of_model m))
      in
      let samples2 = Z.sample ~n:nsamples e2 in
      let e2 =
        List.fold_left samples2 ~init:Z.mk_false ~f:(fun e m -> Z.mk_or e (Z.expr_of_model m))
      in
      let () =
        let module U = Util in
        printf "%aTaking interpolant of:@.%a@.and@.%a@.%a" U.pp_start_style U.Green Z.pp_expr e1 Z.pp_expr e2 U.pp_end_style ()
      in
      Z.interpolant e1 e2
  | _, _ ->
      U.not_implemented "interpolant"
      (*
     printf "interpolant: eliminating@.";
     let t1,t2 = dl_elim_dyn t1, dl_elim_dyn t2 in
     Z.interpolant t1 t2
*)
