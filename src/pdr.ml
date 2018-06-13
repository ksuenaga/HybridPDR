
open Core
module E = Error

open Frame

open Format

(* ((n,fs) : frames) is a configuration of PDR procedure.
   Invariant n = List.length fs holds.
   The list fs is arranged as follows: fs = [R_{n-1}; R_{n-2}; ...; R_0]. *)
type frames = int * Frame.frame list [@@deriving show]
          
type result =
  | Ok of frames
  | Ng of (SpaceexComponent.id, Z3.Model.model) Env.t (* list *)
let pp_result fmt r =
  match r with
  | Ok (_,fs) -> fprintf fmt "Ok:%a" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n") pp_frame) fs
  | Ng menv -> fprintf fmt "Ng(%a)" (Env.pp SpaceexComponent.pp_id (fun fmt m -> fprintf fmt "%s" (Z3.Model.to_string m))) menv
        
type cont_reach_pred =
  { pre : Cnf.t;
    post : Cnf.t;
    dynamics : SpaceexComponent.flow;
    inv : Cnf.t }
[@@deriving show]
  
exception Unsafe of Z3.Model.model list

type vcgen = pre:frame -> post:frame -> cont_reach_pred list [@@deriving show]

(* [XXX] not tested *)        
let init (locs:SpaceexComponent.id list) (initloc:SpaceexComponent.id) i s =
  let st = Cnf.sat_andneg i s  in
  match st with
  | `Unsat -> 
     2, [Frame.frame_lift locs i; Frame.frame_lift_given_id locs initloc (*Cnf.cnf_true*) s]
  | `Sat m ->
     raise (Unsafe [m])
  | `Unknown ->
     E.raise (E.of_string "init: unknown: Cannot proceed.")

(* [XXX] to be implememnted. *)
let discharge_vcs vcs =
  match vcs with
    [] -> true
  | _ -> false
    
(* [XXX] not tested *)
let rec induction (locs:SpaceexComponent.id list) (vcgen : vcgen) (((n,fs) : frames) as t) : frames =
  match fs with
  | [] -> (0,[])
  | hd1::tl ->
     let (_,fs) = induction locs vcgen (n-1,tl) in
     match fs with
     | [] -> (1,[hd1])
     | hd2::tl ->
        let atomics = extract_atomics hd2 in
        let local_invs =
          List.fold_left
            ~init:[]
            ~f:(fun l d ->
              let vcs = vcgen ~pre:(frame_and_cnf hd2 (Cnf.cnf_lift_atomic d)) ~post:(frame_lift locs (Cnf.cnf_lift_atomic d)) in
              if discharge_vcs vcs then
                d::l
              else
                l)
            atomics
        in
        let ret = hd1::hd2::tl in
        let ret =
          List.map
            ~f:(fun frame ->
              List.fold_left
                ~init:frame
                ~f:(fun f inv -> Frame.frame_and_cnf f (Cnf.cnf_lift_atomic inv))
                local_invs)
            ret
        in
        (n,ret)
  (*
  let open Frame in
  assert(n = List.length fs);
  match fs with
  | [] | [_] -> t
  | hd::tl ->
     let (_,fs) = induction locs vcgen ((n-1,tl)) in
     match fs with
     | hd1::hd2::tl ->
        begin
          let disjuncts = extract_disjuncts hd2 in
          let vcs =
            List.fold_left
              ~init:[]
              ~f:(fun l d ->
                let vc = vcgen ~pre:(frame_and_cnf hd2 (Cnf.cnf_lift_atomic d)) ~post:(frame_lift locs (Cnf.cnf_lift_atomic d)) in
                vc)
              disjuncts
          in
          match vcs with
          | [] ->
             induction locs vcgen ((n-1,hd2::tl))
          | _ ->
             E.raise (E.of_string "induction: vc discharge: not implemneted")
        end
     | [hd] -> (1,[hd])
     | _ ->
        E.raise (E.of_string "induction: should not happen.")        
   *)

(* If (and hd1 (not hd2)) is unsatisfiable, then (imply hd1 hd2) is valid. *)
(* [XXX] not tested *)
let is_valid_implication_cnf (c1:Cnf.t) (c2:Cnf.t) =
  match Cnf.sat_andneg c1 c2 with
  | `Unsat -> `Valid
  | `Sat m -> `NotValid m
  | `Unknown -> `NotValidNoModel
     
let is_valid_implication_frame (e1:frame) (e2:frame) =
  Env.fold2
    e1 e2
    ~init:`Valid
    ~f:(fun res (_,c1) (_,c2) ->
      match res with
      | `Valid | `NotValidNoModel -> is_valid_implication_cnf c1 c2
      | `NotValid m -> `NotValid m)
  
(* [XXX] not tested *)
let is_valid ((n,fs) : frames) =
  assert(n = List.length fs);
  match fs with
  | hd1::hd2::_ ->
     begin
       (* Check whether hd1->hd2 is valid. *)
       is_valid_implication_frame hd1 hd2
     end
  | _ ->
     E.raise (E.of_string "is_valid: Should not happen.")

(* [XXX] not tested *)
let expand locs (safe:Cnf.t) ((n,fs) : frames) =
  assert(n = List.length fs);
  match fs with
  | hd::_ ->
     let st =
       Env.fold ~init:`Valid
         ~f:(fun st (_,c) ->
           match st with
             | `NotValidNoModel | `NotValid _ -> st
             | _ -> is_valid_implication_cnf c safe)
         hd
         (* is_valid_implication hd safe *)
     in
     begin
       match st with
       | `Valid ->
          let newframe =
            List.fold_left
              ~init:Env.empty
              ~f:(fun e id -> Env.add id Cnf.cnf_true e)
              locs
          in
          `Expandable(n+1, newframe::fs)
       | `NotValid m -> `NonExpandable m
       | `NotValidNoModel ->
          E.raise (E.of_string "expand: Unknown, cannot proceed.")
     end
  | _ ->
     E.raise (E.of_string "expand: Should not happen")
    
let rec exploreCE (candidates : Z3.Model.model list) (t : frames) =
  let open Format in
  let _ = printf "frames:%a@." pp_frames t in
  (* let _ = printf "candidates:%a@." pp_frames t in *)
  E.raise (E.of_string "exploreCE: not implemented")
  
let to_vcgen (hs : SpaceexComponent.t) =
  let open Frame in
  let open SpaceexComponent in
  let ret ~(pre:frame) ~(post:frame) =
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let srcloc = Env.find_exn hs.locations t.source in
        let dynamics,inv = srcloc.flow,srcloc.inv in
        let pre_source : Cnf.t = Env.find_exn pre t.source in
        let post_target : Cnf.t = Env.find_exn post t.target in
        let wp : Cnf.t = Cnf.cnf_and t.guard (SpaceexComponent.wp_command t.command post_target) in
        {pre=pre_source; post=wp; dynamics=dynamics; inv=inv}::vcs
      )
      hs.transitions
  in
  (* E.raise (E.of_string "to_vcgen: not implemented") *)
  ret
  
(* [XXX] Not tested *)
let rec verify ~locs ~vcgen ~safe ~candidates ~frames
          (* (hybridSystem : SpaceexComponent.t) *)
          (*
          (~locs:SpaceexComponent.id list)
          (~vcgen : pre:frame -> post:frame -> cont_reach_pred list)
          (~safe : Cnf.t)
          (~candidates : Z3.Model.model list)
          (~frames) 
           *)
  =
  assert(candidates = []);
  let _ = printf "frames:%a@." pp_frames frames in
  let (n,frames) as t = frames in
  let t = induction locs vcgen t in
  let res = is_valid t in
  match res with
  | `Valid -> Ok t
  | `NotValid _ | `NotValidNoModel ->
     let res = expand locs safe t in
     begin
       match res with
       | `Expandable (n',frame') ->
          assert(n' = n + 1);
          assert(List.length frame' = n');
          verify ~locs ~vcgen ~safe ~candidates ~frames:(n',frame')
       | `NonExpandable model ->
          let res = exploreCE candidates t in
          begin
            match res with
            | `CEFound trace -> Ng trace
            | `Conflict t -> verify ~locs ~vcgen ~safe ~candidates:[] ~frames:t
          end
     end
