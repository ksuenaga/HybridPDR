
open Core
module E = Error

open Frame

open Format

(* ((n,fs) : frames) is a configuration of PDR procedure.
   Invariant n = List.length fs holds.
   The list fs is arranged as follows: fs = [R_{n-1}; R_{n-2}; ...; R_0]. *)
type frames = Frame.frame list [@@deriving show]
          
type result =
  | Ok of frames
  | Ng of (SpaceexComponent.id * Z3.Model.model) list
let pp_result fmt r =
  match r with
  | Ok fs -> fprintf fmt "Ok:%a" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n") pp_frame) fs
  | Ng ms ->
     fprintf fmt "Ng(%a)"
       (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
          (fun fmt (id,m) -> fprintf fmt "%a:%s" SpaceexComponent.pp_id id (Z3.Model.to_string m)))
       ms
        
type cont_reach_pred =
  { pre : Z3.Expr.expr;
    post : Z3.Expr.expr;
    dynamics : SpaceexComponent.flow;
    inv : Cnf.t }
let pp_cont_reach_pred fmt crp =
  fprintf fmt
    "{ pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %a }"
    (Z3.Expr.to_string crp.pre)
    (Z3.Expr.to_string crp.post)
    SpaceexComponent.pp_flow crp.dynamics
    Cnf.pp crp.inv
  
exception Unsafe of Z3.Model.model list

type vcgen = pre:frame -> post:frame -> cont_reach_pred list [@@deriving show]

(* [XXX] not tested *)        
let init (locs:SpaceexComponent.id list) (initloc:SpaceexComponent.id) i s : frames =
  let st = Cnf.sat_andneg i s  in
  match st with
  | `Unsat -> 
     [Frame.frame_lift locs s; Frame.frame_lift_given_id locs initloc (*Cnf.cnf_true*) i]
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
let rec induction (locs:SpaceexComponent.id list) (vcgen_partial : vcgen) (fs : frames) : frames =
  match fs with
  | [] -> []
  | hd1::tl ->
     let fs = induction locs vcgen_partial tl in
     match fs with
     | [] -> [hd1]
     | hd2::tl ->
        let atomics = extract_atomics hd2 in
        let local_invs =
          List.fold_left
            ~init:[]
            ~f:(fun l d ->
              let vcs = vcgen_partial ~pre:(frame_and_cnf hd2 (Cnf.cnf_lift_atomic d)) ~post:(frame_lift locs (Cnf.cnf_lift_atomic d)) in
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
        ret
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
let is_valid_implication_cnf loc (c1:Cnf.t) (c2:Cnf.t) =
  match Cnf.sat_andneg c1 c2 with
  | `Unsat -> `Valid
  | `Sat m -> `NotValid (loc,m)
  | `Unknown -> `NotValidNoModel
              
let is_valid_implication_frame (e1:frame) (e2:frame) =
  Env.fold2
    e1 e2
    ~init:`Valid
    ~f:(fun res (loc1,c1) (loc2,c2) ->
      assert(loc1=loc2);
      match res with
      | `Valid | `NotValidNoModel ->
         is_valid_implication_cnf loc1 c1 c2
      | `NotValid (loc,m) -> `NotValid (loc,m))
  
(* [XXX] not tested *)
let is_valid (fs : frames) =
  (* assert(n = List.length fs); *)
  match fs with
  | hd1::hd2::_ ->
     begin
       (* Check whether hd1->hd2 is valid. *)
       is_valid_implication_frame hd1 hd2
     end
  | _ ->
     E.raise (E.of_string "is_valid: Should not happen.")

(* [XXX] not tested *)
let expand locs (safe:Cnf.t) (fs : frames) =
  (* assert(n = List.length fs); *)
  match fs with
  | hd::_ ->
     let st =
       Env.fold ~init:`Valid
         ~f:(fun st (loc,c) ->
           match st with
           | `NotValidNoModel | `NotValid _ -> st
           | _ -> is_valid_implication_cnf loc c safe)
         hd
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
          `Expandable(newframe::fs)
       | `NotValid m ->
          `NonExpandable m
       | `NotValidNoModel ->
          E.raise (E.of_string "expand: Unknown, cannot proceed.")
     end
  | _ ->
     E.raise (E.of_string "expand: Should not happen")

let pp_candidate fmt m =
  fprintf fmt "%s" (Z3.Model.to_string m)

  (*
exception CEFound of SpaceexComponent.id * Z3.Model.model

let flow_back ~(post_state:Z3.Model.model) ~(to_hold_in_pre:Cnf.t) ~(inv:Cnf.t) =
  E.raise (E.of_string "flow_back: not implemented.")
   *)
  (*
let rec explore_single_candidate ~loc (*~(hs:SpaceexComponent.t)*) ~(vcgen_total : vcgen) ~(candidates : SpaceexComponent.id*Z3.Model.model) ~(frames : frames) =
  let open SpaceexComponent in
  let cand_loc_id, m = candidates in
  (*
  let cand_loc = Env.find_exn hs.locations cand_loc_id in
  let flow_cand_loc,inv_cand_loc = cand_loc.flow,cand_loc.inv in
   *)
  match frames with
    [] -> `CEFound [cand_loc_id,m]
  | hd1::hd2::tl ->
     (* We need to find the following m' and loc' and m''
        1. m' reaches to m in the flow of cand_loc (we call this flow flow(cand_loc) in the following.
        2. loc' -guard,cmd-> cand_loc is in the transition in hs such that m' = [[cmd]](m'') and m'' |= guard /\ hd2(loc') *)
     (* m' |= wp(cmd,guard /\ hd2), so m' is obtained by (trying to) falisify not(wp(cmd, guard /\ hd2)) along with the reverse of flow(cand_loc). *)
     (*
     let trans_l = MySet.fold ~init:[] ~f:(fun l t -> if t.target = cand_loc_id then t::l else l) hs.transitions in
     let post = Env.find_exn hd1 cand_loc_id in
     let frames_r = ref (hd1::hd2::tl) in
     List.iter
       ~f:(fun t ->
         let loc_pre, guard, cmd = t.source, t.guard, t.command in
         let pre = Env.find_exn hd2 loc_pre in
         let wp = wp_command cmd (Cnf.cnf_and pre guard) in
         let res = flow_back ~post_state:m ~to_hold_in_pre:wp ~inv:(Cnf.cnf_and inv_cand_loc post) in
         match res with
         | `Propagated m ->
            begin
              match explore_single_candidate loc hs (loc_pre,m) (hd2::tl) with
              | `CEFound(loc_m_l) -> `CEFound ((loc_pre,m)::loc_m_l)
              | `Conflict(pred,frames) ->
                 let refined_post = Cnf.cnf_and post pred in
                 let refined_hd1 = Env.add cand_loc_id refined_post hd1 in
                 `Conflict(pred, refined_hd1::frames)
            end
         | `PredFound pred ->
            let refine p frame =
              Env.add cand_loc_id (Cnf.cnf_and post p) frame
            in
            `Conflict(pred, List.map ~f:(refine pred) frames)
       )
       trans_l
      *)
  (* E.raise (E.of_string "explore_single_candidate: not implemented.")      *)
  | [hd] ->
     E.raise (E.of_string "explore_single_candidate: not implemented.")
   *) 

let rec explore_single_candidate_one_step ~(candidate : (int*SpaceexComponent.id*Z3.Model.model)) =
  E.raise (E.of_string "explore_single_candidate_one_step: not implemented")
  
let rec exploreCE ~(vcgen_total:vcgen) ~(candidates : (int*SpaceexComponent.id*Z3.Model.model) list) ~(t : frames) =
  let open Format in
  let _ = printf "frames:%a@." pp_frames t in
  let _ = printf
            "candidates:%a@."
            (pp_print_list
               ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
               (fun fmt (num,loc,m) -> fprintf fmt "%n@%a:%a" num SpaceexComponent.pp_id loc pp_candidate m))
            candidates
  in
  (* let _ = printf "hs:%a@." SpaceexComponent.pp hs in *)
  (* let _ = printf "vcgen:%a@." SpaceexComponent.pp hs in *)
  (* E.raise (E.of_string "exploreCE: not implemented") *)
  match candidates with
  | [] -> `CENotFound t
  | ((num,loc,m) as hd)::tl ->
     if num = 0 then
       `CEFound candidates
     else
       begin
         match explore_single_candidate_one_step ~candidate:hd with
         | `Propagated newcand ->
            exploreCE ~vcgen_total ~candidates:(newcand::tl) ~t:t
         | `Conflict newframe ->
            exploreCE ~vcgen_total ~candidates:tl ~t:newframe
         | `CEFound candidates -> `CEFound candidates
       end
(* E.raise (E.of_string "exploreCE: not implemented") *)
       (*
     begin
       match explore_single_candidate_one_step hd with
     end
        *)
  
let to_vcgen_partial (hs : SpaceexComponent.t) : vcgen =
  let open Frame in
  let open SpaceexComponent in
  let ret ~(pre:frame) ~(post:frame) =
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let srcloc = Env.find_exn hs.locations t.source in
        let dynamics,inv = srcloc.flow,srcloc.inv in
        let pre_source = Env.find_exn pre t.source in
        let post_target : Cnf.t = Env.find_exn post t.target in
        let wp : Z3.Expr.expr = Cnf.cnf_implies t.guard (SpaceexComponent.wp_command t.command post_target) in
        {pre=Cnf.to_z3 pre_source; post=wp; dynamics=dynamics; inv=inv}::vcs
      )
      hs.transitions
  in
  (* E.raise (E.of_string "to_vcgen: not implemented") *)
  ret

let to_vcgen_total (hs : SpaceexComponent.t) : vcgen =
  E.raise (E.of_string "to_vcgen_total: not implemented")  
  
(* [XXX] Not tested *)
let rec verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates ~frames =
  assert(candidates = []);
  let _ = printf "frames:%a@." pp_frames frames in
  let t = frames in
  (* Do induction as much as possible. *)
  let t = induction locs vcgen_partial t in
  (* Check whether the fixpoint is already reached. *)
  let res = is_valid t in
  match res with
  | `Valid -> Ok t
  | `NotValid _ | `NotValidNoModel ->
     (* Inductive invariant is not yet found. *)
     begin
       match frames with
       | hd::_ ->
          (* Check whether the tip of the frames is safe. *)
          let st =
            Env.fold ~init:`Valid
              ~f:(fun st (loc,c) ->
                match st with
                | `NotValidNoModel | `NotValid _ -> st
                | _ -> is_valid_implication_cnf loc c safe)
              hd
          in
          begin
            match st with
            | `Valid ->
               (* the tip of the frame is safe.  Expand the frames. *)
               let newframe = Frame.frame_lift locs Cnf.cnf_true in
               (* Discard the candidates.  Continue verification. *)
               verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates:[] ~frames:(newframe::frames)
            | `NotValid(loc,m) ->
               (* Counterexample is found. *)
               let newcandidates = [(List.length frames) - 1,loc,m] in
               (* Push back the counterexample. *)
               let res = exploreCE ~vcgen_total ~candidates:newcandidates ~t:t in
               begin
                 match res with
                 | `CEFound trace ->
                    (* True counterexample is found. *)
                    Ng (List.map ~f:(fun (_,loc,m) -> (loc,m)) trace)
                 | `CENotFound newframes ->
                    (* The frames are refined with newly found predicates.  Continue verification. *)
                    verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates:[] ~frames:newframes
               end
            | `NotValidNoModel ->
               (* Got stuck. *)
               E.raise (E.of_string "expand: Unknown, cannot proceed.")
          end
       (* frames should never be empty. *)
       | _ -> E.raise (E.of_string "expand: Should not happen")
     end
