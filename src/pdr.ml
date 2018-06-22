open Core_kernel
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
        
type vc_partial = DischargeVC.cont_triple_partial
type vc_total = DischargeVC.cont_triple_total
  
exception Unsafe of Z3.Model.model list

type vcgen_partial = pre:frame -> post:frame -> DischargeVC.cont_triple_partial list
type vcgen_total = pre:frame -> post:Z3.Expr.expr -> DischargeVC.cont_triple_total list

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
    
(* [XXX] not tested *)
let rec induction (locs:SpaceexComponent.id list) (vcgen_partial : vcgen_partial) (fs : frames) : frames =
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
              let res = List.fold_left ~init:true ~f:(fun res vc -> if res then DischargeVC.discharge_vc_partial vcs else res) vcs in
              if res then d::l else l)
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

  
let rec explore_single_candidate_one_step
          ~locs
          ~(candidate : (SpaceexComponent.id * Z3.Model.model))
          ~(vcgen_total : vcgen_total)
          ~(pre : frame)
          ~(post : frame)
  =
  (* If one of the triples has a correct precondition, then the entire vc is discharged *)
  let open Frame in
  let id, m = candidate in
  let triples = vcgen_total ~pre:pre ~post:(Z3Intf.expr_of_model m) in
  let _ =
    printf "triples_total:%a@."
      (pp_print_list
         ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
         DischargeVC.pp_cont_triple_total)
      triples
  in
  let pre =
    List.fold_left
      ~init:None
      ~f:(fun sol triple ->
        match sol with
          Some _ -> sol
        | None ->
           DischargeVC.discharge_vc_total triple)
      triples
  in
  match pre with
    Some(id,m) -> `Propagated(id,m)
  | None -> E.raise (E.of_string "explore_single_candidate_one_step(interpolant): not implemented")
  (*
  let id,model = candidate in
  let post_cnf = Env.find_exn post id in
   *)

(* 
   The number of frames = N
   
   Initial call:
   frames = [F(N-1),F(N-2);...;F(0)] where F(i) is the i-th frame
   candidates = [cand(loc(N-1),N-1)] where cand(i) is the counterexample found at i-th frame.

   In general:
   frames = [F(j+1),F(j);...;F(0)] where F(i) is the i-th frame
   candidates = [cand(loc(j+1),j+1); cand(loc(j+2),j+2); ...; CAND(loc(N-1),N-1)] where cand(loc,i) is the counterexample found at i-th frame at location loc.
*)

(* Invariant: List.hd candidates is the counterexample of List.hd t *)
let rec exploreCE
          ~(locs:SpaceexComponent.id list)
          ~(vcgen_total:vcgen_total)
          ~(candidates : (SpaceexComponent.id * Z3.Model.model) list)
          ~(t : frames)
  =
  let open Format in
  (*
  let _ = printf "frames:%a@." pp_frames t in
  let _ = printf
            "candidates:%a@."
            (pp_print_list
               ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
               (fun fmt (loc,m) -> fprintf fmt "loc=%a:cand=%a" SpaceexComponent.pp_id loc pp_candidate m))
            candidates
  in
   *)
  (* let _ = printf "hs:%a@." SpaceexComponent.pp hs in *)
  (* let _ = printf "vcgen:%a@." SpaceexComponent.pp hs in *)
  (* E.raise (E.of_string "exploreCE: not implemented") *)
  match candidates, t with
  | [], _ -> `CENotFound t
  | _::_, [] -> `CEFound candidates
  | ((loc,m) as hd_cand)::tl_cand, hd_frame::tl_frame ->
     begin
       let hd2 = match tl_frame with hd2::_ -> hd2 | [] -> Frame.frame_lift locs Cnf.cnf_true in
       match explore_single_candidate_one_step ~locs ~candidate:hd_cand ~vcgen_total:vcgen_total ~post:hd_frame ~pre:hd2 with
       | `Propagated newcand ->
          exploreCE ~locs ~vcgen_total ~candidates:(newcand::hd_cand::tl_cand) ~t:tl_frame
       | `Conflict newframe ->
          exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:newframe
     end
(* E.raise (E.of_string "exploreCE: not implemented") *)
       (*
     begin
       match explore_single_candidate_one_step hd with
     end
        *)
  
let to_vcgen_partial (hs : SpaceexComponent.t) : vcgen_partial =
  let open Frame in
  let open SpaceexComponent in
  let open DischargeVC in
  let ret ~(pre:frame) ~(post:frame) =
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let srcloc = Env.find_exn hs.locations t.source in
        let dynamics,inv = srcloc.flow,srcloc.inv in
        let pre_source = Env.find_exn pre t.source in
        let post_target : Cnf.t = Env.find_exn post t.target in
        let wp : Z3.Expr.expr = Cnf.cnf_implies t.guard (SpaceexComponent.wp_command t.command post_target) in
        {pre_loc_partial=t.source; post_loc_partial=t.target; pre_partial=Cnf.to_z3 pre_source; post_partial=wp; dynamics_partial=dynamics; inv_partial=inv}::vcs
      )
      hs.transitions
  in
  (* E.raise (E.of_string "to_vcgen: not implemented") *)
  ret

let to_vcgen_total (hs : SpaceexComponent.t) : vcgen_total =
  let open Frame in
  let open SpaceexComponent in
  let open DischargeVC in
  let ret ~(pre:frame) ~(post:Z3.Expr.expr) =
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let srcloc = Env.find_exn hs.locations t.source in
        let dynamics,inv = srcloc.flow,srcloc.inv in
        let pre_source = Env.find_exn pre t.source in
        (* let post_target : Cnf.t = Env.find_exn post t.target in *)
        let post_target = post in
        let wp : Z3.Expr.expr = Z3Intf.mk_and (Cnf.to_z3 t.guard) (SpaceexComponent.wp_command_z3 t.command post_target) in
        {pre_loc_total=t.source; post_loc_total=t.target; pre_total=Cnf.to_z3 pre_source; post_total=wp; dynamics_total=dynamics; inv_total=inv}::vcs
      )
      hs.transitions
      (* E.raise (E.of_string "to_vcgen: not implemented") *)
  in
  ret

  
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
               let newcandidates = [(loc,m)] in
               (* Push back the counterexample. *)
               let res = exploreCE ~locs ~vcgen_total ~candidates:newcandidates ~t:t in
               begin
                 match res with
                 | `CEFound trace ->
                    (* True counterexample is found. *)
                    Ng (List.map ~f:(fun (loc,m) -> (loc,m)) trace)
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
