open Core_kernel
module E = Error

open Format

(* Configuration of PDR procedure.  The list fs is arranged as
   follows: fs = [Continous R_{rem}; Hybrid R_{n-1}; Hybrid R_{n-2};
   ...; Hybrid R_0]. *)
type frames = Frame.frame list [@@deriving show]
          
type result =
  | Ok of frames
  | Ng of (SpaceexComponent.id * Z3.Model.model) list
let pp_result fmt r =
  match r with
  | Ok fs -> fprintf fmt "Ok:%a" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n") Frame.pp_frame) fs
  | Ng ms ->
     fprintf fmt "Ng(%a)"
       (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
          (fun fmt (id,m) -> fprintf fmt "%a:%s" SpaceexComponent.pp_id id (Z3.Model.to_string m)))
       ms

type vc_partial = DischargeVC.cont_triple_partial
type vc_total = DischargeVC.cont_triple_total
  
exception Unsafe of Z3.Model.model list

(* [XXX] not tested *)        
let init (locs:SpaceexComponent.id list) (initloc:SpaceexComponent.id) i s : frames =
  let st = Cnf.sat_andneg i s  in
  match st with
  | `Unsat -> 
     [Frame.continuous_frame_lift locs Cnf.cnf_true;
      Frame.hybrid_frame_lift locs Cnf.cnf_true;
      Frame.hybrid_frame_lift_given_id locs initloc i]
  | `Sat m ->
     raise (Unsafe [m])
  | `Unknown ->
     E.raise (E.of_string "init: unknown: Cannot proceed.")
    
(* [XXX] not tested *)
let rec induction (locs:SpaceexComponent.id list) (vcgen_partial : DischargeVC.vcgen_partial) (fs : frames) : frames =
  let open Frame in
  match fs with
  | [] -> []
  | hd1::tl ->
     let fs = induction locs vcgen_partial tl in
     match fs with
     | [] -> [hd1]
     | hd2::tl ->
        let atomics = Frame.extract_atomics hd2 in
        let local_invs =
          List.fold_left
            ~init:[]
            ~f:(fun l d ->
              let lifter1 = Frame.mk_lifter hd1 in
              let vcs = vcgen_partial
                          ~pre:(Frame.frame_and_cnf hd2 (Cnf.cnf_lift_atomic d))
                          ~post:(lifter1 locs (Cnf.cnf_lift_atomic d)) in
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


  
(* [XXX] not tested *)
let is_valid (fs : frames) =
  (* assert(n = List.length fs); *)
  match fs with
  | hd1::hd2::hd3::_ ->
     begin
       (* Check whether hd1->hd2 is valid. *)
       Frame.is_valid_implication_frame hd2 hd3
     end
  | _ ->
     E.raise (E.of_string "is_valid: Should not happen.")

(* [XXX] not tested *)
let expand locs (safe:Cnf.t) (fs : frames) =
  (* assert(n = List.length fs); *)
  match fs with
  | hd::tl ->
     assert(Frame.is_continuous_frame hd);
     let st = Frame.is_valid_implication_cnf hd safe in
     (*
       Env.fold ~init:`Valid
         ~f:(fun st (loc,c) ->
           match st with
           | `NotValidNoModel | `NotValid _ -> st
           | _ -> Frame.is_valid_implication_cnf c safe)
         hd
     in
      *)
     begin
       match st with
       | `Valid ->
          let newframe = Frame.continuous_frame_lift locs Cnf.cnf_true in
          (*
            List.fold_left
              ~init:Env.empty
              ~f:(fun e id -> Env.add id Cnf.cnf_true e)
              locs
          in
           *)
          `Expandable(newframe::tl)
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
          ~(vcgen_total : DischargeVC.vcgen_total)
          ~(pre : Frame.frame)
          ~(post : Frame.frame)
  =
  (* If one of the triples has a correct precondition, then the entire vc is discharged *)
  let open Frame in
  let id, m = candidate in
  let () =
    printf "Counterexample at loc %a: %a@." SpaceexComponent.pp_id id Z3Intf.pp_model m;
    printf "Try to propagate from the post region: %a@." Z3Intf.pp_expr (Z3Intf.expr_of_model m);
    printf "To the pre region: %a@." pp_frame pre
  in
  let triples = vcgen_total ~is_continuous:(Frame.is_continuous_frame post) ~pre:pre ~post:(Z3Intf.expr_of_model m) in
  (*
  let _ =
    printf "triples_total:%a@."
      (pp_print_list
         ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
         DischargeVC.pp_cont_triple_total)
      triples
  in
   *)
  (*
  let _ = printf "Propagating CE at %a: %s@." SpaceexComponent.pp_id triples. (Z3.Model.to_string m) in
  let _ = printf "Post: %a@." Frame.pp_frame hd_frame in
  let _ = printf "Pre: %a@." Frame.pp_frame hd2 in
   *)
  let propagated,pre =
    let discharge_results =
      List.map ~f:(fun triple -> DischargeVC.discharge_vc_total ~triple:triple) triples
    in
    List.fold_left
      ~init:(false,[])
      ~f:(fun (propagated,acc) res ->
        match propagated,res with
        | true, _ -> (true,acc)
        | _, `Propagated _ -> (true,[res])
        | _, `Unknown -> (propagated,acc)
        | _,`Conflict _ -> (propagated,res::acc))
      discharge_results
  in
  match propagated,pre with
  | true,[`Propagated(id,m)] ->
     let () =
       printf "Successfuly propagated to the state %a at loc %a@."
         Z3Intf.pp_model m
         SpaceexComponent.pp_id id
     in
    `Propagated(id,m)
  | false, res ->
     assert(List.for_all ~f:(function `Conflict _ -> true | _ -> false) res);
     let r =
       List.fold_left
         ~init:[]
         ~f:(fun acc res ->
           match res with
             `Conflict(loc,intp) -> (loc,intp)::acc
           | _ -> failwith "explore_single_candidate_one_step: cannot happen.")
         res
     in
     let () =
       printf "Conflict: Interpolant obtained@.";
       printf "Interpolant: %a@." Frame.pp_locfmls r in
         (* Z3Intf.pp_model m SpaceexComponent.pp_id id in *)
     `Conflict r
  | _ -> E.raise (E.of_string "explore_single_candidate_one_step: Cannot proceed.")

  (*
  match pre with
  | `Propagated(id,m) ->
     let _ = printf "Successfuly propagated to the state %a at loc %a@." Z3Intf.pp_model m SpaceexComponent.pp_id id in
     `Propagated(id,m)
  | `Conflict(loc,interpolant) -> `Conflict(loc,interpolant)
  | `Unknown ->
     E.raise (E.of_string "explore_single_candidate_one_step: Cannot proceed.")
   *)
  
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
          ~(vcgen_total:DischargeVC.vcgen_total)
          ~(candidates : (SpaceexComponent.id * Z3.Model.model) list)
          ~(t : frames)
  =
  let open Format in
  let () =
    printf "(* Iteration of exploreCE *)@.";
    printf "frames:%a@." pp_frames t;
    printf
      "candidates:%a@."
      (pp_print_list
         ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
         (fun fmt (loc,m) -> fprintf fmt "loc=%a:cand=%a" SpaceexComponent.pp_id loc pp_candidate m))
      candidates
  in
  (* let _ = printf "hs:%a@." SpaceexComponent.pp hs in *)
  (* let _ = printf "vcgen:%a@." SpaceexComponent.pp hs in *)
  (* E.raise (E.of_string "exploreCE: not implemented") *)
  match candidates, t with
  | [], _ ->
     `CENotFound t
  | _::_, [] ->
     `CEFound candidates
  | ((loc,m) as hd_cand)::tl_cand, hd_frame::tl_frame ->
     begin
       let hd2 = match tl_frame with hd2::_ -> hd2 | [] -> Frame.hybrid_frame_lift locs Cnf.cnf_true in
       match explore_single_candidate_one_step ~locs ~candidate:hd_cand ~vcgen_total:vcgen_total ~post:hd_frame ~pre:hd2 with
       | `Propagated newcand ->
          begin
            let res = exploreCE ~locs ~vcgen_total ~candidates:(newcand::hd_cand::tl_cand) ~t:tl_frame in
            match res with
            | `CENotFound resulting_frames ->
               assert(List.length resulting_frames = List.length tl_frame);
               `CENotFound (hd_frame::resulting_frames)
            | `CEFound _ -> res
          end
       | `Conflict l ->
          let original_frames = hd_frame::tl_frame in
          let newframes =
            List.map
              ~f:(fun frame ->
                (*
                let () =
                  printf "Strengthening frame %a@." Frame.pp_frame frame;
                  printf "with locfmls %a@." (Util.pp_list pp_locfml) l
                in
                 *)
                let strengthened = Frame.strengthen ~locfmls:l ~t:frame in
                (* let () = printf "Strengthened frame %a@." Frame.pp_frame strengthened in *)
                strengthened
              )
              original_frames
          in
          let () =
            printf "Original frames: %a@." (Util.pp_list Frame.pp_frame) original_frames;
            printf "Strengthened with interpolant: %a@." Frame.pp_locfmls l;
            printf "At location: %a@." SpaceexComponent.pp_id loc;
            printf "New frames: %a@." (Util.pp_list Frame.pp_frame) newframes;
          in
          (* exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:newframes *)
          `CENotFound newframes
     end
(* E.raise (E.of_string "exploreCE: not implemented") *)
       (*
     begin
       match explore_single_candidate_one_step hd with
     end
        *)
  

  
(* [XXX] Not tested *)
let rec verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates ~frames =
  (* E.raise (E.of_string "verify: task: Sort out log messages before going further."); *)
  let () = printf "@\n(** Iteration of verification **)@." in
  assert(candidates = []);
  let () = printf "frames:%a@." pp_frames frames in
  let t = frames in
  (* Do induction as much as possible. *)
  let () = printf "(** Induction **)@." in
  let () = printf "Frames before: %a@." pp_frames t in
  let t = induction locs vcgen_partial t in
  let () = printf "Frames after: %a@." pp_frames t in
  (* Check whether the fixpoint is already reached. *)
  let res = is_valid t in
  match res with
  | `Valid ->
     let () = printf "(** Safety is proved! **)@." in
     let () = printf "Frames: %a@." pp_frames t in
     Ok t
  | `NotValid _ | `NotValidNoModel ->
     (* Inductive invariant is not yet found. *)
     begin
       match frames with
       | hd::tl ->
          assert(Frame.is_continuous_frame hd);
          (* Check whether the tip of the frames is safe. *)
          let () = printf "(** Check whether the frontier is safe **)@." in          
          let st = Frame.is_valid_implication_cnf hd safe in
            (*
            Env.fold ~init:`Valid
              ~f:(fun st (loc,c) ->
                match st with
                | `NotValidNoModel | `NotValid _ -> st
                | _ -> Cnf.is_valid_implication loc c safe)
              hd
             *)
          begin
            match st with
            | `Valid ->
               (* the tip of the frame is safe.  Expand the frames. *)
               let () = printf "(** The frontier is safe; expanding **)@." in          
               let newframe_continuous = Frame.continuous_frame_lift locs Cnf.cnf_true in
               let newframe_hybrid = Frame.hybrid_frame_lift locs Cnf.cnf_true in
               let newframes = newframe_continuous::newframe_hybrid::tl in
               let () = printf "New frames: %a@." pp_frames newframes in
               (* Discard the candidates.  Continue verification. *)
               verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates:[] ~frames:newframes
            | `NotValid(loc,m) ->
               (* Counterexample is found. *)
               let () = printf "(** The frontier is not safe **)@." in
               let newcandidates = [(loc,m)] in
               (* Push back the counterexample. *)
               let res = exploreCE ~locs ~vcgen_total ~candidates:newcandidates ~t:t in
               begin
                 match res with
                 | `CEFound trace ->
                    (* True counterexample is found. *)
                    Ng (List.map ~f:(fun (loc,m) -> (loc,m)) trace)
                 | `CENotFound newframes ->
                    let () = printf "CE is not found@." in
                    let () = printf "Next iteration with frames %a@." (Util.pp_list Frame.pp_frame) newframes in
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
