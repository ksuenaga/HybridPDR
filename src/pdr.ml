open Core_kernel
module E = Error
module D = DischargeVC

open Format

(* Configuration of PDR procedure.  The array fs is arranged as
   follows: fs = [|R_0; R_1; ...; R_{n-1}; R_{rem}|]. *)
type frames = Frame.frame Array.t
let pp_frames fmt fs =
  for i = 0 to Array.length fs - 1 do
    printf "%d -> %a@." i Frame.pp_frame fs.(i)
  done
          
type result =
  | Ok of frames
  | Ng of (*(SpaceexComponent.id * Z3.Model.model) ce list*) D.ce (* Counterexamlpe at the initial state. *)
let pp_result fmt r =
  match r with
  | Ok fs ->
     fprintf fmt "Ok:%a" (*(pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n") Frame.pp_frame)*) (Util.pp_array Frame.pp_frame) fs
  | Ng ce ->
     fprintf fmt "Ng:%a" D.pp_ce ce
(*
     fprintf fmt "Ng(%a)"
       (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@\n")
          (fun fmt (id,m) -> fprintf fmt "%a:%s" SpaceexComponent.pp_id id (Z3.Model.to_string m)))
       ms
 *)
    
type vc_partial = DischargeVC.cont_triple_partial
type vc_total = DischargeVC.cont_triple_total
  
exception Unsafe of Z3.Model.model list

(* [XXX] not tested *)        
let init (locs:SpaceexComponent.id list) (initloc:SpaceexComponent.id) i s : frames =
  let open Z3Intf in
  let st = callZ3 (mk_and i (mk_not s))  in
  match st with
  | `Unsat -> 
     [| Frame.frame_lift_given_id locs initloc i;
        Frame.frame_lift locs mk_true;
        Frame.frame_lift locs mk_true
     |]
  | `Sat m ->
     raise (Unsafe [m])
  | `Unknown ->
     E.raise (E.of_string "init: unknown: Cannot proceed.")

(* let rec induction_body (vcgen_partial:DischargeVC.vcgen_partial) (fs:frames) (n:int) = *)
    
(* [XXX] not tested *)
let rec induction (locs:SpaceexComponent.id list) (vcgen_partial : DischargeVC.vcgen_partial) (fs : frames) =
  (* [XXX] We need this.  We also need to compute postimage (not simply computing invariant atomic formula. *)
  (* i = Array.length - 1 is the remainder frame.  We don't do
     induction from this i.  Therefore, to Array.length fs - 2. *)
  let open Frame in
  for i = 0 to Array.length fs - 2 do
    let pre_frame = fs.(i) in
    (* Returns [(l1,e1);...;(ln,en)] where (li,ei) means ei holds at location li. *)
    let rec locally_invariant_atomics
              ~(pre_frame:Frame.frame)
              ~(atomics:(SpaceexComponent.id * Z3.Expr.expr) list)
            : (SpaceexComponent.id * Z3.Expr.expr) list =
      List.fold_left
        ~init:[]
        ~f:(fun acc (pre_loc,atomic) ->
          let pre_fml = Frame.find_exn pre_frame pre_loc in
          let () = printf "pre_fml: %a@." 
          let locvcs = vcgen_partial ~is_continuous:false ~pre_loc:pre_loc ~pre_fml:pre_fml ~atomic:atomic in
          let filtered =
            List.fold_left
              ~init:[]
              ~f:(fun acc (vc,l,atomic) ->
                let res = DischargeVC.discharge_vc_partial [vc] in
                if res then (l,atomic)::acc else acc)
              locvcs
          in
        (* Util.not_implemented "locally_invariant_atomics" *)
          filtered
        )
        atomics
    in
    let atomics : (SpaceexComponent.id * Cnf.t) list = Frame.extract_atomics fs.(i) in
    let () = printf "atomics:@[%a@]@." (Util.pp_list (fun fmt (l,e) -> fprintf fmt "%a -> %a" SpaceexComponent.pp_id l Z3Intf.pp_expr e)) atomics in
    let local_invs = locally_invariant_atomics ~pre_frame ~atomics in
    let () = printf "local_invs:@[%a@]@." (Util.pp_list (fun fmt (l,e) -> fprintf fmt "%a -> %a" SpaceexComponent.pp_id l Z3Intf.pp_expr e)) local_invs in
    (*
      List.fold_left
        ~init:[]
        ~f:(fun l (pre_loc,d) ->
          (* let lifter1 = Frame.mk_lifter hd1 in *)
          let vcs = vcgen_partial
                      (* If we are propagating from fs.(n-2) to
                         fs.(n-1) (i.e., remeinder frame) , then this
                         is a purely continous move. *)
                      ~is_continuous:(i = Array.length fs - 2)
                      ~pre_loc:SpaceexComponent.id
                      ~pre:(Frame.frame_and_cnf fs.(i) d)
                      ~post:(Frame.frame_lift locs d) in
          let res =
            List.fold_left
              ~init:true
              ~f:(fun res vc ->
                if res then
                  DischargeVC.discharge_vc_partial vcs
                else res)
              vcs
          in
          if res then d::l else l)
        atomics
     *)
    for j = 1 to i + 1 do
      List.iter
        ~f:(fun inv -> fs.(j) <- Frame.strengthen local_invs fs.(j))
        local_invs
    done
  done

  (*    
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
 *)

  
(* [XXX] not tested *)
let is_valid (fs : frames) =
  (* assert(n = List.length fs); *)
  (*
  let () =
    printf "fspre: %a@." Frame.pp_frame fs.(Array.length fs - 2);
    printf "fspost: %a@." Frame.pp_frame fs.(Array.length fs - 1)
  in
   *)
  Frame.is_valid_implication_frame fs.(Array.length fs - 2) fs.(Array.length fs - 3)
  (*
  match fs with
  | hd1::hd2::hd3::_ ->
     begin
       (* Check whether hd1->hd2 is valid. *)
       Frame.is_valid_implication_frame hd2 hd3
     end
  | _ ->
     E.raise (E.of_string "is_valid: Should not happen.")
   *)

(* [XXX] not tested *)
let expand locs (safe:Cnf.t) (fs : frames) =
  let open Z3Intf in
  let st = Frame.is_valid_implication_cnf fs.(Array.length fs - 1) safe in
  match st with
  | `Valid ->
     let subfs : frames =
       Array.sub fs ~pos:0 ~len:(Array.length fs - 1)
     in
     let tail_part =
       [| Frame.frame_lift locs mk_true;
          Frame.frame_lift locs mk_true;
       |]
     in
     let newframes = Array.concat [subfs; tail_part] in
     `Expandable(newframes)
  | `NotValid(loc,m) ->
     let f = Frame.frame_and_cnf fs.(Array.length fs - 1) (mk_not safe) in
     let e = Frame.find_exn f loc in
     `NonExpandable(loc,e)
  | `NotValidNoModel -> E.raise (E.of_string "expand: Unknown, cannot proceed.")

let pp_candidate fmt m =
  fprintf fmt "%s" (Z3.Model.to_string m)

let rec explore_single_candidate_one_step
          ~locs
          ~(is_continuous : bool)
          ~(candidate : (SpaceexComponent.id * Z3.Expr.expr * int))
          ~(vcgen_total : DischargeVC.vcgen_total)
          ~(pre : Frame.frame)
          ~(post : Frame.frame)
          ~(idx_pre : int)
  =
  (* If one of the triples has a correct precondition, then the entire vc is discharged *)
  let open Frame in
  let open DischargeVC in
  let id, e, idx = candidate in
  let () =
    printf "Counterexample at loc %a in frame %d: %a@." SpaceexComponent.pp_id id idx Z3Intf.pp_expr e;
    printf "Try to propagate from the post region: %a@." pp_frame post;
    printf "Try to propgate to the pre region: %a@." pp_frame pre
  in
  let triples = vcgen_total ~is_continuous:is_continuous ~pre:pre ~post:post ~candidate:candidate in
  let propagated,pre =
    let discharge_results =
      List.map ~f:(fun triple -> DischargeVC.discharge_vc_total ~triple:triple ~idx_pre:idx_pre) triples
    in
    let is_propagated = function Propagated _ -> true | _ -> false in
    let is_conflict = function Conflict _ -> true | _ -> false in
    let propagated = List.exists ~f:is_propagated discharge_results in
    let ret =
      if propagated then
        List.filter ~f:is_propagated discharge_results
      else
        List.filter ~f:is_conflict discharge_results
    in
    propagated,ret
  in
  match propagated,pre with
  | true,l ->
     let () =
       printf "Successfuly propagated to %a@."
         (Util.pp_list DischargeVC.pp_propagated_conflict)
         l
     in
     let pre = List.map ~f:(function Propagated(loc,e,id) -> (loc,e,id) | _ -> assert(false)) pre in
     propagated, `Propagated pre
  | false, res ->
     assert(List.for_all ~f:(function Conflict _ -> true | _ -> false) res);
     let r =
       List.fold_left
         ~init:[]
         ~f:(fun acc res ->
           match res with
             Conflict(loc,intp,_) -> (loc,intp)::acc
           | _ -> failwith "explore_single_candidate_one_step: cannot happen.")
         res
     in
     let () =
       printf "Conflict: Interpolant obtained@.";
       printf "Interpolant: %a@." Frame.pp_locfmls r
     in
     propagated, `Conflict r
(* | _ -> E.raise (E.of_string "explore_single_candidate_one_step: Cannot proceed.") *)

let rec exploreCE
          ~(locs:SpaceexComponent.id list)
          ~(vcgen_total:DischargeVC.vcgen_total)
          ~(candidates : D.ce list)
          ~(t : frames) =
  let open Format in
  let () =
    printf "(* Iteration of exploreCE *)@.";
    printf "frames:%a@." pp_frames t;
    printf
      "candidates:%a@."
      (Util.pp_list D.pp_ce)
      candidates
  in
  (* let _ = printf "hs:%a@." SpaceexComponent.pp hs in *)
  (* let _ = printf "vcgen:%a@." SpaceexComponent.pp hs in *)
  (* E.raise (E.of_string "exploreCE: not implemented") *)
  match candidates with
  | [] ->
     `CENotFound t
  | (loc,m,idx)::tl_cand ->
     if idx = 0 then
       `CEFound(loc,m,idx)
     else
       (* idx > 0 *)
       begin
         (* let postframe = t.(idx) in *)
         (* NB: idx > 0 *)
         let preframe = t.(idx - 1) in
         let postframe = t.(idx) in
         let is_continuous = (idx = Array.length t - 1) in
         (* let hd2 = match tl_frame with hd2::_ -> hd2 | [] -> Frame.hybrid_frame_lift locs Cnf.cnf_true in *) 
         let propagated,res = explore_single_candidate_one_step ~locs ~is_continuous:is_continuous ~candidate:(loc,m,idx) ~vcgen_total:vcgen_total ~post:postframe ~pre:preframe ~idx_pre:(idx-1) in
         match res with
         | `Propagated l ->
           (* All the l are `Propagated *)
           begin
             exploreCE ~locs ~vcgen_total ~candidates:(l @ tl_cand) ~t:t
             (*
              let res = exploreCE ~locs ~vcgen_total ~candidates:(l @ tl_cand) ~t:t in
              match res with
              | `CENotFound resulting_frames ->
                 assert(List.length resulting_frames = List.length tl_frame);
                 `CENotFound (hd_frame::resulting_frames)
              | `CEFound _ -> res
              *)
            end
         | `Conflict l ->
           (* All the l are `Conflict *)
           let () =
             for i = 1 to idx do
               t.(i) <- Frame.strengthen ~locfmls:l ~t:t.(i)
             done;
             if idx < Array.length t - 1 then
               t.(idx + 1) <- Frame.strengthen ~locfmls:l ~t:t.(idx)
           in
           (*
            let () =
              printf "Original frames: %a@." (Util.pp_list Frame.pp_frame) original_frames;
              printf "Strengthened with interpolant: %a@." Frame.pp_locfmls l;
              printf "At location: %a@." SpaceexComponent.pp_id loc;
              printf "New frames: %a@." (Util.pp_list Frame.pp_frame) newframes;
            in
            *)
            (* exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:newframes *)
           exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:t
       end
(* E.raise (E.of_string "exploreCE: not implemented") *)
(*
  begin
  match explore_single_candidate_one_step hd with
        end
        *)
  

  
(* [XXX] Not tested *)
let rec verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates ~frames ~iteration_num =
  (* E.raise (E.of_string "verify: task: Sort out log messages before going further."); *)
  let () = printf "@\n(** Iteration of verification: %d **)@." iteration_num in
  assert(candidates = []);
  let () = printf "frames:%a@." pp_frames frames in
  let t = frames in
  (* Do induction as much as possible. *)
  let () = printf "(** Induction **)@." in
  let () = printf "Frames before: %a@." pp_frames t in
  let () = induction locs vcgen_partial t in
  let () = printf "Frames after: %a@." pp_frames t in
  (* Check whether the fixpoint is already reached. *)
  let res = is_valid t in
  match res with
  | `Valid ->
     let () = printf "(** Safety is proved! **)@." in
     let () = printf "Frames: %a@." pp_frames t in
     Ok t
  | `NotValid _ | `NotValidNoModel ->
     (* Inductive invariant is not reached yet. *)
     begin
       (* Check whether the tip of the frames is safe. *)
       let () = printf "(** Check whether the frontier is safe **)@." in          
       (*       let st = Frame.is_valid_implication_cnf fs.(Array.length t - 1) safe in *)
       let st = expand locs safe t in
            (*
            Env.fold ~init:`Valid
              ~f:(fun st (loc,c) ->
                match st with
                | `NotValidNoModel | `NotValid _ -> st
                | _ -> Cnf.is_valid_implication loc c safe)
              hd
             *)
       match st with
       | `Expandable newframes ->
          (* the tip of the frame is safe.  Expand the frames. *)
          let () = printf "(** The frontier is safe; expanding **)@." in
          (*
          let newframe_continuous = Frame.continuous_frame_lift locs Cnf.cnf_true in
          let newframe_hybrid = Frame.hybrid_frame_lift locs Cnf.cnf_true in
          let newframes = newframe_continuous::newframe_hybrid::tl in
           *)
          let () = printf "New frames: %a@." pp_frames newframes in
          (* Discard the candidates.  Continue verification. *)
          verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates:[] ~frames:newframes ~iteration_num:(iteration_num+1)
       | `NonExpandable(loc,e) ->
          (* Counterexample is found. *)
          let () = printf "(** The frontier is not safe **)@." in
          let newcandidates = [(loc, e, Array.length t - 1)] in
          (* Push back the counterexample. *)
          let res = exploreCE ~locs ~vcgen_total ~candidates:newcandidates ~t:t in
          begin
            match res with
            | `CEFound trace ->
               (* True counterexample is found. *)
               Ng trace
            | `CENotFound newframes ->
               let () = printf "CE is not found@." in
               let () = printf "Next iteration with frames %a@." (Util.pp_array Frame.pp_frame) newframes in
               (* The frames are refined with newly found predicates.  Continue verification. *)
               verify ~locs ~hs ~vcgen_partial ~vcgen_total ~safe ~candidates:[] ~frames:newframes ~iteration_num:(iteration_num+1)
          end
       | `NotValidNoModel ->
          (* Got stuck. *)
          E.raise (E.of_string "expand: Unknown, cannot proceed.")
     end
  (* frames should never be empty. *)

     (*
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
     *)
