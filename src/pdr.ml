(* IC3 core.  Implementation is based on https://www.tcs.cs.tu-bs.de/documents/Logics_Seminar_2014/IC3.pdf *)

open Core_kernel
module E = Error
(* module D = DischargeVC *)
module U = Util
module S = SpaceexComponent
module F = Frame
       
open Format

type ce = S.id * Z3.Expr.expr * int
let pp_ce fmt (loc,e,idx) =
  fprintf fmt "At location %a, at frame %d, CE: %a" S.pp_id loc idx Z3Intf.pp_expr e

(* Configuration of PDR procedure.  The array fs is arranged as
   follows: fs = [|R_0; R_1; ...; R_{n-1}; R_{rem}|]. *)
type frames = Z3.Expr.expr Frame.frame Array.t
let pp_frames fmt fs =
  let open Z3Intf in
  for i = 0 to Array.length fs - 1 do
    printf "%d -> %a@." i (Frame.pp_frame pp_expr) fs.(i)
    done

type result =
  | Ok of frames
  | Ng of ce (* Counterexamlpe at the initial state. *)
let pp_result fmt r =
  let open Z3Intf in
  match r with
  | Ok fs ->
     fprintf fmt "Ok:%a" (Util.pp_array (Frame.pp_frame pp_expr)) fs
  | Ng ce ->
     fprintf fmt "Ng:%a" pp_ce ce

exception ContradictorySpec of Z3.Model.model list

let wp ~(is_continuous:bool) (hs:S.t) (frame:Z3.Expr.expr F.frame) =
  let open SpaceexComponent in
  let open Frame in
  let open Z3Intf in
  let open Dl in
  let locs = locations hs in
  (*
    for each t in hs.transitions, if loc = t.src, then ret(loc) implies

    { [src.flow & src.inv](t.guard => wp(t.cmd, frame(t.post))) }
    while (nondet) {
      assume(src.inv);
      evolve for short time following src.flow
    }
    { guard => wp(cmd, frame(t.post)) }
    assume(guard);
    { wp(cmd, frame(t.post)) }
    cmd;
    { frame(t.post) }
   *)
  if is_continuous then
    List.fold_left
      ~init:(lift locs (mk_dl_prim mk_true))
      ~f:(fun acc locid ->
        let loc = Env.find_exn hs.locations locid in
        let post = Frame.find frame locid in
        let wpcond = mk_dl_dyn loc.flow loc.inv (mk_dl_prim post) in
        apply_on_id (mk_dl_and wpcond) locid acc)
      locs
  else
    MySet.fold
      ~init:(lift locs (mk_dl_prim mk_false))
      ~f:(fun l t ->
        let srcid = t.source in
        let srcloc = Env.find_exn hs.locations srcid in
        let tgtid = t.target in
        (* let tgtloc = Env.find_exn hs.locations tgtid in *)
        let post_z3 = find frame tgtid in
        (* [XXX] Really? mk_and or mk_implies? *)
        let guard = t.guard in
        let cmd = t.command in
        let post_z3_pre = mk_and guard (wp_command_z3 cmd post_z3) in
        (* [XXX] Really? mk_dl_and or mk_dl_or? *)
        apply_on_id (fun e -> mk_dl_or e (mk_dl_dyn srcloc.flow srcloc.inv (mk_dl_prim post_z3_pre))) srcid l)
      hs.transitions
                             
let setup_init_frames ~(hs:S.t) ~(initloc:S.id) ~(init:Z3.Expr.expr) ~(safe:Z3.Expr.expr) : frames =
  let open Z3Intf in
  let open Frame in
  let open Dl in
  let locs = S.locations hs in
  (* init@initloc *)
  let init_frame = lift locs mk_false |> apply_on_id (mk_or init) initloc in
  (* safe@0 \/ ... \/ safe@locmax *)
  let safe_frame = lift locs safe in
  (* 0-step reachability *)
  let () = printf "(* Checking the safety of 0th frame *)@." in
  let st0 = callZ3 (fold ~init:mk_true ~f:(fun z1 (_,z2) -> mk_and z1 z2) ((apply2 mk_and) init_frame ((apply mk_not) safe_frame))) in
  (* 1-step reachability *)
  let () = printf "(* Checking the safety of 1st frame *)@." in
  let st1 =
    dl_discharge
      (fold
         ~f:(fun d1 (_,d2) -> mk_dl_and d1 d2)
         ~init:(mk_dl_prim mk_true)
         ((apply2 mk_dl_and) (apply mk_dl_prim init_frame) (wp ~is_continuous:false hs ((apply mk_not) safe_frame))))
  in
  match st0,st1 with
  | `Unsat,`Unsat -> 
     [| init_frame;
        safe_frame;
        safe_frame
     |]
  | `Sat m, _ ->
     E.raise (E.of_string "0th frame is unsafe.")
  | _, `Sat m ->
     E.raise (E.of_string "1st frame is unsafe.")
  | `Unknown, _ | _, `Unknown ->
     E.raise (E.of_string "init: unknown: Cannot proceed.")

let propagate_clauses ~(hs:S.t) ~(frames:frames) : frames =
  (*
  (* [XXX] We need this.  We also need to compute postimage (not simply computing invariant atomic formula. *)
  (* i = Array.length - 1 is the remainder frame.  We don't do
     induction from this i.  Therefore, to Array.length fs - 2. *)
  let open Frame in
  for i = 0 to Array.length frames - 2 do
    let pre_frame = frames.(i) in
    (* Returns [(l1,e1);...;(ln,en)] where (li,ei) means ei holds at location li. *)
    let open Z3Intf in
    let open SpaceexComponent in
    let rec locally_invariant_atomics
              ~(pre_frame:Z3.Expr.expr frame)
              ~(atomics:(SpaceexComponent.id * Z3.Expr.expr) list)
            : (SpaceexComponent.id * Z3.Expr.expr) list =
      List.fold_left
        ~init:[]
        ~f:(fun acc (pre_loc,atomic) ->
          let open Frame in
          let pre_fml = find pre_frame pre_loc in
          let () = printf "pre_fml: %a@." pp_expr pre_fml in
          let vc = (Dl.mk_dl_prim atomic)
          (*
          let locvcs = vcgen_partial ~is_continuous:false ~pre_loc:pre_loc ~pre_fml:pre_fml ~atomic:atomic in
          let () = printf "locvcs: %a@." (U.pp_list (U.pp_triple pp_cont_triple_partial pp_id pp_expr)) locvcs in
          let filtered =
            List.fold_left
              ~init:[]
              ~f:(fun acc (vc,l,atomic) ->
                let res = DischargeVC.discharge_vc_partial vc in
                if res then (l,atomic)::acc else acc)
              locvcs
          in
           *)
          filtered
        )
        atomics
    in
    let atomics : (SpaceexComponent.id * Z3.Expr.expr) list =
      fold
        ~init:[]
        ~f:(fun acc (l,e) ->
          let atomics = ParseFml.extract_atomics e in
          let ret = List.map ~f:(fun e -> (l,e)) atomics in
          ret @ acc)
        frames.(i)
    in
    let () = printf "atomics:@[%a@]@." (U.pp_list (fun fmt (l,e) -> fprintf fmt "%a -> %a" S.pp_id l pp_expr e) ()) atomics in
    let local_invs = locally_invariant_atomics ~pre_frame ~atomics in
    let () = printf "local_invs:@[%a@]@." (Util.pp_list (fun fmt (l,e) -> fprintf fmt "%a -> %a" SpaceexComponent.pp_id l Z3Intf.pp_expr e) ()) local_invs in
    for j = 1 to i + 1 do
      List.iter
        ~f:(fun inv -> frames.(j) <- Frame.apply (mk_and inv) frames.(j))
        local_invs
    done
  done;
  frames
   *)
  let () = printf "Propagate clause: not implemented." in
  frames


let resolve_conflict (frames: frames) (hs:S.t) (preframe:Z3.Expr.expr Frame.frame) (eframe : Z3.Expr.expr Frame.frame) (loc:S.id) idx : frames =
  let open Z3Intf in
  let () = printf "(** resolve_conflict **)@." in
  let () = printf "Frame %d at location %a@." idx S.pp_id loc in
  let () = printf "preframe: %a@." (F.pp_frame pp_expr) preframe in
  let () = printf "eframe : %a@." (F.pp_frame pp_expr) eframe in
  let locs = S.locations hs in
  (* let not_ce = mk_not (Frame.find eframe loc) in *)
  (* frame1 is the frame obtained by 
     sp cmd (guard && [inverted_flow | inv]preframe) *)
  let open Frame in
  (* invcont_preframe is [inverted_flow | inv]preframe *)
  let invcont_preframe =
    apply_loc
      (fun (loc,z3expr_preframe) ->
        let preloc = Env.find_exn hs.locations loc in
        let preflow_inverted = S.invert_flow preloc.flow in
        let preinv = preloc.inv in
        let ret = Dl.mk_dl_dyn preflow_inverted preinv (Dl.mk_dl_prim z3expr_preframe) in
        ret)
      preframe
  in
  let () = printf "invcont_preframe: %a@." (pp_frame Dl.pp) invcont_preframe in
  let invdisc_preframe =
    MySet.fold
      ~init:(lift locs (Dl.mk_dl_prim mk_false))
      ~f:(fun resframe t ->
        let open S in
        let srcid,tgtid,guard = t.source,t.target,t.guard in
        let invcont_preframe_expr = Frame.find invcont_preframe srcid in
        let res = Dl.mk_dl_and (Dl.mk_dl_prim guard) invcont_preframe_expr in
        (* [XXX] Currently supports only the case where command is skip. *)
        assert(S.command_is_empty t.command);
        resframe |> apply_on_id (Dl.mk_dl_or res) tgtid)
      hs.transitions
  in
  let () = printf "invdisc_preframe: %a@." (pp_frame Dl.pp) invdisc_preframe in
  let inv_preframe_elimed = apply Dl.dl_elim_dyn invdisc_preframe in
  let inv_preframe_elimed = apply2 mk_or inv_preframe_elimed frames.(0) in
  let () = printf "inv_preframe_elimed: %a@." (pp_frame pp_expr) inv_preframe_elimed in
  let () = printf "eframe : %a@." (F.pp_frame pp_expr) eframe in
  let interpolants = apply2 interpolant inv_preframe_elimed eframe in
  let interpolants =
    apply
      (fun st ->
        match st with
        | `InterpolantFound e -> e
        | _ ->
           U.not_implemented "Not implemented: interpolant"
      )
      interpolants
  in
  let () = printf "Interpolant: %a@." (pp_frame pp_expr) interpolants in
  for i = 1 to idx do
    let () = printf "Strengthen: %d@." i in
    frames.(i) <- apply2 mk_and frames.(i) interpolants
  done;
  frames
  (* Returned frame of interpolants *)
  (*
  let itp_frame = ref (lift locs mk_true) in
  let () =
    printf "Constructing frame1@.";
    Frame.fold
      ~init:()
      ~f:(fun _ (loc',pre_z3expr) ->
        let () = printf "loc' : %a, pre_z3expr: %a@." S.pp_id loc' pp_expr pre_z3expr in
        try
          let trans = S.find_trans ~src:loc' ~tgt:loc hs in
          let () = printf "trans : %a@." S.pp_trans trans in
          let preloc = Env.find_exn hs.locations loc' in
          let preflow_inverted = S.invert_flow preloc.flow in
          let preinv = preloc.inv in
          let guard = trans.guard in
          let () = printf "guard : %a@." pp_expr guard in
          (* [XXX] Currently supports only the case where command is skip. *)
          assert(S.command_is_empty trans.command);
          let ret = Dl.mk_dl_and (Dl.mk_dl_prim guard) (Dl.mk_dl_dyn preflow_inverted preinv (Dl.mk_dl_prim pre_z3expr)) in
          let ret = Dl.dl_elim_dyn ret in
          let () = printf "ret: %a@." pp_expr ret in
          let itp = interpolant ret not_ce in
          match itp with
          | `InterpolantFound(itp_z3) ->
             let () = printf "itp: %a@." pp_expr itp_z3 in
             itp_frame := apply_on_id (fun e -> mk_and e itp_z3) loc !itp_frame
          | _ ->
             U.not_implemented "resolve_conflict: interpolant"
        with
        | Not_found -> ()
      )
      preframe
  in
  let () = printf "itp_frame: %a@." (Frame.pp_frame Z3Intf.pp_expr) !itp_frame in
  frames.(idx) <- apply2 mk_and frames.(idx) !itp_frame;
  frames
   *)
  
exception Counterexample of ce

let rec remove_cti (hs:S.t) (cexs:ce list) (frames:frames) : frames =
  let open Frame in
  let open Z3Intf in
  (* Sort in the increasing order of the index part. *)
  let () = printf "remove_cti: Current cexs are:@." in
  let () = printf "%a@." (U.pp_list pp_ce ()) cexs in
  let cexs = List.sort ~compare:(fun (_,_,idx1) (_,_,idx2) -> compare idx1 idx2) cexs in
  match cexs with
  | [] -> frames
  | (loc,e,idx)::tl ->
     let () = printf "remove_cti: processing %a@." pp_ce (loc,e,idx) in
     if idx = 0 then
       raise (Counterexample(loc,e,idx))
     else
       let is_continuous = idx = (Array.length frames - 1) in
       let () = printf "is_continous: %b@." is_continuous in
       let locs = S.locations hs in
       let eframe : Z3.Expr.expr frame = lift locs mk_false |> apply_on_id (mk_or e) loc in
       let () = printf "eframe: %a@." (pp_frame pp_expr) eframe in
       let wpframe : Dl.t frame = wp ~is_continuous hs eframe in
       let () = printf "wpframe: %a@." (pp_frame Dl.pp) wpframe in       
       let preframe : Dl.t frame = apply Dl.mk_dl_prim frames.(idx-1) in
       let () = printf "preframe: %a@." (pp_frame Dl.pp) preframe in       
       let vc = apply2 Dl.mk_dl_and preframe wpframe in
       let () = printf "vc: %a@." (pp_frame Dl.pp) vc in 
       let propagated =
         fold
           ~init:[]
           ~f:(fun acc (loc,e) ->
             let res = Dl.dl_discharge e in
             match res with
               `Sat m -> (loc,expr_of_model m,idx-1)::acc
             | _ -> acc)
           vc
       in
       let () = printf "propagated: %a@." (U.pp_list pp_ce ()) propagated in       
       match propagated with
       | [] ->
          let preframe = F.apply Dl.dl_elim_dyn preframe in
          let newframes = resolve_conflict frames hs preframe eframe loc idx in
          let () = printf "newframes: %a@." pp_frames newframes in
          remove_cti hs tl newframes
       | _ -> remove_cti hs (propagated @ cexs) frames
 
let rec extend_frontier_iter ~(hs:S.t) ~(frames:frames) ~(safe:Z3.Expr.expr) : frames =
  let open Frame in
  let open Z3Intf in
  let locs = S.locations hs in
  let len = Array.length frames in
  let () = printf "(* Checking the safety of the frontier *)@." in
  let wpframe : Dl.t frame = wp ~is_continuous:true hs (lift locs (mk_not safe)) in
  let () = printf "wp: %a@." (pp_frame Dl.pp) wpframe in
  let preframe : Dl.t frame = apply Dl.mk_dl_prim frames.(len-1) in
  let () = printf "pre: %a@." (pp_frame Dl.pp) preframe in
  let vc = apply2 Dl.mk_dl_and preframe wpframe in
  let () = printf "vc: %a@." (pp_frame Dl.pp) vc in
  let cexs =
    fold
      ~init:[]
      ~f:(fun acc (loc,z) ->
        let () = printf "discharging: %a at loc %a@." Dl.pp z S.pp_id loc in
        let res = Dl.dl_discharge z in
        match res with
        | `Unknown | `Unsat ->
           let () = printf "Unsat or unknown@." in
           acc
        | `Sat m ->
           let () = printf "Sat: %a@." pp_model m in
           (loc,expr_of_model m,len-2)::acc
      )
      vc
  in
  let () = printf "cexs: %a@." (U.pp_list pp_ce ()) cexs in
  match cexs with
  | [] -> frames
  | _ -> remove_cti hs cexs frames
  
let extend_frontier ~(frames:frames) ~(hs:S.t) ~(safe:Z3.Expr.expr) : frames =
  let open Frame in
  let () = printf "(* Extending frontier *)@." in
  let () = printf "Old frames: %a@." pp_frames frames in
  let len = Array.length frames in
  let subfs : frames =
    Array.sub frames ~pos:0 ~len:(len-1)
  in
  let locs = S.locations hs in
  let tail_part =
    [| lift locs safe;
       lift locs safe;
    |]
  in
  let newframes = Array.concat [subfs; tail_part] in
  let () = printf "new frames: %a@." pp_frames newframes in
  extend_frontier_iter ~frames:newframes ~safe ~hs
    
let rec verify_iter ~(hs:S.t) ~(safe:Z3.Expr.expr) ~(candidates:ce list) ~(frames:frames) ~(iteration_num:int) =
  let open Frame in
  let open Z3Intf in
  let () = printf "(* Iteration of verification: %d *)@." iteration_num in
  let () = printf "frames:%a@." pp_frames frames in
  try
    let frames = extend_frontier ~frames ~safe ~hs in
    let frames = propagate_clauses ~hs ~frames in
    let k = Array.length frames in
    let res = fold2 ~init:true ~f:(fun b (_,e) (_,e') -> b && (is_valid (mk_implies e e'))) frames.(k-2) frames.(k-3) in
    if res then Ok(frames)
    else
      verify_iter hs safe candidates frames (iteration_num+1)
  with
  | Counterexample ce -> Ng ce
                      
let verify ~(hs:S.t) ~(initloc:S.id) ~(init:Z3.Expr.expr) ~(safe:Z3.Expr.expr) =
  let init_frames = setup_init_frames ~hs ~initloc ~init ~safe in
  verify_iter ~hs ~safe ~candidates:[] ~frames:init_frames ~iteration_num:0

(** Old code **)

(*  
type vc_partial = DischargeVC.cont_triple_partial
type vc_total = DischargeVC.cont_triple_total
  
exception Unsafe of Z3.Model.model list

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
    let open Z3Intf in
    let open SpaceexComponent in
    let open DischargeVC in
    let rec locally_invariant_atomics
              ~(pre_frame:Frame.frame)
              ~(atomics:(SpaceexComponent.id * Z3.Expr.expr) list)
            : (SpaceexComponent.id * Z3.Expr.expr) list =
      List.fold_left
        ~init:[]
        ~f:(fun acc (pre_loc,atomic) ->
          let pre_fml = Frame.find_exn pre_frame pre_loc in
          let () = printf "pre_fml: %a@." pp_expr pre_fml in
          let locvcs = vcgen_partial ~is_continuous:false ~pre_loc:pre_loc ~pre_fml:pre_fml ~atomic:atomic in
          let () = printf "locvcs: %a@." (U.pp_list (U.pp_triple pp_cont_triple_partial pp_id pp_expr)) locvcs in
          let filtered =
            List.fold_left
              ~init:[]
              ~f:(fun acc (vc,l,atomic) ->
                let res = DischargeVC.discharge_vc_partial vc in
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
    for j = 1 to i + 1 do
      List.iter
        ~f:(fun inv -> fs.(j) <- Frame.strengthen local_invs fs.(j))
        local_invs
    done
  done

  
(* [XXX] not tested *)
let is_valid (fs : frames) =
  Frame.is_valid_implication_frame fs.(Array.length fs - 2) fs.(Array.length fs - 3)

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
             Conflict(loc,intp,idx) -> (loc,intp,idx)::acc
           | _ -> failwith "explore_single_candidate_one_step: cannot happen.")
         res
     in
     (*
     let () =
       printf "Conflict: Interpolant obtained@.";
       printf "Interpolant: %a@." Frame.pp_locfmls r
     in
      *)
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
            end
         | `Conflict (hd::_) ->
            (* All the l are `Conflict *)
            let locid,ceExpr,idx = hd in
            let () =
              for i = 1 to idx+1 do
                t.(i) <- Frame.strengthen ~locfmls:[locid,ceExpr] ~t:t.(i)
              done
              (* if idx < Array.length t - 1 then
               *   t.(idx + 1) <- Frame.strengthen ~locfmls:l ~t:t.(idx) *)
            in
            (*
            let () =
              (* printf "Original frames: %a@." (Util.pp_list Frame.pp_frame) original_frames; *)
              printf "Strengthened with interpolant: %a@." Frame.pp_locfmls l;
              printf "At location: %a@." SpaceexComponent.pp_id loc;
              (* printf "New frames: %a@." (Util.pp_list Frame.pp_frame) newframes; *)
            in
             *)
            (* exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:newframes *)
            exploreCE ~locs ~vcgen_total ~candidates:tl_cand ~t:t
         | _ -> U.not_implemented "exploreCE"
       end

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
       match st with
       | `Expandable newframes ->
          (* the tip of the frame is safe.  Expand the frames. *)
          let () = printf "(** The frontier is safe; expanding **)@." in
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
 *)
