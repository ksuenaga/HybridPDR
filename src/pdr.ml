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
  | Ok of int * frames
  | Ng of ce (* Counterexamlpe at the initial state. *)
let pp_result fmt r =
  let open Z3Intf in
  match r with
  | Ok (i, fs) ->
     fprintf fmt "Ok at frame %d:@.%a@." i (Util.pp_array (Frame.pp_frame pp_expr) ()) fs;
       fprintf fmt "Computed invariant is:@.%a@." (Frame.pp_frame pp_expr) fs.(i)
  | Ng ce ->
     fprintf fmt "Ng:%a" pp_ce ce

exception ContradictorySpec of Z3.Model.model list

let wp ~is_partial ~(is_continuous:bool) (hs:S.t) (frame:Z3.Expr.expr F.frame) =
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
  let ret =
    if is_continuous then
      List.fold_left
        ~init:(lift locs (mk_dl_prim mk_true))
        ~f:(fun acc locid ->
          let loc = Env.find_exn hs.locations locid in
          let post = Frame.find frame locid in
          let wpcond = mk_dl_dyn ~is_partial loc.flow loc.inv (mk_dl_prim post) in
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
          let guard = t.guard in
          let cmd = t.command in
          let post_z3_pre =
            if is_partial then
              mk_implies guard (wp_command_z3 cmd post_z3)
            else
              mk_and guard (wp_command_z3 cmd post_z3)
          in
          (* [XXX] Really? mk_dl_and or mk_dl_or? *)
          apply_on_id
            (fun e ->
              mk_dl_or e (mk_dl_dyn ~is_partial srcloc.flow srcloc.inv (mk_dl_prim post_z3_pre))
            )
            srcid l)
        hs.transitions
  in
  ret

exception Counterexample of ce
exception SafetyVerified of int * frames
  
let setup_init_frames ~(hs:S.t) ~(initloc:S.id) ~(init:Z3.Expr.expr) ~(safe:Z3.Expr.expr) : frames =
  let open Z3Intf in
  let open Frame in
  let open Dl in
  let locs = S.locations hs in
  (* init@initloc *)
  let init_frame = lift locs mk_false |> apply_on_id (mk_or init) initloc in
  (* safe@0 \/ ... \/ safe@locmax *)
  let safe_frame = lift locs safe in
  (* let unsafe_frame = lift locs (mk_not safe) in *)
  (* 0-step reachability *)
  let check_resframe res =
    Frame.fold
      ~init:`Unsat
      ~f:(fun b (loc,r) ->
        match b, r with
        | `Sat (l,m), _ -> `Sat (l,m)
        | _, `NotValid(m::_) -> `Sat (loc,m)
        | `Unsat, `Valid -> `Unsat
        | _, `Unknown -> U.not_implemented "setup_init_frames: st0: unknown")
      res
  in
  let res0 =
    apply2
      Dl.is_valid_implication
      (apply Dl.mk_dl_prim init_frame)
      (apply Dl.mk_dl_prim safe_frame)
  in
  let st0 = check_resframe res0 in
  (* 1-step reachability *)
  let wp_frame = wp ~is_partial:true ~is_continuous:false hs safe_frame in
  let res1 = apply2 Dl.is_valid_implication (apply Dl.mk_dl_prim init_frame) wp_frame in
  let st1 = check_resframe res1 in
  match st0,st1 with
  | `Unsat,`Unsat -> 
     [| init_frame;
        safe_frame;
        safe_frame
     |]
  | `Sat(l,m), _ ->
     printf "%a at %a@." Z3Intf.pp_model m S.pp_id l;
     (* E.raise (E.of_string "0th frame is unsafe.") *)
     raise (Counterexample(l,expr_of_model m, 0))
  | _, `Sat(l,m) ->
     printf "%a at %a@." Z3Intf.pp_model m S.pp_id l;
     raise (Counterexample(l,expr_of_model m, 0))
(*
  | `Unknown, _ | _, `Unknown ->
     E.raise (E.of_string "init: unknown: Cannot proceed.")
 *)

let propagate_clauses ~(hs:S.t) ~(frames:frames) : frames =
  let open Frame in
  let open SpaceexComponent in
  let open Z3Intf in
  let () =
    lazy (printf "(*** Propagate clauses ***)@.") |> U.debug !U.debug_events
  in
  let locs = locations hs in
  (* i = Array.length - 1 is the remainder frame.  We don't do
     induction from this i.  Therefore, to Array.length fs - 2. *)
  for i = 0 to Array.length frames - 2 do
    let () =
      lazy (printf "Frame %d@." i) |> U.debug !U.debug_propagate_clauses
    in
    (* Atomic formulas in this frame. *)
    let atomics : Z3.Expr.expr list =
      fold
        ~init:[]
        ~f:(fun acc (l,e) ->
          let atomics = ParseFml.extract_atomics e in
          List.dedup_and_sort ~compare:compare (atomics @ acc))
        frames.(i)
    in
    let () =
      lazy (printf "atomics: %a@." (U.pp_list pp_expr ()) atomics) |> U.debug !U.debug_propagate_clauses
    in
    (* Filter out the formulas that do not hold at the initial frame (i.e., frames.(0)). *)
    let atomics =
      List.filter
        atomics
        ~f:(fun fml ->
          let postframe : Z3.Expr.expr frame = lift locs fml in
          let res = apply is_valid (apply2 mk_implies frames.(0) postframe) in
          Frame.fold
            ~f:(fun acc (_,b) -> acc && b)
            ~init:true
            res
        )
    in
    let () =
      lazy (printf "filtered atomics: %a@." (U.pp_list pp_expr ()) atomics) |> U.debug !U.debug_propagate_clauses
    in
    (* Filter out the formulas that are not invariants with respect to frames.(i). *)
    let atomics =
      List.filter
        atomics
        ~f:(fun a ->
          let preframe = apply (mk_and a) frames.(i) in
          let postframe = lift locs a in
          let wp = wp ~is_partial:true ~is_continuous:false hs postframe in
          (* let wp_elimed = apply Dl.dl_elim_dyn wp in *)
          (* let vc = apply2 mk_implies preframe wp_elimed in *)
          (* let vc = apply2 mk_implies preframe wp in *)
          let res = apply2 Dl.is_valid_implication (apply Dl.mk_dl_prim preframe) wp in
          let res =
            fold
              ~init:true
              ~f:(fun b (_,r) ->
                match b,r with
                | true,`Valid -> true
                | _ -> false)
              res
          in
          res)
    in
    let invfml = List.fold_left ~init:mk_true ~f:mk_and atomics in
    let () =
      lazy (printf "%ainvfml:%a%a@." U.pp_start_style U.Green pp_expr invfml U.pp_end_style ();
            printf "%apre:%a%a@." U.pp_start_style U.Green (Frame.pp_frame pp_expr) frames.(i) U.pp_end_style ())
      |> U.debug !U.debug_propagate_clauses
    in
    (* Strengthen frames.(i) with atomics. *)
    frames.(i) <- apply (mk_and invfml) (frames.(i) |> apply simplify)
  done;
  frames

let resolve_conflict_query (is_continuous:bool) (tactic_in: In_channel.t) (frames: frames) (hs:S.t) (preframe:Z3.Expr.expr Frame.frame) (eframe : Z3.Expr.expr Frame.frame) (loc:S.id) idx : frames =
  let open Z3Intf in
  let vars : string list = Env.domain hs.params |> List.map ~f:SpaceexComponent.string_of_id in
  let () =
    lazy (
        printf "(*** resolve_conflict_query ***)@.";
        printf "Frame %d at location %a@." idx S.pp_id loc;
        printf "preframe: %a@." (F.pp_frame pp_expr) preframe;
        printf "eframe : %a@." (F.pp_frame pp_expr) eframe
      ) |> U.debug !U.debug_events
  in
  let locs = S.locations hs in
  let open Frame in
  (* [XXX] Factor out the common computation shared with "not continuous" case. *)
  let compute_interpolant_frame_continuous () =
    List.fold_left
      locs
      ~init:(lift locs mk_false)
      ~f:(fun resframe locid ->
        let open S in
        let open Z3Intf in
        (* let srcid,tgtid,guard = t.source,t.target,t.guard in *)
        let loc = Env.find_exn hs.locations locid in
        let () = printf "Location: %a@." pp_id locid in
        let preframe_expr = Frame.find preframe locid in
        let preflow,preinv = loc.flow, loc.inv in
        let cex = Frame.find eframe locid in
        let interpolant =
          let open Sexp in
          let initexpr = Frame.find frames.(0) locid in
          let () =
            printf "Give an interpolant for the following problem:@.";
            printf "Pre:%a@." pp_expr preframe_expr;
            printf "Flow:%a@." SpaceexComponent.pp_flow preflow;
            printf "Inv:%a@." pp_expr preinv;
            printf "Continuous:%b@." is_continuous;
            printf "Init:%a@." pp_expr initexpr;
            printf "------@.";
            printf "CEX:%a@." pp_expr cex;
            printf ">>> @.";
          in
          let cex_unsat = Z3Intf.callZ3 cex in
          let pre_unsat = Z3Intf.callZ3 preframe_expr in
          let init_unsat = Z3Intf.callZ3 initexpr in
          match cex_unsat, pre_unsat, init_unsat with
          | `Unsat, _, _ ->
             (* If cex is unsatifiable, then true is an interpolant. *)
             mk_true
          | _, `Unsat, `Unsat ->
             (* Both pre and init are unsat, then false is an interpolant *)
             mk_false
          | _ ->
             let queried =
               try Sexp.input_sexp tactic_in with
               | End_of_file -> Sexp.input_sexp stdin
             in
             let () = printf "input sexp: %a@." Sexp.pp queried in
             let z3 = ParseFml.sexp_to_z3 queried in
             let () = printf "z3:%a@." pp_expr z3 in
             z3
        in
        let res = apply_on_id (mk_or interpolant) locid resframe in
        res)
    |> apply simplify
  in
  let compute_interpolant_frame_not_continuous () =
    MySet.fold
      ~init:(lift locs mk_false)
      ~f:(fun resframe t ->
        let open S in
        let open Z3Intf in
        let srcid,tgtid,guard = t.source,t.target,t.guard in
        let () =
          printf "Transition: %a --> %a@." pp_id srcid pp_id tgtid
        in
        (* let invcont_preframe_expr = Frame.find invcont_preframe srcid in *)
        let preframe_expr = Frame.find preframe srcid in
        let preflow,preinv =
          let preloc = Env.find_exn hs.locations srcid in
          preloc.flow, preloc.inv
        in
        let cex = Frame.find eframe tgtid in
        let interpolant =
          let open Sexp in
          let initexpr = Frame.find frames.(0) srcid in
          let () =
            printf "Give an interpolant for the following problem:@.";
            printf "Pre:%a@." pp_expr preframe_expr;
            printf "Flow:%a@." SpaceexComponent.pp_flow preflow;
            printf "Inv:%a@." pp_expr preinv;
            printf "Guard:%a@." pp_expr guard;
            printf "Command:%a@." SpaceexComponent.pp_command t.command;
            printf "Continuous:%b@." is_continuous;
            printf "Init:%a@." pp_expr initexpr;
            printf "------@.";
            printf "CEX:%a@." pp_expr cex;
            printf ">>> @.";
          in
          let cex_unsat = Z3Intf.callZ3 cex in
          let pre_unsat = Z3Intf.callZ3 preframe_expr in
          let init_unsat = Z3Intf.callZ3 initexpr in
          match cex_unsat, pre_unsat, init_unsat with
          | `Unsat, _, _ ->
             (* If cex is unsatifiable, then true is an interpolant. *)
             mk_true
          | _, `Unsat, `Unsat ->
             (* Both pre and init are unsat, then false is an interpolant *)
             mk_false
          | _ ->
             let queried =
               try Sexp.input_sexp tactic_in with
               | End_of_file -> Sexp.input_sexp stdin
             in
             let () = printf "input sexp: %a@." Sexp.pp queried in
             let z3 = ParseFml.sexp_to_z3 queried in
             let () = printf "z3:%a@." pp_expr z3 in
             z3
        in
        let res = apply_on_id (mk_or interpolant) tgtid resframe in
        res)
      hs.transitions |> apply simplify
  in
  let interpolant_frame =
    if is_continuous then
      compute_interpolant_frame_continuous ()
    else
      compute_interpolant_frame_not_continuous ()
  in
  for i = 1 to idx do
    let () = lazy (printf "Strengthen: %d@." i) |> Util.debug !Util.debug_resolve_conflict in
    let () =
      lazy (printf "Before:@.%a@." pp_frames frames) |> Util.debug !Util.debug_resolve_conflict
    in
    frames.(i) <- apply2 mk_or frames.(0) (apply2 mk_and frames.(i) interpolant_frame);
    let () =
      lazy (printf "After:@.%a@." pp_frames frames) |> Util.debug !Util.debug_resolve_conflict
    in
    ()
  done;
  let ret = frames |> Array.map ~f:(apply simplify) in
  let () =
    lazy (printf "New frames:@.%a@." pp_frames ret)
    |> Util.debug !Util.debug_resolve_conflict
  in
  ret
  
let resolve_conflict (is_continuous:bool) (frames: frames) (hs:S.t) (preframe:Z3.Expr.expr Frame.frame) (eframe : Z3.Expr.expr Frame.frame) (loc:S.id) idx : frames =
  let open Z3Intf in
  let () =
    lazy (
        printf "(*** resolve_conflict ***)@.";
        printf "Frame %d at location %a@." idx S.pp_id loc;
        printf "preframe: %a@." (F.pp_frame pp_expr) preframe;
        printf "eframe : %a@." (F.pp_frame pp_expr) eframe
      ) |> U.debug !U.debug_events
  in
  let locs = S.locations hs in
  (* let not_ce = mk_not (Frame.find eframe loc) in *)
  let open Frame in
  (* invcont_preframe is [inverted_flow | inv]preframe *)
  let invcont_preframe =
    apply_loc
      (fun (loc,z3expr_preframe) ->
        let preloc = Env.find_exn hs.locations loc in
        let preflow_inverted = S.invert_flow preloc.flow in
        let preinv = preloc.inv in
        let ret = Dl.mk_dl_dyn ~is_partial:false preflow_inverted preinv (Dl.mk_dl_prim z3expr_preframe) in
        ret)
      preframe |> apply Dl.simplify
  in
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
      hs.transitions |> apply Dl.simplify
  in
  (* let inv_preframe_elimed = (* apply Dl.dl_elim_dyn invdisc_preframe *) invdisc_preframe in *)
  let invdisc_preframe = apply2 Dl.mk_dl_or invdisc_preframe (apply Dl.mk_dl_prim frames.(0)) in
  let interpolants = apply2 Dl.interpolant invdisc_preframe (apply Dl.mk_dl_prim eframe) in
  let interpolants =
    apply_loc
      (fun (loc,st) ->
        match st with
        | `InterpolantFound e -> simplify e
        | _ ->
            mk_true
            (* F.find eframe loc |> mk_not *)
      )
      interpolants
  in
  let () =
    lazy (printf "invcont_preframe: %a@." (pp_frame Dl.pp) invcont_preframe;
          printf "invdisc_preframe: %a@." (pp_frame Dl.pp) invdisc_preframe;
          printf "eframe : %a@." (F.pp_frame pp_expr) eframe;
          printf "Interpolant: %a@." (pp_frame pp_expr) interpolants)
    |> Util.debug !Util.debug_resolve_conflict
  in
  for i = 1 to idx do
    let () = lazy (printf "Strengthen: %d@." i) |> Util.debug !Util.debug_resolve_conflict in
    frames.(i) <- apply2 mk_and frames.(i) interpolants
  done;
  let ret = Array.map ~f:(apply simplify) frames in
  let () =
    lazy (printf "New frames:@.%a@." pp_frames ret)
    |> Util.debug !Util.debug_resolve_conflict
  in
  ret
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
  

let rec remove_cti (tactic_in:In_channel.t) (hs:S.t) (cexs:ce list) (frames:frames) : frames =
  let open Frame in
  let open Z3Intf in
  (* Sort in the increasing order of the index part. *)
  let () =
    lazy (printf "(*** remove_cti ***)@.";
          printf "Current cexs are:@.";
          printf "%a@." (U.pp_list pp_ce ()) cexs)
    |> Util.debug !Util.debug_events
  in
  let cexs = List.sort ~compare:(fun (_,_,idx1) (_,_,idx2) -> compare idx1 idx2) cexs in
  match cexs with
  | [] -> frames
  | (loc,e,idx)::tl ->
     let () =
       lazy (printf "remove_cti: processing %a@." pp_ce (loc,e,idx))
       |> Util.debug !Util.debug_remove_cti
     in
     if idx = 0 then
       raise (Counterexample(loc,e,idx))
     else
       (* Check that (loc,e,idx) is still a counterexample *)
       let e' = Frame.find frames.(idx) loc in
       mk_and e e' |> callZ3 |>
         function
         | `Unsat ->
            (* If this is not a counterexample anymore, skip this. *)
            remove_cti tactic_in hs tl frames
         | `Sat _ | `Unknown ->
            let is_continuous = idx = (Array.length frames - 1) in
            let locs = S.locations hs in
            let preframe : Dl.t frame = apply Dl.mk_dl_prim frames.(idx-1) in
            let propagated = propagate_one_step ~is_continuous ~hs ~ce:(loc,e,idx) ~preframe in
            let propagated_init = propagate_one_step ~is_continuous ~hs ~ce:(loc,e,idx)
                                    ~preframe:(frames.(0) |> apply Dl.mk_dl_prim) in
            match propagated_init,propagated with
            | hd::_, _ ->
               raise (Counterexample(hd))
            | _, [] ->
               let preframe = frames.(idx-1) in
               let eframe : Z3.Expr.expr frame = lift locs mk_false |> apply_on_id (mk_or e) loc in
               (* let preframe = F.apply Dl.dl_elim_dyn preframe in *)
               let newframes =
                 if !U.query_for_reolsve_conflict then
                   resolve_conflict_query is_continuous tactic_in frames hs preframe eframe loc idx
                 else
                   resolve_conflict is_continuous frames hs preframe eframe loc idx
               in
               let () = lazy (printf "newframes: %a@." pp_frames newframes) |> Util.debug !Util.debug_remove_cti in
               remove_cti tactic_in hs tl newframes
            | _ -> remove_cti tactic_in hs (propagated @ cexs) frames
and propagate_one_step ~is_continuous ~hs ~preframe ~ce =
  let open Frame in
  let open Z3Intf in
  let loc,e,idx = ce in
  let locs = S.locations hs in

  let eframe : Z3.Expr.expr frame = lift locs mk_false |> apply_on_id (mk_or e) loc in

  let wpframe : Dl.t frame = wp ~is_partial:false ~is_continuous hs eframe in
  let () =
    lazy (printf "is_continous: %b@." is_continuous;
          printf "eframe: %a@." (pp_frame pp_expr) eframe;
          printf "wpframe: %a@." (pp_frame Dl.pp) wpframe;
          printf "preframe: %a@." (pp_frame Dl.pp) preframe)
    |> U.debug !U.debug_propagate_one_step
  in
  let res = apply2 Dl.is_satisfiable_conjunction preframe wpframe in
  let propagated =
    Frame.fold
      res
      ~init:[]
      ~f:(fun l (loc,r) ->
        match r with
        | `Unsat -> l
        | `Sat m -> (loc,expr_of_model m,idx-1)::l
        | `Unknown ->
           U.not_implemented "propagated: unknown")
  in
  let () =
    lazy (printf "backpropagated to:@.%a@.from:@.%a@." (U.pp_list pp_ce ()) propagated (Frame.pp_frame pp_expr) eframe)
    |> U.debug !U.debug_propagate_one_step
  in
  propagated

let%test _ =
  let open Z3Intf in
  let open Frame in
  let hs = SpaceexComponent.parse_from_channel (In_channel.create (!Config.srcroot ^ "/examples/examples/circle/circle.xml")) |> List.hd_exn in
  let loc1,loc2 = S.id_of_string "1", S.id_of_string "2" in
  let x,y = mk_real_var "x", mk_real_var "y" in
  let expr =
    [mk_le x (mk_real_numeral_float (5433.0 /. 3125.0));
     mk_ge x (mk_real_numeral_float (5433.0 /. 3125.0));
     mk_le y (mk_real_numeral_float (990679.0 /. 1000000.0));
     mk_ge y (mk_real_numeral_float (990679.0 /. 1000000.0))]
    |> List.fold_left ~init:mk_true ~f:mk_and
  in
  let ce = (loc2, expr, 1) in
  let preframe = lift [loc1; loc2] (Dl.mk_dl_prim mk_true) in
  let eframe = lift [loc1; loc2] mk_false |> apply_on_id (mk_or expr) loc2 in
  let propagated = propagate_one_step ~is_continuous:false ~hs ~preframe ~ce in
  let () =
    lazy (
        printf "TEST: backpropagated to:@.%a@.from:@.%a@." (U.pp_list pp_ce ()) propagated (Frame.pp_frame pp_expr) eframe)
    |> U.debug false
  in
  List.for_all propagated
    ~f:(fun (id,e,_) ->
      if S.string_of_id id = "2" then
        callZ3 e = `Unsat
      else
        true) &&
    List.exists propagated
      ~f:(fun (id,e,_) ->
        S.string_of_id id = "1" && (callZ3 e |> function `Sat _ -> true | _ -> false))
                    (*
backpropagated to:
At location "2", at frame 1, CE: (and (<= y (- 1.0)) (>= y (- 1.0)) (<= x 0.0) (>= x 0.0))
At location "1", at frame 1, CE: (and (<= y (- (/ 5433.0 3125.0)))
     (>= y (- (/ 5433.0 3125.0)))
     (<= x (/ 990679.0 1000000.0))
     (>= x (/ 990679.0 1000000.0)))
from:
[("1", false);
  ("2",
   (or (and (<= y (- (/ 5433.0 3125.0)))
         (>= y (- (/ 5433.0 3125.0)))
         (<= x (/ 990679.0 1000000.0))
         (>= x (/ 990679.0 1000000.0)))
    false))
  ]
                     *)

let rec extend_frontier_iter ~(tactic_in:In_channel.t) ~(hs:S.t) ~(frames:frames) ~(safe:Z3.Expr.expr) : frames =
  let open Frame in
  let open Z3Intf in
  let locs = S.locations hs in
  let len = Array.length frames in
  let preframe : Dl.t frame = apply Dl.mk_dl_prim frames.(len-1) in
  let wpframe : Dl.t frame = wp ~is_partial:true ~is_continuous:true hs (lift locs safe) in
  let () =
    lazy (printf "(*** Checking the safety of the frontier ***)@.";
          printf "pre: %a@." (pp_frame Dl.pp) preframe;
          printf "wp: %a@." (pp_frame Dl.pp) wpframe)
    |> U.debug !U.debug_events
  in
  let res = apply2 Dl.is_valid_implication preframe wpframe in
  (*
  let vc = apply2 Dl.mk_dl_and preframe wpframe in
  let () = printf "res: %a@." (pp_frame Dl.pp) vc in
   *)
  let cexs =
    fold
      ~init:[]
      ~f:(fun acc (loc,r) ->
        (*
        let () = printf "discharging: %a at loc %a@." Dl.pp z S.pp_id loc in
        let res = Dl.dl_discharge z in
         *)
        match r with
          | `Valid ->
              acc
          | `NotValid ms ->
              (* let () = printf "Cex: %a@." pp_model m in *)
              let newcex = List.map ms ~f:(fun m -> (loc,expr_of_model m,len-1)) in
              newcex @ acc
          | `Unknown ->
              U.not_implemented "extend_frontier_iter: unknown"
        )
      res
  in
  (* let () = printf "cexs: %a@." (U.pp_list pp_ce ()) cexs in *)
  match cexs with
  | [] -> frames
  | _ -> remove_cti tactic_in hs cexs frames
  
let extend_frontier ~tactic_in ~(frames:frames) ~(hs:S.t) ~(safe:Z3.Expr.expr) : frames =
  let open Frame in
  let () =
    lazy (printf "(*** Extending frontier ***)@.";
          printf "Old frames: %a@." pp_frames frames)
    |> U.debug !U.debug_events
  in
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
  let () =
    lazy (printf "new frames: %a@." pp_frames newframes)
    |> U.debug !U.debug_extend_frontier
  in
  extend_frontier_iter ~tactic_in ~frames:newframes ~safe ~hs
    
let rec verify_iter ~tactic_in ~(hs:S.t) ~(safe:Z3.Expr.expr) ~(candidates:ce list) ~(frames:frames) ~(iteration_num:int) =
  let open Frame in
  let open Z3Intf in
  let open SpaceexComponent in
  let frames = Array.map ~f:(apply simplify) frames in
  let () =
    lazy (printf "(*** Iteration of verification: %d ***)@." iteration_num;
          printf "frames:%a@." pp_frames frames)
    |> U.debug !U.debug_events
  in
  try
    let frames = Array.map ~f:(apply simplify) frames in
    let frames = extend_frontier ~tactic_in ~frames ~safe ~hs in
    let frames = propagate_clauses ~hs ~frames in
    let () =
      lazy (printf "current frames: %a@." pp_frames frames) |> U.debug !U.debug_events
    in
    let k = Array.length frames in
    for i = 1 to k-2 do
      let () =
        lazy (printf "Verifying safety at frame %d and %d@." (i-1) i) |> U.debug !U.debug_verify_iter
      in
      if fold2 ~init:true
          ~f:(fun b (loc,e) (loc',e') ->
              let () =
                lazy (printf "frame %d at loc %a: %a@." i S.pp_id loc pp_expr e;
                      printf "frame %d at loc %a: %a@." (i-1) S.pp_id loc' pp_expr e')
                |> U.debug !U.debug_verify_iter
              in
              b && (is_valid (mk_implies e e'))
             )
          frames.(i) frames.(i-1)
      then
        raise (SafetyVerified(i-1,frames))
    done;
    verify_iter ~tactic_in ~hs ~safe ~candidates ~frames ~iteration_num:(iteration_num+1)
  with
  | Counterexample ce -> Ng ce
  | SafetyVerified(i,frames) -> Ok (i,frames)
                      
let verify ~(tactic_in:In_channel.t) ~(hs:S.t) ~(initloc:S.id) ~(init:Z3.Expr.expr) ~(safe:Z3.Expr.expr) =
  try
    let init_frames = setup_init_frames ~hs ~initloc ~init ~safe in
    verify_iter ~tactic_in ~hs ~safe ~candidates:[] ~frames:init_frames ~iteration_num:0
  with
  | Counterexample ce -> Ng ce
