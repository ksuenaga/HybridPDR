open Core_kernel
open Format

module E = Error

type cont_triple_partial =
  { pre_loc_partial : SpaceexComponent.id;
    post_loc_partial : SpaceexComponent.id;
    flow_partial : SpaceexComponent.flow;
    pre_partial : Z3.Expr.expr;
    post_partial : Z3.Expr.expr;
    dynamics_partial : SpaceexComponent.flow;
    inv_partial : Z3.Expr.expr }

type cont_triple_total =
  { pre_loc_total : SpaceexComponent.id;
    post_loc_total : SpaceexComponent.id;
    pre_total : Z3.Expr.expr;
    post_total : Z3.Expr.expr;
    dynamics_total : SpaceexComponent.flow;
    inv_total : Z3.Expr.expr }

type index = int (* Index for the frames. *) [@@deriving show]
type ce = SpaceexComponent.id * Z3.Expr.expr * index
let pp_ce fmt (loc,e,idx) =
  fprintf fmt "At location %a, at frame %d, CE: %a" SpaceexComponent.pp_id loc idx Z3Intf.pp_expr e
  
type result =
  Conflict of ce
| Propagated of ce
 
let pp_cont_triple_partial fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv = %s }"
    SpaceexComponent.pp_id crp.pre_loc_partial
    SpaceexComponent.pp_id crp.post_loc_partial
    (Z3.Expr.to_string crp.pre_partial)
    (Z3.Expr.to_string crp.post_partial)
    SpaceexComponent.pp_flow crp.dynamics_partial
    (Z3.Expr.to_string crp.inv_partial)
let pp_cont_triple_total fmt crp =
  fprintf fmt
    "{ pre_loc=%a;@\n post_loc=%a;@\n pre = %s;@\n post = %s;@\n dynamics = %a;@\n inv_total = %s }"
    SpaceexComponent.pp_id crp.pre_loc_total
    SpaceexComponent.pp_id crp.post_loc_total
    (Z3.Expr.to_string crp.pre_total)
    (Z3.Expr.to_string crp.post_total)
    (* SpaceexComponent.pp_id crp.pre_loc *)
    SpaceexComponent.pp_flow crp.dynamics_total
    (Z3.Expr.to_string crp.inv_total)


(* [XXX] Add comment on the intuition. *)
type vcgen_partial =
  is_continuous:bool ->
  (* pre:Frame.frame -> *)
  pre_loc:SpaceexComponent.id ->
  pre_fml:Z3.Expr.expr ->
  atomic:Z3.Expr.expr ->
  (* post:Frame.frame -> *)
  (* post:Cnf.t -> *)
  (cont_triple_partial * SpaceexComponent.id * Z3.Expr.expr) list
(* The return value (loc, vcs) means that, all of the vcs need to be
   satisfied for the post to hold at location loc. *)
type vcgen_total =
  is_continuous:bool ->
  pre:Frame.frame ->
  post:Frame.frame ->
  candidate:ce ->
  cont_triple_total list
  
let to_vcgen_partial (hs : SpaceexComponent.t) : vcgen_partial =
  let open Frame in
  let open SpaceexComponent in
  let open DischargeVC in
  let ret ~(is_continuous : bool) ~(pre_loc : SpaceexComponent.id) ~(pre_fml : Z3.Expr.expr) ~(atomic : Z3.Expr.expr) =
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let open SpaceexComponent in
        let srcloc = Env.find_exn hs.locations t.source in
        let () = printf "t.source: %a@." pp_id t.source in
        let () = printf "srcloc.id: %a@." pp_id srcloc.id in
        if srcloc.id = pre_loc then
          let dynamics,inv = srcloc.flow,srcloc.inv in
          (* let pre_source = Frame.find_exn pre t.source in *)
          (* let post_target : Cnf.t = Frame.find_exn post t.target in *)
          let wp : Z3.Expr.expr =
            if (* Frame.is_continuous_frame post *) is_continuous then
              (* post_target *)
              atomic
            else
              Z3Intf.mk_implies t.guard (SpaceexComponent.wp_command t.command atomic)
          in
          ({pre_loc_partial=t.source;
            post_loc_partial=t.target;
            flow_partial=srcloc.flow;
            pre_partial=Z3Intf.mk_and pre_fml atomic;
            post_partial=wp;
            dynamics_partial=dynamics;
            inv_partial=inv},t.target,atomic)::vcs
        else
          vcs
      )
      hs.transitions
  in
  ret

let to_vcgen_total (hs : SpaceexComponent.t) : vcgen_total =
  let open Frame in
  let open SpaceexComponent in
  (* let open DischargeVC in*)
  let ret ~(is_continuous:bool) ~(pre:frame) ~(post:frame) ~(candidate:ce) =
    let loc,e,_ = candidate in
    let transitions = MySet.filter ~f:(fun t -> t.target = loc) hs.transitions in
    MySet.fold
      ~init:[]
      ~f:(fun vcs t ->
        let open Z3Intf in
        let srcloc = Env.find_exn hs.locations t.source in
        let tgtloc = Env.find_exn hs.locations t.target in
        let dynamics,inv_pre = srcloc.flow,srcloc.inv in
        let inv_post = tgtloc.inv in
        let pre_source = Frame.find_exn pre t.source in
        (* let post_target = Env.find_exn post t.target in *)
        let post_target = mk_and inv_post (mk_and (Frame.find_exn post t.target) e) in
        let wp : Z3.Expr.expr =
          if is_continuous then
            post_target
          else
            mk_and t.guard (SpaceexComponent.wp_command_z3 t.command post_target)
        in
        {pre_loc_total=t.source; post_loc_total=t.target; pre_total=pre_source; post_total=wp; dynamics_total=dynamics; inv_total=inv_pre}::vcs
      )
      transitions
      (* E.raise (E.of_string "to_vcgen: not implemented") *)
  in
  ret

let rec backward_simulation
          ?(discretization_rate = 0.01)
          ~(pre_loc : SpaceexComponent.id)
          ~(post : Z3.Expr.expr)
          ~(flow : SpaceexComponent.flow)
          ~(inv : Z3.Expr.expr)
          ~(pre : Z3.Expr.expr)
          ~(history : Z3.Expr.expr list)
          ~(idx_pre : int) : result =
  let open Z3Intf in
  let () = printf "backward_simulation@." in
  let potentialCE = simplify (mk_and inv (mk_and post pre)) in
  (* let potentialCE = simplify (mk_and post pre) in *)
  let checkCE = callZ3 potentialCE in
  match checkCE with
  | `Sat _ -> Propagated(pre_loc, potentialCE, idx_pre)
  | (`Unsat | `Unknown) ->
     let newpost = SpaceexComponent.prev_time ~discretization_rate ~flow ~post in
     let () = printf "newpost:%a@." pp_expr newpost in
     let checkConflict = callZ3 (simplify (mk_and newpost inv)) in
     match checkConflict with
     | `Unsat ->
        (* Assumption available here: post && pre is unsat. inv && prev(post) is unsat. *)
        (* Debug here *)
        (* Post is empty.  We found a conflict.  Compute an interpoalnt and return it. *)
        let e1 = simplify pre in
        let e2 = simplify (List.fold_left ~init:mk_false ~f:mk_or (newpost::history)) in
        (* let e1 = simplify (mk_and inv e1) in *)
        (* let e2 = simplify (mk_and inv e2) in *)
        (* let e2 = post in *)
        let () =
          printf "e1:%s@." (Z3.Expr.to_string (simplify e1));
          printf "e2:%s@." (Z3.Expr.to_string (simplify e2))
        in
        let intp = interpolant e1 e2 in
        let res =
          begin
            match intp with
            | `InterpolantFound intp ->
               let intp = simplify intp in
               let () = printf "Obtained interpolant (at %a): %s@." SpaceexComponent.pp_id pre_loc (Z3.Expr.to_string intp) in
               Conflict(pre_loc,intp,idx_pre)
            | `InterpolantNotFound ->
               Util.not_implemented "intp not found"
            | `NotUnsatisfiable ->
               assert(false)
          end
        in
        res
     | (`Unknown | `Sat _) ->
        (* Post may be still nonempty but CE is not found.  Go further. *)
        backward_simulation
          ~discretization_rate ~pre_loc ~post:newpost ~flow
          ~inv ~pre ~history:(post::history) ~idx_pre
  
  (*
  (* Check whether post is already empty. *)
  let checkPostEmpty = callZ3 (mk_and inv post) in
  (* Check whether there is a CE. *)
  let checkCE = callZ3 (mk_and (mk_and pre inv) post) in
  match checkCE, checkPostEmpty with
  | `Sat m, _ ->
     (* Counterexample found *)
     `Propagated(pre_loc, m)
  | (`Unsat | `Unknown), (`Sat _ | `Unknown) ->
     (* Post may be still nonempty but CE is not found.  Go further. *)
     let newpost = SpaceexComponent.prev_time ~discretization_rate ~flow ~post in
     backward_simulation
       ~discretization_rate ~pre_loc ~post:newpost ~flow
       ~inv ~pre ~history:(post::history)
  | (`Unsat | `Unknown), `Unsat ->
   *)
     
let%test _ =
  let open Z3Intf in
  let pre = (mk_le (mk_real_var "x") (mk_real_numeral_float 1.0)) in
  let post = (mk_and
               (mk_le (mk_real_var "y") (mk_real_numeral_float 0.0))
               (mk_eq (mk_real_var "x") (mk_real_numeral_float 2.0))) in
  let res =
    backward_simulation
      ~discretization_rate:0.01
      ~pre_loc:(SpaceexComponent.id_of_string "1")
      ~post:post
      ~flow:(SpaceexComponent.parse_flow "y'==x & x'==-y")
      ~inv:(mk_ge (mk_real_var "y") (mk_real_numeral_float 0.0))
      ~pre:pre
      ~history:[]
      ~idx_pre:0
  in
  match res with
  | Conflict(pre_loc,intp,idx) ->
     let vc1 = callZ3 (mk_and pre (mk_not intp)) in
     let vc2 = callZ3 (mk_and intp post) in
     let cond = (pre_loc = (SpaceexComponent.id_of_string "1")) in
     begin
       match vc1,vc2,cond with
         `Unsat,`Unsat,true -> true
       | _ -> false
     end
  | _ -> false
                  
(*  Util.not_implemented "backward_simulation" *)
  
let pp_propagated_conflict fmt p =
  match p with
  | Propagated(id,e,idx_pre) ->
     printf "Propagated to loc: %a, sample: %a, idx_pre:%d" SpaceexComponent.pp_id id Z3Intf.pp_expr e idx_pre
  | Conflict(id, intp, idx_pre) ->
     printf "Conflict at loc: %a, interp: %s, idx_pre:%d" SpaceexComponent.pp_id id (Z3.Expr.to_string intp) idx_pre

(* [XXX] Premature rough implementation *)
let discharge_vc_total ~(triple:cont_triple_total) ~(idx_pre:int) =
  let open Z3Intf in
  let () = printf "discharge_vc_total: %a@." pp_cont_triple_total triple in
  let () =
    printf "(* discharge_vc_total *)@.";
    printf "Post: %s@." (Z3.Expr.to_string triple.post_total);
    printf "Pre loc: %a@." SpaceexComponent.pp_id triple.pre_loc_total;
    printf "Post loc: %a@." SpaceexComponent.pp_id triple.post_loc_total;
    printf "Pre: %s@." (Z3.Expr.to_string triple.pre_total);
    printf "Inv: %s@." (Z3.Expr.to_string triple.inv_total);
    printf "Flow: %a@." SpaceexComponent.pp_flow triple.dynamics_total
  in
  let res =
    backward_simulation
      ~discretization_rate:0.01
      ~pre_loc:triple.pre_loc_total
      ~post:triple.post_total
      ~flow:triple.dynamics_total
      ~inv:triple.inv_total
      ~pre:triple.pre_total
      ~history:[]
      ~idx_pre:idx_pre
  in
  let () =
    printf "Result: %a@." pp_propagated_conflict res
    (* match res with Propagated(id,m,_) -> printf "Propagated@." | Conflict _ -> printf "Conflict.@." *)
  in
  res
(* E.raise (E.of_string "discharge_vc_total: not implemented.") *)


(* [XXX] to be implememnted. *)
let discharge_vc_partial (vc:cont_triple_partial) =
  (* Util.not_implemented "discharge_vc_partial" *)
  let open SpaceexComponent in
  let open Z3Intf in

  (* Check that vc.pre_partial is locally invariant. *)
  let prevfml =
    prev_time
      ~discretization_rate:0.01
      ~flow:vc.flow_partial
      ~post:vc.pre_partial
  in
  let res = Z3Intf.is_valid (mk_implies (mk_and vc.pre_partial vc.inv_partial) prevfml) in
  if res then
    Z3Intf.is_valid (mk_implies vc.pre_partial vc.post_partial)
  else
    false
  (*
  if SpaceexComponent.is_contractive
       ~inv:vc.inv_partial
       ~flow:vc.dynamics_partial
       ~fml:vc.pre_partial
       (* ~post:vc.post_partial *)
  then
    Util.not_implemented "discharge_vc_total"
  else
    false
   *)
