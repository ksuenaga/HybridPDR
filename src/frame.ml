open Core_kernel
open Format
module S = SpaceexComponent

type 'a frame = (S.id,'a) Env.t

let pp_frame pf fmt frame =
  fprintf fmt "%a" (Env.pp S.pp_id pf) frame
           
module E = Error

let lift (locs:S.id list) (v : 'a) : 'a frame =
  List.fold_left
    ~init:Env.empty
    ~f:(fun e (l:S.id) -> Env.add l v e)
    locs

let find (frame:'a frame) (loc:S.id) : 'a = Env.find_exn frame loc
  
let apply_on_id (f : 'a -> 'a) (id:S.id) (frame:'a frame) : 'a frame =
  let e = find frame id in
  Env.add id (f e) frame

let apply (f : 'a -> 'b) (frame:'a frame) : 'b frame =
  Env.map f frame

let apply2 (f : 'a -> 'b -> 'c) (frame1:'a frame) (frame2:'b frame) : 'c frame =
  Env.map2 f frame1 frame2

let fold ~(init : 'b) ~(f : 'b -> 'a -> 'b) (frame:'a frame) : 'b =
  let open Z3Intf in
  Env.fold ~init:init ~f:(fun e1 (_,e2) -> f e1 e2) frame
    
(*         
let extract_atomics (f:frame) =
  Env.fold
    ~init:[]
    ~f:(fun acc (loc,c) ->
      (List.map ~f:(fun c -> (loc,c)) (ParseFml.extract_atomics c)) @ acc)
    f
  
let frame_lift_given_id
      (locs:S.id list)
      (id:S.id)
      (cnf:Cnf.t)
    : frame =
  let default = frame_lift locs Z3Intf.mk_false in
  Env.add id cnf default
  
let equal f1 f2 = Env.equal f1 f2

let pp_locfml fmt (id,e) =
  fprintf fmt "loc %a: %s" S.pp_id id (Z3.Expr.to_string e)
  
let pp_locfmls fmt l =
  fprintf fmt "%a" (Util.pp_list pp_locfml) l
                
let strengthen ~(locfmls:(S.id*Z3.Expr.expr) list) ~(t:frame) : frame =
  let ret =
    List.fold_left
      ~init:t
      ~f:(fun frame (loc,fml) ->
        (*
        let _ = printf "before frame: %a@." pp_frame frame in
        let _ = printf "loc: %a@." S.pp_id loc in
         *)
        let p = Env.find_exn t loc in
        (* let fml = Cnf.cnf_lift_atomic (Cnf.z3_to_atomic fml) in *)
        (*
        let _ = printf "fml: %a@." Cnf.pp fml in
        let _ = printf "p(before): %a@." Cnf.pp p in
         *)
        let p = Z3Intf.simplify (Z3Intf.mk_and p fml) in
        (* let _ = printf "p(after): %a@." Cnf.pp p in *)
        let frame = Env.add loc p frame in
        (* let _ = printf "after frame: %a@." pp_frame frame in *)
        frame
      )
      locfmls
  in
  (* let _ = printf "Returned frame: %a@." pp_frame ret in *)
  ret

let from_assoclist l : frame =
  List.fold_left
    ~init:Env.empty
    ~f:(fun env (l,cnf) ->
      Env.add l cnf env
    )
    l
                                          
let is_valid_implication_frame (e1:frame) (e2:frame) =
  Env.fold2
    e1 e2
    ~init:`Valid
    ~f:(fun res (loc1,c1) (loc2,c2) ->
      assert(loc1=loc2);
      match res with
      | `Valid | `NotValidNoModel ->
         Cnf.is_valid_implication loc1 c1 c2
      | `NotValid (loc,m) -> `NotValid (loc,m))

let is_valid_implication_cnf (e:frame) (c:Cnf.t) =
  let locs = Env.domain e in
  is_valid_implication_frame e (frame_lift locs c)

  
let%test _ =
  let open Z3Intf in
  let open S in
  let pf =
    from_assoclist
      [(id_of_string "1", Z3Intf.mk_true);
       (id_of_string "2", Z3Intf.mk_false)]
  in
  let e1 = mk_not (mk_eq (mk_add (mk_real_var "y") (mk_neg (mk_real_var "x"))) (mk_real_numeral_float (-.2.0))) in
  let locfmls = 
    [(id_of_string "1", e1);
     (id_of_string "2", mk_false)]
  in
  (* let frame = pf in *)
  let frame = strengthen ~locfmls:locfmls ~t:pf in
  let e1' = simplify e1 in
  let e2cnf = Env.find_exn frame (id_of_string "1") in
  let e2' = simplify e2cnf in
  let _ =
    printf "e1': %s@." (Z3.Expr.to_string e1');
    printf "e2cnf: %a@." Cnf.pp e2cnf;
    printf "e2': %s@." (Z3.Expr.to_string e2')
  in
  Z3Intf.expr_equal e1' e2'

 *)
