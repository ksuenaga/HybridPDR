open Core

exception DupStateId

type ('loc,'var) state =
  { id : 'loc;
    inv : Guard.t;
    dynamics : 'var Dynamics.t
  }
type ('loc,'var) transition =
  { src : ('loc,'var) state;
    target : ('loc,'var) state;
    guard : Guard.t;
    action : Action.t
  }

type ('loc,'var) t =
  { states : ('loc,'var) state list;
    transitions : ('loc,'var) transition list;
    init : 'loc; }

let empty_automaton id =
  { states = [];
    transitions = []; 
    init = id;
  }

let add_state st t =
  { t with states = st::t.states }

let%test _ =
  let open Guard in
  let open Dynamics in
  try
    ignore(empty_automaton 1 |> add_state { id = 1; inv = guardFalse; dynamics = empty_dynamics [1]});
    false
  with
  DupStateId -> true
;;  

