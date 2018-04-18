type 'a stateId = 'a
type 'a state =
  { id : 'a stateId;
    inv : Guard.t;
    dynamics : Dynamics.t
  }
type 'a transition =
  { src : 'a state;
    target : state;
    guard : Guard.t;
    action : Action.t
  }
