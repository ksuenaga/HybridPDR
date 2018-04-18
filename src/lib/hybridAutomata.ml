
type stateId = int
type state =
  { id : stateId;
    inv : Guard.t;
    dynamics : Dynamics.t
  }
type transition =
  { src : state;
    target : state;
    guard : Guard.t;
    action : Action.t
  }

