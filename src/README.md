# Hybrid PDR

# Requirement
  * jbuilder
  * Core
  * Sexplib

```
opam install jbuilder core sexplib
```

# How to build
```
dune build hpdrMain.exe --profile=release
```

# How to run

Run hpdrMain.exe (created in `_build/default`) with commandline arguments `-model`, `-init`, `-safe`, and `-initid`.  Use `--help` option for the meaning of each commandline arguments.

Tactics should be fed via the standard input.

Example:
```
_build/default/hpdrMain.exe -model examples/examples/circle/circle.xml -init "x <= 0.5" -safe "x <= 1.0" -initid 1 < examples/examples/circle/circle_tactic1.smt2
```

# Running test
```
dune runtest
```
