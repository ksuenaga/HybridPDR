# Hybrid PDR

## Depedency (APT packages)
  * gfortran
  * libgmp-dev
  * m4
  * python

```
apt update && apt install gfortran libgmp-dev m4 python
```

## Requirement (OPAM packages)
  * bau
  * bisect_ppx
  * core
  * dune
  * mparser
  * ocaml-migrate-parsetree
  * odepack
  * ppx_deriving
  * ppx_driver
  * sexplib
  * xml-light
  * z3

```
opam install bau bisect_ppx core dune mparser ocaml-migrate-parsetree odepack ppx_deriving ppx_driver sexplib xml-light z3
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

## Running test

```
dune runtest
```
