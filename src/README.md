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

## How to build

```
dune build hpdrMain.exe --profile=release
```

This command will produce `src/_build/default/hpdrMain.exe`.

## Running test

```
dune runtest
```
