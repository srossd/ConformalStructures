# ConformalStructures

[![tests](https://github.com/srossd/ConformalStructures/actions/workflows/tests.yml/badge.svg?branch=dev)](https://github.com/srossd/ConformalStructures/actions/workflows/tests.yml)

A Mathematica package for constructing conformally invariant spacetime
structures in `D = 2, 3, 4` dimensions, for correlators of up to four operators
of arbitrary scaling dimension and spin. It can also find algebraic relations
between structures and their derivatives by fitting rational functions of the
conformal cross-ratios.

## Installation

The package depends on [TensorTools](https://github.com/srossd/TensorTools).
Running the installer resolves that dependency and installs both:

```mathematica
Get["https://raw.githubusercontent.com/srossd/ConformalStructures/main/ConformalStructures/Install.m"]
```

Then load it in any session with

```mathematica
<< ConformalStructures`
```

## Conventions

- **Dimensions** `Δs` are given as a list, one entry per operator.
- **Spins** are given as a list, one entry per operator: in 3D each entry is a
  single (half-)integer spin; in 2D and 4D each entry is a pair `{left, right}`.
- **Signature** defaults to Euclidean; switch with `SetSignature["Lorentzian"]`.

## Examples

### Counting and building correlators

```mathematica
ConformalCorrelatorCount[3, {1, 1, 0}]                 (* 3 *)
ConformalCorrelatorCount[4, {{0,0},{0,0},{0,0},{0,0}}] (* 1 *)

(* a basis of independent structures, returned as Tensors *)
structs = ConformalCorrelators[3, {1, 1, 1}, {1, 1, 0}];   (* {Tensor[..], Tensor[..], Tensor[..]} *)
CanonicallyOrderedComponents[structs[[1]]]                  (* explicit components *)
```

`ConformalCorrelators[dim, Δs, spins, derivs, perm]` optionally takes a list of
derivatives and a permutation of the points; pass `"DefectCodimension" -> q`
for defect configurations.

### Checking conformal invariance

`ConformalTest` returns the action of a special conformal generator on each
correlator, which must vanish identically in the spacetime points:

```mathematica
test = CanonicallyOrderedComponents /@ ConformalTest[4, {3, 3}, {{1/2,1/2}, {1/2,1/2}}, {1, 2}];
pt   = Thread[Flatten@Array[x, {2, 4}] -> RandomInteger[10, 8]];
Simplify[test /. pt]      (* all zeros *)
```

### Relations between structures

`StructureRelations` returns a matrix `R` of coefficients (rational functions of
the cross-ratios for four-point / defect configurations) such that
`R . structures == 0`:

```mathematica
StructureRelations[ConformalCorrelators[3, {1,1,1}, {1,1,0}]]   (* {} : already independent *)

a = ConformalCorrelators[4, {1,1,1,2}, {{0,0},{0,0},{0,0},{1/2,1/2}}, {}, {1,2,3,4}];
b = ConformalCorrelators[4, {1,1,1,2}, {{0,0},{0,0},{0,0},{1/2,1/2}}, {}, {2,1,3,4}];
StructureRelations[Join[a, b]]    (* relations with u, v dependent coefficients *)
```

### Signature

```mathematica
SetSignature["Lorentzian"];   (* flushes cached components via ClearConformalCache[] *)
SetSignature["Euclidean"];
```

## Tests

A regression suite lives in [`Tests/RegressionTests.wls`](Tests/RegressionTests.wls).
Run it from the repository root:

```
wolframscript -file Tests/RegressionTests.wls
```

It checks conformal invariance via `ConformalTest`, the correctness of
`StructureRelations`, agreement of the `fastEval` numeric path with
`Components` / `CanonicallyOrderedComponents`, and cache invalidation on
signature changes. The script exits with a nonzero status if any check fails.

## Branches and CI

Development happens on `dev` (the development layout, with this `Tests/`
directory); `main` holds the flattened, install-ready paclet that
[`Install.m`](Install.m) downloads. `main` is generated automatically — do not
edit it by hand.

On every push to `dev`:

1. [`tests.yml`](.github/workflows/tests.yml) runs the regression suite on a
   self-hosted runner (a Raspberry Pi, where the Wolfram Engine is free and
   pre-licensed).
2. If the tests pass, [`deploy.yml`](.github/workflows/deploy.yml) flattens
   `dev`'s `ConformalStructures/` source plus `PacletInfo.m` and `README.md`
   into the root of `main` and pushes it (no Wolfram needed — pure file
   restructuring, so it runs on a free GitHub-hosted runner).
