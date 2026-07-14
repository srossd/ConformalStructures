# ConformalStructures

[![tests](https://github.com/srossd/ConformalStructures/actions/workflows/tests.yml/badge.svg?branch=dev)](https://github.com/srossd/ConformalStructures/actions/workflows/tests.yml)

A Mathematica package for constructing conformally invariant spacetime
structures in `D = 2, 3, 4, 6` dimensions, for correlators of up to four operators
of arbitrary scaling dimension and spin (in 6D, arbitrary SO(6) ≅ SU(4)
representation). It can also find algebraic relations between structures and their
derivatives by fitting rational functions of the conformal cross-ratios.

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
  single (half-)integer spin; in 2D and 4D each entry is a pair `{left, right}`;
  in 6D each entry is a triple of SU(4) Dynkin labels `{a, b, c}` (so a scalar is
  `{0,0,0}`, a vector `{0,1,0}`, and the stress tensor `{0,2,0}`).
- **Signature** defaults to Euclidean; switch with `SetSignature["Lorentzian"]`.
- **6D counting** uses SU(4) tensor-product decompositions from
  [GroupMath](https://renatofonseca.net/groupmath), loaded on demand (only
  `ConformalCorrelatorCount[6, …]` needs it).

## Examples

### Counting and building correlators

```mathematica
ConformalCorrelatorCount[3, {1, 1, 0}]                     (* 3 *)
ConformalCorrelatorCount[4, {{0,0},{0,0},{0,0},{0,0}}]     (* 1 *)
ConformalCorrelatorCount[6, {{0,1,0},{0,1,0},{0,1,0}}]     (* 4 : <VVV> in 6D *)

(* a basis of independent structures, returned as Tensors *)
structs = ConformalCorrelators[3, {1, 1, 1}, {1, 1, 0}];   (* {Tensor[..], Tensor[..], Tensor[..]} *)
CanonicallyOrderedComponents[structs[[1]]]                  (* explicit components *)

(* 6D works the same way, with SU(4) Dynkin labels for the spins *)
structs6 = ConformalCorrelators[6, {4, 4, 4}, {{0,1,0}, {0,1,0}, {0,1,0}}];  (* 4 structures *)
```

`ConformalCorrelators[dim, Δs, spins, derivs, perm]` optionally takes a list of
derivatives and a permutation of the points; pass `"DefectCodimension" -> q`
for defect configurations. (In 6D these two options are not yet supported —
6D covers bulk correlators of local operators.)

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

This works in 6D as well (four-point relations with SU(4) spins); higher-spin 6D
correlators use a fast numeric evaluation path, so it stays practical.

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
The 6D suite lives in [`Tests/SixDTests.wls`](Tests/SixDTests.wls).
