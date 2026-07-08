#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Regression tests for the ConformalStructures package.

   Run from the repository root with:

       wolframscript -file Tests/RegressionTests.wls

   The script loads the package from the working directory (not the copy
   installed under $UserBaseDirectory) so that local edits are tested.

   Two kinds of checks are performed:

     1. ConformalTest invariance.  ConformalTest[dim, dims, spins, perm]
        builds the expression obtained by acting with a special conformal
        generator on each conformal correlator.  By conformal invariance this
        must vanish identically as a function of the spacetime points, so we
        substitute random integer coordinates and assert that every component
        is numerically zero.

     2. StructureRelations correctness.  For a set of structures the function
        returns a matrix R (with cross-ratio-dependent entries for 4-point /
        defect configurations) such that R . structures == 0.  For each test
        we check both that the number of relations equals
        (#structures - rank) and that the relations vanish numerically at a
        generic configuration. *)

(* ------------------------------------------------------------------ *)
(* Load the local package                                             *)
(* ------------------------------------------------------------------ *)

repoRoot = DirectoryName[DirectoryName[$InputFileName]];
If[repoRoot === "", repoRoot = Directory[]];
PrependTo[$Path, repoRoot];
Needs["ConformalStructures`"];

loaded = FindFile["ConformalStructures`"];
Print["Loaded package from: ", loaded];
forwardSlash[p_] := StringReplace[p, "\\" -> "/"];
If[! StringContainsQ[forwardSlash[loaded], forwardSlash[repoRoot]],
  Print["ERROR: package loaded from ", loaded, ", not the repository checkout (", repoRoot,
        "). Aborting so the tests are not run against a different installed/deployed copy."];
  Exit[1]];

(* private helpers used to build cross-ratio-specific configurations *)
gp  = ConformalStructures`Private`genericPoint;
crs = ConformalStructures`Private`crossRatios;
scr = ConformalStructures`Private`safeCrossRatios;
fastEval    = ConformalStructures`Private`fastEval;
fastEvalCOC = ConformalStructures`Private`fastEvalCOC;

(* ------------------------------------------------------------------ *)
(* Minimal test harness                                               *)
(* ------------------------------------------------------------------ *)

$passed = 0;
$failed = 0;

assert[label_, cond_, info_: ""] := If[TrueQ[cond],
   $passed++; Print["  PASS  ", label],
   $failed++; Print["  FAIL  ", label, If[info === "", "", "  (" <> ToString[info] <> ")"]]
];

tol = 10^-8;

(* ------------------------------------------------------------------ *)
(* 1. ConformalTest invariance                                        *)
(* ------------------------------------------------------------------ *)

(* maximum absolute value of the SCT-acted correlator components at a
   random integer configuration; returns the numeric residual, or a
   String if the case is empty / fails to numericize. *)
conformalResidual[dim_, ds_, spins_, perm_, npts_, seed_: 11] := Module[
   {ct, comps, pt, res},
   ct = ConformalTest[dim, ds, spins, perm];
   If[Length[ct] == 0, Return["empty (no correlators)"]];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   SeedRandom[seed];
   pt = Thread[Flatten@Array[x, {npts, dim}] -> RandomInteger[{2, 40}, npts dim]];
   res = N[Flatten[comps /. pt]];
   If[AllTrue[res, NumericQ], Max[Abs[res]], "non-numeric residual"]
];

testConformal[label_, dim_, ds_, spins_, perm_, npts_] := Module[{r},
   r = conformalResidual[dim, ds, spins, perm, npts];
   assert[label <> "  [residual=" <> ToString[r, InputForm] <> "]", NumericQ[r] && r < tol, r]
];

Print["=== ConformalTest invariance (Euclidean) ==="];
SetSignature["Euclidean"];
testConformal["2D 3pt scalars",            2, {1, 1, 1},    {{0, 0}, {0, 0}, {0, 0}},                     {1, 2, 3},    3];
(* 2D is 2 mod 4, so this exercises the Weyl rotation generator that was broken
   there (WeylChargeConjugationMatrix vanishes); a spinning 2D case guards it. *)
testConformal["2D 3pt with a left-moving spin-1", 2, {2, 1, 1}, {{1, 0}, {0, 0}, {0, 0}},                 {1, 2, 3},    3];
testConformal["3D 3pt scalar-scalar-vector",3, {1, 1, 2},   {0, 0, 1},                                    {1, 2, 3},    3];
testConformal["3D 4pt with one vector",    3, {1, 1, 1, 2}, {0, 0, 0, 1},                                 {1, 2, 3, 4}, 4];
testConformal["4D 3pt with one vector",    4, {2, 1, 1},    {{1/2, 1/2}, {0, 0}, {0, 0}},                 {1, 2, 3},    3];
testConformal["4D 4pt scalars",            4, {1, 1, 1, 1}, {{0, 0}, {0, 0}, {0, 0}, {0, 0}},             {1, 2, 3, 4}, 4];

(* Changing the signature now flushes the memoized component caches (via
   ClearConformalCache, called from SetSignature), so a correlator may be
   reused across signatures; the dedicated section below tests exactly that. *)
Print["=== ConformalTest invariance (Lorentzian) ==="];
SetSignature["Lorentzian"];
testConformal["4D 2pt fermions",           4, {3, 3},       {{1/2, 1/2}, {1/2, 1/2}},                     {1, 2},       2];
testConformal["3D 2pt vectors",            3, {2, 2},       {1, 1},                                       {1, 2},       2];
SetSignature["Euclidean"];

(* ------------------------------------------------------------------ *)
(* 2. StructureRelations correctness                                  *)
(* ------------------------------------------------------------------ *)

(* rank of the structure components at a fixed generic configuration *)
structRank[structs_, dim_, q_, zz_: 7/3, i_: 1] := MatrixRank@N@Table[
   Flatten[Normal[CanonicallyOrderedComponents[s]] /. gp[dim, q, zz, i]], {s, structs}];

(* residual of relMatrix . structComponents at a fixed generic configuration *)
relationResidual[structs_, rel_, dim_, q_, zz_: 7/3, i_: 1] := Module[{comps, relN},
   If[Length[Normal[rel]] == 0, Return[0]];
   comps = Table[Flatten[Normal[CanonicallyOrderedComponents[s]] /. gp[dim, q, zz, i]], {s, structs}];
   relN  = Normal[rel] /. Thread[crs[dim, q] -> scr[q][[i]]];
   Max[Abs[N[Flatten[{relN . comps}]]]]
];

(* StructureRelations prints live progress in console mode; silence it. *)
quietRelations[structs_] := Block[{Print = (Null &), Run = (0 &)},
   StructureRelations[structs]];

Print["=== StructureRelations ==="];

(* (A) An independent set has no relations. *)
Module[{cc, rel},
   cc  = ConformalCorrelators[3, {1, 1, 1}, {1, 1, 0}];   (* 3 independent structures *)
   rel = quietRelations[cc];
   assert["3D 3pt vector-vector-scalar: independent set has no relations",
      Length[Normal[rel]] == 0, Normal[rel]]
];

(* (B) Two copies of one structure give exactly one relation s1 - s2 = 0. *)
Module[{s, rel},
   s   = ConformalCorrelators[3, {1, 1, 1}, {1, 1, 0}][[1]];
   rel = quietRelations[{s, s}];
   assert["duplicate structure -> {{1,-1}}", Normal[rel] === {{1, -1}}, Normal[rel]]
];

(* (C) 4D 4pt with one vector, two point orderings: an overcomplete set of 4
       structures spanning a 2-dimensional space.  Expect 2 relations whose
       coefficients are rational functions of the cross-ratios u, v. *)
Module[{ds, sp, a, b, structs, rel, rank, nrel, res},
   ds = {1, 1, 1, 2};
   sp = {{0, 0}, {0, 0}, {0, 0}, {1/2, 1/2}};
   a = ConformalCorrelators[4, ds, sp, {}, {1, 2, 3, 4}];
   b = ConformalCorrelators[4, ds, sp, {}, {2, 1, 3, 4}];
   structs = Join[a, b];
   rel  = quietRelations[structs];
   nrel = Length[Normal[rel]];
   rank = structRank[structs, 4, None];
   res  = relationResidual[structs, rel, 4, None];
   assert["4D 4pt vector (two orderings): #relations == #structs - rank",
      nrel == Length[structs] - rank, {nrel, Length[structs], rank}];
   assert["4D 4pt vector (two orderings): relations involve cross-ratios",
      ! FreeQ[Normal[rel], u | v], Normal[rel]];
   assert["4D 4pt vector (two orderings): relations vanish numerically  [residual=" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol, res]
];

(* ------------------------------------------------------------------ *)
(* 3. fastEval agrees with Components / CanonicallyOrderedComponents   *)
(* ------------------------------------------------------------------ *)

(* fastEval / fastEvalCOC are the numeric fast paths used by
   fittedRelations.  At a generic configuration they must reproduce the
   ordinary Components / CanonicallyOrderedComponents evaluated at the same
   point, i.e. for cross-ratio sample index ci:

     fastEval[s, z, scr[q][[ci]]]    == Components[s]                  /. gp[dim,q,z,ci]
     fastEvalCOC[s, z, scr[q][[ci]]] == CanonicallyOrderedComponents[s] /. gp[dim,q,z,ci]

   The fast path only applies to single-correlator structures with at least 4
   indices (and at most one derivative), so the cases below are spinning. *)

Print["=== fastEval vs Components / CanonicallyOrderedComponents ==="];
SetSignature["Euclidean"];

(* maximum |difference| between the fast and ordinary evaluations *)
feDiff[fastFn_, slowFn_, struct_, dim_, q_, z_, ci_] := Max@Abs@N@Flatten[
   fastFn[struct, z, scr[q][[ci]]] - (Normal[slowFn[struct]] /. gp[dim, q, z, ci])];

(* check both fastEvalCOC (canonical) and fastEval (natural order) for a structure *)
testFastEval[label_, dim_, ds_, sp_, derivs_, perm_, q_, z_: 3, ci_: 11] := Module[{cc, s, dCanon, dNat},
   cc = ConformalCorrelators[dim, ds, sp, derivs, perm, "DefectCodimension" -> q];
   If[Length[cc] == 0, assert[label <> " (no correlators built)", False]; Return[]];
   s = cc[[1]];
   dCanon = feDiff[fastEvalCOC, CanonicallyOrderedComponents, s, dim, q, z, ci];
   assert[label <> ": fastEvalCOC == CanonicallyOrderedComponents  [max|diff|=" <> ToString[dCanon, InputForm] <> "]",
      NumericQ[dCanon] && dCanon < tol, dCanon];
   dNat = feDiff[fastEval, Components, s, dim, q, z, ci];
   assert[label <> ": fastEval == Components  [max|diff|=" <> ToString[dNat, InputForm] <> "]",
      NumericQ[dNat] && dNat < tol, dNat];
];

testFastEval["4D 4pt, two vectors",        4, {1, 1, 1, 1}, {{1/2, 1/2}, {1/2, 1/2}, {0, 0}, {0, 0}}, {}, {1, 2, 3, 4}, None];
testFastEval["3D 4pt, two spin-1",         3, {1, 1, 1, 1}, {1, 1, 0, 0},                             {}, {1, 2, 3, 4}, None];
testFastEval["4D codim-2 defect 2pt, two vectors", 4, {2, 2}, {{1/2, 1/2}, {1/2, 1/2}},               {}, {1, 2},       2, 3, 5];
testFastEval["4D 4pt scalars, one \[PartialD] derivative", 4, {1, 1, 1, 1}, {{0, 0}, {0, 0}, {0, 0}, {0, 0}}, {{"\[PartialD]", 1}}, {1, 2, 3, 4}, None];
testFastEval["4D 4pt scalars, one u derivative",           4, {1, 1, 1, 1}, {{0, 0}, {0, 0}, {0, 0}, {0, 0}}, {{"u", 1}},          {1, 2, 3, 4}, None];

(* ------------------------------------------------------------------ *)
(* 4. Signature change invalidates the caches                         *)
(* ------------------------------------------------------------------ *)

(* The 3D scalar-scalar-vector correlator was already built and tested under
   Euclidean signature above.  Switching to Lorentzian must flush the cached
   (Euclidean) components so the same correlator is rebuilt; otherwise the stale
   components fail the Lorentzian invariance check.  We exercise a full round
   trip Euclidean -> Lorentzian -> Euclidean reusing the one correlator. *)

Print["=== Signature change invalidates caches ==="];
passUnder[sig_] := Module[{r},
   SetSignature[sig];
   r = conformalResidual[3, {1, 1, 2}, {0, 0, 1}, {1, 2, 3}, 3];
   NumericQ[r] && r < tol
];
assert["3D ssv invariant under Euclidean (cached from earlier)", passUnder["Euclidean"]];
assert["3D ssv invariant after switch to Lorentzian (cache flushed)", passUnder["Lorentzian"]];
assert["3D ssv invariant after switch back to Euclidean (cache flushed)", passUnder["Euclidean"]];

(* ClearConformalCache may also be called directly. *)
assert["ClearConformalCache[] runs without error and returns Null",
   ClearConformalCache[] === Null];

(* ------------------------------------------------------------------ *)
(* d = 6 (SO(6) = SU(4)) suite                                        *)
(* ------------------------------------------------------------------ *)

(* Run the d=6 tests in-process so CI covers them too.  $sixDIncluded tells
   SixDTests.wls to skip its own package-load, harness, and summary/Exit and
   share this suite's $passed / $failed tally. *)
$sixDIncluded = True;
Get[FileNameJoin[{repoRoot, "Tests", "SixDTests.wls"}]];

(* ------------------------------------------------------------------ *)
(* Summary                                                            *)
(* ------------------------------------------------------------------ *)

Print["================================================================"];
Print[ToString[$passed], " passed, ", ToString[$failed], " failed."];
If[$failed > 0, Exit[1], Exit[0]];
