#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Validation for the d = 6 (SO(6) = SU(4)) support.

   Runs standalone from the repository root:

       wolframscript -file Tests/SixDTests.wls

   It is also included by Tests/RegressionTests.wls, which runs the whole suite
   (main + d=6) under one process on CI: RegressionTests sets $sixDIncluded and
   Get[]s this file, so the package-load, harness, and summary/Exit below are
   skipped and the two suites share one $passed / $failed tally.  Milestone P0
   covers the representation primitives: the SU(4) Dynkin -> Young-projector
   pipeline and spinIndices[6], validated by reproducing known irrep dimensions. *)

If[! TrueQ[$sixDIncluded],
   repoRoot = DirectoryName[DirectoryName[$InputFileName]];
   If[repoRoot === "", repoRoot = Directory[]];
   PrependTo[$Path, repoRoot];
   Needs["ConformalStructures`"];
   loaded = FindFile["ConformalStructures`"];
   Print["Loaded package from: ", loaded];
   forwardSlash[p_] := StringReplace[p, "\\" -> "/"];
   If[! StringContainsQ[forwardSlash[loaded], forwardSlash[repoRoot]],
     Print["ERROR: package loaded from ", loaded, ", not the repository checkout (", repoRoot, ")."];
     Exit[1]];
];

dim6      = ConformalStructures`Private`su4IrrepDimension;
partition = ConformalStructures`Private`dynkinToPartition;
boxes     = ConformalStructures`Private`dynkinBoxes;
validQ    = ConformalStructures`Private`validDynkinQ;
spinInds  = ConformalStructures`Private`spinIndices;

If[! TrueQ[$sixDIncluded],
   $passed = 0; $failed = 0;
   assert[label_, cond_, info_: ""] := If[TrueQ[cond],
      $passed++; Print["  PASS  ", label],
      $failed++; Print["  FAIL  ", label, If[info === "", "", "  (" <> ToString[info, InputForm] <> ")"]]];
];

(* ------------------------------------------------------------------ *)
(* P0.1 SU(4) irrep dimensions via the Young projector                *)
(* ------------------------------------------------------------------ *)

Print["=== SU(4) irrep dimensions (Young projector) ==="];
(* {Dynkin, expected dim, description} *)
dimCases = {
   {{1, 0, 0}, 4,  "Weyl spinor"},
   {{0, 1, 0}, 6,  "vector"},
   {{2, 0, 0}, 10, "self-dual 3-form"},
   {{1, 1, 0}, 20, "vector-spinor"},
   {{0, 2, 0}, 20, "stress tensor (traceless sym rank 2)"},
   {{0, 0, 0}, 1,  "singlet"},
   {{0, 0, 1}, 4,  "anti-chiral spinor"},
   {{1, 0, 1}, 15, "adjoint"},
   {{3, 0, 0}, 20, "[3,0,0]"},
   {{1, 1, 1}, 64, "[1,1,1]"}
};
Do[assert[
   ToString[c[[1]], InputForm] <> "  (" <> c[[3]] <> ")  dim=" <> ToString[dim6[c[[1]]]],
   dim6[c[[1]]] === c[[2]], {dim6[c[[1]]], c[[2]]}], {c, dimCases}];

(* ------------------------------------------------------------------ *)
(* P0.2 Dynkin -> partition / box count                               *)
(* ------------------------------------------------------------------ *)

Print["=== Dynkin -> partition / boxes ==="];
assert["partition[{0,2,0}] == {2,2}",   partition[{0, 2, 0}] === {2, 2}];
assert["partition[{1,1,0}] == {2,1}",   partition[{1, 1, 0}] === {2, 1}];
assert["partition[{0,0,1}] == {1,1,1}", partition[{0, 0, 1}] === {1, 1, 1}];
assert["boxes[{0,2,0}] == 4",           boxes[{0, 2, 0}] === 4];
assert["boxes[{1,1,0}] == 3",           boxes[{1, 1, 0}] === 3];
(* box count equals number of tensor factors = tableau size *)
assert["boxes == Total[partition] for {1,1,1}", boxes[{1, 1, 1}] === Total[partition[{1, 1, 1}]]];

(* ------------------------------------------------------------------ *)
(* P0.3 spin-label validation and spinIndices[6]                      *)
(* ------------------------------------------------------------------ *)

Print["=== spin-label convention + spinIndices[6] ==="];
assert["validDynkinQ[{0,2,0}]",        TrueQ[validQ[{0, 2, 0}]]];
assert["! validDynkinQ[{0,2}]",        ! TrueQ[validQ[{0, 2}]]];
assert["! validDynkinQ[{1,-1,0}]",     ! TrueQ[validQ[{1, -1, 0}]]];
assert["! validDynkinQ[1]",            ! TrueQ[validQ[1]]];

(* a 3-pt correlator of stress tensor [0,2,0], vector [0,1,0], spinor [1,0,0]:
   index count per operator = a+2b+3c = 4, 2, 1 -> 7 Weyl-spinor indices total. *)
Module[{spins = {{0, 2, 0}, {0, 1, 0}, {1, 0, 0}}, inds},
   inds = spinInds[6, spins, {}, {1, 2, 3}];
   assert["spinIndices[6] total index count == sum of boxes",
      Length[inds] === Total[boxes /@ spins], {Length[inds], Total[boxes /@ spins]}];
   assert["spinIndices[6] are all lowered WeylSpinor[6]",
      MatchQ[inds, {Lowered[WeylSpinor[6]] ..}], inds]];

(* ------------------------------------------------------------------ *)
(* P1 ConformalCorrelatorCount[6] (little-group singlet counting)      *)
(* ------------------------------------------------------------------ *)

Print["=== ConformalCorrelatorCount[6] ==="];
With[{sc = {0, 0, 0}, v = {0, 1, 0}, T = {0, 2, 0}, sp = {1, 0, 0}, asp = {0, 0, 1}, vsp = {1, 1, 0}},
   cnt[reps_] := ConformalCorrelatorCount[6, reps];
   (* 2-point: nonzero iff same rep.  In 2 mod 4 the coordinate slash sigma_{AB}
      lives in 4(x)4, so the short string pairs a chiral rep with ITSELF, not with
      its SU(4) dual [c,b,a].  Real reps (a==c) are self-dual so unaffected. *)
   assert["2pt <T T> == 1 (real rep)",          cnt[{T, T}] === 1, cnt[{T, T}]];
   assert["2pt <T V> == 0",                     cnt[{T, v}] === 0, cnt[{T, v}]];
   assert["2pt <4 4bar> == 0 (dual pairing has no structure)", cnt[{sp, asp}] === 0, cnt[{sp, asp}]];
   assert["2pt <4 4> == 1 (same-chirality short string)",      cnt[{sp, sp}] === 1, cnt[{sp, sp}]];
   assert["2pt <4bar 4bar> == 1",               cnt[{asp, asp}] === 1, cnt[{asp, asp}]];
   (* 3-point: # of symmetric-traceless [0,k,0] in the product *)
   assert["3pt <sss> == 1",  cnt[{sc, sc, sc}] === 1, cnt[{sc, sc, sc}]];
   assert["3pt <ssT> == 1",  cnt[{sc, sc, T}] === 1, cnt[{sc, sc, T}]];
   assert["3pt <ssV> == 1",  cnt[{sc, sc, v}] === 1, cnt[{sc, sc, v}]];
   assert["3pt <sVV> == 2",  cnt[{sc, v, v}] === 2, cnt[{sc, v, v}]];
   assert["3pt <VVV> == 4  (V1V2V3 + 3 H_ij V_k)", cnt[{v, v, v}] === 4, cnt[{v, v, v}]];
   assert["3pt <TTT> == 11 (CPPR building-block count)", cnt[{T, T, T}] === 11, cnt[{T, T, T}]];
   assert["3pt <ss[1,1,0]> == 0 (vector-spinor, no SO(5) singlet)", cnt[{sc, sc, vsp}] === 0, cnt[{sc, sc, vsp}]];
   (* 4-point: Sum (b+1) over [a,b,c] with a==c *)
   assert["4pt <ssss> == 1", cnt[{sc, sc, sc, sc}] === 1, cnt[{sc, sc, sc, sc}]];
   assert["4pt <sssV> == 2 (6 -> 4+1+1 under SO(4))", cnt[{sc, sc, sc, v}] === 2, cnt[{sc, sc, sc, v}]];
   assert["4pt <sssT> == 3", cnt[{sc, sc, sc, T}] === 3, cnt[{sc, sc, sc, T}]];
];

(* ------------------------------------------------------------------ *)
(* P2 two-point functions (Young projection + ConformalTest)          *)
(* ------------------------------------------------------------------ *)

Print["=== P2 two-point functions ==="];
SetSignature["Euclidean"];
tol6 = 10^-8;

(* vector [0,1,0]: full special-conformal invariance check *)
Module[{cc, ct, comps, pt, res},
   cc = ConformalCorrelators[6, {4, 4}, {{0, 1, 0}, {0, 1, 0}}, {}, {1, 2}];
   assert["vector 2pt: nstructs == count == 1",
      Length[cc] == 1 && ConformalCorrelatorCount[6, {{0, 1, 0}, {0, 1, 0}}] == 1, Length[cc]];
   ct = ConformalTest[6, {4, 4}, {{0, 1, 0}, {0, 1, 0}}, {1, 2}];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   SeedRandom[11]; pt = Thread[Flatten@Array[x, {2, 6}] -> RandomInteger[{2, 30}, 12]];
   res = Max[Abs[N[Flatten[comps /. pt]]]];
   assert["vector 2pt: ConformalTest residual ~ 0  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* stress tensor [0,2,0]: build + nonzero (its ConformalTest at 8 indices is slow) *)
Module[{cc, pt, mag},
   cc = ConformalCorrelators[6, {6, 6}, {{0, 2, 0}, {0, 2, 0}}, {}, {1, 2}];
   assert["stress tensor 2pt: nstructs == count == 1",
      Length[cc] == 1 && ConformalCorrelatorCount[6, {{0, 2, 0}, {0, 2, 0}}] == 1, Length[cc]];
   SeedRandom[5]; pt = Thread[Flatten@Array[x, {2, 6}] -> RandomInteger[{2, 30}, 12]];
   mag = Max[Abs[N[Flatten[Normal[CanonicallyOrderedComponents[cc[[1]]]] /. pt]]]];
   assert["stress tensor 2pt: structure is nonzero", NumericQ[mag] && mag > 0, mag]];

(* chiral spinor [1,0,0]: the 2pt is the same-chirality short string <4 4> (NOT the
   SU(4) dual <4 4bar>).  In 2 mod 4 the coordinate slash sigma_{AB} lives in 4(x)4,
   so a chiral rep pairs with ITSELF.  Regression for the Correlators.m:100 count. *)
Module[{cc, ct, comps, pt, mag, res},
   cc = ConformalCorrelators[6, {3, 3}, {{1, 0, 0}, {1, 0, 0}}, {}, {1, 2}];
   assert["chiral spinor 2pt <4 4>: nstructs == count == 1",
      Length[cc] == 1 && ConformalCorrelatorCount[6, {{1, 0, 0}, {1, 0, 0}}] == 1, Length[cc]];
   assert["chiral spinor 2pt: dual pairing <4 4bar> has no structure (count 0)",
      ConformalCorrelatorCount[6, {{1, 0, 0}, {0, 0, 1}}] == 0 &&
      ConformalCorrelators[6, {3, 3}, {{1, 0, 0}, {0, 0, 1}}, {}, {1, 2}] === {},
      ConformalCorrelatorCount[6, {{1, 0, 0}, {0, 0, 1}}]];
   SeedRandom[5]; pt = Thread[Flatten@Array[x, {2, 6}] -> RandomInteger[{2, 30}, 12]];
   mag = Max[Abs[N[Flatten[Normal[CanonicallyOrderedComponents[cc[[1]]]] /. pt]]]];
   assert["chiral spinor 2pt: structure is nonzero", NumericQ[mag] && mag > 0, mag];
   ct = ConformalTest[6, {3, 3}, {{1, 0, 0}, {1, 0, 0}}, {1, 2}];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   res = Max[Abs[N[Flatten[comps /. pt]]]];
   assert["chiral spinor 2pt: ConformalTest residual ~ 0  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* ------------------------------------------------------------------ *)
(* P3 three-point functions (SU(4) alpha-system)                      *)
(* ------------------------------------------------------------------ *)

Print["=== P3 three-point functions ==="];

(* the alpha-system reduces the overcomplete set to exactly the structure count *)
Module[{s = {0, 0, 0}, v = {0, 1, 0}, T = {0, 2, 0}},
   assert["3pt <ssV>: #independent == count == 1",
      Length[ConformalCorrelatorExpressions[6, {s, s, v}]] == 1 == ConformalCorrelatorCount[6, {s, s, v}]];
   assert["3pt <sVV>: #independent == count == 2",
      Length[ConformalCorrelatorExpressions[6, {s, v, v}]] == 2 == ConformalCorrelatorCount[6, {s, v, v}]];
   assert["3pt <VVV>: #independent == count == 4",
      Length[ConformalCorrelatorExpressions[6, {v, v, v}]] == 4 == ConformalCorrelatorCount[6, {v, v, v}]];
   assert["3pt <ssT>: #independent == count == 1",
      Length[ConformalCorrelatorExpressions[6, {s, s, T}]] == 1 == ConformalCorrelatorCount[6, {s, s, T}]]];

(* a 3pt correlator built by the alpha-system is conformally invariant *)
Module[{ct, comps, pt, res},
   ct = ConformalTest[6, {3, 3, 4}, {{0, 0, 0}, {0, 0, 0}, {0, 1, 0}}, {1, 2, 3}];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   SeedRandom[11]; pt = Thread[Flatten@Array[x, {3, 6}] -> RandomInteger[{2, 30}, 18]];
   res = Max[Abs[N[Flatten[comps /. pt]]]];
   assert["3pt <ssV>: ConformalTest residual ~ 0  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* chiral (non-self-conjugate) 3pt: op2 = 4 (undotted), op3 = 4bar (dotted), so
   their connecting string uses an odd number of coordinate slashes (opposite
   chirality). *)
Module[{sp = {1, 0, 0}, asp = {0, 0, 1}, s = {0, 0, 0}, ct, comps, pt, res},
   assert["3pt <s psi psibar>: count == 1", ConformalCorrelatorCount[6, {s, sp, asp}] == 1];
   ct = ConformalTest[6, {3, 4, 4}, {s, sp, asp}, {1, 2, 3}];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   SeedRandom[11]; pt = Thread[Flatten@Array[x, {3, 6}] -> RandomInteger[{2, 30}, 18]];
   res = Max[Abs[N[Flatten[comps /. pt]]]];
   assert["3pt <s psi psibar> (chiral, 4-4bar): ConformalTest residual ~ 0  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* ------------------------------------------------------------------ *)
(* P4 four-point functions (alpha-system + single-frame reduction)     *)
(* ------------------------------------------------------------------ *)

Print["=== P4 four-point functions ==="];

Module[{s = {0, 0, 0}, v = {0, 1, 0}},
   (* all-scalar: a single trivial (index-free) structure -- guards the scalar
      edge case in the reduction *)
   assert["4pt <ssss>: #independent == count == 1",
      Length[ConformalCorrelatorExpressions[6, {s, s, s, s}]] == 1 == ConformalCorrelatorCount[6, {s, s, s, s}]];
   assert["4pt <sssV>: #independent == count == 2",
      Length[ConformalCorrelatorExpressions[6, {s, s, s, v}]] == 2 == ConformalCorrelatorCount[6, {s, s, s, v}]];
   assert["4pt <ssVV>: #independent == count == 5",
      Length[ConformalCorrelatorExpressions[6, {s, s, v, v}]] == 5 == ConformalCorrelatorCount[6, {s, s, v, v}]]];

(* a 4pt structure built by the alpha-system is conformally invariant *)
Module[{s = {0, 0, 0}, v = {0, 1, 0}, ct, comps, pt, res},
   ct = ConformalTest[6, {3, 3, 3, 4}, {s, s, s, v}, {1, 2, 3, 4}];
   comps = Normal[CanonicallyOrderedComponents[#]] & /@ ct;
   SeedRandom[11]; pt = Thread[Flatten@Array[x, {4, 6}] -> RandomInteger[{2, 30}, 24]];
   res = Max[Abs[N[Flatten[comps /. pt]]]];
   assert["4pt <sssV>: ConformalTest residual ~ 0  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* Independence of a 4pt basis must be judged at a SINGLE conformal frame: for
   n >= 4 points the cross-ratios vary between frames, so structures related by a
   cross-ratio function are dependent as tensor structures even though stacking
   several frames makes them look independent.  Guard that (a) the reduced <ssVV>
   basis has full rank at one generic frame and (b) the overcomplete set's rank is
   inflated when frames are stacked -- the artifact the single-frame reduction
   avoids. *)
Module[{s = {0, 0, 0}, v = {0, 1, 0}, bc, od, op, reps, parts, red, full, cfg, cfgs, m, single, stacked},
   bc = ConformalStructures`Private`buildCorrelator6;
   od = ConformalStructures`Private`opDottedQ6;
   op = ConformalStructures`Private`opPartition6;
   reps = {s, s, v, v};
   parts = MapThread[op, {reps, od /@ reps}];
   red = ConformalCorrelatorExpressions[6, reps];
   full = ConformalCorrelatorExpressions[6, reps, "Overcomplete" -> True];
   SeedRandom[99]; cfg = RandomInteger[{2, 40}, 24];
   m = Table[Flatten[{N[Normal[bc[Sequence @@ ex, parts]] /. Thread[Flatten@Array[x, {4, 6}] -> cfg], 30]}], {ex, red}];
   single = MatrixRank[m];
   assert["4pt <ssVV>: reduced basis has full rank (5) at a single frame",
      single == Length[red] == 5, single];
   SeedRandom[7]; cfgs = Table[RandomInteger[{2, 40}, 24], {3}];
   m = Table[Flatten@Table[N[Normal[bc[Sequence @@ ex, parts]] /. Thread[Flatten@Array[x, {4, 6}] -> c], 30], {c, cfgs}], {ex, full}];
   stacked = MatrixRank[m];
   assert["4pt <ssVV>: stacking frames inflates overcomplete rank above the count (motivates single-frame reduction)",
      stacked > 5, {stacked, Length[full]}]];

(* StructureRelations for a 6d 4pt overcomplete set: two point orderings of <sssV>
   span a 2-dimensional space, so there are 2 relations whose coefficients are
   cross-ratio functions.  (2-index structures take the exact genericPoint path;
   >=4-index structures take the d=6 fastEval fast path -- see the agreement test
   below.) *)
Module[{sc = {0, 0, 0}, v = {0, 1, 0}, gp, crs, scr, oA, oB, structs, rank, rel, res},
   gp = ConformalStructures`Private`genericPoint;
   crs = ConformalStructures`Private`crossRatios;
   scr = ConformalStructures`Private`safeCrossRatios;
   oA = ConformalCorrelators[6, {3, 3, 3, 4}, {sc, sc, sc, v}, {}, {1, 2, 3, 4}];
   oB = ConformalCorrelators[6, {3, 3, 3, 4}, {sc, sc, sc, v}, {}, {2, 1, 3, 4}];
   structs = Join[oA, oB];
   rank = MatrixRank@N@Table[Flatten[Normal[CanonicallyOrderedComponents[s]] /. gp[6, None, 7/3, 1]], {s, structs}];
   rel = Block[{Print = (Null &), Run = (0 &)}, StructureRelations[structs]];
   assert["4pt <sssV> relations: #relations == #structs - rank == 2",
      Length[Normal[rel]] == Length[structs] - rank == 2, {Length[Normal[rel]], Length[structs], rank}];
   assert["4pt <sssV> relations: coefficients involve cross-ratios u,v",
      ! FreeQ[Normal[rel], Alternatives @@ crs[6, None]], Normal[rel]];
   res = Module[{comps, relN},
      comps = Table[Flatten[Normal[CanonicallyOrderedComponents[s]] /. gp[6, None, 7/3, 1]], {s, structs}];
      relN = Normal[rel] /. Thread[crs[6, None] -> scr[None][[1]]];
      Max[Abs[N[Flatten[{relN . comps}]]]]];
   assert["4pt <sssV> relations: vanish numerically  [" <> ToString[res, InputForm] <> "]",
      NumericQ[res] && res < tol6, res]];

(* The d=6 fastEval fast path (fastEval.m) evaluates each string-structure piece at
   the concrete frame and Young-projects the numeric tensor product; it must
   reproduce the exact CanonicallyOrderedComponents / Components at every sampled
   frame.  This is what accelerates fittedRelations for >=4-index structures
   (<ssVV> relations: ~13 s vs ~400 s via the symbolic path).

   CAUTION: do NOT introduce top-level symbols named u or v in this suite -- they
   are the cross-ratio symbols in crossRatios[6]/uvpt, and shadowing them at global
   scope corrupts genericPoint (its uvpt coordinate silently becomes a vector).
   Module-local names are safe, but this test uses vv to be unambiguous. *)
Module[{sc = {0, 0, 0}, vv = {0, 1, 0}, fe, feCOC, gp, scr, corr, dCOC, dNat},
   fe = ConformalStructures`Private`fastEval;
   feCOC = ConformalStructures`Private`fastEvalCOC;
   gp = ConformalStructures`Private`genericPoint;
   scr = ConformalStructures`Private`safeCrossRatios;
   corr = ConformalCorrelators[6, {4, 4, 4, 4}, {sc, sc, vv, vv}, {}, {1, 2, 3, 4}];
   dCOC = Max@Table[Max[Abs[N[Flatten[Normal[feCOC[corr[[k]], z, scr[None][[ii]]]]] -
        Flatten[Normal[CanonicallyOrderedComponents[corr[[k]]]] /. gp[6, None, z, ii]]]]],
      {k, Length[corr]}, {z, {2, 3}}, {ii, {1, 7}}];
   dNat = Max@Table[Max[Abs[N[Flatten[Normal[fe[corr[[k]], z, scr[None][[ii]]]]] -
        Flatten[Normal[Components[corr[[k]]]] /. gp[6, None, z, ii]]]]],
      {k, Length[corr]}, {z, {2, 3}}, {ii, {1, 7}}];
   assert["6d fastEvalCOC == CanonicallyOrderedComponents (<ssVV>)  [max|diff|=" <> ToString[dCOC, InputForm] <> "]",
      NumericQ[dCOC] && dCOC < tol6, dCOC];
   assert["6d fastEval == Components (<ssVV>)  [max|diff|=" <> ToString[dNat, InputForm] <> "]",
      NumericQ[dNat] && dNat < tol6, dNat]];

(* Derivative correlators (slow path): the D=6 descendant machinery -- spinIndices
   plus the derivative BuildTensor index bookkeeping.  A spinor derivative adds two
   undotted Weyl indices (D=6 is 2 mod 4).  We validate against the direct
   TensorSpinorDerivative of the base structure: (a) the index list, (b) for a
   derivative on op2 -- where op1's indices precede the derivative's, exercising the
   interleaving reorder -- the built components equal TSD[base] transposed per the
   index convention, and (c) for a derivative on a dotted operator (chirality split)
   the value multiset is preserved. *)
Module[{vv = {0, 1, 0}, sc = {0, 0, 0}, sp = {1, 0, 0}, asp = {0, 0, 1},
        gp, tsd, spinInds, base, deriv, built, ref, dbase, dderiv, dbuilt, dref, dInt, ms},
   gp = ConformalStructures`Private`genericPoint;
   tsd = Symbol[First[Names["*`TensorSpinorDerivative"]]];
   spinInds = ConformalStructures`Private`spinIndices;
   assert["6d deriv: spinIndices adds two Weyl indices per spinor derivative",
      spinInds[6, {sc, sc, sc, sc}, {{"\[PartialD]", 1}}, {1, 2, 3, 4}] ===
        {Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]}];
   (* <VV>, derivative on op2: spinIndices places the derivative's indices after
      op1's, while TSD prepends them -> reorder {3,4,1,2,5,6} *)
   base = ConformalCorrelators[6, {3, 3}, {vv, vv}, {}, {1, 2}][[1]];
   deriv = ConformalCorrelators[6, {3, 3}, {vv, vv}, {{"\[PartialD]", 2}}, {1, 2}][[1]];
   built = Normal[Components[deriv]] /. gp[6, None, 2, 1];
   ref = Normal[Components[tsd[base, 6, 2]]] /. gp[6, None, 2, 1];
   dInt = Max[Abs[N[Flatten[built - TensorTranspose[ref, {3, 4, 1, 2, 5, 6}]]]]];
   assert["6d deriv <VV>, d on op2: built == TSD[base] reordered per convention  [" <> ToString[dInt, InputForm] <> "]",
      NumericQ[dInt] && dInt < tol6, dInt];
   (* <s psi psibar>, derivative on the dotted psibar (op3): chirality split *)
   dbase = ConformalCorrelators[6, {2, 2, 2}, {sc, sp, asp}, {}, {1, 2, 3}][[1]];
   dderiv = ConformalCorrelators[6, {2, 2, 2}, {sc, sp, asp}, {{"\[PartialD]", 3}}, {1, 2, 3}][[1]];
   assert["6d deriv on dotted op: index list has three Weyl and one dotted-Weyl",
      Sort[spinInds[6, {sc, sp, asp}, {{"\[PartialD]", 3}}, {1, 2, 3}]] ===
        Sort[{Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]], Lowered[DottedWeylSpinor[6]]}]];
   dbuilt = Normal[Components[dderiv]] /. gp[6, None, 2, 1];
   dref = Normal[Components[tsd[dbase, 6, 3]]] /. gp[6, None, 2, 1];
   ms = Max[Abs[Sort[N@Chop[Flatten[dbuilt], 10^-8]] - Sort[N@Chop[Flatten[dref], 10^-8]]]];
   assert["6d deriv on dotted op (chirality split): value multiset matches TSD[base]  [" <> ToString[ms, InputForm] <> "]",
      NumericQ[ms] && ms < tol6, ms]];

(* fastEval for d=6 derivative correlators: u/v derivatives take the accelerated
   piece-wise path; spacetime (\[PartialD]) derivatives fall back to a direct
   Components evaluation inside fastEval.  Both must reproduce
   CanonicallyOrderedComponents.  Uses <sssV> (a spinning operator) so the derivative
   structure has >= 4 indices -- the regime fittedRelations routes to fastEval. *)
Module[{sc = {0, 0, 0}, vv = {0, 1, 0}, feC, gp, scr, du, dd, diffU, diffD},
   feC = ConformalStructures`Private`fastEvalCOC;
   gp = ConformalStructures`Private`genericPoint;
   scr = ConformalStructures`Private`safeCrossRatios;
   du = ConformalCorrelators[6, {2, 2, 2, 3}, {sc, sc, sc, vv}, {{"u", 4}}, {1, 2, 3, 4}][[1]];
   dd = ConformalCorrelators[6, {2, 2, 2, 3}, {sc, sc, sc, vv}, {{"\[PartialD]", 4}}, {1, 2, 3, 4}][[1]];
   diffU = Max@Table[Max[Abs[N[Flatten[Normal[feC[du, z, scr[None][[ii]]]]] -
        Flatten[Normal[CanonicallyOrderedComponents[du]] /. gp[6, None, z, ii]]]]], {z, {2, 3}}, {ii, {1, 7}}];
   diffD = Max@Table[Max[Abs[N[Flatten[Normal[feC[dd, z, scr[None][[ii]]]]] -
        Flatten[Normal[CanonicallyOrderedComponents[dd]] /. gp[6, None, z, ii]]]]], {z, {2, 3}}, {ii, {1, 7}}];
   assert["6d u-derivative fastEvalCOC == CanonicallyOrderedComponents (<sssV>)  [" <> ToString[diffU, InputForm] <> "]",
      NumericQ[diffU] && diffU < tol6, diffU];
   assert["6d \[PartialD]-derivative fastEvalCOC == CanonicallyOrderedComponents (<sssV>)  [" <> ToString[diffD, InputForm] <> "]",
      NumericQ[diffD] && diffD < tol6, diffD]];

(* Fast 6d \[PartialD] path across the cases that stress its index bookkeeping:
   a MULTI-FACTOR correlator (<ssVV>, where an operator's indices are split across
   string factors -- regression for the "move derivative indices to front"
   permutation, which was silently correct only for a single factor) and a
   DOTTED operator (<s 4 4bar>, exercising the dsiPerm branch). *)
Module[{sc = {0, 0, 0}, vv = {0, 1, 0}, sp = {1, 0, 0}, asp = {0, 0, 1}, feC, gp, scr, cases, diffs},
   feC = ConformalStructures`Private`fastEvalCOC;
   gp = ConformalStructures`Private`genericPoint;
   scr = ConformalStructures`Private`safeCrossRatios;
   cases = {
      {"<ssVV> \[PartialD] on op3 (multi-factor)", {2, 2, 3, 3}, {sc, sc, vv, vv}, 3, {1, 2, 3, 4}},
      {"<s 4 4bar> \[PartialD] on op3 (dotted op)", {3, 4, 4}, {sc, sp, asp}, 3, {1, 2, 3}}};
   Do[Module[{corr, diff},
      corr = ConformalCorrelators[6, c[[2]], c[[3]], {{"\[PartialD]", c[[4]]}}, c[[5]]][[1]];
      diff = Max@Table[Max[Abs[N[Flatten[Normal[feC[corr, z, scr[None][[ii]]]]] -
           Flatten[Normal[CanonicallyOrderedComponents[corr]] /. gp[6, None, z, ii]]]]], {z, {2, 3}}, {ii, {1, 7}}];
      assert["6d \[PartialD]-derivative fastEvalCOC == COC  " <> c[[1]] <> "  [" <> ToString[diff, InputForm] <> "]",
         NumericQ[diff] && diff < tol6, diff]], {c, cases}]];

(* summary / Exit only when run standalone; when included the parent suite tallies *)
If[! TrueQ[$sixDIncluded],
   Print["================================================================"];
   Print[ToString[$passed], " passed, ", ToString[$failed], " failed."];
   If[$failed > 0, Exit[1], Exit[0]];
];
