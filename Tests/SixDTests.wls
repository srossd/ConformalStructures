#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Validation for the in-progress d = 6 (SO(6) = SU(4)) support.

   Run from the repository root with:

       wolframscript -file Tests/SixDTests.wls

   This is separate from Tests/RegressionTests.wls (the stable CI suite) while
   d = 6 is under construction.  Milestone P0 covers the representation
   primitives: the SU(4) Dynkin -> Young-projector pipeline and spinIndices[6],
   validated by reproducing known irrep dimensions. *)

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

dim6      = ConformalStructures`Private`su4IrrepDimension;
partition = ConformalStructures`Private`dynkinToPartition;
boxes     = ConformalStructures`Private`dynkinBoxes;
validQ    = ConformalStructures`Private`validDynkinQ;
spinInds  = ConformalStructures`Private`spinIndices;

$passed = 0; $failed = 0;
assert[label_, cond_, info_: ""] := If[TrueQ[cond],
   $passed++; Print["  PASS  ", label],
   $failed++; Print["  FAIL  ", label, If[info === "", "", "  (" <> ToString[info, InputForm] <> ")"]]];

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
   (* 2-point: nonzero iff conjugate reps *)
   assert["2pt <T Tbar> == 1 (self-conjugate)", cnt[{T, T}] === 1, cnt[{T, T}]];
   assert["2pt <T V> == 0",                     cnt[{T, v}] === 0, cnt[{T, v}]];
   assert["2pt <4 4bar> == 1",                  cnt[{sp, asp}] === 1, cnt[{sp, asp}]];
   assert["2pt <4 4> == 0",                     cnt[{sp, sp}] === 0, cnt[{sp, sp}]];
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
Print["================================================================"];
Print[ToString[$passed], " passed, ", ToString[$failed], " failed."];
If[$failed > 0, Exit[1], Exit[0]];
