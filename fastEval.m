uvz[struct : {stringstruct[dim_, _, q_], __}, perm_, deriv_] := uvz[struct, perm, deriv] = 
  Simplify[Normal@Components[
      If[deriv === None, 
         Tensor[{struct}], 
       	 TensorSpinorDerivative[Tensor[{struct}], dim, InversePermutation[perm][[deriv]]]
      ]
  ] /. evalRule[dim, perm, q, zValue, crossRatios[dim, q]],
  zValue > 1];

evalRule[dim_, pointperm_, q_, z_, ratios_] := genericPoint[dim, q, z] /. x[i_, j_] :> If[i > Length[pointperm], None, x[InversePermutation[pointperm][[i]], j]] /. Thread[crossRatios[dim, q] -> ratios];

perms[groupLengths_] := 
 Join @@@ 
  Tuples[Table[
    Permutations[Range@groupLengths[[i]]] + 
     Total[groupLengths[[;; i - 1]]], {i, Length[groupLengths]}]]

fastEval[expr_?NumericQ, ___] := expr;
fastEval[expr_, perm_, pointperm_, groupLengths_, z_, ratios_] := Module[{dim, qdefect, pieces, piecesEval, unsym, syms},
  dim = expr[[1, 1, 1, 1]];
  qdefect = expr[[1, 1, 1, 3]];
  pieces = uvz[#, pointperm, None] & /@ expr[[1]];
  piecesEval = pieces /. Append[Thread[crossRatios[dim, qdefect] -> ratios], zValue -> z];
  unsym = TensorTranspose[TensorProduct @@ piecesEval, InversePermutation@perm];
  syms = perms[groupLengths];
  If[syms == {}, unsym, 1/Length[syms] Sum[TensorTranspose[unsym, p], {p, syms}]]
 ];
 
 

(* d = 6: the operators' reps are Young-projected (SU(4)), which the generic
   symmetrization below does not reproduce, so this more specific rule takes over.
   Evaluate each string-structure piece at the CONCRETE frame and Young-project the
   numeric tensor product -- the numeric counterpart of buildCorrelator6.  Each
   piece is a small 4x4 tensor, cached and reused across structures that share the
   string.  (Do not push symbolic cross-ratios through uvz as the generic path
   does: for 6d string structures the concrete substitution keeps the pieces
   rectangular.) *)
fastEvalPiece6[spec_, perm_, q_, z_, ratios_] := fastEvalPiece6[spec, perm, q, z, ratios] =
   Normal[Components[Tensor[{spec}]]] /. evalRule[6, perm, q, z, ratios];

(* the spinor derivative of a single string piece, evaluated at the concrete frame:
   TensorSpinorDerivative prepends the two \[Sigma]-derivative Weyl indices before the
   piece's own indices.  Cached like fastEvalPiece6. *)
fastEvalDerivPiece6[spec_, perm_, didx_, q_, z_, ratios_] := fastEvalDerivPiece6[spec, perm, didx, q, z, ratios] =
   Normal[Components[TensorSpinorDerivative[Tensor[{spec}], 6, didx]]] /. evalRule[6, perm, q, z, ratios];

fastEval[Tensor[{{correlator[6, \[CapitalDelta]s_, spins_, {}, perm_, q_, i_], ___}}], z_, ratios_] :=
   With[{ex = ConformalCorrelatorExpressions[6, spins, "DefectCodimension" -> q][[i]]},
     (Explicit@KinematicPrefactor[6, \[CapitalDelta]s, spins, "DefectCodimension" -> q] /. evalRule[6, perm, q, z, ratios]) youngProject6[
        TensorTranspose[TensorProduct @@ (fastEvalPiece6[#, perm, q, z, ratios] & /@ ex[[1, 1]]), InversePermutation@ex[[2]]],
        MapThread[opPartition6, {spins, opDottedQ6 /@ spins}]]];

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {}, perm_, q_, i_], ___}}], z_, ratios_] := (Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, "DefectCodimension" -> q] /. evalRule[dim, perm, q, z, ratios]) fastEval[
     Sequence @@ ConformalCorrelatorExpressions[dim, spins, "DefectCodimension" -> q][[i]],
     perm,
     2 Flatten[spins],
     z,
     ratios
];

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {{s : ("u" | "v"), didx_}}, perm_, q_, i_], inds___}}],z_, ratios_] := Module[{deriv, rest, nbefore},
   rest = fastEval[Tensor[{{correlator[dim, \[CapitalDelta]s, spins, {}, perm, q, i], inds}}], z, ratios];
   deriv = Normal[Components[TensorSpinorDerivative[ToExpression[s][dim, perm, "DefectCodimension" -> q], dim, perm[[didx]]]]] /. evalRule[dim, Range[Length[\[CapitalDelta]s]], q, z, ratios];
   (* indices of the operators before didx; the spinor derivative contributes two
      indices either way (sigma is {Weyl, dotted} in 0 mod 4, {Weyl, Weyl} in 2 mod 4) *)
   nbefore = If[dim == 6, Total[opBoxes6[#, opDottedQ6[#]] & /@ spins[[;; didx - 1]]], 2 Total[Flatten[spins[[;; didx - 1]]]]];
   TensorTranspose[
      TensorProduct[deriv, rest],
      InversePermutation@Join[2 + Range[nbefore], {1, 2}, 2 + Range[nbefore + 1, Length[{inds}]]]
   ]
];

(* d = 6 spacetime (\[PartialD]) derivative, fast path.  The generic piece-wise path
   below assumes the d<=4 symmetrization and rational uvz evaluation, neither of which
   holds in 6D (reps are Young-projected, uvz goes ragged), so this rule assembles the
   descendant numerically from concrete-frame pieces.  TensorSpinorDerivative is a
   derivation, so d(prefactor . Young[(x)pieces]) splits by the product rule into
   d(prefactor) (x) base  +  prefactor . Young[d((x)pieces)], with the two \[Sigma]-derivative
   indices prepended.  Young projection acts only on the operator blocks (the derivative
   indices ride along as trivial {1} blocks).  The result is assembled in
   TensorSpinorDerivative's native index order, then reordered into spinIndices order by
   the SAME fullPerm the slow BuildTensor uses (Correlators.m), so the two agree exactly.
   Defect derivatives (q =!= None) fall through to the Components rule below. *)
fastEval[Tensor[{{correlator[6, \[CapitalDelta]s_, spins_, {{"\[PartialD]", didx_}}, perm_, q_, i_], inds___}}], z_, ratios_] /; q === None :=
  Module[{ex, factors, invperm, partitions, nfac, pieces, ranks, ntot, rule,
     derivpieces, prefactorEval, dprefactor, bdBase, augPart, offsets, term2core,
     native, bd, baseexpr, indsPerX, siPerm, dsiPerm, siPos, dsiPos, fullPerm, ph},
   ex = ConformalCorrelatorExpressions[6, spins, "DefectCodimension" -> q][[i]];
   factors = ex[[1, 1]]; invperm = InversePermutation@ex[[2]];
   partitions = MapThread[opPartition6, {spins, opDottedQ6 /@ spins}];
   nfac = Length[factors];
   pieces = fastEvalPiece6[#, perm, q, z, ratios] & /@ factors;
   ranks = ArrayDepth /@ pieces; ntot = Total[ranks];
   rule = evalRule[6, perm, q, z, ratios];
   derivpieces = fastEvalDerivPiece6[#, perm, didx, q, z, ratios] & /@ factors;
   prefactorEval = Explicit@KinematicPrefactor[6, \[CapitalDelta]s, spins, "DefectCodimension" -> q] /. rule;
   dprefactor = Normal[Components[TensorSpinorDerivative[Explicit@KinematicPrefactor[6, \[CapitalDelta]s, spins, "DefectCodimension" -> q], 6, didx]]] /. rule;
   bdBase = youngProject6[TensorTranspose[TensorProduct @@ pieces, invperm], partitions];
   (* product rule: sum over which factor is differentiated; route the two derivative
      indices (prepended in each derivpiece) to the END, invperm the originals, Young-
      project the operator blocks (derivative indices as trivial {1} tails), then move
      the derivative indices to the front -> [d1, d2, base indices] = native order *)
   augPart = Join[partitions, {{1}, {1}}];
   offsets = Prepend[Accumulate[Most[ranks]], 0];
   term2core = Sum[
      TensorTranspose[
         TensorProduct @@ Table[If[k == l, derivpieces[[k]], pieces[[k]]], {k, nfac}],
         InversePermutation@Join[Complement[Range[ntot + 2], offsets[[l]] + {1, 2}], offsets[[l]] + {1, 2}]
      ], {l, nfac}];
   term2core = TensorTranspose[term2core, Join[invperm, ntot + {1, 2}]];
   term2core = youngProject6[term2core, augPart];
   term2core = TensorTranspose[term2core, Join[2 + Range[ntot], {1, 2}]];
   native = TensorProduct[dprefactor, bdBase] + prefactorEval term2core;
   (* reorder into spinIndices order using the slow path's fullPerm *)
   bd = ConformalCorrelators[6, \[CapitalDelta]s, spins, {}, perm][[i]];
   baseexpr = TensorSpinorDerivative[bd, 6, perm[[didx]]];
   indsPerX = Table[With[{dt = opDottedQ6[spins[[j]]]},
      {If[dt, 0, opBoxes6[spins[[j]], dt]], If[dt, opBoxes6[spins[[j]], dt], 0]}] + Count[{didx}, j] {2, 0}, {j, Length[\[CapitalDelta]s]}];
   siPerm = Flatten@{
      {Total[indsPerX[[;; didx - 1, 1]]] + {1, 2}},
      Table[With[{own = indsPerX[[k, 1]] - 2 Count[{didx}, k]},
         Total[indsPerX[[;; k, 1]]] - own + Range[own]], {k, Length[\[CapitalDelta]s]}]};
   dsiPerm = Flatten@{{}, Table[With[{own = indsPerX[[k, 2]]},
         Total[indsPerX[[;; k, 2]]] - own + Range[own]], {k, Length[\[CapitalDelta]s]}]};
   siPos = Position[Indices[baseexpr], Lowered[WeylSpinor[6]]][[;; , 1]];
   dsiPos = Position[Indices[baseexpr], Lowered[DottedWeylSpinor[6]]][[;; , 1]];
   fullPerm = withCounts[Indices[baseexpr]] /. {
      {Lowered[WeylSpinor[6]], j_} :> siPos[[siPerm[[j]]]],
      {Lowered[DottedWeylSpinor[6]], j_} :> dsiPos[[dsiPerm[[j]]]]};
   Block[{ph},
      BuildTensor[{ph, Sequence @@ Indices[baseexpr]}] = native;
      TensorTranspose[CanonicallyOrderedComponents@TensorPermute[Tensor[{Prepend[Indices[baseexpr], ph]}], fullPerm], Ordering@{inds}]
   ]
  ];

(* d = 6 \[PartialD] derivative on a defect (q =!= None): correct but not accelerated. *)
fastEval[t : Tensor[{{correlator[6, _, _, {{"\[PartialD]", _}}, perm_, q_, _], ___}}], z_, ratios_] :=
   Normal[Components[t]] /. evalRule[6, perm, q, z, ratios];

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {{"\[PartialD]", didx_}}, perm_, q_, i_], inds___}}], z_, ratios_] :=
  Module[{rule, prefactor, prefactorEval, pdidx, expr, factors, pieces, piecesEval, derivpieces, derivpiecesEval, fp1, fp2, nbefore, nafter, unsym, groupLengths, syms},
   rule = evalRule[dim, perm, q, z, ratios];
   prefactor = Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, "DefectCodimension" -> q];
   prefactorEval = prefactor /. rule;
   pdidx = perm[[didx]];
   expr = ConformalCorrelatorExpressions[dim, spins, "DefectCodimension" -> q][[i]];
   factors = expr[[1]] /. {t_Tensor :> t[[1]], 1 -> {}};
   pieces = uvz[#, perm, None] & /@ factors;
   piecesEval = pieces /. Append[Thread[crossRatios[dim, q] -> ratios], zValue -> z];
   derivpieces = uvz[#, perm, pdidx] & /@ factors;
   derivpiecesEval = derivpieces /. Append[Thread[crossRatios[dim, q] -> ratios], zValue -> z];
   nbefore = 2 Total[Flatten[spins[[;; didx - 1]]]];
   nafter = 2 Total[Flatten[spins[[didx ;;]]]];
   fp1 = Join[nbefore + {1, 2}, Range[nbefore], 2 + Range[nbefore + 1, nbefore + nafter]];
   fp2 = Join[{1, 2}, 2 + InversePermutation@PermutationList[expr[[2]], 2 Length[factors]]];
   unsym = TensorTranspose[TensorTranspose[
      prefactorEval Sum[
         TensorTranspose[TensorProduct @@ Table[If[k == l, derivpiecesEval[[k]], piecesEval[[k]]], {k, Length[factors]}],
          Join[2 + Range[2 (l - 1)], {1, 2}, 2 + Range[2 l - 1, 2 Length[factors]]]
         ],
         {l, Length[factors]}
      ] + TensorProduct @@ Prepend[piecesEval, Normal@Components@TensorSpinorDerivative[prefactor, dim, didx] /. rule], 
      fp2], fp1];
   groupLengths = 2 Flatten[spins];
   syms = Select[Permutations[Range@Total[groupLengths]], Sort /@ TakeList[(Range@Total[groupLengths])[[#]], groupLengths] == TakeList[Range@Total[groupLengths], groupLengths] &];
   SparseArray@If[syms == {}, unsym, 
     1/Length[syms] Sum[TensorTranspose[unsym, p], {p, TensorTools`Private`riffleIn[#, {1, 2} + nbefore] & /@ syms}]
   ]
];

fastEval[expr : Tensor[{a___, t : {correlator[dim_, _, _, _, perm_, q_, _], inds___}, b___}], z_, ratios_] := Block[{tmpname},
  BuildTensor[{tmpname, inds}] := fastEval[Tensor[{t}], z, ratios];
  Normal@Components[expr /. t -> {tmpname, inds}] /. evalRule[dim, perm, q, z, ratios]
]

fastEval[expr : Contract[Tensor[{a___, t : {correlator[dim_, _, _, _, perm_, q_, _], inds___}, b___}], _], z_, ratios_] := Block[{tmpname},
  BuildTensor[{tmpname, inds}] := fastEval[Tensor[{t}], z, ratios];
  Normal@Components[expr /. t -> {tmpname, inds}] /. evalRule[dim, perm, q, z, ratios]
]

fastEval[expr : TensorPermute[Tensor[{a___, t : {correlator[dim_, _, _, _, perm_, q_, _], inds___}, b___}], _], z_, ratios_] := Block[{tmpname},
  BuildTensor[{tmpname, inds}] := fastEval[Tensor[{t}], z, ratios];
  Normal@Components[expr /. t -> {tmpname, inds}] /. evalRule[dim, perm, q, z, ratios]
]

fastEval[expr : TensorPermute[Contract[Tensor[{a___, t : {correlator[dim_, _, _, _, perm_, q_, _], inds___}, b___}], _], _], z_, ratios_] := Block[{tmpname},
  BuildTensor[{tmpname, inds}] := fastEval[Tensor[{t}], z, ratios];
  Normal@Components[expr /. t -> {tmpname, inds}] /. evalRule[dim, perm, q, z, ratios]
]

fastEvalCOC[expr_, z_, ratios_] := TensorTranspose[fastEval[expr, z, ratios], InversePermutation[Ordering@Indices[expr]]]; 