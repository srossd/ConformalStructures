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

(* d = 6 spacetime (\[PartialD]) derivative.  The piece-wise fast path below assumes
   the d<=4 symmetrization and the rational uvz evaluation, neither of which holds in
   6D (the reps are Young-projected and uvz goes ragged), so this more specific rule
   evaluates the derivative structure directly via its Components -- correct, though
   not accelerated.  A fast 6D \[PartialD] path is future work; the u and v
   derivatives above already take the fast path. *)
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