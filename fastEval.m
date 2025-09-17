uvz[struct : {stringstruct[dim_, _, q_], __}, perm_, deriv_] := uvz[struct, perm, deriv] = 
  Simplify[Normal@Components[
      If[deriv === None, 
         Tensor[{struct}], 
       	 TensorSpinorDerivative[Tensor[{struct}], dim, InversePermutation[perm][[deriv]]]
      ]
  ] /. evalRule[dim, perm, q, zValue, crossRatios[q]],
  zValue > 1];

evalRule[dim_, pointperm_, q_, z_, ratios_] := genericPoint[dim, q, z] /. x[i_, j_] :> If[i > Length[pointperm], None, x[InversePermutation[pointperm][[i]], j]] /. Thread[crossRatios[q] -> ratios];

fastEval[expr_?NumericQ, ___] := expr;
fastEval[expr_, perm_, pointperm_, groupLengths_, z_, ratios_] := Module[{qdefect, pieces, piecesEval, unsym, syms},
  qdefect = expr[[1, 1, 1, 3]];
  pieces = uvz[#, pointperm, None] & /@ expr[[1]];
  piecesEval = pieces /. Append[Thread[crossRatios[qdefect] -> ratios], zValue -> z];
  unsym = TensorTranspose[TensorProduct @@ piecesEval, InversePermutation@perm];
  syms = Select[Permutations[Range@Total[groupLengths]], Sort /@ TakeList[(Range@Total[groupLengths])[[#]], groupLengths] == TakeList[Range@Total[groupLengths], groupLengths] &];
  If[syms == {}, unsym, 1/Length[syms] Sum[TensorTranspose[unsym, p], {p, syms}]]
 ];
 
 

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {}, perm_, q_, i_], ___}}], z_, ratios_] := (Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, "DefectCodimension" -> q] /. evalRule[dim, perm, q, z, ratios]) fastEval[
     Sequence @@ ConformalCorrelatorExpressions[dim, spins, "DefectCodimension" -> q][[i]], 
     perm, 
     2 Flatten[spins], 
     z, 
     ratios
];

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {{s : ("u" | "v"), didx_}}, perm_, q_, i_], inds___}}],z_, ratios_] := Module[{deriv, rest, nbefore},
   rest = fastEval[Tensor[{{correlator[dim, \[CapitalDelta]s, spins, {}, perm, q, i], inds}}], z, ratios];
   deriv = Normal[Components[TensorSpinorDerivative[ToExpression[s][dim, perm], dim, perm[[didx]]]]] /. evalRule[dim, Range[Length[\[CapitalDelta]s]], q, z, ratios];
   nbefore = 2 Total[Flatten[spins[[;; didx - 1]]]];
   TensorTranspose[
      TensorProduct[deriv, rest], 
      InversePermutation@Join[2 + Range[nbefore], {1, 2}, 2 + Range[nbefore + 1, Length[{inds}]]]
   ]
];

fastEval[Tensor[{{correlator[dim_, \[CapitalDelta]s_, spins_, {{"\[PartialD]", didx_}}, perm_, q_, i_], inds___}}], z_, ratios_] := 
  Module[{rule, prefactor, prefactorEval, pdidx, expr, factors, pieces, piecesEval, derivpieces, derivpiecesEval, fp1, fp2, nbefore, nafter, unsym, groupLengths, syms},
   rule = evalRule[dim, perm, q, z, ratios];
   prefactor = Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, "DefectCodimension" -> q];
   prefactorEval = prefactor /. rule;
   pdidx = perm[[didx]];
   expr = ConformalCorrelatorExpressions[dim, spins, "DefectCodimension" -> q][[i]];
   factors = expr[[1]] /. {t_Tensor :> t[[1]], 1 -> {}};
   pieces = uvz[#, perm, None] & /@ factors;
   piecesEval = pieces /. Append[Thread[crossRatios[q] -> ratios], zValue -> z];
   derivpieces = uvz[#, perm, pdidx] & /@ factors;
   derivpiecesEval = derivpieces /. Append[Thread[crossRatios[q] -> ratios], zValue -> z];
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