(* Wolfram Language package *)

AddTensorHead[TensorDerivative];
TensorDerivative[0, _, _] := 0;

Symbolic[
   TensorDerivative[
    a_. t : (_Tensor | _Contract | _TensorPermute | 
        TensorProduct[x_, y__] | _Correlator), dim_, i_]] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[Spacetime[dim]]}, {"("}, If[a =!= 1, {a}, Nothing]}, 
   Symbolic[t], {{")"}}];
Symbolic[TensorDerivative[a_, dim_, i_]] /; FreeQ[a, Tensor] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[Spacetime[dim]]}, {"("}, If[a =!= 1, {a}, Nothing]}, {{")"}}];
Indices[TensorDerivative[t_, dim_, i_]] := 
  Prepend[Indices[t], Lowered[Spacetime[dim]]];
TensorPermutation[TensorDerivative[t_, dim_, i_]] := 
  Join[{1}, 1 + TensorPermutation[t]];
TensorTools`Private`DisplayTemplate[td_TensorDerivative] := 
  TensorTools`Private`DisplayTemplate[Symbolic[td]];


InactiveComponents[TensorDerivative[t_, dim_, i_]] := 
  With[{comps = Components[t]}, 
   If[ArrayQ[comps], 
    SparseArray[
     Flatten@Table[
       ArrayRules@comps /. 
        HoldPattern[a_ -> b_] :> Prepend[a, k] -> D[b, x[i, k]], {k, 
        dim}], Prepend[Dimensions[comps], dim]], 
    SparseArray[Table[D[comps, x[i, k]], {k, dim}]]]];

TensorDerivative[a_. Contract[t_, pairs_], dim_, i_] := 
  Contract[TensorDerivative[a t, dim, i], pairs + 1];
TensorDerivative[a_. TensorPermute[t_, perm_, OptionsPattern[]], dim_,
    i_] := TensorPermute[TensorDerivative[a t, dim, i], 
   Join[{1}, perm + 1]];
   
TensorSpinorDerivative[t_, dim_, i_] := If[EvenQ[dim],
   Contract[
    TensorProduct[SigmaTensor[dim], 
     MetricTensor[dim, "Raised" -> True], 
     TensorDerivative[t, dim, i]], {{1, 4}, {5, 6}}],
   Contract[
    TensorProduct[GammaTensor[dim], ChargeConjugationMatrix[dim], 
     MetricTensor[dim, "Raised" -> True], 
     TensorDerivative[t, dim, i]], {{1, 6}, {3, 4}, {7, 8}}]
   ];
   
su2irrep /: Times[su2irrep[i_], su2irrep[j_]] := 
  Sum[su2irrep[k], {k, Abs[i - j], i + j}];
su2irrep /: Power[su2irrep[i_], n_] /; n > 1 := 
  Times[su2irrep[i], Power[su2irrep[i], n - 1]];
countSU2singlets[spins__] := 
  FirstCase[Fold[Expand[#1 #2] &, su2irrep[0], su2irrep /@ {spins}], 
    n_. su2irrep[0] :> n] /. _?MissingQ -> 0;

Options[ConformalCorrelatorCount] = {"DefectCodimension" -> None};
ConformalCorrelatorCount[3, spins_, OptionsPattern[]] := Switch[{Length[spins], OptionValue["DefectCodimension"]},
  {2, None},
  Boole[Equal @@ spins],
  {3, None},
  Count[Total /@ Tuples[Range[-#, #] & /@ spins], 0], (*u(1) little group*)
  {x_ /; x >= 4, None},
  Times @@ (2 spins + 1) (*trivial little group*),
  {_, _},
  Infinity
]

ConformalCorrelatorCount[4, spins_, OptionsPattern[]] := Switch[{Length[spins], OptionValue["DefectCodimension"]},
  {2, None},
  Boole[spins[[1]] == Reverse[spins[[2]]]],
  {3, None},
  countSU2singlets @@ (Flatten@spins),(*su(2) little group*)
  {4, None},
  Count[Total /@ Tuples[Range[-#, #] & /@ Flatten[spins]], 
   0] ,(*u(1) little group*)
  {x_ /; x >= 5, None},
  Times @@ (2 Flatten@spins + 1) (*trivial little group*),
  {_, _},
  Infinity
]

Options[ConformalCorrelatorBuildingBlocks] = {"DefectCodimension" -> None, "Overcomplete" -> False};
ConformalCorrelatorBuildingBlocks[dim_, npts_, {i_, j_}, signs_List : {1,1}, opt : OptionsPattern[]] := 
ConformalCorrelatorBuildingBlocks[dim, npts, {i, j}, signs, opt] = If[OptionValue["Overcomplete"],
  Table[
     If[i != j || Length[sub] > 0, StringStructure[dim, {i, Sequence @@ sub, j}, signs, "DefectCodimension" -> OptionValue["DefectCodimension"]], Nothing], 
        {sub, Select[
           Subsets[If[OptionValue["DefectCodimension"] === None,Complement[Range[npts], {i, j}],Complement[Prepend[Flatten[Table[{{k, "Defect"}, {k, "Transverse"}}, {k, npts}], 1], 0], {{i, "Transverse"}, {j, "Transverse"}}]]], 
    	 OddQ[dim] || (-1)^(Length[#] + dim/2) (Times @@ signs) == -1 &]
    	}
  ],
  IndependentSet[ConformalCorrelatorBuildingBlocks[dim, npts, {i, j}, signs, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True]]
];

buildCorrelator[expr_, perm_, groupLengths_] := Module[{unsym, syms},
   unsym = TensorTranspose[Components[expr], InversePermutation@perm];
   syms = 
    Select[Permutations[Range@Total[groupLengths]], 
     Sort /@ TakeList[(Range@Total[groupLengths])[[#]], 
         groupLengths] == 
       TakeList[Range@Total[groupLengths], groupLengths] &];
   If[syms == {}, unsym, 
    1/Length[syms] Sum[TensorTranspose[unsym, p], {p, syms}]]
   ];

Clear[ConformalCorrelatorExpressions];
Options[ConformalCorrelatorExpressions] = {"DefectCodimension" -> None, "Overcomplete" -> False};
ConformalCorrelatorExpressions[3, spins_, opt : OptionsPattern[]] := 
  ConformalCorrelatorExpressions[3, spins, opt] = If[ConformalCorrelatorCount[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Flatten[Table[
      {TensorProduct @@ tup, 
       Ordering[
        Join @@ Cases[tup, stringstruct[_, is_] :> is[[{1, -1}]], All]]}
      , {sol, 
       Solve[Join[
         Thread[Sum[\[Alpha][i, 
              j] (SparseArray[{{i} -> 1/2}, {Length[spins]}] + SparseArray[{{j} -> 1/2}, {Length[
                spins]}]), {i, Length[spins]}, {j, i, 
             Length[spins]}] == spins], 
         Flatten@Table[\[Alpha][i, j] >= 0, {i, Length[spins]}, {j, 
            i, Length[spins]}]], 
        Flatten@Table[\[Alpha][i, j], {i, Length[spins]}, {j, i, 
           Length[spins]}], Integers]},
      {tup, 
       Tuples[Flatten[
         Table[ConformalCorrelatorBuildingBlocks[3, Length[spins], 
           List @@ var, "DefectCodimension" -> OptionValue["DefectCodimension"]], {var, Keys[sol]}, {ii, var /. sol}], 1]]}
      ], 1],
    Module[{full, inds},
     full = 
      ConformalCorrelatorExpressions[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True];
     inds = 
      IndependentSet[full, "Indices" -> True, 
       "TensorFunction" -> Function[{x}, Flatten[Table[fastEval[Sequence @@ x, 2 spins, genericPoint[3, OptionValue["DefectCodimension"], z, 1]], {z, 2, 5}]]], 
       "MaxIndependent" -> ConformalCorrelatorCount[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]]];
     full[[inds]]
     ]
    ]
  ];
ConformalCorrelatorExpressions[4, spins_, opt : OptionsPattern[]] := 
  ConformalCorrelatorExpressions[4, spins, opt] = If[ConformalCorrelatorCount[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Flatten[Table[
      {TensorProduct @@ tup, 
       Ordering[
        Join @@ Cases[
          tup, {stringstruct[_, is_], Lowered[h_[4]], 
            Lowered[h2_[4]]} :> 
           Thread[{is[[{1, -1}]], {h, h2} /. {WeylSpinor -> 1, 
               DottedWeylSpinor -> 2}}], All]]}
      , {sol, 
       Solve[Join[
         Thread[Sum[\[Alpha][i, j, k, 
              l] (SparseArray[{{i, k} -> 1/2}, {Length[spins], 2}] + 
  SparseArray[{{j, l} -> 1/2}, {Length[spins], 2}]), {i, Length[spins]}, {j, i, 
             Length[spins]}, {k, 2}, {l, 2}] == spins], 
         Flatten@Table[\[Alpha][i, j, k, l] >= 0, {i, 
            Length[spins]}, {j, i, Length[spins]}, {k, 2}, {l, 2}]],
         Flatten@
         Table[\[Alpha][i, j, k, l], {i, Length[spins]}, {j, i, 
           Length[spins]}, {k, 2}, {l, 2}], Integers]},
      {tup, 
       Tuples[Flatten[
         
         Table[ConformalCorrelatorBuildingBlocks[4, Length[spins], 
           List @@ var[[;; 2]], (List @@ 
              var[[3 ;;]]) /. {2 -> -1}, "DefectCodimension" -> OptionValue["DefectCodimension"]], {var, Keys[sol]}, {ii, 
           var /. sol}], 1]]}
      ], 1],
    Module[{full, inds},
     full = 
      ConformalCorrelatorExpressions[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True];
     inds = 
      IndependentSet[full, "Indices" -> True, 
       "TensorFunction" -> Function[{x}, Flatten[Table[fastEval[Sequence @@ x, 2 Flatten[spins], genericPoint[4, OptionValue["DefectCodimension"], z, 1]], {z, 2, 5}]]], 
       "MaxIndependent" -> ConformalCorrelatorCount[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]]];
     full[[inds]]
     ]
    ]
  ];

spinIndices[3, spins_, derivs_, perm_] := 
  Table[Lowered[DiracSpinor[3]], 2 (Total[spins] + Length[derivs])];
spinIndices[4, spins_, derivs_, perm_] := Flatten[Table[{
     Table[{Lowered[WeylSpinor[4]], Lowered[DottedWeylSpinor[4]]}, Count[derivs[[;;, 2]], i]],
     Table[Lowered[WeylSpinor[4]], 2 spins[[i,1]]], 
     Table[Lowered[DottedWeylSpinor[4]], 2 spins[[i, 2]]]
  }, {i, Length[spins]}]];

Options[KinematicPrefactor] = {"DefectCodimension" -> None};
KinematicPrefactor[dim_, \[CapitalDelta]s_, spins_, OptionsPattern[]] := Module[{kappas = \[CapitalDelta]s + (Total /@ spins)}, 1/Which[
	   OptionValue["DefectCodimension"] =!= None && Length[\[CapitalDelta]s] == 2,
	   CoordinateSquared[dim, 1, "Transverse" -> True]^(kappas[[1]]/2) CoordinateSquared[dim, 2, "Transverse" -> True]^(kappas[[2]]/2),
	   Length[\[CapitalDelta]s] == 2,
	   CoordinateSquared[dim, 1, 2]^kappas[[1]],
	   Length[\[CapitalDelta]s] == 3,
	   CoordinateSquared[dim, 1, 2]^((Total[kappas] - 2 kappas[[3]])/2) CoordinateSquared[dim, 1, 3]^((Total[kappas] - 2 kappas[[2]])/2) CoordinateSquared[dim, 2, 3]^((Total[kappas] - 2 kappas[[1]])/2),
	   Length[\[CapitalDelta]s] == 4,
	   (CoordinateSquared[dim, 2, 4]/CoordinateSquared[dim, 1, 4])^((kappas[[2]] - kappas[[1]])/2) (CoordinateSquared[dim, 1, 4]/CoordinateSquared[dim, 1, 3])^((kappas[[4]] - kappas[[3]])/2) CoordinateSquared[dim, 1, 2]^((kappas[[1]] + kappas[[2]])/2) CoordinateSquared[dim, 3, 4]^((kappas[[3]] + kappas[[4]])/2)
	]
];

Options[ConformalCorrelators] = {"DefectCodimension" -> None};
ConformalCorrelators[dim_, \[CapitalDelta]s_, spins_] := 
  ConformalCorrelators[dim, \[CapitalDelta]s, spins, {}];
ConformalCorrelators[dim_, \[CapitalDelta]s_, spins_, derivs_] := 
  ConformalCorrelators[dim, \[CapitalDelta]s, spins, derivs, 
   Range@Length[\[CapitalDelta]s]];
ConformalCorrelators[dim_, \[CapitalDelta]s_, spins_, derivs_, 
  perm_, opt : OptionsPattern[]] := 
 ConformalCorrelators[dim, \[CapitalDelta]s, spins, derivs, perm, opt] = 
  Module[{exprs, structs, rules},
   If[derivs === {},
    exprs = ConformalCorrelatorExpressions[dim, spins, opt];
    structs = 
     buildCorrelator[Sequence @@ #, 2 Flatten[spins]] & /@ exprs;
    rules = If[perm === Automatic, {}, x[i_, j_] :> x[perm[[i]], j]];
    Do[
     BuildTensor[{correlator[dim, \[CapitalDelta]s, spins, derivs, 
         perm, OptionValue["DefectCodimension"], i], Sequence @@ spinIndices[dim, spins, derivs, perm]}] = 
      If[ArrayQ[structs[[i]]], SparseArray, Identity][Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, opt] Normal[structs[[i]]] /. rules],
     {i, Length[structs]}
     ]
    ];
   Table[
    Tensor[{{correlator[dim, \[CapitalDelta]s, spins, derivs, perm, OptionValue["DefectCodimension"], i],
        Sequence @@ spinIndices[dim, spins, derivs, perm]}}], {i, 
     ConformalCorrelatorCount[dim, spins]}]
   ]
   
BuildTensor[{correlator[dim_, \[CapitalDelta]s_, spins_, {}, perm_, q_, i_], inds___}] /; {inds} == spinIndices[dim, spins, {}, perm] := (
	ConformalCorrelators[dim, \[CapitalDelta]s, spins, {}, perm, "DefectCodimension" -> q];
	BuildTensor[{correlator[dim, \[CapitalDelta]s, spins, {}, perm, q, i], inds}]
);
   
ConformalTest[dim_, \[CapitalDelta]s_, spins_, perm_, 
   opt : OptionsPattern[]] := Module[{indices, similar, indperm},
   indices = If[dim == 3,
     Flatten[Table[{perm[[i]], {}}, {i, Length[spins]}, {j, 2 spins[[i]]}], 1],
     Flatten[
      Table[{perm[[i]], {"Weyl" -> True, "Dotted" -> k == 2}}, {i, 
        Length[spins]}, {k, 2}, {j, 2 spins[[i, k]]}], 2]
     ];
   Table[
    Sum[2 Contract[
         TensorProduct[MetricTensor[dim], Coordinate[dim, i], 
          Coordinate[dim, i], 
          TensorDerivative[struct, dim, i]], {{2, 3}, {4, 5}}] - 
       CoordinateSquared[dim, i] TensorDerivative[struct, dim, i] + 
       2 \[CapitalDelta]s[[InversePermutation[perm][[i]]]] Contract[
         TensorProduct[MetricTensor[dim], Coordinate[dim, i], 
          struct], {{2, 3}}], {i, Length[\[CapitalDelta]s]}] + Sum[
      similar = 
       1 + Select[Range[j], indices[[#, 2]] == indices[[j, 2]] &];
      indperm = 
       InversePermutation@PermutationList[Cycles[{similar}], Length[indices] + 1];
      2 TensorPermute[
        Contract[
         TensorProduct[Coordinate[dim, indices[[j, 1]]], 
          RotationGenerators[dim, Sequence @@ indices[[j, 2]]], 
          struct], {{1, 3}, {5, j + 5}}], indperm], {j, 
       Length@indices}], {struct, 
     ConformalCorrelators[dim, \[CapitalDelta]s, spins, {}, perm, opt]}]
   ];
   
withCounts[xs_] := 
  Last@FoldList[
    Function[{list, x}, 
     Append[list, {x, Count[list[[;; , 1]], x] + 1}]], {}, xs];

BuildTensor[
    t : {correlator[dim_, \[CapitalDelta]s_, spins_, derivs_, perm_, q_,
       i_], idxs___}] /; derivs =!= {} := 
  BuildTensor[t] = 
   Module[{bd = 
      ConformalCorrelators[dim, \[CapitalDelta]s, spins, {}, perm, "DefectCodimension" -> q][[i]],
      pd = derivs /. {type_, n_Integer} :> {type, perm[[n]]}, 
     indsPerX, siPerm, dsiPerm, siPos, dsiPos, fullPerm, baseexpr, 
     expr},
    indsPerX = 
     Table[2 spins[[j]] + 
       If[OddQ[dim], 2, 1] Count[derivs[[;; , 2]], j], {j, 
       Length[\[CapitalDelta]s]}];
    siPerm = If[EvenQ[dim],
      Flatten@{
        Table[
         Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] + 
          Total[indsPerX[[;; derivs[[j, 2]] - 1, 1]]] + 1, {j, 
          Length[derivs]}], 
        Table[Total[indsPerX[[;; k, 1]]] - 2 spins[[k, 1]] + 
          Range[2 spins[[k, 1]]], {k, Length[\[CapitalDelta]s]}]
        },
      Flatten@{
        Table[
         2 Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] + 
          Total[indsPerX[[;; derivs[[j, 2]] - 1]]] + {1, 2}, {j, 
          Length[derivs]}], 
        Table[Total[indsPerX[[;; k]]] - 2 spins[[k]] + 
          Range[2 spins[[k]]], {k, Length[\[CapitalDelta]s]}]
        }
      ];
    dsiPerm = If[EvenQ[dim],
      Flatten@{
        Table[
         Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] + 
          Total[indsPerX[[;; derivs[[j, 2]] - 1, 2]]] + 1, {j, 
          Length[derivs]}], 
        Table[Total[indsPerX[[;; k, 2]]] - 2 spins[[k, 2]] + 
          Range[2 spins[[k, 2]]], {k, Length[\[CapitalDelta]s]}]
        },
      {}
      ];
    baseexpr = Fold[
      Switch[#2[[1]],
        "\[PartialD]", TensorSpinorDerivative[#1, dim, #2[[2]]],
        "u", 
        TensorProduct[
         TensorSpinorDerivative[u[dim, perm], dim, #2[[2]]], #1],
        "v", 
        TensorProduct[
         TensorSpinorDerivative[v[dim, perm], dim, #2[[2]]], #1]
        ] &, bd, Reverse[pd]];
    siPos = 
     Position[Indices[baseexpr], 
       Lowered[If[EvenQ[dim], WeylSpinor, DiracSpinor][dim]]][[;; , 1]];
    dsiPos = 
     Position[Indices[baseexpr], Lowered[DottedWeylSpinor[dim]]][[;; ,
        1]];
    fullPerm = 
     withCounts[
       Indices[baseexpr]] /. {{Lowered[
          If[EvenQ[dim], WeylSpinor, DiracSpinor][dim]], j_} :> 
        siPos[[siPerm[[j]]]], {Lowered[DottedWeylSpinor[dim]], j_} :> 
        dsiPos[[dsiPerm[[j]]]]};
    expr = TensorPermute[baseexpr, fullPerm];
    TensorTranspose[CanonicallyOrderedComponents@expr, Ordering@{idxs}]
];