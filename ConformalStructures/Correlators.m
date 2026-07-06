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
NCON[TensorDerivative[a_, dim_, i_]] := NCON[TensorProduct[Tensor[{{("\[PartialD]")^Row[{"(", i, ")"}], Lowered[Spacetime[dim]]}, {"("}}], a, Tensor[{{")"}}]]];
Format[td_TensorDerivative, TraditionalForm] := TensorTools`Private`tensorFormat[td];


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
ConformalCorrelatorCount[2, spins_, OptionsPattern[]] := Infinity;
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

(* d=6 counting uses SU(4) tensor-product decompositions from GroupMath, loaded on
   demand so that d = 2,3,4 do not depend on it.  The number of n-point structures
   is the number of little-group singlets in the product of the operators' reps.
   By the SO(6)->SO(5)->SO(4) branching (Gelfand-Tsetlin interlacing), an SO(6)
   irrep [a,b,c] contributes:
     3pt (SO(5) singlets): 1 iff it is symmetric-traceless [0,k,0], else 0;
     4pt (SO(4) singlets): (b+1) iff a==c, else 0.
   Verified against the CPPR building-block counts, e.g. <VVV>=4, <TTT>=11. *)
ConformalCorrelatorCount::nogm = "d=6 correlator counting requires the GroupMath package (https://renatofonseca.net/groupmath), which was not found on $Path.";
ensureGroupMath[] := ensureGroupMath[] =
  If[FindFile["GroupMath`"] === $Failed, Message[ConformalCorrelatorCount::nogm]; False, Quiet[Needs["GroupMath`"]]; True];

ConformalCorrelatorCount[6, spins_, OptionsPattern[]] := Switch[{Length[spins], OptionValue["DefectCodimension"]},
  {2, None},
  Boole[spins[[2]] === Reverse[spins[[1]]]],
  {3, None},
  If[ensureGroupMath[], Total@Cases[GroupMath`ReduceRepProduct[GroupMath`SU4, spins], {{0, _, 0}, mult_} :> mult], $Failed],
  {4, None},
  If[ensureGroupMath[], Total@Cases[GroupMath`ReduceRepProduct[GroupMath`SU4, spins], {{da_, db_, dc_}, mult_} /; da == dc :> (db + 1) mult], $Failed],
  {x_ /; x >= 5, None},
  If[ensureGroupMath[], Times @@ (GroupMath`DimR[GroupMath`SU4, #] & /@ spins), $Failed],
  {_, _},
  Infinity
]

Options[ConformalCorrelatorBuildingBlocks] = {"DefectCodimension" -> None, "Overcomplete" -> False};
ConformalCorrelatorBuildingBlocks[dim_, npts_, {i_, j_}, signs_, opt : OptionsPattern[]] := 
ConformalCorrelatorBuildingBlocks[dim, npts, {i, j}, signs, opt] = If[OptionValue["Overcomplete"],
  Table[
     If[i != j || Length[sub] > 0, StringStructure[dim, {i, Sequence @@ sub, j}, signs, "DefectCodimension" -> OptionValue["DefectCodimension"]], Nothing], 
        {sub, Select[
           Subsets[If[OptionValue["DefectCodimension"] === None,Complement[Range[npts], {i, j}],Complement[Prepend[Flatten[Table[{{k, "Defect"}, {k, "Transverse"}}, {k, npts}], 1], 0], {{i, "Transverse"}, {j, "Transverse"}}]]], 
    	 OddQ[dim] || (-1)^(Length[If[EvenQ[OptionValue["DefectCodimension"]], DeleteCases[#, 0], #]] + dim/2) (Times @@ signs) == -1 &]
    	}
  ],
  IndependentSet[ConformalCorrelatorBuildingBlocks[dim, npts, {i, j}, signs, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True], Method -> "Fold"]
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
ConformalCorrelatorExpressions[2, spins_, opt : OptionsPattern[]] := 
  ConformalCorrelatorExpressions[2, spins, opt] = If[ConformalCorrelatorCount[2, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Module[{vars},
       vars = Flatten@Table[\[Alpha][i,j,k,l], {i, Length[spins]}, {j, Length[spins]}, {k, 2}, {l, k, 2}];
       Flatten[Table[
      {TensorProduct @@ tup, 
       Ordering[
        Join @@ Cases[
          tup, {stringstruct[_, is_, _], Lowered[h_[2]], 
            Lowered[h2_[2]]} :> 
           Thread[{is[[{1, -1}]], {h, h2} /. {WeylSpinor -> 1, 
               DottedWeylSpinor -> 2}}], All]]}
      , {sol, 
       Solve[Join[
         Thread[Sum[\[Alpha][i, j, k, 
              l] (SparseArray[{{i, k} -> 1/2}, {Length[spins], 2}] + 
  SparseArray[{{j, l} -> 1/2}, {Length[spins], 2}]), {i, Length[spins]}, {j, Length[spins]}, {k, 2}, {l, k, 2}] == spins], (# >= 0) & /@ vars],
         vars, Integers]},
      {tup, 
       Tuples[Flatten[
         
         Table[ConformalCorrelatorBuildingBlocks[2, Length[spins], 
           List @@ var[[;; 2]], (List @@ var[[3 ;;]]) /. {2 -> -1}, "DefectCodimension" -> OptionValue["DefectCodimension"]], {var, Keys[sol]}, {ii, 
           var /. sol}], 1]]}
      ], 1]],
    Module[{full, inds},
     full = 
      ConformalCorrelatorExpressions[2, spins, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True];
     inds = 
      IndependentSet[full, "Indices" -> True, 
       "TensorFunction" -> Function[{x}, Flatten[Table[fastEval[Sequence @@ x, Range@Length[spins], 2 Flatten[spins], zz, safeCrossRatios[OptionValue["DefectCodimension"]][[1]]], {zz, 2, 5}]]], 
       "MaxIndependent" -> ConformalCorrelatorCount[2, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]]];
     full[[inds]]
     ]
    ]
  ];
ConformalCorrelatorExpressions[3, spins_, opt : OptionsPattern[]] := 
  ConformalCorrelatorExpressions[3, spins, opt] = If[ConformalCorrelatorCount[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Module[{vars},
       vars = Flatten@Table[\[Alpha][i, j], {i, Length[spins]}, {j, i, Length[spins]}];
    Flatten[Table[
      {TensorProduct @@ tup, 
       Ordering[
        Join @@ Cases[tup, stringstruct[_, is_, _] :> is[[{1, -1}]], All]]}
      , {sol, 
       Solve[Join[
         Thread[Sum[\[Alpha][i, 
              j] (SparseArray[{{i} -> 1/2}, {Length[spins]}] + SparseArray[{{j} -> 1/2}, {Length[
                spins]}]), {i, Length[spins]}, {j, i, 
             Length[spins]}] == spins], (# >= 0 &) /@ vars], vars, Integers]},
      {tup, 
       Tuples[Flatten[
         Table[ConformalCorrelatorBuildingBlocks[3, Length[spins], 
           List @@ var, {1,1}, "DefectCodimension" -> OptionValue["DefectCodimension"]], {var, Keys[sol]}, {ii, var /. sol}], 1]]}
      ], 1]
    ],
    Module[{full, inds},
     full = 
      ConformalCorrelatorExpressions[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True];
     inds = 
      IndependentSet[full, "Indices" -> True, 
       "TensorFunction" -> Function[{x}, Flatten[Table[fastEval[Sequence @@ x, Range@Length[spins], 2 spins, z, safeCrossRatios[OptionValue["DefectCodimension"]][[1]]], {z, 2, 5}]]], 
       "MaxIndependent" -> ConformalCorrelatorCount[3, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]]];
     full[[inds]]
     ]
    ]
  ];
ConformalCorrelatorExpressions[4, spins_, opt : OptionsPattern[]] := 
  ConformalCorrelatorExpressions[4, spins, opt] = If[ConformalCorrelatorCount[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Module[{vars},
       vars = Flatten@Table[\[Alpha][i,j,k,l], {i, Length[spins]}, {j, Length[spins]}, {k, 2}, {l, k, 2}];
       Flatten[Table[
      {TensorProduct @@ tup, 
       Ordering[
        Join @@ Cases[
          tup, {stringstruct[_, is_, _], Lowered[h_[4]], 
            Lowered[h2_[4]]} :> 
           Thread[{is[[{1, -1}]], {h, h2} /. {WeylSpinor -> 1, 
               DottedWeylSpinor -> 2}}], All]]}
      , {sol, 
       Solve[Join[
         Thread[Sum[\[Alpha][i, j, k, 
              l] (SparseArray[{{i, k} -> 1/2}, {Length[spins], 2}] + 
  SparseArray[{{j, l} -> 1/2}, {Length[spins], 2}]), {i, Length[spins]}, {j, Length[spins]}, {k, 2}, {l, k, 2}] == spins], (# >= 0) & /@ vars],
         vars, Integers]},
      {tup, 
       Tuples[Flatten[
         
         Table[ConformalCorrelatorBuildingBlocks[4, Length[spins], 
           List @@ var[[;; 2]], (List @@ var[[3 ;;]]) /. {2 -> -1}, "DefectCodimension" -> OptionValue["DefectCodimension"]], {var, Keys[sol]}, {ii, 
           var /. sol}], 1]]}
      ], 1]],
    Module[{full, inds},
     full = 
      ConformalCorrelatorExpressions[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"], "Overcomplete" -> True];
     inds = 
      IndependentSet[full, "Indices" -> True, 
       "TensorFunction" -> Function[{x}, Flatten[Table[fastEval[Sequence @@ x, Range@Length[spins], 2 Flatten[spins], z, safeCrossRatios[OptionValue["DefectCodimension"]][[1]]], {z, 2, 5}]]], 
       "MaxIndependent" -> ConformalCorrelatorCount[4, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]]];
     full[[inds]]
     ]
    ]
  ];

spinIndices[2, spins_, derivs_, perm_] := Flatten[Table[{
     Table[{Lowered[WeylSpinor[2]], Lowered[WeylSpinor[2]]}, Count[derivs[[;;, 2]], i]],
     Table[Lowered[WeylSpinor[2]], 2 spins[[i,1]]], 
     Table[Lowered[DottedWeylSpinor[2]], 2 spins[[i, 2]]]
  }, {i, Length[spins]}]];
spinIndices[3, spins_, derivs_, perm_] := 
  Table[Lowered[DiracSpinor[3]], 2 (Total[spins] + Length[derivs])];
spinIndices[4, spins_, derivs_, perm_] := Flatten[Table[{
     Table[{Lowered[WeylSpinor[4]], Lowered[DottedWeylSpinor[4]]}, Count[derivs[[;;, 2]], i]],
     Table[Lowered[WeylSpinor[4]], 2 spins[[i,1]]],
     Table[Lowered[DottedWeylSpinor[4]], 2 spins[[i, 2]]]
  }, {i, Length[spins]}]];

(* ------------------------------------------------------------------ *)
(* 6d (SO(6) = SU(4)) representation machinery                         *)
(* ------------------------------------------------------------------ *)

(* An SO(6) irrep is specified by an SU(4) Dynkin label {a, b, c}.  The
   corresponding Young diagram over the fundamental (Weyl spinor) 4 has row
   lengths {a+b+c, b+c, c}, so the operator carries a+2b+3c Weyl-spinor indices,
   Young-projected onto that diagram.  Pure-fundamental SU(4) tableaux are
   irreducible, so no trace removal is needed. *)

validDynkinQ[label_] := MatchQ[label, {Repeated[_Integer?NonNegative, {3}]}];
dynkinToPartition[{a_, b_, c_}] := DeleteCases[{a + b + c, b + c, c}, 0];
dynkinBoxes[{a_, b_, c_}] := a + 2 b + 3 c;

(* rows and columns (as position blocks) of the row-filled tableau of shape lambda *)
tableauBlocks[lambda_] := With[{rows = TakeList[Range[Total[lambda]], lambda]},
   {rows, Table[Select[rows, Length[#] >= j &][[;; , j]], {j, Max[lambda]}]}];

(* all permutations (as image lists) that permute only within the given blocks *)
blockPerms[blocks_, n_] := Map[
   Function[choice, Module[{s = Range[n]}, Do[s[[blocks[[k]]]] = choice[[k]], {k, Length[blocks]}]; s]],
   Tuples[Permutations /@ blocks]];

(* Young symmetrizer c = (row symmetrizer).(column antisymmetrizer), as a list of
   {index permutation, coefficient} terms suitable for a TensorTranspose average. *)
youngSymmetrizerTerms[lambda_] := youngSymmetrizerTerms[lambda] = Module[{n = Total[lambda], rows, cols, terms},
   If[n == 0, Return[{{{}, 1}}]];
   {rows, cols} = tableauBlocks[lambda];
   terms = Flatten[Table[{rho[[kappa]], Signature[kappa]},
       {rho, blockPerms[rows, n]}, {kappa, blockPerms[cols, n]}], 1];
   {#[[1, 1]], Total[#[[;; , 2]]]} & /@ GatherBy[terms, First]];

(* dimension of the SU(4) irrep = rank of the Young symmetrizer on (C^4)^{tensor n};
   used to validate the projector against known representation dimensions. *)
su4IrrepDimension[label_] := Module[{lambda = dynkinToPartition[label], n, tuples, idx, permMat},
   n = Total[lambda];
   If[n == 0, Return[1]];
   tuples = Tuples[Range[4], n];
   idx = First /@ PositionIndex[tuples];
   permMat[sigma_] := SparseArray[Table[{idx[t[[sigma]]], idx[t]} -> 1, {t, tuples}], {4^n, 4^n}];
   MatrixRank[Total[Function[term, term[[2]] permMat[term[[1]]]] /@ youngSymmetrizerTerms[lambda]]]];

(* Chirality convention: an undotted Weyl index is the fundamental 4 = [1,0,0], a
   dotted Weyl index the antifundamental 4bar = [0,0,1].  A rep [a,b,c] is realized
   either undotted (Young shape dynkinToPartition[{a,b,c}]) or dotted (shape of the
   conjugate {c,b,a}); the canonical choice is whichever has fewer boxes (undotted
   if a >= c, else dotted), so a rep and its conjugate get matching box counts and
   their connecting strings pair 4 with 4bar. *)
opDottedQ6[rep_] := rep[[1]] < rep[[3]];
opPartition6[rep_, dotted_] := dynkinToPartition[If[dotted, Reverse[rep], rep]];
opBoxes6[rep_, dotted_] := Total[opPartition6[rep, dotted]];
opIndexHead6[dotted_] := If[dotted, DottedWeylSpinor, WeylSpinor];

(* Index list of a d=6 correlator.  Each operator contributes opBoxes6 indices of a
   single chirality (Weyl if a>=c, dotted-Weyl otherwise).  A spinor derivative on an
   operator contributes two undotted Weyl indices (D=6 is 2 mod 4, so the sigma
   tensor is {Weyl, Weyl}); these lead each operator's block, matching the D=2/D=4
   convention and the derivative BuildTensor's index placement. *)
spinIndices[6, spins_, derivs_, perm_] := Flatten[Table[
   With[{d = opDottedQ6[spins[[i]]]}, {
      Table[{Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]}, Count[derivs[[;; , 2]], i]],
      Table[Lowered[opIndexHead6[d][6]], opBoxes6[spins[[i]], d]]
   }], {i, Length[spins]}]];

(* Young-project an already-assembled, index-reordered tensor: symmetrize each
   operator's index block with the Young symmetrizer for its rep (partition),
   rather than fully symmetrizing.  Shared by buildCorrelator6 (symbolic assembly)
   and the d=6 fast path in fastEval.m (numeric assembly at a concrete frame). *)
youngProject6[unsym_, partitions_] := Module[{boxes = Total /@ partitions, starts},
   starts = Prepend[Accumulate[Most[boxes]], 0];
   Total[Function[combo,
       (Times @@ combo[[;; , 2]]) TensorTranspose[unsym,
          Join @@ Table[starts[[k]] + combo[[k, 1]], {k, Length[partitions]}]]
      ] /@ Tuples[youngSymmetrizerTerms /@ partitions]]
];

(* like buildCorrelator, but projects each operator's index block with the Young
   symmetrizer for its rep (partition) rather than fully symmetrizing. *)
buildCorrelator6[expr_, perm_, partitions_] :=
   youngProject6[TensorTranspose[Components[expr], InversePermutation@perm], partitions];

(* SU(4) alpha-system: distribute each operator's spinor-index boxes among strings
   S_i X..X S_j between operators (a string deposits one index at each endpoint) and
   loops S_i X..X S_i (which deposit two indices on one operator, coupling it to
   coordinates).  For a chirality assignment chi (one per operator) the box counts
   n_i are fixed and the distribution solves  sum_j alpha[{i,j}] (with a loop {i,i}
   counting twice) == n_i.  Each solution, fanned over the building blocks' X
   insertions, gives a candidate structure; the reduction Young-projects and keeps
   an independent subset (exact arithmetic), capped at ConformalCorrelatorCount. *)
ConformalCorrelatorExpressions[6, spins_, opt : OptionsPattern[]] :=
 ConformalCorrelatorExpressions[6, spins, opt] =
  If[ConformalCorrelatorCount[6, spins, "DefectCodimension" -> OptionValue["DefectCodimension"]] == 0, {},
   If[OptionValue["Overcomplete"],
    Module[{npts = Length[spins], q = OptionValue["DefectCodimension"], chi, boxes, pairs, avars, sols},
     chi = opDottedQ6 /@ spins;
     boxes = MapThread[opBoxes6, {spins, chi}];
     pairs = Join[Table[{i, i}, {i, npts}], Subsets[Range[npts], {2}]];
     avars = \[Alpha] /@ pairs;
     sols = Solve[Join[
        Table[Sum[Count[pairs[[p]], k] \[Alpha][pairs[[p]]], {p, Length[pairs]}] == boxes[[k]], {k, npts}],
        Thread[avars >= 0]], avars, Integers];
     Join @@ Table[
       With[{slots = Flatten[Table[
            ConformalCorrelatorBuildingBlocks[6, npts, pairs[[p]],
              {If[chi[[pairs[[p, 1]]]], -1, 1], If[chi[[pairs[[p, 2]]]], -1, 1]}, "DefectCodimension" -> q],
            {p, Length[pairs]}, {ii, \[Alpha][pairs[[p]]] /. sol}], 1]},
        Table[
          {TensorProduct @@ tup,
           Ordering[Join @@ Cases[tup,
             {stringstruct[_, is_, _], Lowered[h_[6]], Lowered[h2_[6]]} :>
               Thread[{is[[{1, -1}]], {h, h2} /. {WeylSpinor -> 1, DottedWeylSpinor -> 2}}], All]]},
          {tup, Tuples[slots]}]],
       {sol, sols}]
    ],
    (* Reduce the overcomplete set to an independent basis at a SINGLE generic
       configuration.  Independence must be judged at one conformal frame: for
       n >= 4 points the cross-ratios are fixed there, so two structures related by
       a cross-ratio function (S_i = f(u,v) S_j) -- genuinely dependent as tensor
       structures -- stay dependent, whereas stacking several frames would inflate
       the rank above the true count.  For n <= 3 there are no cross-ratios and
       every frame gives the same rank.

       The bare structures carry only Sqrt[(X_ij^2)^2]-type factors from the string
       normalizations; at integer coordinates those radicands are perfect squares,
       so Sqrt auto-evaluates and the components are exact rationals -- no floating
       point and none of the algebraic numbers that a non-square configuration
       would introduce.  The reduction is therefore done in exact arithmetic via
       IndependentSet[..., Method -> "Fold"], whose indQ samples components when the
       flattened vector is wide, keeping the exact linear algebra fast. *)
    Module[{full, partitions, q = OptionValue["DefectCodimension"], npts = Length[spins],
            tensors, cfg},
     full = ConformalCorrelatorExpressions[6, spins, "DefectCodimension" -> q, "Overcomplete" -> True];
     partitions = MapThread[opPartition6, {spins, opDottedQ6 /@ spins}];
     tensors = Normal[buildCorrelator6[Sequence @@ #, partitions]] & /@ full;
     cfg = BlockRandom[SeedRandom[1]; Thread[Flatten@Array[x, {npts, 6}] -> RandomInteger[{2, 40}, npts 6]]];
     full[[IndependentSet[tensors, "TensorFunction" -> (Flatten[{Normal[# /. cfg]}] &),
        "Indices" -> True, Method -> "Fold"]]]
    ]
   ]
  ];

(* scaling "spin" entering the kinematic prefactor: the leading orthogonal weight
   l1.  For d=2,3,4 this is the sum of the spin labels; for d=6 (SU(4) Dynkin
   {a,b,c}) it is b + (a+c)/2. *)
repScalingSpin[dim_, rep_] := Total[rep];
repScalingSpin[6, {a_, b_, c_}] := b + (a + c)/2;

Options[KinematicPrefactor] = {"DefectCodimension" -> None};
KinematicPrefactor[dim_, \[CapitalDelta]s_, spins_, opt : OptionsPattern[]] := Module[{kappas = \[CapitalDelta]s + (repScalingSpin[dim, #] & /@ spins)}, 1/Which[
	   OptionValue["DefectCodimension"] =!= None && Length[\[CapitalDelta]s] == 2,
	   CoordinateSquared[dim, 1, "Transverse" -> True, opt]^(kappas[[1]]/2) CoordinateSquared[dim, 2, "Transverse" -> True, opt]^(kappas[[2]]/2),
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
  	If[Length[\[CapitalDelta]s] == 2 && !Equal@@\[CapitalDelta]s, Return[{}]];
   exprs = ConformalCorrelatorExpressions[dim, spins, opt];
   If[derivs === {},
    structs = buildCorrelator[Sequence @@ #, 2 Flatten[spins]] & /@ exprs;
    rules = If[perm === Automatic, {}, x[i_, j_] :> x[perm[[i]], j]];
    Do[
     BuildTensor[{correlator[dim, \[CapitalDelta]s, spins, derivs, perm, OptionValue["DefectCodimension"], i], Sequence @@ spinIndices[dim, spins, derivs, perm]}] = 
      If[ArrayQ[structs[[i]]], SparseArray, Identity][Explicit@KinematicPrefactor[dim, \[CapitalDelta]s, spins, opt] Normal[structs[[i]]] /. rules],
     {i, Length[structs]}
     ]
    ];
   Table[
    Tensor[{{correlator[dim, \[CapitalDelta]s, spins, derivs, perm, OptionValue["DefectCodimension"], i],
        Sequence @@ spinIndices[dim, spins, derivs, perm]}}], {i, 
     Min[ConformalCorrelatorCount[dim, spins, opt], Length[exprs]]}]
   ]

(* d=6 build: same shape as the generic path, but projects each operator's index
   block with its Young symmetrizer (buildCorrelator6) instead of symmetrizing. *)
ConformalCorrelators[6, \[CapitalDelta]s_, spins_, derivs_, perm_, opt : OptionsPattern[]] :=
 ConformalCorrelators[6, \[CapitalDelta]s, spins, derivs, perm, opt] =
  Module[{exprs, structs, rules, q = OptionValue["DefectCodimension"], partitions},
   If[Length[\[CapitalDelta]s] == 2 && ! Equal @@ \[CapitalDelta]s, Return[{}]];
   exprs = ConformalCorrelatorExpressions[6, spins, opt];
   partitions = MapThread[opPartition6, {spins, opDottedQ6 /@ spins}];
   If[derivs === {},
    structs = buildCorrelator6[Sequence @@ #, partitions] & /@ exprs;
    rules = If[perm === Automatic, {}, x[i_, j_] :> x[perm[[i]], j]];
    Do[
     BuildTensor[{correlator[6, \[CapitalDelta]s, spins, derivs, perm, q, i], Sequence @@ spinIndices[6, spins, derivs, perm]}] =
      If[ArrayQ[structs[[i]]], SparseArray, Identity][Explicit@KinematicPrefactor[6, \[CapitalDelta]s, spins, opt] Normal[structs[[i]]] /. rules],
     {i, Length[structs]}]
   ];
   Table[Tensor[{{correlator[6, \[CapitalDelta]s, spins, derivs, perm, q, i], Sequence @@ spinIndices[6, spins, derivs, perm]}}],
    {i, Min[ConformalCorrelatorCount[6, spins, opt], Length[exprs]]}]
  ];

BuildTensor[{correlator[dim_, \[CapitalDelta]s_, spins_, {}, perm_, q_, i_], inds___}] /; {inds} == spinIndices[dim, spins, {}, perm] := (
	ConformalCorrelators[dim, \[CapitalDelta]s, spins, {}, perm, "DefectCodimension" -> q];
	BuildTensor[{correlator[dim, \[CapitalDelta]s, spins, {}, perm, q, i], inds}]
);
   
ConformalTest[dim_, \[CapitalDelta]s_, spins_, perm_, 
   opt : OptionsPattern[]] := Module[{indices, similar, indperm},
   indices = Which[
     dim == 3,
     Flatten[Table[{perm[[i]], {}}, {i, Length[spins]}, {j, 2 spins[[i]]}], 1],
     dim == 6,
     (* canonical per-operator chirality; opBoxes6 indices each *)
     Flatten[Table[With[{d = opDottedQ6[spins[[i]]]},
        Table[{perm[[i]], {"Weyl" -> True, "Dotted" -> d}}, opBoxes6[spins[[i]], d]]], {i, Length[spins]}], 1],
     True,
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
      similar = Prepend[2 + Select[Range[j - 1], indices[[#, 2]] == indices[[j, 2]] &], 2];
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
     Table[If[dim == 6,
        (* {undotted, dotted}: an operator carries opBoxes6 indices of one chirality,
           each derivative adds two undotted (D=6 is 2 mod 4) *)
        With[{dt = opDottedQ6[spins[[j]]]}, {If[dt, 0, opBoxes6[spins[[j]], dt]], If[dt, opBoxes6[spins[[j]], dt], 0]}] + Count[derivs[[;; , 2]], j] {2, 0},
        2 spins[[j]] + Count[derivs[[;; , 2]], j] Which[dim == 4, {1, 1}, dim == 3, 2, dim == 2, {2, 0}]], {j,
       Length[\[CapitalDelta]s]}];
    siPerm = Which[dim == 4, (* 2D and 3D both have undotted indices on derivatives *)
      Flatten@{
        Table[
         Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] + 
          Total[indsPerX[[;; derivs[[j, 2]] - 1, 1]]] + 1, {j, 
          Length[derivs]}], 
        Table[Total[indsPerX[[;; k, 1]]] - 2 spins[[k, 1]] + 
          Range[2 spins[[k, 1]]], {k, Length[\[CapitalDelta]s]}]
        },
      dim == 3,
      Flatten@{
        Table[
         2 Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] + 
          Total[indsPerX[[;; derivs[[j, 2]] - 1]]] + {1, 2}, {j, 
          Length[derivs]}], 
        Table[Total[indsPerX[[;; k]]] - 2 spins[[k]] + 
          Range[2 spins[[k]]], {k, Length[\[CapitalDelta]s]}]
        },
      dim == 2,
      Flatten@{
        Table[
         2 Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] +
          Total[indsPerX[[;; derivs[[j, 2]] - 1, 1]]] + {1, 2}, {j,
          Length[derivs]}],
        Table[Total[indsPerX[[;; k, 1]]] - 2 spins[[k, 1]] +
          Range[2 spins[[k, 1]]], {k, Length[\[CapitalDelta]s]}]
        },
      dim == 6,  (* like dim==2 (two undotted per derivative); the operator's own
                    undotted count is indsPerX[[k,1]] minus the derivative indices *)
      Flatten@{
        Table[
         2 Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] +
          Total[indsPerX[[;; derivs[[j, 2]] - 1, 1]]] + {1, 2}, {j,
          Length[derivs]}],
        Table[With[{own = indsPerX[[k, 1]] - 2 Count[derivs[[;; , 2]], k]},
           Total[indsPerX[[;; k, 1]]] - own + Range[own]], {k, Length[\[CapitalDelta]s]}]
        }
      ];
    dsiPerm = If[EvenQ[dim],
      Flatten@{
        (* A spinor derivative carries a dotted index only in D = 0 (mod 4), where
           the sigma tensor is {Weyl, dotted-Weyl}; in D = 2 (mod 4) it is
           {Weyl, Weyl} (two undotted, cf. siPerm's dim==2 branch), so it deposits
           no dotted index and this derivative part must be empty -- otherwise it
           emits a phantom dotted slot per derivative that collides with the
           operator-dotted slots (harmless in 2D, where Weyl indices are
           1-dimensional, but corrupting in 6D). *)
        If[Mod[dim, 4] == 0,
         Table[
          Count[derivs[[;; j - 1, 2]], derivs[[j, 2]]] +
           Total[indsPerX[[;; derivs[[j, 2]] - 1, 2]]] + 1, {j,
           Length[derivs]}],
         {}],
        Table[With[{own = If[dim == 6, indsPerX[[k, 2]], 2 spins[[k, 2]]]},
           Total[indsPerX[[;; k, 2]]] - own + Range[own]], {k, Length[\[CapitalDelta]s]}]
        },
      {}
      ];
    baseexpr = Fold[
      Switch[#2[[1]],
        "\[PartialD]", TensorSpinorDerivative[#1, dim, #2[[2]]],
        "u", 
        TensorProduct[
         TensorSpinorDerivative[u[dim, perm, "DefectCodimension" -> q], dim, #2[[2]]], #1],
        "v", 
        TensorProduct[
         TensorSpinorDerivative[v[dim, perm, "DefectCodimension" -> q], dim, #2[[2]]], #1],
        "z", 
        TensorProduct[
         TensorSpinorDerivative[z[dim, perm], dim, #2[[2]]], #1],
        "zb", 
        TensorProduct[
         TensorSpinorDerivative[zb[dim, perm], dim, #2[[2]]], #1]
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