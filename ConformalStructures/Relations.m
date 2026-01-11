(* Wolfram Language package *)

zeroVecQ[vec_] := 
  MatchQ[vec, {0 ..}] || 
   MatchQ[Simplify[ArrayRules[vec][[;; , 2]]], {0 ..}];

(*fix this to prevent false positive*)
indQ[basis_, vec_] := 
  ! zeroVecQ[vec] && (Length[basis] == 0 ||
     If[Length[basis[[1]]] < Max[2000, 20 Length[basis]], 
      Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True], 
      Module[{nzidxs, chosen, sol, sbz}, 
       nzidxs = Select[ArrayRules[vec][[;; , 1, 1]], IntegerQ];
       chosen = 
        RandomSample[nzidxs, Min[Length[nzidxs], 20 Length[basis]]];
       sol = 
        Quiet@LinearSolve[Transpose[basis][[chosen]], vec[[chosen]]];
       If[Head[sol] === LinearSolve, 
          True, 
          sbz = sol . basis - vec;
          !zeroVecQ[sbz] (*could be that a different solution would have worked*)
        ]
      ]
    ]);

Options[IndependentSet] = {"TensorFunction" -> CanonicallyOrderedComponents, "Rules" -> {}, "MaxIndependent" -> Infinity, "Indices" -> False, Method -> Automatic};
IndependentSet[{}] := {};
IndependentSet[tensors_, opt : OptionsPattern[]] := Module[{comp1},
	comp1 = Flatten[OptionValue["TensorFunction"][First[tensors]]];
	Switch[OptionValue["Method"],
		Automatic,
		If[Length[comp1] <= 100000 && AllTrue[comp1, NumericQ] && OptionValue["MaxIndependent"] > Length[tensors]/2,
			ISReduce[tensors, Sequence @@ FilterRules[{opt}, Keys[Options[ISReduce]]]],
			ISFold[tensors, Sequence @@ FilterRules[{opt}, Keys[Options[ISFold]]]]
		],
		"Fold",
		ISFold[tensors, Sequence @@ FilterRules[{opt}, Keys[Options[ISFold]]]],
		_,
		ISReduce[tensors, Sequence @@ FilterRules[{opt}, Keys[Options[ISReduce]]]]
	]
];
  
Options[ISFold] = {"TensorFunction" -> CanonicallyOrderedComponents, "Rules" -> {}, "MaxIndependent" -> Infinity, "Indices" -> False};  
ISFold[tensors_, OptionsPattern[]] := 
  If[! ArrayQ[OptionValue["TensorFunction"][tensors[[1]]]], 
   If[TrueQ[OptionValue["Indices"]], {1}, tensors[[{1}]]], 
   With[{indices = 
      Module[{runningComps = 
         SparseArray[{}, {Length[tensors], 
           Length[Flatten[OptionValue["TensorFunction"][tensors[[1]]]]]}]},
        Fold[If[Length[#1] < OptionValue["MaxIndependent"], 
          With[{comp = 
             Flatten@Normal@
                OptionValue["TensorFunction"][tensors[[#2]]] /. 
              OptionValue["Rules"]}, 
           If[indQ[runningComps[[;; Length[#1]]], comp], 
            runningComps[[Length[#1] + 1]] = comp; 
            Append[#1, #2], #1]], #1] &, {}, Range@Length[tensors]]]},
     If[TrueQ[OptionValue["Indices"]], indices, tensors[[indices]]]]];
     
Options[ISReduce] = {"TensorFunction" -> CanonicallyOrderedComponents, "Rules" -> {}, "MaxIndependent" -> Infinity, "Indices" -> False};
ISReduce[tensors_, opt :OptionsPattern[]] := Module[{comps, reduced, indices},
	comps = Normal[Flatten@*OptionValue["TensorFunction"] /@ tensors] /. OptionValue["Rules"];
	reduced = RowReduce[Transpose[N@comps]];
	indices = (Table[FirstPosition[row, x : Except[0] /; NumericQ[x]], {row, reduced}] /. _?MissingQ -> Nothing)[[;; , 1]];
	If[Length[indices] > 0 && MatrixRank[comps[[indices]]] != Length[indices],
		ISFold[tensors, opt],
		If[OptionValue["Indices"], indices, tensors[[indices]]]
	]
];

uvpt[dim_, None] := {
   x[1, i_] :> Which[i == dim - 1, (Sqrt[-u^2 - (-1 + v)^2 + 2 u (1 + v)]/(u - 2(v + 1))), i == dim, (v - 1)/(u - 2(v + 1)), True, 0],
   x[2,  _] :> 0, 
   x[3, i_] :> -Boole[i == dim], 
   x[4, i_] :> Boole[i == dim]
};

uvpt[dim_, q_Integer] /; q > 1 := 
   {x[1, i_] :> Boole[i == 4],
 x[2, 3] -> Sqrt[-((-1 + v^2) (8 u v^2 + 2 v^3 + 4 u Sqrt[v^2 (-1 + 2 u + v) (1 + 2 u + v)] + v (-1 + 8 u^2 + 2 Sqrt[v^2 (-1 + 2 u + v) (1 + 2 u + v)])))]/Sqrt[v], 
 x[2, 4] -> 2 u v + v^2 + Sqrt[v^2 (-1 + 2 u + v) (1 + 2 u + v)], 
 x[2, _] -> 0};
 
uvpt[dim_, 1] = {x[1, 4] -> 1, x[1, _] -> 0, x[2, 4] -> 1, x[2, 3] -> 2 Sqrt[u], x[2, _] -> 0};

sct[dim_, x_, b_] := (x - b x . Components[MetricTensor[dim]] . x)/(1 - 2 (b . Components[MetricTensor[dim]] . x) + (b . Components[MetricTensor[dim]] . b) (x . Components[MetricTensor[dim]] . x));

genericPoint[dim_, q_, z_] := Simplify@Flatten@Table[x[i, j] -> sct[dim, Table[x[i, kk] /. uvpt[dim, q], {kk, dim}], If[q===None,PadLeft,PadRight][{z}, dim]][[j]], {i, 4}, {j, dim}];
genericPoint[dim_, q_, z_, i_] := genericPoint[dim, q, z] /. Thread[crossRatios[q] -> safeCrossRatios[q][[i]]];

crossRatios[None] = {u, v};
crossRatios[1] = {u};
crossRatios[q_Integer] /; q > 1 := {u,v};

(* solutions (u, v) to u = p^2, v = q^2, (u - v - 1)^2 - 4v^2 = r^2, found using ratpoints *)
safeCrossRatios[None] := {{25/16, 9/16}, {1369/2704, 9/16}, {755161/641601, 1156/2025}, {45369/
  85264, 48841/85264}, {837225/559504, 27889/48400}, {4356/7225, 361/
  625}, {5329/9216, 5329/9216}, {160801/121104, 70225/121104}, {729/
  400, 67081/115600}, {11881/6400, 3721/6400}, {21025/24336, 14161/
  24336}, {34969/24336, 14161/24336}, {42025/24336, 14161/
  24336}, {741321/547600, 12769/21904}, {15129/13456, 7921/
  13456}, {269361/529984, 1849/3136}, {1, 100/169}, {61009/41616, 
  24649/41616}, {142129/156816, 93025/156816}, {18769/35344, 21025/
  35344}, {15376/14161, 729/1225}, {18769/30976, 18769/30976}, {1681/
  2500, 1521/2500}, {24336/34225, 841/1369}, {13225/13456, 8281/
  13456}, {395641/336400, 8281/13456}, {5329/4624, 71289/
  115600}, {42025/56644, 121/196}, {116281/67600, 1681/2704}, {127449/
  168100, 4225/6724}, {126025/121104, 76729/121104}, {7225/13456, 
  8649/13456}, {7921/4624, 74529/115600}, {748225/529984, 2025/
  3136}, {38809/32400, 841/1296}, {354025/219024, 841/1296}, {9801/
  19600, 12769/19600}, {84681/59536, 38809/59536}, {25921/14400, 9409/
  14400}, {5776/11025, 289/441}, {21904/11025, 289/441}, {643204/
  370881, 289/441}, {219961/231361, 900/1369}, {21025/18496, 12321/
  18496}, {585225/405769, 1600/2401}, {81/64, 7225/10816}, {159201/
  270400, 7225/10816}, {1521/2500, 1681/2500}, {370881/422500, 1681/
  2500}, {7569/5476, 3721/5476}, {137641/270400, 1089/1600}, {18769/
  20736, 14161/20736}, {76729/41616, 28561/41616}, {2304/1225, 841/
  1225}, {107584/207025, 841/1225}, {170569/121104, 83521/
  121104}, {180625/132496, 91809/132496}, {25/36, 25/36}, {48841/
  30276, 25/36}, {180625/94864, 66049/94864}, {71289/67600, 47089/
  67600}, {233289/150544, 105625/150544}, {4225/4624, 3249/
  4624}, {78961/144400, 101761/144400}, {83521/59536, 42025/
  59536}, {35721/48400, 1369/1936}, {826281/739600, 21025/
  29584}, {77841/40000, 28561/40000}, {459684/474721, 121/
  169}, {42025/24336, 17689/24336}, {27225/33856, 24649/33856}, {7569/
  5776, 4225/5776}, {182329/115600, 84681/115600}, {866761/435600, 
  12769/17424}, {1369/1936, 35721/48400}, {1296/841, 625/
  841}, {113569/90000, 67081/90000}, {196/225, 169/225}, {51076/65025,
   169/225}, {189225/150544, 113569/150544}, {15129/13225, 400/
  529}, {9409/7396, 5625/7396}, {72361/44944, 34225/44944}, {140625/
  132496, 101761/132496}, {101761/63504, 48841/63504}, {395641/490000,
   15129/19600}, {38025/44944, 34969/44944}, {47961/38416, 29929/
  38416}, {349281/649636, 3025/3844}, {7225/13689, 64/81}, {9801/
  10000, 7921/10000}, {410881/608400, 19321/24336}, {18769/15376, 
  12321/15376}, {68121/67600, 54289/67600}, {24649/33856, 27225/
  33856}, {21904/38025, 1225/1521}, {628849/435600, 14161/
  17424}, {9409/5184, 4225/5184}, {20449/13456, 11025/13456}, {168921/
  85264, 70225/85264}, {11881/14400, 11881/14400}, {881721/672400, 
  22201/26896}, {66049/34225, 1156/1369}, {9801/18496, 15625/
  18496}, {34969/44944, 38025/44944}, {225/196, 169/196}, {14161/
  24336, 21025/24336}, {34225/24336, 21025/24336}, {169/225, 196/
  225}, {70225/126736, 110889/126736}, {1, 256/289}, {127449/85264, 
  75625/85264}, {2809/3136, 2809/3136}, {16/9, 1369/1521}, {2500/1681,
   1521/1681}, {14161/20736, 18769/20736}, {78961/71824, 65025/
  71824}, {93025/156816, 142129/156816}, {841/441, 400/441}, {485809/
  275625, 400/441}, {30625/24336, 22201/24336}, {559504/342225, 1849/
  2025}, {3249/4624, 4225/4624}, {210681/384400, 14161/
  15376}, {199809/160000, 5929/6400}, {116281/90000, 83521/
  90000}, {106929/99856, 93025/99856}, {165649/144400, 136161/
  144400}, {3969/2704, 64009/67600}, {2809/2704, 2601/2704}, {74529/
  144400, 139129/144400}, {89401/84100, 3249/3364}, {4225/4356, 4225/
  4356}, {4225/7744, 7569/7744}, {7921/10000, 9801/10000}, {8281/
  13456, 13225/13456}, {28561/14400, 14161/14400}, {54289/67600, 
  68121/67600}, {83521/67600, 68121/67600}, {82369/48400, 48841/
  48400}, {450241/547600, 22201/21904}, {231361/176400, 7225/
  7056}, {2601/2704, 2809/2704}, {76729/121104, 126025/
  121104}, {24649/19600, 20449/19600}, {720801/659344, 20449/
  19600}, {47089/67600, 71289/67600}, {101761/132496, 140625/
  132496}, {30625/21904, 23409/21904}, {93025/99856, 106929/
  99856}, {184041/115600, 124609/115600}, {573049/810000, 34969/
  32400}, {559504/312481, 2025/1849}, {65025/71824, 78961/
  71824}, {485809/250000, 441/400}, {2500/1521, 1681/1521}, {423801/
  462400, 20449/18496}, {2704/1369, 1521/1369}, {7921/13456, 15129/
  13456}, {61009/44944, 50625/44944}, {289/256, 289/256}, {12321/
  18496, 21025/18496}, {33489/18496, 21025/18496}, {136161/144400, 
  165649/144400}, {169/196, 225/196}, {71289/115600, 5329/4624}, {225/
  169, 196/169}, {727609/469225, 196/169}, {14161/8100, 9409/
  8100}, {5625/3136, 3721/3136}, {881721/906304, 3721/3136}, {841/
  1296, 38809/32400}, {5929/4624, 5625/4624}, {938961/846400, 41209/
  33856}, {12321/15376, 18769/15376}, {67081/40000, 48841/
  40000}, {474721/774400, 38025/30976}, {84681/168100, 8281/
  6724}, {68121/67600, 83521/67600}, {21904/30625, 1521/1225}, {29929/
  38416, 47961/38416}, {11025/5776, 7225/5776}, {113569/150544, 
  189225/150544}, {20449/19600, 24649/19600}, {22201/24336, 30625/
  24336}, {67081/90000, 113569/90000}, {7225/10816, 81/64}, {5625/
  7396, 9409/7396}, {5625/4624, 5929/4624}, {537289/781456, 5929/
  4624}, {83521/90000, 116281/90000}, {804609/672400, 34969/
  26896}, {7921/6084, 7921/6084}, {4225/5776, 7569/5776}, {625/1156, 
  1521/1156}, {15129/10000, 529/400}, {70225/121104, 160801/
  121104}, {196/169, 225/169}, {51076/48841, 225/169}, {127449/220900,
   11881/8836}, {50625/44944, 61009/44944}, {91809/132496, 180625/
  132496}, {277729/250000, 13689/10000}, {114921/67600, 3721/
  2704}, {3721/5476, 7569/5476}, {87025/59536, 82369/59536}, {137641/
  76176, 105625/76176}, {7225/5184, 7225/5184}, {459684/339889, 169/
  121}, {23409/21904, 30625/21904}, {38025/21904, 30625/
  21904}, {42025/59536, 83521/59536}, {21025/24336, 34225/
  24336}, {88804/46225, 2601/1849}, {24649/48400, 68121/
  48400}, {83521/121104, 170569/121104}, {35721/34225, 1936/
  1369}, {38809/59536, 84681/59536}, {14161/24336, 34969/24336}, {1, 
  36/25}, {613089/846400, 48841/33856}, {21904/38025, 2209/
  1521}, {14161/8464, 12321/8464}, {107584/142129, 1225/841}, {82369/
  59536, 87025/59536}, {24649/41616, 61009/41616}, {64009/67600, 3969/
  2704}, {137641/184041, 1600/1089}, {37249/20736, 30625/
  20736}, {9409/6084, 9025/6084}, {1521/1681, 2500/1681}, {370881/
  284089, 2500/1681}, {42025/28224, 42025/28224}, {62500/110889, 121/
  81}, {75625/85264, 127449/85264}, {529/400, 15129/10000}, {11025/
  13456, 20449/13456}, {219961/152100, 1369/900}, {5776/7225, 441/
  289}, {34969/19600, 29929/19600}, {76729/48400, 74529/48400}, {625/
  841, 1296/841}, {38809/21025, 1296/841}, {332929/360000, 22201/
  14400}, {33489/18496, 28561/18496}, {485809/828100, 7569/
  4900}, {9025/6084, 9409/6084}, {105625/150544, 233289/150544}, {9/
  16, 25/16}};
  
safeCrossRatios[1] = RandomSample[List /@ (FareySequence[70]^2)];
        
safeCrossRatios[qq_Integer] /; qq > 1 := safeCrossRatios[qq] = RandomSample@Module[{qs, vs},
	qs = Select[Rest@Most@FareySequence[70], FreeQ[Sqrt[2 # (Sqrt[1 + #^2] + #) + 1], Power[_, 1/2] | Power[_, -1/2]] &];
	vs = Select[Rest@Most@FareySequence[70], FreeQ[Sqrt[#^2 - 1], Power[_, 1/2] | Power[_, -1/2]] &];
	Rest@Flatten[Table[{(Sqrt[1 + q^2] - v)/2, v}, {q, qs}, {v, vs}], 1]
]
  
  
Options[fitRational] = {"Prefactors" -> {1 &}};
fitRational[data_, deg_, opt : OptionsPattern[]] := 
  Module[{numParams = Dimensions[data][[2]] - 1, params, monomials, 
    numMonomials, rat, mats, found, ans}, 
   params = (ToExpression["\\[Formal" <> # <> "]"] & /@ 
       RotateLeft[Capitalize@Alphabet[], 20])[[;; numParams]];
   monomials = 
    Times @@@ (params^# & /@ 
       Select[Tuples[Range[0, deg], numParams], Total[#] <= deg &]);
   If[AllTrue[data[[;; , -1]], # === 0 &], 0 &, 
    numMonomials = 
     Table[monomials /. Thread[params -> Most[pt]], {pt, data}];
    rat = 
     FirstCase[data[[;; , -1]], 
      x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
         Cases[x, Power[y_, 1/2 | -1/2] :> Sqrt[y], All])];
    mats = 
     Table[ArrayFlatten[{{(pf @@@ (Most /@ 
              data)) numMonomials, (data[[;; , -1]]/
            rat) numMonomials}}], {pf, OptionValue["Prefactors"]}];
    found = False;
    ans = 0;
    Do[If[MatrixRank[N@mats[[i]]] != Min[Dimensions[mats[[i]]]], 
      With[{null = NullSpace[mats[[i]]]}, 
       If[null =!= {}, 
        ans = -rat (OptionValue["Prefactors"][[i]] @@ 
            params) null[[1, ;; Length[monomials]]] . monomials/
           null[[1, Length[monomials] + 1 ;;]] . monomials;
        If[ans =!= 0, found = True;
         Break[]];]]], {i, Length[mats]}];
    If[found, 
     With[{params2 = params, ans2 = Simplify[ans]}, 
      Function[Evaluate@params2, ans2]], None]]];

unrollRows[mat_, subset_, numRows_] := 
  SparseArray[
   ArrayRules[
     mat] /. {a_Integer, b_Integer} :> {subset[[a]], b}, {numRows, 
    Length[mat[[1]]]}];
    
StructureRelations[structs_] := StructureRelations[structs] = If[
     First@Cases[structs, c_correlator :> c[[-2]], All] =!= None || (Length[structs] >= 4 || First@Cases[structs, c_correlator :> Length[c[[2]]], All] >= 4), 
   	 fittedRelations[structs],
   	 symbolicRelations[structs]
  ];
  
crossRatioAssumptions[q_] := If[q === None, And @@ (1/2 < # < 2 & /@ crossRatios[q]), And @@ (0 < # & /@ crossRatios[q])];
symbolicRelations[structs_] := With[{q = First@Cases[structs, correlator[___, q_, _] :> q, All], dim = First@Cases[structs, correlator[dim_, ___] :> dim, All]},
   If[# === {}, {}, FullSimplify[RowReduce[#, ZeroTest -> (Function[expr, Simplify[expr, crossRatioAssumptions[q]] === 0])], crossRatioAssumptions[q]]] &@
   FullSimplify[NullSpace[Flatten[Table[Transpose@ArrayFlatten[Flatten@*List@*CanonicallyOrderedComponents /@ structs] /. genericPoint[dim, q, z], {z, 2, 5}], 1]], crossRatioAssumptions[q]]
];
    
fittedRelations[structs_] := 
   Block[{zmax = 3, dim, q, structComps, idxs, other, ans, step, sols, safes, rule, todo, mat1, mat2},
    dim = First@Cases[structs, correlator[dim_, ___] :> dim, All];
    q = First@Cases[structs, correlator[___, q_, _] :> q, All];
    safes = safeCrossRatios[q];
    structComps = Flatten[Table[Transpose[ArrayFlatten[Flatten[{Normal[
       If[
         First@Cases[#,{_correlator, inds___} :> Length[{inds}],All] >= 4 && Max@Cases[#, c_correlator :> Length[c[[4]]],All] <= 1,
           fastEvalCOC[#,z,safes[[11]]],
           Normal[CanonicallyOrderedComponents[#]] /. genericPoint[dim, q, z, 11]
       ]
    ]}] & /@ structs]], {z, 2, zmax}], 1];
    idxs = Length[structs] + 1 - IndependentSet[Reverse@Transpose@structComps, "Indices" -> True];
    other = Complement[Range@Length[structs], idxs];
    ans = Association@Table[{j, idxs[[i]]} -> -None, {j, Length[other]}, {i, Length[idxs]}];
    step = 0;
    sols = {};
    If[! TrueQ[$consoleMode], 
     Monitor[
       While[! FreeQ[ans, None], 
         todo = Select[Range@Length[other], Function[j, AnyTrue[ans /@ Table[{j, idx}, {idx, idxs}], # === -None &]]];
         sols = Join[sols, 
           Table[structComps = Flatten[Table[Transpose[Table[Flatten[{
              Which[
                 ! MemberQ[Join[idxs, other[[todo]]], structIdx], 
                   Table[0, Length[structComps]/(zmax - 1)],
                 First@Cases[structs[[structIdx]],{_correlator, inds___} :> Length[{inds}],All] >= 4 && Max@Cases[structs[[structIdx]], c_correlator :> Length[c[[4]]],All] <= 1,
                   fastEvalCOC[structs[[structIdx]],z,safes[[ii]]],
                 True, 
                   Normal[CanonicallyOrderedComponents[structs[[structIdx]]]] /. genericPoint[dim, q, z, ii]
               ]
            }], {structIdx, Length[structs]}]], {z, 2, zmax}], 1];
            mat1 = structComps[[;; , idxs]];
            mat2 = structComps[[;; , other[[todo]]]];
            Quiet@Check[{safes[[ii]], unrollRows[Transpose@LinearSolve[mat1, mat2], todo, Length[other]]}, Nothing], {ii, Length[sols] + 1, (step + 1) (step + 4)}]
         ];
       Do[If[
          ans[{j, idxs[[i]]}] === -None, 
          ans[{j, idxs[[i]]}] = -((fitRational[sols /. {{uvs__}, b_} :> {uvs, b[[j, i]]}, step, "Prefactors" -> Which[q === None, {1 &, Sqrt[#1] &, Sqrt[#2] &, Sqrt[#1 #2] &}, q === 2, {1 &, Sqrt[1 - #2^2] &}, True, {1 &}]] @@ crossRatios[q]) /. _None -> None)], 
          {j, Length[other]}, {i, Length[idxs]}
       ];
       step = step + 1
       ], 
      Panel[Row[{
         Column[{
            Style["Spacetime Structure Relations", 14, Bold], 
            Spacer[10], 
            Style[ToString@StringForm["Degree ``", step], Bold], 
            Spacer[10], 
            ToString@StringForm["Fit points: ``/``", If[IntegerQ[ii], ii, (step + 1) (step + 4)], (step + 1) (step + 4)]
         }], 
         Spacer[50], 
         MatrixPlot[
          Table[If[ans[{j, idxs[[i]]}] === -None, Orange, White], {j, Length[other]}, {i, Length[idxs]}], 
          FrameTicks -> None, 
          Mesh -> True, 
          ImageSize -> 100
         ]
      }]]
     ], 
     While[! FreeQ[ans, None], 
         todo = Select[Range@Length[other], Function[j, AnyTrue[ans /@ Table[{j, idx}, {idx, idxs}], # === -None &]]];
         sols = Join[sols, 
            Table[
               Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
	           Print["Spacetime Structure Relations"];
	           Print["Degree ", step];
	         
	           Print[ToString@StringForm["Fit points: ``/``", If[IntegerQ[ii], ii, (step + 1) (step + 4)], (step + 1) (step + 4)]];
	         
	           Print[ToString@StringForm["Found functions: ``/``", Sum[Boole[ans[{j, idxs[[i]]}] =!= -None], {j, Length[other]}, {i, Length[idxs]}], Length[other] Length[idxs]]];
         
         structComps = Flatten[Table[Transpose[Table[Flatten[{
              Which[
                 ! MemberQ[Join[idxs, other[[todo]]], structIdx], 
                   Table[0, Length[structComps]/(zmax - 1)],
                 First@Cases[structs[[structIdx]],{_correlator, inds___} :> Length[{inds}],All] >= 4 && Max@Cases[structs[[structIdx]], c_correlator :> Length[c[[4]]],All] <= 1,
                   fastEvalCOC[structs[[structIdx]],z,safes[[ii]]],
                 True, 
                   Normal[CanonicallyOrderedComponents[structs[[structIdx]]]] /. genericPoint[dim, q, z, ii]
               ]
            }], {structIdx, Length[structs]}]], {z, 2, zmax}], 1];
            mat1 = structComps[[;; , idxs]];
            mat2 = structComps[[;; , other[[todo]]]];
            Quiet@Check[{safes[[ii]], unrollRows[Transpose@LinearSolve[mat1, mat2], todo, Length[other]]}, Nothing], {ii, Length[sols] + 1, (step + 1) (step + 4)}]
         ];
         Do[If[
          ans[{j, idxs[[i]]}] === -None, 
          ans[{j, idxs[[i]]}] = -((fitRational[sols /. {{uvs__}, b_} :> {uvs, b[[j, i]]}, step, "Prefactors" -> Which[q === None, {1 &, Sqrt[#1] &, Sqrt[#2] &, Sqrt[#1 #2] &}, q === 2, {1 &, Sqrt[1 - #2^2] &}, True, {1 &}]] @@ crossRatios[q]) /. _None -> None)], 
          {j, Length[other]}, {i, Length[idxs]}
         ];
       
         step = step + 1;
         Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
    ]];
    If[Length[other] == 0, {}, SparseArray[Join[Normal[ans], Table[{j, other[[j]]} -> 1, {j, Length[other]}]]]]
 ];