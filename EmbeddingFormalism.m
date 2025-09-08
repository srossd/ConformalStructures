(* Wolfram Language package *)

$signatureFactor = 1;
SignatureFactor[] := $signatureFactor;

SetSignature["Lorentzian"] := ($signatureFactor = I;);
SetSignature["Euclidean"] := ($signatureFactor = 1;);

SetSignature::badsig = 
  "The signature `` is not recognized; use \"Lorentzian\" or \
\"Euclidean\".";
SetSignature[sig_] := Message[SetSignature::badsig, sig];

IndexData[Spacetime[dim_]] := Index[dim + 2, "Greek", 12];
IndexData[DiracSpinor[dim_]] := 
  Index[2^Floor[dim/2], "Greek", 1, Style[#, Bold] &];
IndexData[WeylSpinor[dim_]] := Index[2^Floor[(dim - 1)/2], "Greek", 1];
IndexData[DottedWeylSpinor[dim_]] := 
  Index[2^Floor[(dim - 1)/2], "Greek", 1, OverDot];

IndexData[EmbeddingSpacetime[dim_]] := 
  Index[dim + 2, "Latin", 13, Capitalize];
IndexData[EmbeddingDiracSpinor[dim_]] := 
  Index[2^Floor[dim/2 + 1], "Greek", 1, Style[Capitalize[#], Bold] &];
IndexData[EmbeddingWeylSpinor[dim_]] := 
  Index[2^Floor[(dim - 1)/2 + 1], "Greek", 1, Capitalize];
IndexData[EmbeddingDottedWeylSpinor[dim_]] := 
  Index[2^Floor[(dim - 1)/2 + 1], "Greek", 1, OverDot@*Capitalize];

Options[metric] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
BuildTensor[{metric[opt : OptionsPattern[]], h_[Spacetime[dim_]], h_[Spacetime[dim_]]}] := Module[{diag, q},
   diag = Prepend[Table[1, dim - 1], $signatureFactor^2];
   If[OptionValue[metric, {opt}, "DefectCodimension"] =!= None,
      q = OptionValue[metric, {opt}, "DefectCodimension"];
      If[OptionValue[metric, {opt}, "Defect"],     diag = ReplacePart[diag, Table[{i} -> 0, {i, dim - q + 1, dim}]]];
      If[OptionValue[metric, {opt}, "Transverse"], diag = ReplacePart[diag, Table[{i} -> 0, {i, dim - q}]]];
   ];
   SparseArray@DiagonalMatrix[diag]
];

Options[MetricTensor] = {"Raised" -> False, "DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
MetricTensor[dim_, opt : OptionsPattern[]] := 
  Tensor[{{metric[Sequence @@ FilterRules[{opt}, Options[metric]]], 
     If[OptionValue["Raised"], Raised, Lowered][Spacetime[dim]], 
     If[OptionValue["Raised"], Raised, Lowered][Spacetime[dim]]
  }}];

BuildTensor[{metric[opt : OptionsPattern[]], h_[EmbeddingSpacetime[dim_]], h_[EmbeddingSpacetime[dim_]]}] := Module[{diag, q},
   diag = Join[{1,-1},Prepend[Table[1, dim - 1], $signatureFactor^2]];
   If[OptionValue[metric, {opt}, "DefectCodimension"] =!= None,
      q = OptionValue[metric, {opt}, "DefectCodimension"];
      If[OptionValue[metric, {opt}, "Defect"],     diag = ReplacePart[diag, Table[{i} -> 0, {i, dim + 2 - q + 1, dim + 2}]]];
      If[OptionValue[metric, {opt}, "Transverse"], diag = ReplacePart[diag, Table[{i} -> 0, {i, dim + 2 - q}]]];
   ];
   SparseArray@DiagonalMatrix[diag]
];
    
Options[EmbeddingMetricTensor] = {"Raised" -> False, "Defect" -> False, "Transverse" -> False};
EmbeddingMetricTensor[dim_, opt : OptionsPattern[]] := 
  Tensor[{{metric[Sequence @@ FilterRules[{opt}, Options[metric]]],
     If[OptionValue["Raised"], Raised, Lowered][EmbeddingSpacetime[dim]], 
     If[OptionValue["Raised"], Raised, Lowered][EmbeddingSpacetime[dim]]
  }}];

Unprotect[KroneckerProduct];
KroneckerProduct[a_] := a;
Protect[KroneckerProduct];

gammas[2] := {PauliMatrix[1], PauliMatrix[2]};
gammas[n_Integer] /; n > 1 && EvenQ[n] := 
  Join[KroneckerProduct[#, 
      IdentityMatrix[2^(n/2 - 1)]] & /@ {PauliMatrix[1], 
     PauliMatrix[2]}, 
   KroneckerProduct[PauliMatrix[3], #] & /@ gammas[n - 2]];
gammas[n_Integer] /; n > 1 && OddQ[n] := 
  Append[gammas[n - 1], 
   KroneckerProduct @@ Table[PauliMatrix[3], Floor[n/2]]];
gammas[p_, q_] := Join[Table[I, p], Table[1, q]] gammas[p + q];

gammaStar[n_] := I^Floor[n/2] Dot @@ gammas[n];
gammaStar[p_, q_] := gammaStar[p + q];

perm[args__] := 
  Module[{star = gammaStar[args]}, 
   Join[Position[Diagonal[star], 1][[;; , 1]], 
    Position[Diagonal[star], -1][[;; , 1]]]];
weylGammas[args__] := gammas[args][[;; , perm[args], perm[args]]];
weylGammaStar[args__] := gammaStar[args][[perm[args], perm[args]]];

Cmat[p_, q_] := Cmat[p + q];
Cmat[n_] := -(KroneckerProduct @@ 
     Take[Riffle[Table[PauliMatrix[If[Mod[n, 4] == 3, 2, 1]], n], 
       Table[PauliMatrix[If[Mod[n, 4] == 3, 1, 2]], n]], 
      UpTo[Floor[n/2]]] . gammaStar[n])[[perm[n], perm[n]]];

BuildTensor[{chargeconj[], Lowered[DiracSpinor[dim_]], Lowered[DiracSpinor[dim_]]}] := SparseArray@Inverse[Cmat[dim]];
BuildTensor[{chargeconj[], Raised[DiracSpinor[dim_]], Raised[DiracSpinor[dim_]]}] := SparseArray@Transpose[Cmat[dim]];

Options[ChargeConjugationMatrix] = {"Raised" -> False};
ChargeConjugationMatrix[dim_, OptionsPattern[]] := 
 Tensor[{{chargeconj[], 
    If[OptionValue["Raised"], Raised, Lowered][DiracSpinor[dim]], 
    If[OptionValue["Raised"], Raised, Lowered][DiracSpinor[dim]]}}];
    
BuildTensor[{chargeconj[], Lowered[WeylSpinor[dim_]], Lowered[WeylSpinor[dim_]]}] := Components@Contract[TensorProduct[WeylProjection[dim], WeylProjection[dim, "Transpose" -> True]], {{2, 3}}];
BuildTensor[{chargeconj[], Lowered[DottedWeylSpinor[dim_]], Lowered[DottedWeylSpinor[dim_]]}] := Components@Contract[TensorProduct[WeylProjection[4, "Dotted" -> True], WeylProjection[4, "Transpose" -> True, "Dotted" -> True]], {{2, 3}}];
BuildTensor[{chargeconj[], Raised[WeylSpinor[dim_]], Raised[WeylSpinor[dim_]]}] := Inverse@Transpose@Components[WeylChargeConjugationMatrix[dim]];
BuildTensor[{chargeconj[], Raised[DottedWeylSpinor[dim_]], Raised[DottedWeylSpinor[dim_]]}] := Inverse@Transpose@Components[WeylChargeConjugationMatrix[dim, "Dotted" -> True]];
    
Options[WeylChargeConjugationMatrix] = {"Dotted" -> False, "Raised" -> False};    
WeylChargeConjugationMatrix[dim_, OptionsPattern[]] /; EvenQ[dim] :=
 Tensor[{{chargeconj[], 
    If[OptionValue["Raised"], Raised, Lowered][If[OptionValue["Dotted"], DottedWeylSpinor, WeylSpinor][dim]], 
    If[OptionValue["Raised"], Raised, Lowered][If[OptionValue["Dotted"], DottedWeylSpinor, WeylSpinor][dim]]}}];

BuildTensor[{"\[Gamma]", Lowered[Spacetime[dim_]], 
    Lowered[DiracSpinor[dim_]], Raised[DiracSpinor[dim_]]}] := 
  SparseArray@If[$signatureFactor == I, weylGammas[1, dim - 1], weylGammas[dim]];
GammaTensor[dim_] := 
 Tensor[{{"\[Gamma]", Lowered[Spacetime[dim]], 
    Lowered[DiracSpinor[dim]], Raised[DiracSpinor[dim]]}}]

BuildTensor[{"\!\(\*SubscriptBox[\(\[Gamma]\), \(*\)]\)", 
    Lowered[DiracSpinor[dim_]], Raised[DiracSpinor[dim_]]}] := 
  SparseArray@If[$signatureFactor == I, weylGammaStar[1, dim - 1], 
   weylGammaStar[dim]];
ChiralGamma[dim_] := 
  Tensor[{{"\!\(\*SubscriptBox[\(\[Gamma]\), \(*\)]\)", 
     Lowered[DiracSpinor[dim]], Raised[DiracSpinor[dim]]}}];

\[Delta]mat[dim_] := 
  If[$signatureFactor == I, weylGammas[1, dim - 1][[1]], 
   IdentityMatrix[2^Floor[dim/2]]];
Dmat[dim_] := $signatureFactor^2 KroneckerProduct[-I PauliMatrix[2], 
    IdentityMatrix[2^Floor[dim/2]]];

BuildTensor[{"\[CapitalGamma]", Lowered[EmbeddingSpacetime[dim_]], 
    Lowered[EmbeddingDiracSpinor[dim_]], 
    Raised[EmbeddingDiracSpinor[dim_]]}] := 
  SparseArray@Join[{ArrayFlatten[{{0, 
       Inverse[\[Delta]mat[dim]]}, {\[Delta]mat[dim], 0}}], 
    ArrayFlatten[{{0, Inverse[\[Delta]mat[dim]]}, {-\[Delta]mat[dim], 
       0}}]},
   ArrayFlatten[{{#, 
        0}, {0, -\[Delta]mat[dim] . # . 
          Inverse[\[Delta]mat[dim]]}}] & /@ 
    Components[GammaTensor[dim]]];
EmbeddingGammaTensor[dim_] := 
 Tensor[{{"\[CapitalGamma]", Lowered[EmbeddingSpacetime[dim]], 
    Lowered[EmbeddingDiracSpinor[dim]], 
    Raised[EmbeddingDiracSpinor[dim]]}}]
    
Options[SigmaTensor] = {"Bar" -> False};
SigmaTensor[dim_, OptionsPattern[]] /; EvenQ[dim] := 
  Tensor[{{"\[Sigma]", Lowered[Spacetime[dim]], 
     Sequence @@ Switch[{Mod[dim, 4], OptionValue["Bar"]},
       {0, False},
       {Lowered[WeylSpinor[dim]], Lowered[DottedWeylSpinor[dim]]},
       {0, True},
       {Raised[DottedWeylSpinor[dim]], Raised[WeylSpinor[dim]]},
       {2, False},
       {Lowered[WeylSpinor[dim]], Lowered[WeylSpinor[dim]]},
       {2, True},
       {Lowered[DottedWeylSpinor[dim]], Lowered[DottedWeylSpinor[dim]]}
       ]}}];
BuildTensor[{"\[Sigma]", Lowered[Spacetime[dim_]], 
     Lowered[WeylSpinor[dim_]], Lowered[DottedWeylSpinor[dim_]]}] /; 
  Mod[dim, 4] == 0 := 
 SparseArray@TensorTranspose[
  Components@
   Contract[
    TensorProduct[WeylProjection[dim], GammaTensor[dim], 
     WeylProjection[dim, "Transpose" -> True, "Dotted" -> True]], {{2,
       4}, {5, 6}}], {2, 1, 3}]
BuildTensor[{"\[Sigma]", Lowered[Spacetime[dim_]], 
     Raised[DottedWeylSpinor[dim_]], Raised[WeylSpinor[dim_]]}] /; 
  Mod[dim, 4] == 2 := 
 SparseArray@TensorTranspose[
  Components@
   Contract[
    TensorProduct[WeylProjection[dim], GammaTensor[dim], 
     WeylProjection[dim, "Transpose" -> True]], {{2, 4}, {5, 6}}], {2,
    1, 3}]


Options[point] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
BuildTensor[{point[i__Integer, opt : OptionsPattern[]], Raised[Spacetime[dim_]]}] := Module[{full, q},
   full = Table[x[{i}[[1]], j] - If[Length[{i}] == 1, 0, x[{i}[[2]], j]], {j, dim}];
   If[OptionValue[point, {opt}, "DefectCodimension"] =!= None,
      q = OptionValue[point, {opt}, "DefectCodimension"];
      If[OptionValue[point, {opt}, "Defect"],     full = ReplacePart[full, Table[{j} -> 0, {j, dim - q + 1, dim}]]];
      If[OptionValue[point, {opt}, "Transverse"], full = ReplacePart[full, Table[{j} -> 0, {j, dim - q}]]];
   ];
   SparseArray[full]
];

Options[Coordinate] = {"Defect" -> False, "Transverse" -> False};
Coordinate[dim_, i__Integer, opt : OptionsPattern[]] := Tensor[{{point[i, opt], Raised[Spacetime[dim]]}}];

CoordinateSquared[dim_, i_, j_] /; i > j := CoordinateSquared[dim, j, i];
AddExplicitRule[CoordinateSquared[dim_, i__Integer, opt : OptionsPattern[]] :> Components@Contract[TensorProduct[MetricTensor[dim, opt], Coordinate[dim, i], Coordinate[dim, i]], {{1, 3}, {2, 4}}]];
AddExplicitRule[InnerProduct[dim_, i_, j_, opt : OptionsPattern[]] :> Components@Contract[TensorProduct[MetricTensor[dim, opt], Coordinate[dim, i], Coordinate[dim, j]], {{1, 3}, {2, 4}}]];
      
AddExplicitRule[u[dim_, {ii_, jj_, kk_, ll_}] :> (CoordinateSquared[dim, ii, jj] CoordinateSquared[dim, kk, ll])/(CoordinateSquared[dim, ii, kk] CoordinateSquared[dim, jj, ll])];
AddExplicitRule[v[dim_, {ii_, jj_, kk_, ll_}] :> (CoordinateSquared[dim, ii, ll] CoordinateSquared[dim, jj, kk])/(CoordinateSquared[dim, ii, kk] CoordinateSquared[dim, jj, ll])];

AddExplicitRule[u[dim_, {ii_, jj_}, opt : OptionsPattern[]] :> CoordinateSquared[dim, ii, jj]/(4 Sqrt[CoordinateSquared[dim, ii, "Transverse" -> True, opt] CoordinateSquared[dim, jj, "Transverse" -> True, opt]])]; 
AddExplicitRule[v[dim_, {ii_, jj_}, opt : OptionsPattern[]] :> InnerProduct[dim, ii, jj, "Transverse" -> True, opt]/(Sqrt[CoordinateSquared[dim, ii, "Transverse" -> True, opt] CoordinateSquared[dim, jj, "Transverse" -> True, opt]])];


Options[CoordinateSlash] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
CoordinateSlash[dim_, i_, opt : OptionsPattern[]] := Contract[TensorProduct[Coordinate[dim, i, opt], GammaTensor[dim]], {{1, 2}}]

BuildTensor[{point[i__Integer, opt : OptionsPattern[]], Raised[EmbeddingSpacetime[dim_]]}] := Module[{full, q},
   full = Join[{-((-1 + Explicit[CoordinateSquared[dim, i]])/2), (-1 - Explicit[CoordinateSquared[dim, i]])/2}, Components[Coordinate[dim, i]]];
   If[OptionValue[point, {opt}, "DefectCodimension"] =!= None,
      q = OptionValue[point, {opt}, "DefectCodimension"];
      If[OptionValue[point, {opt}, "Defect"],     full = ReplacePart[full, Table[{j} -> 0, {j, dim + 2 - q + 1, dim + 2}]]];
      If[OptionValue[point, {opt}, "Transverse"], full = ReplacePart[full, Table[{j} -> 0, {j, dim + 2 - q}]]];
   ];
   SparseArray[full]
];

Options[EmbeddingCoordinate] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
EmbeddingCoordinate[dim_, i__Integer, opt : OptionsPattern[]] := Tensor[{{point[i, opt], Raised[EmbeddingSpacetime[dim]]}}];

Options[EmbeddingCoordinateSlash] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
EmbeddingCoordinateSlash[dim_, i__Integer, opt : OptionsPattern[]] := Contract[TensorProduct[EmbeddingCoordinate[dim, i, opt], EmbeddingGammaTensor[dim]], {{1, 2}}]


BuildTensor[{OverBar[spinor[i_]], Lowered[DiracSpinor[dim_]], Raised[EmbeddingDiracSpinor[dim_]]}] := SparseArray@ArrayFlatten[{{IdentityMatrix[2^Floor[dim/2]], -Components[CoordinateSlash[dim, i]] . \[Delta]mat[dim]}}];
BuildTensor[{spinor[i_], Lowered[EmbeddingDiracSpinor[dim_]], Raised[DiracSpinor[dim_]]}] := SparseArray@ArrayFlatten[{{Components[CoordinateSlash[dim, i]]}, {\[Delta]mat[dim]}}];

Options[EmbeddingPolarizationSpinor] = {"Bar" -> False};
EmbeddingPolarizationSpinor[dim_, i_, OptionsPattern[]] := If[OptionValue["Bar"], 
   Tensor[{{OverBar[spinor[i]], Lowered[DiracSpinor[dim]], Raised[EmbeddingDiracSpinor[dim]]}}], 
   Tensor[{{spinor[i], Lowered[EmbeddingDiracSpinor[dim]], Raised[DiracSpinor[dim]]}}]
];

projPlus[dim_] := 
  SparseArray[UnitVector[2^Floor[dim/2], #] & /@ Range[2^(Floor[dim/2] - 1)]];
projMinus[dim_] := 
  SparseArray[UnitVector[2^Floor[dim/2], 2^(Floor[dim/2] - 1) + #] & /@ Range[2^(Floor[dim/2] - 1)]];

(*a lowered Dirac index is (lowered Weyl, raised dotted Weyl) in D=4n, \
(lowered Weyl, lowered dotted Weyl) in D=4n+2*)
BuildTensor[{"P", Lowered[WeylSpinor[dim_]], Raised[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 0 := projPlus[dim];
BuildTensor[{"P", Raised[DottedWeylSpinor[dim_]], Raised[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 0 := projMinus[dim];
BuildTensor[{"P", Lowered[WeylSpinor[dim_]], Raised[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 2 := projPlus[dim];
BuildTensor[{"P", Lowered[DottedWeylSpinor[dim_]], Raised[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 2 := projMinus[dim];
BuildTensor[{"P", Raised[DiracSpinor[dim_]], Lowered[WeylSpinor[dim_]]}] /; Mod[dim, 4] == 0 := Transpose[projPlus[dim]];
BuildTensor[{"P", Raised[DiracSpinor[dim_]], Raised[DottedWeylSpinor[dim_]]}] /; Mod[dim, 4] == 0 := Transpose[projMinus[dim]];
BuildTensor[{"P", Raised[DiracSpinor[dim_]], Lowered[WeylSpinor[dim_]]}] /; Mod[dim, 4] == 2 := Transpose[projPlus[dim]];
BuildTensor[{"P", Raised[DiracSpinor[dim_]], Lowered[DottedWeylSpinor[dim_]]}] /; Mod[dim, 4] == 2 := Transpose[projMinus[dim]];

(*a raised Dirac index is (raised Weyl, lowered dotted Weyl) in D=4n, \
and (raised Weyl, raised dotted Weyl) in D=4n+2*)
BuildTensor[{"P", Raised[WeylSpinor[dim_]], Lowered[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 0 := projPlus[dim];
BuildTensor[{"P", Lowered[DottedWeylSpinor[dim_]], Lowered[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 0 := projMinus[dim];
BuildTensor[{"P", Raised[WeylSpinor[dim_]], Lowered[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 2 := projPlus[dim];
BuildTensor[{"P", Raised[DottedWeylSpinor[dim_]], Lowered[DiracSpinor[dim_]]}] /; Mod[dim, 4] == 2 := projMinus[dim];
BuildTensor[{"P", Lowered[DiracSpinor[dim_]], Raised[WeylSpinor[dim_]]}] /; Mod[dim, 4] == 0 := Transpose[projPlus[dim]];
BuildTensor[{"P", Lowered[DiracSpinor[dim_]], Lowered[DottedWeylSpinor[dim_]]}] /; Mod[dim, 4] == 0 := Transpose[projMinus[dim]];
BuildTensor[{"P", Lowered[DiracSpinor[dim_]], Raised[WeylSpinor[dim_]]}] /; Mod[dim, 4] == 2 := Transpose[projPlus[dim]];
BuildTensor[{"P", Lowered[DiracSpinor[dim_]], Raised[DottedWeylSpinor[dim_]]}] /; Mod[dim, 4] == 2 := Transpose[projMinus[dim]];

Clear[WeylProjection];
Options[WeylProjection] = {"Dotted" -> False, "Transpose" -> False};
WeylProjection[dim_, OptionsPattern[]] /; Mod[dim, 2] == 0 := 
 Switch[{Mod[dim, 4], OptionValue["Dotted"], OptionValue["Transpose"]},
  {0, False, False},
  Tensor[{{"P", Lowered[WeylSpinor[dim]], Raised[DiracSpinor[dim]]}}],
  {0, True, False},
  Contract[
   TensorProduct[
    Tensor[{{"P", Lowered[DottedWeylSpinor[dim]], 
       Lowered[DiracSpinor[dim]]}}], 
    ChargeConjugationMatrix[dim, "Raised" -> True]], {{2, 3}}],
  {0, False, True},
  Contract[
   TensorProduct[ChargeConjugationMatrix[dim], 
    Tensor[{{"P", Raised[DiracSpinor[dim]], 
       Lowered[WeylSpinor[dim]]}}]], {{2, 3}}],
  {0, True, True},
  Tensor[{{"P", Lowered[DiracSpinor[dim]], 
     Lowered[DottedWeylSpinor[dim]]}}],
  {2, False, False},
  Tensor[{{"P", Lowered[WeylSpinor[dim]], Raised[DiracSpinor[dim]]}}],
  {2, True, False},
  Tensor[{{"P", Lowered[DottedWeylSpinor[dim]], 
     Raised[DiracSpinor[dim]]}}],
  {2, False, True},
  Contract[
   TensorProduct[ChargeConjugationMatrix[dim], 
    Tensor[{{"P", Raised[DiracSpinor[dim]], 
       Lowered[WeylSpinor[dim]]}}]], {{2, 3}}],
  {2, True, True},
  Contract[
   TensorProduct[ChargeConjugationMatrix[dim], 
    Tensor[{{"P", Raised[DiracSpinor[dim]], 
       Lowered[DottedWeylSpinor[dim]]}}]], {{2, 3}}]
  ]

BuildTensor[{"M", Lowered[Spacetime[dim_]], Lowered[Spacetime[dim_]], 
    Lowered[DiracSpinor[dim_]], Raised[DiracSpinor[dim_]]}] := 
  TensorTranspose[#, {1, 3, 2, 4}] - 
     TensorTranspose[#, {2, 3, 1, 4}] &@(1/
     4 Components@
      Contract[
       TensorProduct[GammaTensor[dim], GammaTensor[dim]], {{3, 5}}]);
       
BuildTensor[{"M", Lowered[Spacetime[dim_]], Lowered[Spacetime[dim_]], Lowered[WeylSpinor[dim_]], Raised[WeylSpinor[dim_]]}] := TensorTranspose[Components[
   Contract[TensorProduct[WeylProjection[dim], RotationGenerators[dim], WeylProjection[dim, "Transpose" -> True], WeylChargeConjugationMatrix[4, "Raised" -> True]], {{2, 5}, {6, 7}, {8, 10}}]
], {3, 1, 2, 4}];

BuildTensor[{"M", Lowered[Spacetime[dim_]], Lowered[Spacetime[dim_]], Lowered[DottedWeylSpinor[dim_]], Raised[DottedWeylSpinor[dim_]]}] := TensorTranspose[Components[
   Contract[TensorProduct[WeylProjection[dim, "Dotted" -> True], RotationGenerators[dim], WeylProjection[dim, "Dotted" -> True, "Transpose" -> True], WeylChargeConjugationMatrix[4, "Raised" -> True, "Dotted" -> True]], {{2, 5}, {6, 7}, {8, 10}}]
], {3, 1, 2, 4}];

Options[RotationGenerators] = {"Weyl" -> False, "Dotted" -> False};
RotationGenerators[dim_, OptionsPattern[]] := Which[OddQ[dim] || !OptionValue["Weyl"],
   Tensor[{{"M", Lowered[Spacetime[dim]], Lowered[Spacetime[dim]], Lowered[DiracSpinor[dim]], Raised[DiracSpinor[dim]]}}],
   OptionValue["Dotted"],
   Tensor[{{"M", Lowered[Spacetime[dim]], Lowered[Spacetime[dim]], Lowered[DottedWeylSpinor[dim]], Raised[DottedWeylSpinor[dim]]}}],
   True,
   Tensor[{{"M", Lowered[Spacetime[dim]], Lowered[Spacetime[dim]], Lowered[WeylSpinor[dim]], Raised[WeylSpinor[dim]]}}]
];

Options[eps] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
BuildTensor[{eps[opt : OptionsPattern[]], inds___}] /; !FreeQ[{inds}, Spacetime] := Module[{dim, q},
  dim = First@Cases[{inds}, Spacetime[dim_] :> dim, All];
  If[OptionValue[eps, {opt}, "DefectCodimension"] =!= None,
     q = OptionValue[eps, {opt}, "DefectCodimension"];
     Which[
        OptionValue[eps, {opt}, "Defect"], SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim - q]}], Table[dim, dim - q]],
        OptionValue[eps, {opt}, "Transverse"], SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim - q + 1, dim]}], Table[dim, q]],
        True, SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim]}], Table[dim, dim]]
     ],
     SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim]}], Table[dim, dim]]
  ]
];
BuildTensor[{eps[opt : OptionsPattern[]], inds___}] /; !FreeQ[{inds}, EmbeddingSpacetime] := Module[{dim, q},
  dim = First@Cases[{inds}, EmbeddingSpacetime[dim_] :> dim, All];
  If[OptionValue[eps, {opt}, "DefectCodimension"] =!= None,
     q = OptionValue[eps, {opt}, "DefectCodimension"];
     Which[
        OptionValue[eps, {opt}, "Defect"], SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim + 2 - q]}], Table[dim + 2, dim + 2 - q]],
        OptionValue[eps, {opt}, "Transverse"], SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim + 2 - q + 1, dim + 2]}], Table[dim + 2, q]],
        True, SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim]}], Table[dim + 2, dim + 2]]
     ],
     SparseArray[Table[perm -> Signature[perm], {perm, Permutations@Range[dim + 2]}], Table[dim + 2, dim + 2]]
  ]
];

Options[LeviCivita] = {"Raised" -> False, "DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
LeviCivita[dim_, opt : OptionsPattern[]] := Module[{q, rl, num},
   q = If[OptionValue["DefectCodimension"] === None, 0, OptionValue["DefectCodimension"]];
   rl = If[OptionValue["Raised"], Raised, Lowered];
   num = Which[OptionValue["Defect"], dim - q, OptionValue["Transverse"] && q > 0, q, True, dim];
   Tensor[{{eps[Sequence @@ FilterRules[{opt}, Options[eps]]], Sequence @@ Table[rl[Spacetime[dim]], num]}}]
]; 

Options[EmbeddingLeviCivita] = {"Raised" -> False, "DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False};
EmbeddingLeviCivita[dim_, opt : OptionsPattern[]] := Module[{q, rl, num},
   q = If[OptionValue["DefectCodimension"] === None, 0, OptionValue["DefectCodimension"]];
   rl = If[OptionValue["Raised"], Raised, Lowered];
   num = Which[OptionValue["Defect"], dim + 2 - q, OptionValue["Transverse"] && q > 0, q, True, dim];
   Tensor[{{eps[Sequence @@ FilterRules[{opt}, Options[eps]]], Sequence @@ Table[rl[EmbeddingSpacetime[dim]], num]}}]
]; 

BuildTensor[{y["DefectCodimension" -> q_], Lowered[EmbeddingDiracSpinor[dim_]], Raised[EmbeddingDiracSpinor[dim_]]}] := Module[{epsilon, num},
   epsilon = EmbeddingLeviCivita[dim, "Raised" -> True, "DefectCodimension" -> q, "Transverse" -> True];
   num = Length@Indices[epsilon];
   (1/num!) Components@Contract[TensorProduct[epsilon, Sequence @@ Table[EmbeddingGammaTensor[dim], num]], Join[Table[{i, num + 3(i - 1) + 1}, {i, num}], Table[{num + 3i, num + 3i + 2}, {i, num - 1}]]]
]

Options[YTensor] = {"DefectCodimension" -> None};
YTensor[dim_, opt : OptionsPattern[]] := Tensor[{{y["DefectCodimension" -> OptionValue["DefectCodimension"]], Lowered[EmbeddingDiracSpinor[dim]], Raised[EmbeddingDiracSpinor[dim]]}}];

Options[DiracStringStructure] = {"DefectCodimension" -> None};
DiracStringStructure[dim_, is_, OptionsPattern[]] := Block[{S, Sbar, X, js, XX, prefactor},
  S[i_] := EmbeddingPolarizationSpinor[dim, i];
  Sbar[i_] := EmbeddingPolarizationSpinor[dim, i, "Bar" -> True];
  
  X[0] := YTensor[dim, "DefectCodimension" -> OptionValue["DefectCodimension"]];
  X[i_Integer] := EmbeddingCoordinateSlash[dim, i];
  X[{i_, "Defect"}] :=EmbeddingCoordinateSlash[dim, i, "DefectCodimension" -> OptionValue["DefectCodimension"], "Defect" -> True];
  X[{i_, "Transverse"}] :=EmbeddingCoordinateSlash[dim, i, "DefectCodimension" -> OptionValue["DefectCodimension"], "Transverse" -> True];
  
  js = DeleteCases[is /. {j_, _String} :> j, 0];
  
  XX[i_, j_] := If[OptionValue["DefectCodimension"] =!= None, 
        InnerProduct[dim, i, j, "DefectCodimension" -> OptionValue["DefectCodimension"], "Transverse" -> True], 
        CoordinateSquared[dim, i, j]
  ];
  
  prefactor = If[Length[is] == 3, Sqrt[XX[js[[1]], js[[-1]]]/(XX[js[[1]], js[[2]]] XX[js[[2]], js[[3]]])], 1/Sqrt[Product[XX[js[[i]], js[[Mod[i - 1, Length[js] - 2] + 2]]], {i, 2, Length[js] - 1}]]];
  
  prefactor Contract[
   TensorProduct @@ 
    Join[{Sbar[is[[1]]]}, X /@ is[[2 ;; -2]], {S[is[[-1]]]}], 
   Table[{2, 3} + 2 (n - 1), {n, Length[is] - 1}]]
	]

Options[ConformalTest] = {"DefectCodimension" -> None};
ConformalTest[dim_, is_, opt : OptionsPattern[]] := Module[{struct},
  struct = 
   Tensor[{{"S", Lowered[DiracSpinor[dim]], Raised[DiracSpinor[dim]]}}];
  TensorInterpret[struct[1, 2] = DiracStringStructure[dim, is, opt][1, 2]];
  
  Sum[2 Contract[
        TensorProduct[MetricTensor[dim], Coordinate[dim, i], 
         Coordinate[dim, i], 
         TensorDerivative[struct, dim, i]], {{2, 3}, {4, 5}}] - 
      CoordinateSquared[dim, i] TensorDerivative[struct, dim, i] + 
      2 \[CapitalDelta][i] Contract[
        TensorProduct[MetricTensor[dim], Coordinate[dim, i], 
         struct], {{2, 3}}], {i, DeleteCases[DeleteDuplicates[is /. {j_, _String} :> j], 0]}] + 
    Sum[2 i Contract[
        TensorProduct[Coordinate[dim, is[[i]]], 
         RotationGenerators[dim], 
         struct], {{1, 3}, {If[i == 1, 5, 4], If[i == 1, 6, 7]}}], {i, {1, -1}}] /. \[CapitalDelta][i_] :> 0
  ]

Options[StringStructure] = {"DefectCodimension" -> None};
StringStructure[dim_, is_, signs_ : {1, 1}, opt : OptionsPattern[]] := 
 StringStructure[dim, is, signs, opt] = Module[{struct, tensor},
   struct = If[OddQ[dim],
     TensorInterpret[
      DiracStringStructure[dim, is, opt][
        1, -1] ChargeConjugationMatrix[dim][-1, 2]], 
     Module[{s = DiracStringStructure[dim, is, opt], 
       p1 = WeylProjection[dim, "Dotted" -> signs[[1]] == -1], 
       p2 = WeylProjection[dim, "Dotted" -> signs[[2]] == -1, 
         "Transpose" -> True]},
      TensorInterpret[p1[1, -1] s[-1, -2] p2[-2, 2]]
      ]
     ];
   tensor = {stringstruct[dim, is], 
     Lowered[Which[OddQ[dim], DiracSpinor[dim], signs[[1]] == 1, 
       WeylSpinor[dim], signs[[1]] == -1, DottedWeylSpinor[dim]]], 
     Lowered[Which[OddQ[dim], DiracSpinor[dim], signs[[2]] == 1, 
       WeylSpinor[dim], signs[[2]] == -1, DottedWeylSpinor[dim]]]};
   BuildTensor[tensor] = Components[struct];
   Tensor[{tensor}]
   ]