(* ::Package:: *)

(* Wolfram Language package *)

BeginPackage["ConformalStructures`"]
(* Exported symbols added here with SymbolName::usage *)


x::usage = "x[\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)is the \*
StyleBox[\(\!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)th\)] component of the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] coordinate";
point::usage = "point[\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] denotes the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th coordinate";
metric::usage = "metric[] denotes the metric tensor";
chargeconj::usage = "chargeconj[] denotes the charge conjugation matrix";
eps::usage = "eps[] denotes the Levi-Civita tensor";
y::usage = "y[] denotes the Y tensor";
spinor::usage = "spinor[\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] denotes the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th polarization spinor";
stringstruct::usage = "stringstruct[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)] denotes a structure of the form SX...XS with indices given by \!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)";
correlator::usage = "correlator[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"derivs\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] denotes the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th correlator of operators in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions with scaling dimensions \!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)spins \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)and so on.";

u::usage = "u[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)] gives the cross-ratio \!\(\*
StyleBox[\"u\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions with points in the order \!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)";
v::usage = "v[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)] gives the cross-ratio \!\(\*
StyleBox[\"v\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions with points in the order \!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)";

TensorDerivative::usage = "TensorDerivative[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\)] is the derivative of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with respect to the spacetime point with index \!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\).";
TensorSpinorDerivative::usage = "TensorDerivative[\!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\)] is the spinorial derivative of \!\(\*
StyleBox[\"tensor\",\nFontSlant->\"Italic\"]\) with respect to the spacetime point with index \!\(\*
StyleBox[\"idx\",\nFontSlant->\"Italic\"]\).";

IndependentSet::usage = "IndependentSet[\!\(\*
StyleBox[\"tensors\",\nFontSlant->\"Italic\"]\)] gives a maximal subset of \!\(\*
StyleBox[\"tensors\",\nFontSlant->\"Italic\"]\) that are linearly independent."

SetSignature::usage = "SetSignature[\!\(\*
StyleBox[\"sig\",\nFontSlant->\"Italic\"]\)] sets the signature to \!\(\*
StyleBox[\"sig\",\nFontSlant->\"Italic\"]\), where \!\(\*
StyleBox[\"sig\",\nFontSlant->\"Italic\"]\) is either \"Lorentzian\" or \"Euclidean\".";

Spacetime::usage = "Spacetime[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a spacetime index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
DiracSpinor::usage = "DiracSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a Dirac spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
WeylSpinor::usage = "WeylSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a Weyl spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions, with \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) even";
DottedWeylSpinor::usage = "DottedWeylSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a dotted Weyl spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions, with \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) even";
EmbeddingSpacetime::usage = "EmbeddingSpacetime[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a spacetime index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions";
EmbeddingDiracSpinor::usage = "EmbeddingDiracSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a Dirac spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions";
EmbeddingWeylSpinor::usage = "EmbeddingWeylSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a Weyl spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions, with \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) even";
EmbeddingDottedWeylSpinor::usage = "EmbeddingDottedWeylSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a dotted Weyl spinor index in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions, with \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) even";

MetricTensor::usage = "MetricTensor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the metric tensor in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
EmbeddingMetricTensor::usage = "EmbeddingMetricTensor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the metric tensor in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions";

ChargeConjugationMatrix::usage = "ChargeConjugationMatrix[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the charge conjugation matrix with Dirac spinor indices in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
WeylChargeConjugationMatrix::usage = "WeylChargeConjugationMatrix[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the charge conjugation matrix in even \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions with Weyl spinor indices";

GammaTensor::usage = "GammaTensor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the tensor of \[Gamma]-matrices in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
EmbeddingGammaTensor::usage = "EmbeddingGammaTensor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the tensor of \[CapitalGamma]-matrices in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions";
ChiralGamma::usage = "ChiralGamma[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives \!\(\*SubscriptBox[\(\[Gamma]\), \(*\)]\) in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";

Coordinate::usage = "Coordinate[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] coordinate in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
CoordinateSlash::usage = "CoordinateSlash[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th coordinate in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions contracted with the \[Gamma]-matrices";
CoordinateSquared::usage = "Coordinate[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the norm-squared of the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] coordinate in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";
InnerProduct::usage = "InnerProduct[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)] gives the inner product between coordinates \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions.";

LeviCivita::usage = "LeviCivita[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the Levi-Civita tensor in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";

EmbeddingCoordinate::usage = "EmbeddingCoordinate[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th coordinate in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions";
EmbeddingCoordinateSlash::usage = "EmbeddingCoordinateSlash[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)] gives the \!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th coordinate in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) + 2 dimensions contracted with the \[CapitalGamma]-matrices";
EmbeddingPolarizationSpinor::usage = "EmbeddingPolarizationSpinor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) gives the \*
StyleBox[\(\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)th\)] polarization spinor in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)+ 2 dimensions, with a Dirac spinor index";

EmbeddingLeviCivita::usage = "EmbeddingLeviCivita[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives the Levi-Civita tensor in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)+2 dimensions";

YTensor::usage = "YTensor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)gives the contraction of the defect-aligned Levi-Civita tensor with embedding-space gamma matrices";

WeylProjection::usage = "WeylProjection[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a projection matrix from a Dirac spinor index to a Weyl spinor index, when \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) is even";

RotationGenerators::usage = "RotationGenerators[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)] gives a tensor of the rotation generators in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions, with Dirac spinor indices";

DiracStringStructure::usage = "DiracStringStructure[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)] gives a building block structure of the form \!\(\*OverscriptBox[\(S\), \(_\)]\)X...XS, with mixed Dirac spinor indices, with coordinate labels given by \!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)";
StringStructure::usage = "DiracStringStructure[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)] gives a building block structure of the form \!\(\*OverscriptBox[\(S\), \(_\)]\)X...XS, with lowered indices (Dirac in odd dimensions, Weyl in even dimensions) with coordinate labels given by \!\(\*
StyleBox[\"is\",\nFontSlant->\"Italic\"]\)";

KinematicPrefactor::usage = "KinematicPrefactor[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)] gives the kinematic prefactor for a structure in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions with scaling dimensions \!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\) and spins \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)"

ConformalCorrelatorCount::usage = "ConformalCorrelatorCount[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)] gives the number of independent conformal correlators in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions on operators with spins \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)";
ConformalCorrelatorBuildingBlocks::usage = "ConformalCorrelatorBuildingBlocks[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"npts\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"{\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"i\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"signs\",\nFontSlant->\"Italic\"]\)] gives a list of all building block structurs in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions for a correlator with \!\(\*
StyleBox[\"npts\",\nFontSlant->\"Italic\"]\) pts of the form \!\(\*SubscriptBox[OverscriptBox[\(S\), \(_\)], \(i\)]\)X...\!\(\*SubscriptBox[\(XS\), \(j\)]\), with the types of Weyl spinor indices specified by \!\(\*
StyleBox[\"signs\",\nFontSlant->\"Italic\"]\) (not needed in odd dimensions)";
ConformalCorrelatorExpressions::usage = "ConformalCorrelatorExpressions[\!\(\*
StyleBox[\"dims\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)] gives a list of expressions for independent conformal correlators in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions for a correlator of operators with spins \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\), along with index permutations"
ConformalCorrelators::usage = "ConformalCorrelators[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\)] gives a list of conformal correlator tensors for operators with scaling dimensions \!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\) and spins \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\) dimensions";

StructureRelations::usage = "StructureRelations[\!\(\*
StyleBox[\"structs\",\nFontSlant->\"Italic\"]\)] gives a list of linear relations among \!\(\*
StyleBox[\"structs\",\nFontSlant->\"Italic\"]\), involving cross-ratios if the structures involve four points (or two points with a defect)"

ConformalTest::usage = "ConformalTest[\!\(\*
StyleBox[\"dim\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"\[CapitalDelta]s\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"spins\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"perm\",\nFontSlant->\"Italic\"]\)] constructs an expression for the action of a special conformal generator on the conformal correlators (which should vanish)"


EndPackage[]
