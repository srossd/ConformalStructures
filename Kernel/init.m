(* Wolfram Language Init File *)

BeginPackage["ConformalStructures`", {"TensorTools`"}]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

$consoleMode = ($FrontEnd === Null);

Get[ "ConformalStructures`Usage`"]
Get[ "ConformalStructures`Correlators`"]
Get[ "ConformalStructures`EmbeddingFormalism`"]
Get[ "ConformalStructures`fastEval`"]
Get[ "ConformalStructures`Formatting`"]
Get[ "ConformalStructures`Relations`"]

End[]

EndPackage[]