(* Wolfram Language package *)
     
Format[x[i_, j_], TraditionalForm] := Subsuperscript[x, i, j];
MakeBoxes[Power[x[i_,j_], n_], TraditionalForm] := SuperscriptBox[RowBox[{"(", SubsuperscriptBox["x",i,j], ")"}], ToBoxes[n, TraditionalForm]]

Options[symb] = {"DefectCodimension" -> None, "Defect" -> False, "Transverse" -> False, "Bar" -> False};
symb[base_, OptionsPattern[]] := Which[OptionValue["Bar"], OverBar[base], OptionValue["Defect"], Superscript[base, "\[DoubleVerticalBar]"], OptionValue["Transverse"], Superscript[base, "\[UpTee]"], True, base];

Format[chargeconj[], TraditionalForm] := "\[GothicCapitalC]";
Format[metric[opt : OptionsPattern[]], TraditionalForm] := symb["\[Eta]", opt];
Format[eps[opt : OptionsPattern[]], TraditionalForm] := symb["\[Epsilon]", opt];
Format[sigma[opt : OptionsPattern[]], TraditionalForm] := symb["\[Sigma]", opt];
Format[y[opt : OptionsPattern[]], TraditionalForm] := symb["Y", opt];
Format[point[i__, opt : OptionsPattern[]], TraditionalForm] := Subscript[symb["x", opt], i];
Format[basis[opt : OptionsPattern[]], TraditionalForm] := symb["e", opt];

Format[CoordinateSquared[dim_, i__Integer, opt : OptionsPattern[]], TraditionalForm] := Superscript[Abs[Subscript[symb["x", opt], i]], 2];
Format[InnerProduct[dim_, i_, j_, opt : OptionsPattern[]], TraditionalForm] := Row[{"(",Subscript[symb["x", opt], i], "\[CenterDot]", Subscript[symb["x", opt], j], ")"}];

Unprotect[Power];
Format[Power[CoordinateSquared[dim_, i__Integer, opt : OptionsPattern[]], n_], TraditionalForm] := If[2 n == 1, Abs[Subscript[symb["x", opt], i]], Superscript[Abs[Subscript[symb["x", opt], i]], 2 n]];
Protect[Power];

Format[spinor[i_], TraditionalForm] := Subscript["S", i];

Format[stringstruct[dim_, is_, q_], TraditionalForm] := 
  Subsuperscript[Subscript["\[ScriptCapitalS]", ToString[dim] <> "D" <> If[q === None,"",",q = "<>ToString[q]<>";"]],
    is[[2 ;; -2]] /. {{i_, "Defect"} -> Superscript[i, "\[DoubleVerticalBar]"], {i_, "Transverse"} -> Superscript[i, "\[UpTee]"], 0 -> "Y"}, is[[{1, -1}]]];
    
Format[correlator[dim_, \[CapitalDelta]s_, spins_, derivs_, perm_, q_, i_], TraditionalForm] := Module[{perm2},
   perm2 = If[perm === Automatic, Range@Length[\[CapitalDelta]s], perm];
   Subsuperscript[
    Row[Append[
      Table[If[d[[1]] == "\[PartialD]", 
        Superscript["\[PartialD]", 
         "(" <> ToString[perm2[[d[[2]]]]] <> ")"], Row[{Superscript["\[PartialD]",
            "(" <> ToString[perm2[[d[[2]]]]] <> ")"], "(", 
          ToExpression[d[[1]]][dim, perm2], ")"}]], {d, derivs}], 
      "\[ScriptCapitalS]"]],
    Row[{ToString[dim] <> "D", ";", perm2, ";", Sequence @@ If[q =!= None, {"q = ", q, ";"}, {}],  i}], 
    Row[Subscript @@@ Riffle[Thread[{spins, \[CapitalDelta]s}], ";"]]
	]
 ];
   
uvpowers[dim_, i_, perm_] /; dim > 2 := 
 uvpowers[dim, i, perm] = 
  With[{p1 = {1, -1, 0, 0, -1, 1}, p2 = {0, -1, 1, 1, -1, 0}, 
    q = (CoordinateSquared[dim, ##] D[If[i == 1, u, v][dim, perm] /. TensorTools`Private`explicitRules, 
         CoordinateSquared[dim, ##]])/(If[i == 1, u, v][dim, perm] /. TensorTools`Private`explicitRules) & @@@
       Subsets[Range[4], {2}]},
   SolveValues[
     Thread[Flatten[q - \[Alpha]1 p1 - \[Alpha]2 p2] == 
       0], {\[Alpha]1, \[Alpha]2}][[1]]
   ]

sp[perm_] := First@Sort[{perm, perm[[{2, 1, 4, 3}]], perm[[{3, 4, 1, 2}]], perm[[{4, 3, 2, 1}]]}];
zcross = {
	z[dim_, perm_] :> <| {1,2,3,4} -> z, {1,2,4,3} -> z/(z-1), {1,3,2,4} -> 1/z, {1,3,4,2} -> 1/(1-z), {1,4,2,3} -> (z-1)/z, {1,4,3,2} -> 1-z|>[sp[perm]], 
	zb[dim_, perm_] :> <| {1,2,3,4} -> z, {1,2,4,3} -> z/(z-1), {1,3,2,4} -> 1/z, {1,3,4,2} -> 1/(1-z), {1,4,2,3} -> (z-1)/z, {1,4,3,2} -> 1-z|>[sp[perm]] /. z-> zb
};

Format[expr : z[dim_, perm : {_,_,_,_}], TraditionalForm] := (expr /. zcross /. {z -> "Z", zb -> "\!\(\*OverscriptBox[\(Z\), \(_\)]\)"});
Format[expr : zb[dim_, perm : {_,_,_,_}], TraditionalForm] := (expr /. zcross /. {z -> "Z", zb -> "\!\(\*OverscriptBox[\(Z\), \(_\)]\)"});
     
Format[u[dim_, perm : {_,_,_,_}], TraditionalForm] := "U"^#1 "V"^#2 & @@ uvpowers[dim, 1, perm];
Format[v[dim_, perm : {_,_,_,_}], TraditionalForm] := "U"^#1 "V"^#2 & @@ uvpowers[dim, 2, perm];

Format[u[dim_, perm : {_,_}], TraditionalForm] := "U";
Format[v[dim_, perm : {_,_}], TraditionalForm] := "V";