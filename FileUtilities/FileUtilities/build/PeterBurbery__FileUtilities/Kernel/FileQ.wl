BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

FileQ

Begin["`Private`"]

(* Define your public and private symbols here. *)

FileQ[args___] := {Needs["GeneralUtilities`"]; 
  GeneralUtilities`FileQ[args]}
End[] (* End `Private` *)

EndPackage[]