BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

DirectorySize

Begin["`Private`"]

(* Define your public and private symbols here. *)

DirectorySize[] := DirectorySize[Directory[]];
DirectorySize[dir_ /; DirectoryQ[dir]] := 
 Quantity[Internal`DirectoryByteCount[dir], "Bytes"]
End[] (* End `Private` *)

EndPackage[]