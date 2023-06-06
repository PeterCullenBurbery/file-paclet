BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

ReadPNG

Begin["`Private`"]

(* Define your public and private symbols here. *)
Clear[ReadPNG];
ReadPNG[File[file_String]] := ReadPNG[file];
ReadPNG[file_String] := Module[{image},
   image = First[Image`ImportExportDump`ImageReadPNG[file]];
   If[ImageQ[image], image, $Failed]
   ];
ReadPNG[___] := $Failed
End[] (* End `Private` *)

EndPackage[]