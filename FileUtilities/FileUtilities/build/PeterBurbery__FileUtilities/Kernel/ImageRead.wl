BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

ImageRead

Begin["`Private`"]

(* Define your public and private symbols here. *)
Clear[ImageRead];
ImageRead[type_, File[file_String]] := ImageRead[type, file];
ImageRead[type : ("JPEG" | "PNG" | "TIFF"), file_String] := 
  Module[{function, image},
   function = Switch[type,
     "JPEG" | "JPG", Image`ImportExportDump`ImageReadJPEG,
     "PNG", Image`ImportExportDump`ImageReadPNG,
     "TIFF", Image`ImportExportDump`ImageReadTIFF];
   image = First[function[file]];
   If[ImageQ[image], image, $Failed]
   ];
ImageRead[___] := $Failed
End[] (* End `Private` *)

EndPackage[]