BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

BackupFile

Begin["`Private`"]

(* Define your public and private symbols here. *)

BackupFile // ClearAll;
BackupFile[nb_NotebookObject] := BackupFile@NotebookFileName@nb;
BackupFile[f_File] := BackupFile@First@f;
BackupFile[nb_ : None] := Module[{f, b, e, vn, fn},
  	f = If[nb === None, NotebookSave[]; NotebookFileName[], nb
    ];
  	If[! FileExistsQ@f, Return@$Failed];
      f = ExpandFileName[f];
      b = FileBaseName[f];
  	e = FileExtension[f];
  	vn = 0;
  	While[
   (*
   TODO: 
      - Add option for cloud things?
    - Add option for timestamp, min count, .bak's e.g. 
      ``` fn =FileNameJoin[{FileNameDrop[f],b<>"."<>e<>"."<>
   StringReplace[DateString[Now],{" "->"_",":"->"_"}]<>".bak"}];
          ```
   *)
   fn = FileNameJoin[{FileNameDrop[f], 
      b <> "-" <> ToString@vn <> "." <> e}];
   FileExistsQ[fn],
   	vn++;
   	];
  	
  CopyFile[f, fn]
  ]
End[] (* End `Private` *)

EndPackage[]