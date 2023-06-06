BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

FileNameEdit

Begin["`Private`"]

(* Define your public and private symbols here. *)

FileNameEdit // ClearAll;

FileNameEdit::"delx" = "FileNameEdit failed to delete file `1`.";
FileNameEdit::"copyx" = "FileNameEdit failed copying `1` to `2`.";

Options[FileNameEdit] = {
	"Prepend" -> "", "Append" -> "", "Edit" -> None, "NewDirectory" -> None, 
	"Action" -> False, OverwriteTarget -> False,
	"AddMissingExtension" -> False, "LowercaseExtension" -> False, "Lowercase" -> False
	(* TODO: add "CollisionFunction" \[Rule] Automatic *)
};

FileNameEdit[c_CloudObject, opts:OptionsPattern[]] := FileNameEdit[c[[1]], opts]
FileNameEdit[s_ /; MatchQ[s, _File|_String], opts:OptionsPattern[]] := Module[
	{ext, efun, prepend, append, name, performEdit, oldDir, newDir, newFileName}, 
	
	If[DirectoryQ[s], Return @ $Failed];
	
	{oldDir, name, ext} = Through[{DirectoryName, FileBaseName, FileExtension} @ s];
	
	{efun, prepend, append, newDir, performEdit} = OptionValue @ {"Edit", "Prepend", 
		"Append", "NewDirectory", "Action"};
	
	If[performEdit && !FileExistsQ[s], Return @ $Failed];
	
	If[newDir === None, newDir = oldDir,
		If[newDir === "Temp", 
			newDir = CreateDirectory[], (* create tmp dir *)
			If[StringQ @ newDir && ! DirectoryQ @ newDir,	
				Check[CreateDirectory[newDir], Return[$Failed, Module]] (* attempt to create new dir *)
			]
		]
	];
	
	If[ext == "" && OptionValue @ "AddMissingExtension" && FileExistsQ[s], 
		ext = FileFormat[s]
	];
	
	ext = If[OptionValue @ "Lowercase" || 
		OptionValue @ "LowercaseExtension", ToLowerCase, Identity] @ ext;
	
	If[efun =!= None, name = ToString @ efun[name]];
	If[prepend =!= None, name = ToString[prepend] <> name];
	If[append =!= None, name = name <> ToString[append]];
	
	If[ext =!= "", name = name <> "." <> ext];
	If[OptionValue @ "Lowercase", name = ToLowerCase @ name];
	
	If[newDir === "", 
		newFileName = name, (* recall FileNameJoin@{""} is "/" *)
		newFileName = FileNameJoin @ {newDir, name}
	];

	If[performEdit == False, Return[newFileName, Module]]; (* only return new name *)
	
	(* try to perform edit on disk *)
	If[performEdit === "Copy",
		Check[
			c = Echo@CopyFile[s, newFileName, OverwriteTarget -> OptionValue[OverwriteTarget]],
			Message[FileNameEdit::"copyx", newFileName]; Return[$Failed, Module]
		];
		Return[newFileName, Module]
	];
	
	If[performEdit === "Move",
		Check[
			c = CopyFile[s, newFileName, OverwriteTarget -> OptionValue[OverwriteTarget]],
			Message[FileNameEdit::"copyx", s, newFileName]; Return[$Failed, Module]
		];
		Check[
			If[s != newFileName, DeleteFile[s], Print @ "Deletion skipped!"],
			Message[FileNameEdit::"delx", s]; Return[$Failed, Module]
		];
		Return[newFileName, Module]
	];	
	
	Return[$Failed, Module]	
]
End[] (* End `Private` *)

EndPackage[]