BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

FileTreePicker

Begin["`Private`"]

(* Define your public and private symbols here. *)

ClearAll[FileTreePicker, viewer, fileToggler];

Options[FileTreePicker] = {
	FileNameForms -> All,
	MaxItems -> Infinity
};

FileTreePicker[files_, opts : OptionsPattern[]] := FileTreePicker[files, SystemDialogInput["Directory"], opts];

FileTreePicker[files_, dirName_, opts : OptionsPattern[]] := FileTreePicker[files, dirName, Infinity, opts];

FileTreePicker[Dynamic[fileList_], dirName_?DirectoryQ, depth_, opts : OptionsPattern[]] := Enclose[
	Module[{
		fileAssoc
	},
		fileAssoc = ConfirmBy[
			FileSystemMap[File, dirName, depth, Sequence @@ FilterRules[{opts}, Options[FileSystemMap]]],
			AssociationQ
		];
		fileAssoc = FixedPoint[DeleteCases[#, <||>, Infinity]&, fileAssoc];
		If[ !ListQ[fileList], fileList = {}];
		fileList = Intersection[fileList, Cases[fileAssoc, _String, {-1}]];
		DynamicModule[{},
			With[{
				viewerContent = fileAssoc //. {
					a_Association :> Normal[a]
				}
			},
				viewer[Dynamic[fileList], viewerContent]
			],
			SaveDefinitions -> True,
			Initialization :> (
				If[ !ListQ[fileList], fileList = {}]
			)
		]
	]
];

FileTreePicker[___] := $Failed;

fileToggler[Dynamic[files_], file_] := CheckboxBar[
	Dynamic[
		If[ MemberQ[files, file], {file}, {}],
		Function[
			files = If[ MemberQ[files, file],
				DeleteCases[files, file],
				Append[files, file]
			]
		]
	],
	{file -> FileNameTake[file]},
	Method -> "Active"
];

viewer[_, {}] := "No files to show";

viewer[Dynamic[files_], list_List] := Column[
	Replace[
		list,
		{
			Verbatim[Rule][file_, File[name_]] :> fileToggler[Dynamic[files], name],
			Verbatim[Rule][dir_, dirContents_List] :> OpenerView[
				{
					dir,
					Dynamic[
						viewer[Dynamic[files], dirContents],
						SynchronousUpdating -> False,
						SingleEvaluation -> True,
						TrackedSymbols :> {}
					]
				},
				False,
				Method -> "Active"
			]
		},
		{1}
	]
];
End[] (* End `Private` *)

EndPackage[]