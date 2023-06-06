(* ::Package:: *)

BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

RelativePath

Begin["`Private`"]

(* Define your public and private symbols here. *)


(*Definition*)

RelativePath // ClearAll;

(*Messages*)

RelativePath::badfile =
"The specified argument `1` should be a valid string or File.";

RelativePath::ostype =
"The value of option OperatingSystem -> `1` must be one of \"MacOSX\", \"Windows\", or \"Unix\".";

RelativePath::argt =
"RelativePath called with `1` arguments; 1 or 2 arguments are expected.";

RelativePath::nonopt =
"Options expected (instead of `1`) beyond position `2` in `3`. An option must be a rule or a list of rules.";

(*Options*)

RelativePath // Options = {
    OperatingSystem :> $OperatingSystem
};

(*Main definition*)

RelativePath[ path_, opts: OptionsPattern[ ] ] := 
    Module[ { dir, result },
        dir = Directory[ ];
        result = Catch[ relativePath[ dir, path, OptionValue @ OperatingSystem ], $top ];
        result /; result =!= $fail
    ];

RelativePath[ path1_, path2: Except[ _? OptionQ ], opts: OptionsPattern[ ] ] := 
    Module[ { result },
        result = Catch[ relativePath[ path1, path2, OptionValue @ OperatingSystem ], $top ];
        result /; result =!= $fail
    ];

(*Error cases*)

RelativePath[ ] :=
    Null /; message[ "argt", 0 ];

RelativePath[ a_, b_, inv: Except[ _? OptionQ ], c___ ] := 
    Null /; message[ "nonopt", inv, 2, HoldForm @ RelativePath[ a, b, inv, c ] ];

(*Dependencies*)
(*
fileNameJoin*)

fileNameJoin // ClearAll;

fileNameJoin[ list: { ___String }, os_ ] := 
    Quiet[
        Check[
            FileNameJoin[ list, OperatingSystem -> os ],
            message[ "ostype", os ];
            Throw[ $fail, $top ],
            FileNameJoin::ostype
        ],
        FileNameJoin::ostype
    ];

fileNameJoin[ ___ ] := 
    Throw[ $fail, $top ];

(*fileNameSplit*)

fileNameSplit // ClearAll;

fileNameSplit[ file_String, os_ ] := 
    Quiet[
        Check[
            FileNameSplit[ file, OperatingSystem -> os ],
            message[ "ostype", os ];
            Throw[ $fail, $top ],
            FileNameSplit::ostype
        ],
        FileNameSplit::ostype
    ];

fileNameSplit[ ___ ] :=
    Throw[ $fail, $top ];

(*expandFileName*)

expandFileName // ClearAll;

expandFileName[ file_String /; StringStartsQ[ file, "file://" ], os_ ] := (
    LocalObject;
    expandFileName[ LocalObjects`URIToPath @ file, os ]
);

expandFileName[ obj_CloudObject, _ ] := 
    expandCloudObject @ obj;

expandFileName[ url_URL, _ ] := 
    expandURL @ url;

expandFileName[ file_, os_ ] := 
    If[ os === $OperatingSystem,
        Quiet[
            Check[
                ExpandFileName @ file,
                message[ "badfile", file ];
                Throw[ $fail, $top ],
                ExpandFileName::badfile
            ],
            ExpandFileName::badfile
        ],
        file
    ];

(*expandCloudObject*)

expandCloudObject // ClearAll;

expandCloudObject[ obj_CloudObject ]:=
    Module[ { parsed, path },
        parsed = URLParse @ obj;
        path = If[ MatchQ[ parsed, KeyValuePattern[ "Path" -> { "", "obj"|"env"|"objects", _ } ] ],
            Quiet[
                Check[
                    CloudObject[ obj, CloudObjectNameFormat -> "UserURLBase" ],
                    obj,
                    CloudObjectNameFormat::una
                ],
                CloudObjectNameFormat::una
            ], 
            obj
        ];
        expandURL @ path
    ];
(*
expandURL*)

expandURL // ClearAll;

expandURL[ obj: _CloudObject|_URL ] :=
    Module[ { url },
        url = URLBuild @ URLParse @ obj;
        If[ ! StringQ @ url, 
            Throw[ $fail, $top ], 
            url
        ]
    ];

(*relativePath*)

relativePath // ClearAll;

relativePath[ url1: _CloudObject|_URL, url2_, Except[ "Unix" ] ] :=
    relativePath[ url1, url2, "Unix" ];

relativePath[ url1_, url2: _CloudObject|_URL, Except[ "Unix" ] ] :=
    relativePath[ url1, url2, "Unix" ];

relativePath[ path1_, path2_, os_ ] := 
    Module[ { full1, full2, split1, split2, first1, first2, common, drop1, drop2, dots },
        full1 = expandFileName[ path1, os ];
        full2 = expandFileName[ path2, os ];
        split1 = fileNameSplit[ full1, os ];
        split2 = fileNameSplit[ full2, os ];
        first1 = First[ split1, "" ];
        first2 = First[ split2, "" ];
        
        If[ os === "Windows" && first1 =!= first2,
            Throw[ full2, $top ]
        ];

        common = Length @ ResourceFunction[ "LongestCommonPrefix" ][ split1, split2 ];
        drop1 = Drop[ split1, common ];
        drop2 = Drop[ split2, common ];
        dots = Join[ ConstantArray[ "..", Length @ drop1 ], drop2 ];
        fileNameJoin[ dots, os ]
    ];

(*message*)

message // ClearAll;

message[ tag_, args___ ] := (
    ResourceFunction[ "ResourceFunctionMessage" ][
        MessageName[ RelativePath, tag ],
        args
    ];
    False
);
End[] (* End `Private` *)

EndPackage[]
