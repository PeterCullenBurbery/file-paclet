(* ::Package:: *)

BeginPackage["PeterBurbery`FileUtilities`"]

(* Declare your package's public symbols here. *)

CreateRandomFile

Begin["`Private`"]

(* Define your public and private symbols here. *)
CreateRandomFile // ClearAll;
$inDef = False;
$debug = True;

beginDefinition // ClearAll;
beginDefinition // Attributes = { HoldFirst };
beginDefinition::unfinished = "\
Starting definition for `1` without ending the current one.";

beginDefinition[ s_Symbol ] /; $debug && $inDef :=
    WithCleanup[
        $inDef = False
        ,
        Print @ TemplateApply[ beginDefinition::unfinished, HoldForm @ s ];
        beginDefinition @ s
        ,
        $inDef = True
    ]

    beginDefinition[ s_Symbol ] :=
    WithCleanup[ Unprotect @ s; ClearAll @ s, $inDef = True ];

    endDefinition // beginDefinition;

    endDefinition // Attributes = { HoldFirst };

    endDefinition[ s_Symbol ] := endDefinition[ s, DownValues ];

    endDefinition[ s_Symbol, None ] := $inDef = False;

    endDefinition[ s_Symbol, DownValues ] :=
    WithCleanup[
        AppendTo[ DownValues @ s,
                  e: HoldPattern @ s[ ___ ] :>
                      throwInternalFailure @ HoldForm @ e
        ],
        $inDef = False
    ]
    endDefinition[ s_Symbol, SubValues  ] :=
    WithCleanup[
        AppendTo[ SubValues @ s,
                  e: HoldPattern @ s[ ___ ][ ___ ] :>
                      throwInternalFailure @ HoldForm @ e
        ],
        $inDef = False
    ]
    endDefinition[ s_Symbol, list_List ] :=
    endDefinition[ s, # ] & /@ list
    endDefinition // endDefinition;
    CreateRandomFile::filex =
"`1` already exists.";
CreateRandomFile::nodir =
"Directory `1` not found.";
CreateRandomFile::writefail =
"Failed to write `2` bytes to `1`.";

CreateRandomFile::invarg1 =
"Expected a valid file path or non-negative integer instead of `1`.";

CreateRandomFile::invarg2 =
"Expected a non-negative integer instead of `1`.";

CreateRandomFile::invargs =
"Invalid argument pattern.";

CreateRandomFile // Attributes = { };

CreateRandomFile // Options = {
    BatchSize                     -> Automatic,
    CreateIntermediateDirectories -> True,
    OpenAppend                    -> False,
    OverwriteTarget               -> False,
    ProgressReporting             -> True
};

$string    = _String  ? StringQ;
$byteCount = _Integer ? NonNegative;
$fileSpec  = Automatic | $string | File[ $string ] | _CloudObject | _LocalObject;

CreateRandomFile[ bytes: $byteCount, opts: OptionsPattern[ ] ] :=
    catchTop @ CreateRandomFile[ Automatic, bytes, opts ];

    CreateRandomFile[ Automatic, bytes: $byteCount, opts: OptionsPattern[ ] ] :=
    catchTop @ CreateRandomFile[ createFileName[ ], bytes, opts ]

    CreateRandomFile[
    path: $string,
    bytes: $byteCount,
    opts: OptionsPattern[ ]
] :=
    catchTop @ CreateRandomFile[ File @ path, bytes, opts ]

    CreateRandomFile[
    File[ path: $string ],
    bytes: $byteCount,
    opts: OptionsPattern[ ]
] :=
    catchTop @ checkResult[
        toFile @ writeRandomBytes[
            absoluteFileName @ path,
            bytes,
            OptionValue @ BatchSize,
            OptionValue @ ProgressReporting,
            OptionValue @ OpenAppend,
            OptionValue @ CreateIntermediateDirectories,
            OptionValue @ OverwriteTarget
        ],
        bytes
    ]

    CreateRandomFile[
    co_CloudObject,
    bytes: $byteCount,
    opts: OptionsPattern[ ]
] :=
    catchTop @ Module[ { overwrite, tmp },
        overwrite = TrueQ @ OptionValue @ OverwriteTarget;
        If[ ! overwrite && FileExistsQ @ co, throwFailure[ "filex", co ] ];
        WithCleanup[
            tmp = CreateRandomFile[ bytes, opts ],
            CopyFile[ tmp, co, OverwriteTarget -> overwrite ],
            DeleteFile @ tmp
        ]
    ]
    CreateRandomFile[
    lo_LocalObject,
    bytes: $byteCount,
    opts: OptionsPattern[ ]
] :=
    catchTop @ Module[ { overwrite, path, new },
        overwrite = TrueQ @ OptionValue @ OverwriteTarget;
        If[ ! overwrite && FileExistsQ @ lo, throwFailure[ "filex", lo ] ];
        Check[ Export[ lo, { }, "Binary" ], $failed = True ];
        path = LocalObjects`AuxPathName @ lo;
        If[ ! FileExistsQ @ path, throwFailure[ "writefail", lo, bytes ] ];
        new = CreateRandomFile[ path, bytes, OverwriteTarget -> True, opts ];
        If[ FileExistsQ @ new, lo, throwFailure[ "writefail", lo, bytes ] ]
    ]
    CreateRandomFile[ inv: Except[ $fileSpec|$byteCount ], ___ ] :=
    catchTop @ throwFailure[ "invarg1", inv ];

    CreateRandomFile[ $fileSpec, inv: Except[ $byteCount ], ___ ] :=
    catchTop @ throwFailure[ "invarg2", inv ];

    CreateRandomFile[ OptionsPattern[ ] ] :=
    catchTop @ throwFailure[ "argt", CreateRandomFile, 0, 1, 2 ];

    CreateRandomFile[
    path: $fileSpec | PatternSequence[ ],
    bytes: $byteCount,
    inv: Except[ OptionsPattern[ ] ],
    rest___
] :=
    catchTop @ throwFailure[
        "nonopt",
        inv,
        Length @ Hold[ path, bytes ],
        HoldForm @ CreateRandomFile[ path, bytes, inv, rest ]
    ];
CreateRandomFile[
    path: $fileSpec | PatternSequence[ ],
    bytes: $byteCount,
    inv: Except[ OptionsPattern[ ] ],
    rest___
] :=
    catchTop @ throwFailure[
        "nonopt",
        inv,
        Length @ Hold[ path, bytes ],
        HoldForm @ CreateRandomFile[ path, bytes, inv, rest ]
    ];
CreateRandomFile[ ___ ] := catchTop @ throwFailure[ "invargs" ];

catchTop // beginDefinition;

catchTop // Attributes = { HoldFirst };
catchTop[ eval_ ] :=
    Block[ { $catching = True, $failed = False, catchTop = # & },
        Catch[ eval, $top ]
    ];
catchTop // endDefinition;
throwFailure // beginDefinition;
throwFailure // Attributes = { HoldFirst };

throwFailure[ tag_String, params___ ] :=
    throwFailure[ MessageName[ CreateRandomFile, tag ], params ]


    throwFailure[ msg_, args___ ] :=
    Module[ { failure },
        failure = messageFailure[ msg, Sequence @@ HoldForm /@ { args } ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

    throwFailure // endDefinition;

    messageFailure // beginDefinition;
    messageFailure // Attributes = { HoldFirst };

    messageFailure[ args___ ] :=
    Module[ { quiet },
        quiet = If[ TrueQ @ $failed, Quiet, Identity ];
        WithCleanup[
            quiet @ ResourceFunction[ "MessageFailure" ][ args ],
            $failed = True
        ]
    ];

    messageFailure // endDefinition;

    throwInternalFailure // beginDefinition;

    throwInternalFailure // Attributes = { HoldFirst };

    throwInternalFailure[ eval_, a___ ] :=
    throwFailure[ CreateRandomFile::internal,
                  $bugReportLink,
                  HoldForm @ eval,
                  a
    ];

    throwInternalFailure // endDefinition;

    $bugReportLink := $bugReportLink = Hyperlink[
    "Report this issue \[RightGuillemet]",
    URLBuild @ <|
        "Scheme"   -> "https",
        "Domain"   -> "resources.wolframcloud.com",
        "Path"     -> { "FunctionRepository", "feedback-form" },
        "Fragment" -> SymbolName @ CreateRandomFile
    |>
];

toFile // beginDefinition;

toFile[ str_String ] := File @ str;

toFile[ path_File ] := Flatten @ path;
toFile[ other___ ] := other;

toFile // endDefinition;

validResultQ // beginDefinition;

validResultQ[ path: _String | _File, bytes_ ] :=
    FileExistsQ @ path && FileByteCount @ path >= bytes

    validResultQ // endDefinition;

    checkResult // beginDefinition;

    checkResult[ path: _String | _File, bytes_ ] :=
    If[ validResultQ[ path, bytes ],
        path,
        throwFailure[ "writefail", path, bytes ]
    ];
    checkResult // endDefinition;
    writeRandomBytes // beginDefinition;
    writeRandomBytes[
    stream_,
    0,
    targetBlockSize_,
    progress_,
    append_,
    createInt_,
    overwrite_
] :=
    Module[ { fileExists },
        fileExists = Quiet[ FileExistsQ @ stream, FileExistsQ::badfile ];

        If[ fileExists && ! TrueQ @ overwrite && ! TrueQ @ append,
            throwFailure[ "filex", stream ]
        ];

        CreateFile[ stream, CreateIntermediateDirectories -> createInt ]
    ];
    writeRandomBytes[
    stream_,
    bytes_,
    targetBlockSize_,
    progress_,
    append_,
    createInt_,
    overwrite_
] :=
    Module[ { fileExists, dir, remaining, blockSize, cell, block },
        fileExists = Quiet[ FileExistsQ @ stream, FileExistsQ::badfile ];

        If[ fileExists && ! TrueQ @ overwrite && ! TrueQ @ append,
            throwFailure[ "filex", stream ]
        ];


        dir = If[ TrueQ @ createInt,
                  GeneralUtilities`EnsureDirectory @ DirectoryName @ stream,
                  DirectoryName @ stream
              ];


        If[ ! DirectoryQ @ dir, throwFailure[ "fdnfnd", dir ] ];

        If[ ! TrueQ @ append && fileExists, DeleteFile @ stream ];
        remaining = bytes;

        blockSize =
            Replace[
                targetBlockSize,
                Except[ _Integer? Positive ] :>
                    Min[
                        Ceiling[ bytes * 128^(-1) ],
                        Ceiling[ MemoryAvailable[ ] * 64^(-1) ],
                        2^25
                    ]
            ];

        cell = showProgress[ progress && bytes > 1000000, bytes, remaining ];

        While[
            (block = Min[ remaining, blockSize ]) > 0,
            write[ stream, ByteArray @ RandomInteger[ 255, block ] ];
            remaining -= block
        ];

        NotebookDelete @ cell;
        Remove @ remaining;
        Replace[ closeAll @ stream, { { out_ } :> out } ]
    ];
    writeRandomBytes // endDefinition;
    showProgress // beginDefinition;

    showProgress // Attributes = { HoldRest };
    showProgress[ True, bytes_, remaining_ ] :=
    PrintTemporary @ Dynamic @ GeneralUtilities`ProgressPanel[
        "Writing file...",
        Row @ {
            byteString[ bytes - remaining ],
            "/",
            byteString @ bytes,
            " ",
            PercentForm[ N[ 1 - (remaining * bytes^(-1)) ], 3 ]
        },
        1 - (remaining * bytes^(-1))
    ]
    showProgress[ ___ ] := Null;
    showProgress // endDefinition;

    byteString // beginDefinition;

    byteString[ x_ ] := TextString @ NumberForm[ btq @ x, 3 ];

    byteString // endDefinition;

    btq // beginDefinition;
    btq := btq = ResourceFunction[ "BytesToQuantity", "Function" ];

    btq // endDefinition;

    write // beginDefinition;
    write[ path_String? FileExistsQ, data_ ] :=
    Module[ { stream },

        stream =
            Replace[
                Streams @ AbsoluteFileName @ path,
                {
                    { } :>
                        OpenAppend[
                            AbsoluteFileName @ path,
                            BinaryFormat -> True
                        ],
                    { str_OutputStream, ___ } :> str
                }
            ];

        write[ stream, data ]
    ];
    write[ path_String, data_ ] := write[ CreateFile @ path, data ];

    write[ stream_OutputStream, data_ ] := BinaryWrite[ stream, data ];
write // endDefinition;
closeAll // beginDefinition;

closeAll[ path_String? FileExistsQ ] :=
    Map[
        Close,
        Cases[
            Streams[ ],
            (InputStream | OutputStream)[ f_, ___ ] /;
                Quiet @ AbsoluteFileName @ f === AbsoluteFileName @ path
        ]
    ]

    closeAll[ path_String ] :=
    Close /@ Cases[ Streams[ ], (InputStream | OutputStream)[ path, ___ ] ]
    closeAll[ stream_ ] := Close @ stream;

    closeAll // endDefinition;
    absoluteFileName // beginDefinition;
    absoluteFileName[ path_String ] /; DirectoryName @ path === "" :=
    FileNameJoin @ { Directory[ ], path };
    absoluteFileName[ path_String ] :=
    FileNameJoin @ { DirectoryName @ path, FileNameTake @ path };

    absoluteFileName[ File[ path_String ] ] := absoluteFileName @ path
    absoluteFileName // endDefinition;

    createFileName // beginDefinition;

    createFileName[ ] :=
    Module[ { tmp }, WithCleanup[ tmp = CreateFile[ ], DeleteFile @ tmp ] ]

    createFileName // endDefinition;
End[] (* End `Private` *)

EndPackage[]
