(* $Id: ImportExport.m,v 6.1 2009/05/23 18:04:45 harrison Exp $ *)

(* :Title: Importing and Exporting ASCII Data Files *)

(* :Context: EDA`Import`Export` *)

(* :Author: David M. Harrison *)

(* :Summary:
	ImportData - reads an ASCII file producing a EDA format
			data set.
	ExportData - writes an ASCII file from a EDA format data set.
*)

(* :Copyright: Copyright 1995 - 2009, Wolfram Research, Inc. *)

(* :Package Version: 1.2 *)

(* :Mathematica Version: 6.0 *)

(* :History:
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
	Version 1.2 by David M. Harrison, May 2009.
*)

(* :Keywords:
*)

(* :Sources:
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

BeginPackage["EDA`ImportExport`"]

Unprotect[ImportData, ExportData]

ImportData::usage = 
	"ImportData[file] reads an ASCII file and returns a dataset. The
	'file' is assumed to be in the current directory. ImportData[file,
	directory] reads 'file' in 'directory'."

AllNumeric::usage =
	"AllNumeric is an option to ImportData. If AllNumeric is set to the default
	(True), the file (excluding any headers), is assumed to be all numerics.
	If set to False, then the file can contain words, as well as numbers.
	In this case, ImportData will run more slowly."

FormatData::usage =
	"FormatData is an option to ImportData that specifies if the
	returned data is formatted according to the specification of the
	Experimental Data Analyst. When set to True (the default) with
	either three or four variables in the data set, the returned
	data is formatted. When set to False or there are exactly
	two or more than four variables in the data set, then each
	data point is returned as a flat list."

InputVariables::usage =
	"InputVariables is an option to ImportData that specifies the number
	of variables in the input dataset. If Automatic (the default), each
	line of the input file is assumed to be a data point and the number of
	variables is calculated by the amount of data in the first line
	of the input file."

OutputVariables::usage =
	"OutputVariables is an option to ImportData that specifies the
	number of variables to be output. If Automatic (the default), it
	is equal to the number of input variables."

HeaderLines::usage =
	"HeaderLines is an option to ImportData that specifies the number of
	lines at the beginning of the file that include header information and
	should be skipped. If set to 0 (the default), no initial lines are
	skipped. Blank lines are counted."

NumData::usage =
	"NumData is an option to ImportData that specifies the amount of
	data to be read. If set to All (the default), all data
	is read."

TrailerLines::usage =
	"TrailerLines is an option to ImportData that specifies the number of
	lines at the end of the file that do not contain data and should
	be ignored. If set to 0 (the default), no trailer lines are
	ignored. In all cases, blank lines are ignored in counting the number
	of lines. If AllNumeric is set to True (the default), lines containing
	non-numbers are also ignored in counting and a warning message is
	generated. If AllNumeric is set to False, all lines except blank
	ones are counted."

UseVariables::usage = 
	"UseVariables is an option to ImportData that specifies the fields
	in the input records that are to be extracted and used in the
	output record. The values are extracted in the order in which
	they are given in the list."

Options[ImportData] = {
	AllNumeric -> True,
	FormatData -> True,
	InputVariables -> Automatic,
	HeaderLines -> 0,
	NumData -> All,
	OutputVariables -> Automatic,
	TrailerLines -> 0,
	UseVariables -> Automatic,
	WordSeparators -> {" ", "\t"}
}

(* Messages in case things go wrong. *)
ImportData::badopt = "Option `1` is not known."
ImportData::dataorder =
	"The specified UseVariables of `1` is inconsistent with having a
	data set of `2` variables in the dataset."
ImportData::firstshort =
	"The length of the first record is less than the last record."
ImportData::inconsistent =
	"There is no data. Perhaps the contents of file `1` are inconsistent
	with the specified options."
ImportData::nodata =
	"Could not read any data from file `1`."
ImportData::padding =
	"Warning: padding the last line with `1` zeros."
ImportData::sparse =
	"Warning: not all lines of the file contain the same number of 
	fields."
ImportData::trailerlines =
	"More TrailerLines specified then there are lines in file `1`."

ExportData::usage =
	"ExportData[file, data] writes data into a file. By default, the
	numbers are written in InputForm; a DataForm option can change InputForm
	to others such as FortranForm. By default the word separator is a
	tab unless Padding has been specified; a separator option can change
	a tab to an arbitrary string."

DataForm::usage =
	"DataForm is an option to ExportData. It specifies the way that
	numbers are written. Valid values include InputForm (the default),
	FortranForm, and others."

Overwrite::usage =
	"Overwrite is an option to ExportData. If Overwrite is set to False (the default) and the file to be written already exists in the current directory, then ExportData issues a message and exits. If Overwrite is
	set to True, then ExportData will overwrite the file."

Padding::usage =
	"Padding is an option to ExportData. If set to None (the default),
	no padding is performed. Otherwise, its value is used by
	PaddedForm to format the numbers; in this case, a separator is
	set to the empty string \"\"."

Separator::usage =
	"Separator is an option to ExportData that specifies the string
	that is to separate the numbers in a row. The default is Tab 	unless Padding is specified, in which case it is set to the
	empty string \"\"."

Options[ExportData] = {
	DataForm -> InputForm,
	Overwrite -> False,
	Padding -> None,
	Separator -> Tab
}

ExportData::badopt = "Option `1` is not known."
ExportData::dataformat =
	"The input data set is scrambled."
ExportData::exists =
	"File `1` already exists in the current directory. To overwrite
	the existing file, set the Overwrite option to ExportData to True."

Begin["`Private`"]

ImportData[file_?StringQ, dir_?StringQ, opts___?OptionQ] := Module[
	{answer, oldDirectory},

	oldDirectory = Directory[];
	Check[SetDirectory[dir],
		Return[$Failed]
	];
	answer = ImportData[file, opts];
	SetDirectory[oldDirectory];
	answer

]

ImportData[file_?StringQ, opts___?OptionQ] /; (Length[FileNames[file]] == 1) :=
	Module[
		{
			data,
			i,j,		(* dummy variables *)
			numvars,	(* number of variables *)
			optAllNumeric,
			optFormatData,
			optInputVariables,
			optHeaderLines,
			optNumData,
			optOutputVariables,
			optTrailerLines,
			optUseVariables,
			optWordSeparators,
			stream,		(* the input stream *)
			words		(* the words in the data *)
		},

	i = Complement[ First /@ {opts}, First /@ Options[ImportData] ];
    If[Length[i] != 0,
        Message[ImportData::badopt, #]& /@ i;
        Return[$Failed]
    ];

	optAllNumeric = AllNumeric /. {opts} /.  Options[ImportData];
	optFormatData = FormatData /. {opts} /.  Options[ImportData];
	optInputVariables = InputVariables /. {opts} /.
		Options[ImportData];
	optHeaderLines = HeaderLines /. {opts} /. Options[ImportData];
	optNumData = NumData /. {opts} /. Options[ImportData];
	optOutputVariables = OutputVariables /. {opts} /.
		Options[ImportData];
	optTrailerLines = TrailerLines /. {opts} /. Options[ImportData];
	optUseVariables = UseVariables /. {opts} /.  Options[ImportData];
	optWordSeparators = WordSeparators /. {opts} /.  Options[ImportData];

	(*
	 * If we try using, say, a comman as the WordSeparator,
	 * we must read in as strings and then convert.
	 *)
	If[optWordSeparators != {" ","\t"},
		optAllNumeric = False
	];

	If[optAllNumeric === True,
		words = Real,
	(* else *)
		words = Word
	];

    If[Check[ stream = OpenRead[file], $Failed] === $Failed,
       	Return[$Failed]
    ];

	(* Skip header lines, then read in the data line by line *)
	Skip[stream, Record, optHeaderLines, NullRecords -> True];
	If[optNumData === All,
		data = ReadList[stream, words, 
			RecordLists -> True,
			WordSeparators -> optWordSeparators ],
	(* else *)
		data = ReadList[stream, words, optNumData,
			RecordLists -> True,
			WordSeparators -> optWordSeparators ]; 
	];
	Close[stream];

	If[Length[data] == 0,
		Message[ImportData::nodata, file];
		Return[$Failed]
	];

	If[ optTrailerLines >= Length[data],
		Message[ImportData::trailerlines, file];
		Return[$Failed]
	];
	data = Drop[data, -optTrailerLines];

	(* 
	 * First formatting:  each data point is a flat list within
	 * data.
	 *)
	If[optInputVariables === Automatic,
		numvars = Length[First[data]],
	(* else *)
		numvars = optInputVariables;
		data = Partition[Flatten[data], optInputVariables];
	];

	(*
	 * Do we need to pad the last line?  If so, take it apart,
	 * append zeroes, and put it back together again.
	 *)
	i = Length[First[data]] - Length[Last[data]];
	If[i != 0,
		Message[ImportData::padding, i];
		If[optAllNumeric === True,
			i = Table[0, {i}],
		(* else *)
			i = Table["0", {i}]
		];
		data = Partition[Join[Flatten[data],i], numvars];
	];

	If[Length[data] == 0,
		Message[ImportData::inconsistent, file];
		Return[$Failed]
	];

	If[Length[Union[Length /@ data]] != 1,
		Message[ImportData::sparse];
	];

	If[optUseVariables =!= Automatic,
		If[ Max[optUseVariables] > numvars || Min[optUseVariables] < 1,
			Message[ImportData::dataorder, optUseVariables, numvars];
			Return[$Failed]
		];
		data = Part[#,optUseVariables]& /@ data;
		numvars = Length[First[data]];
	];

	If[optOutputVariables =!= Automatic,
		numvars = optOutputVariables;
		data = Partition[Flatten[data], optOutputVariables];
	];

	(*
	 * Now we should have only numbers in sub-lists.  If
	 * AllNumeric is not True, then they are currently represented
	 * as strings.  Thus, we need to convert.
	 *)
	If[optAllNumeric =!= True,
		data = StringJoin[
			StringInsert[#, " ", -1]& /@ Flatten[data]
		];
		stream = StringToStream[data];
		data = ReadList[stream,Number];
		Close[stream];
		data = Partition[data,numvars];
	];

	(*
	 * Final formatting: put data into the "standard" EDA format.
	 *)
	If[optFormatData === True,
		Switch[numvars,
			1,	data = Flatten[data],
			3,	data = Apply[({#1,{#2,#3}})&, data, 2],
			4,	data = Partition[#,2]& /@ data
		];
	];

	data //N

]

(* internal variable used by myPadFunction *)
padvalue =.

(* 
 * internal function used if Padding is set in the call
 * to ExportData.
 *)
myPadFunction[number_] := PaddedForm[number, padvalue];

ExportData[file_?StringQ, data_, opts___?OptionQ] := Module[

	{
		i,j,			(* dummy variables *)
		npoints,		(* number of data points *)
		nvars,			(* number of variables *)
		optDataForm,
		optOverwrite,
		optPadding,
		optSeparator,
		strm,			(* the output stream *)
		work			(* working version of data *)
	},


	i = Complement[ First /@ {opts}, First /@ Options[ExportData] ];
    If[Length[i] != 0,
        Message[ExportData::badopt, #]& /@ i;
        Return[$Failed]
    ];

	optDataForm = DataForm /. {opts} /. Options[ExportData];
	optOverwrite = Overwrite /. {opts} /. Options[ExportData];
	optPadding = Padding /. {opts} /. Options[ExportData];
	optSeparator = Separator /. {opts} /. Options[ExportData];

	If[optPadding =!= None,
		padvalue = optPadding;
		optDataForm = myPadFunction;
		optSeparator = "";
	];

	npoints = Length[data];
	If[VectorQ[data],
		work = data;
		nvars = 1,
	(* else *)
		work = Flatten /@ data;
		nvars = Length[First[work]];
	];

	If[ npoints < 1 || nvars < 1,
		Message[ExportData::dataformat];
		Return[$Failed]
	];

	If[ Length[FileNames[file]] != 0 && optOverwrite === False,
		Message[ExportData::exists, file];
		Return[$Failed]
	];

	If[Check[ strm = OpenWrite[file], $Failed] === $Failed,
		Return[$Failed]
	];

	For[ i = 1, i <= npoints, i++,
		If[ nvars == 1,
			WriteString[strm,
				ToString[optDataForm[ work[[i]] ]], "\n"],
		(* else *)
			(* Note we do the last column after this loop *)
			For[ j = 1, j < nvars, j++,
				WriteString[strm,
					ToString[optDataForm[ work[[i,j]] ]],
					optSeparator];
			];
			WriteString[strm,
					ToString[optDataForm[ work[[i,nvars]] ]],
					"\n"];
		];
	];

	Close[strm];
]

End[]

SetAttributes[ImportData, ReadProtected]
Protect[ImportData]

SetAttributes[ExportData, ReadProtected]
Protect[ExportData]

EndPackage[]
