(* $Id: Utilities.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Utility Routines for EDA Pack *)

(* :Context: EDA`Utilities` *)

(* :Author: David M. Harrison *)

(* :Summary:
	AdjustSignificantFigures - adjust significant figures based on
		the error.
	ChiSquareProbability - returns the probability given a ChiSquared
		and the DegreesOfFreedom.
	DataParameters - return information about a EDA data set.
	LoadData - load supplied real-world data.
	UnpackData - unpack a EDA data set.
	FilterOptions[] - for Version 9.0 and later, from the now obsolete
		Utilities`FilterOptions`
*)

(* :Copyright: Copyright 1995 - 2013, Wolfram Research, Inc.*)

(* :Package Version: 1.3 *)

(* :Mathematica Version: 9.0 *)

(* :History:
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
	Version 1.2 by David M. Harrison, May 2009.
	Version 1.3 by David M. Harrison, February 2013.
*)

(* :Keywords:
*)

(* :Sources:
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

(*
 * Various utilities and often used functions.
 * Contents:
 *	AdjustSignificantFigures
 *	ChiSquareProbability
 *	DataParameters
 *	LoadData
 *	UnpackData
 *)
BeginPackage["EDA`Utilities`",
	"EDA`Common`"
]

Unprotect[
	AdjustSignificantFigures,
	ChiSquareProbability,
	DataParameters,
	LoadData,
	UnpackData
]

If[$VersionNumber >= 9.0,
	Unprotect[FilterOptions]
];

AdjustSignificantFigures::usage =
	"AdjustSignificantFigures[data] returns the data with the significant
	figures adjusted. 'data' can be of the form {value, error}, or of
	the form {{value1, error1},{value2, error2}, ... , {valueN, errorN}}. 
	In both cases the significant figures in value are defined by the
	two most significant figures in error, by default. This number
	can be changed with an ErrorDigits option."

ChiSquareProbability::usage =
	"ChiSquareProbability[chisq, dof] returns the probability in percent
	that an experiment will return a chi-squared statistic greater than
	chisq, when the degrees of freedom are dof."

ErrorDigits::usage =
	"ErrorDigits is an option to AdjustSignificantFigures that sets the
	number of significant figures in the error. The default value
	is 2."

LoadData::usage =
	"LoadData[label] loads an EDA data set identified by label. If
	the EDAShowProgress option is set to True (the default), the actual
	name(s) of the data set(s) being loaded are given as a Message,
	otherwise they are not. In many cases, but not all, the name of
	the data set is the same as label, except that the name of the data
	set has the string \"Data\" appended. LoadData[], i.e., no input
	argument, lists the legal values for label."

Options[LoadData] = {
	EDAShowProgress -> True
}

AbsoluteZero::usage = 
	"AbsoluteZero is a name that can be given to LoadData."
Anscombe::usage = "Anscombe is a name that can be given to LoadData."
Boyle::usage = "Boyle is a name that can be given to LoadData."
Chwirut::usage = "Chwirut is a name that can be given to LoadData."
CThDating::usage = "CThDating is a name that can be given to LoadData."
Cobalt60::usage = "Cobalt60 is a name that can be given to LoadData."
Darwin::usage = "Darwin is a name that can be given to LoadData."
FreeFall::usage = "FreeFall is a name that can be given to LoadData."
Ganglion::usage = "Ganglion is a name that can be given to LoadData."
Interneuron::usage = "Interneuron is a name that can be given to LoadData."
KPiPi::usage = "KPiPi is a name that can be given to LoadData."
MassSpec::usage = "MassSpec is a name that can be given to LoadData."
MassSpec2::usage = "MassSpec2 is a name that can be given to LoadData."
Ozone::usage = "Ozone is a name that can be given to LoadData."
PearsonYork::usage = "PearsonYork is a name that can be given to LoadData."
Population::usage = "Population is a name that can be given to LoadData."
EDAReactance::usage = "EDAReactance is a name that can be given to LoadData."
Siegel::usage = "Siegel is a name that can be given to LoadData."
Thermocouple::usage = "Thermocouple is a name that can be given to LoadData."
Wampler::usage = "Wampler is a name that can be given to LoadData."

LoadData::name = 
	"Loading: `1`"
LoadData::noname = 
	"Name `1` is not known.  LoadData[] will list all known names."
LoadData::contents = 
	"The names that are valid arguments to LoadData are: `1`"

DataParameters::usage =
	"DataParameters[data] determines the number of data points, npoints,
	and number of variables, nvars, in data and returns {npoints,nvars}.
	The data is assumed to be in the format of Experimental Data
	Analyst."

UnpackData::usage =
	"UnpackData[data] takes data formatted in the EDA standard
	and returns {npoints, nvars, x, errx, y, erry} in that order, where
	`npoints' is the number of data points, `nvars' the number of
	variables, `x' the independent variable, `errx' the error in `x',
	`y' the dependent variable, and `erry' the error in `y'. If `nvars'
	is 1, the `x' returned is {1, 2, ... , npoints}. If no errors appear
	in the data, the error variables are filled in with a list of 1s whose
	length is equal to `npoints'. In all cases, the data is returned sorted
	by the value of the independent variable."

UnpackData::baderror = 
	"An error in the data was specified as zero or negative."
UnpackData::dataformat =
	"The data format appears to be scrambled."
UnpackData::nodata =
	"There appears to be no data to unpack."

Options[AdjustSignificantFigures] = {
	ErrorDigits -> 2
}
AdjustSignificantFigures::wrongval = 
	"The ErrorDigits option must be a positive integer."

If[$VersionNumber >= 9.0,
	FilterOptions::usage = "FilterOptions[symbol, options..] returns a sequence
		of those options that are valid options for symbol. 
		FilterOptions[{opts..}, options..] filters out options 
		with names opts.";
];

Begin["`Private`"]

AdjustSignificantFigures[data_?(MatrixQ[#, NumericQ]&) , opts___Rule] :=
	AdjustSignificantFigures[#,opts]& /@ data

AdjustSignificantFigures[data_?(VectorQ[#, NumericQ]&) , opts___Rule] /;
	(Length[data] == 2 && data[[2]] != 0) := Module[
	{
		error,
		factor = 1,
		optErrorDigits,
		value
	},

	optErrorDigits = ErrorDigits /. {opts} /. 
		Options[AdjustSignificantFigures];

	If[ IntegerQ[optErrorDigits] == False,
		Message[AdjustSignificantFigures::wrongval];
		Return[$Failed];
	];
	If[ optErrorDigits < 1,
		Message[AdjustSignificantFigures::wrongval];
		Return[$Failed];
	];

	value = data[[1]];
	error = data[[2]];

	If[ error <= 10^(optErrorDigits - 1) ,
		While[Length[IntegerDigits[Round[error]]] != optErrorDigits ||
				Round[error] == 0,
			factor *= 10;
			error *= 10;
		],
	(* else *)
		While[ Length[IntegerDigits[Round[error]]] != optErrorDigits,
			factor /= 10;
			error /= 10;
		];
	];
	value *= factor;
	{Round[value]/factor, Round[error]/factor} //N
]

AdjustSignificantFigures[data_?(VectorQ[#, NumericQ]&), opts___Rule] /;
	(data[[2]] == 0) := data

ChiSquareProbability[chisq_?NumericQ, dof_?IntegerQ]  /;
	dof > 0 && chisq >= 0 := 100 * Gamma[dof/2, chisq/2] /
	Gamma[dof/2] //N

DataParameters[data_] := Module[
	{
		npoints,	(* number of points in data *)
		nvars		(* number of variables for each point *)
	},

	npoints = Length[data];

	If[VectorQ[data],
		nvars = 1,
	(* else *)
		nvars = Length[Flatten[First[data]]]
	];

	{npoints,nvars}
]


(* datacontents lists the contents of the EDA`Data` directory.
 * The first entry is the name by which the data can be loaded with
 * LoadData, the second is the file name in the EDA`Data` directory,
 * and the third entry is the string which names the data set(s) being
 * loaded.  The reason for the difference between the first and
 * second entries is that the descritive name (the first) is often
 * longer than the 8 character limit of DOS files.
 *)
datacontents = {
	{AbsoluteZero, "AbsZero`", "AbsoluteZeroData"},
	{Anscombe, "Anscombe`", "AnscombeData[[i]], for i = 1,2,3,4"},
	{Boyle, "Boyle`", "BoyleData"},
	{Chwirut, "Chwirut`", "ChwirutData"},
	{CThDating, "CThDatng`", "CThDatingData"},
	{Cobalt60, "Cobalt60`", "Cobalt60Data"},
	{Darwin, "Darwin`", "CrossFertilizedData\nand SelfFertilizedData"},
	{FreeFall, "FreeFall`", "FreeFallData"},
	{Ganglion, "Ganglion`", "GanglionData"},
	{Interneuron, "Intrnron`", "InterneuronData"},
	{KPiPi, "KPiPi`", "KPiPiData"},
	{MassSpec, "MassSpec`", "MassSpecData"},
	{MassSpec2, "MssSpec2`", "MassSpec2Data"},
	{Ozone, "Ozone`", "OzoneData"},
	{PearsonYork, "PrsnYork`", "PearsonYorkData"},
	{Population, "Popltion`", "PopulationData"},
	{EDAReactance, "Rctnce`", "ReactanceData"},
	{Siegel, "Siegel`", "SiegelData"},
	{Thermocouple, "Thrmcple`", "ThermocoupleData"},
	{Wampler, "Wampler`", "WamplerData"}
};

LoadData[name_, opts___?OptionQ] := Module[
	{
		i,j,k,
		choice, file,
		optShowProgress
	},


	optShowProgress = EDAShowProgress /. {opts} /. Options[LoadData];

	choice = Cases[ datacontents, {i_, j_, k_} /; i === name ];
	If[Length[choice] != 1,
		Message[LoadData::noname, name];
		Return[$Failed];
	];

	If[optShowProgress,
		Message[LoadData::name, (Last /@ choice) //TableForm];
	];

	choice = Flatten[choice];
	file = "EDA`Data`" <> Part[choice,2];

	Get[file];
]

LoadData[] := Module[
	{prePrint},
	(* save the state of $MessagePrePrint and reset *)
	prePrint = $MessagePrePrint;
	$MessagePrePrint = Identity;
	Message[LoadData::contents,
		(First /@ datacontents) //ColumnForm ];
	(* reset $MessagePrePrint to original state *)
	$MessagePrePrint = prePrint;
]

(*
 * A NumericQ test would be nice, but for large data takes an
 * very long time.  So -- we trust the user.
 *)
UnpackData[data_] := Module[
	{
		datasorted,
		flatdata,
		n = Length[data],
		nvars,
		x,errx,y,erry
	},

	errx = Table[1,{n}];
	erry = Table[1,{n}];

	If[Length[data] == 0,
		Message[UnpackData::nodata];
		Return[$Failed];
	];

	If[VectorQ[data],
		nvars = 1;
		x = Table[i, {i,n}]//N;
		y = data//N,
	(* else *)
		(* A heuristic to catch some common screwed up data formats. *)
		If[Length[Dimensions[datasorted]] == 1,
			Message[UnpackData::dataformat];
			Return[$Failed];
		];

		(* 
		 * The OrderedQ test is for performance reasons for a typical case.
		 * twj says we can probably count on OrderedQ and Sort working for
		 * all types of EDA data sets in all future releases.
		 *)
		If[OrderedQ[data],
			datasorted = data,
		(* else *)
			datasorted = Sort[data]
		];

		nvars = Length[Flatten[First[datasorted]]];
		flatdata = Flatten /@ datasorted;
		Switch[nvars,

			2,	x = First /@ flatdata//N;
				y = Last /@ flatdata//N,

			3,	x = First /@ flatdata//N;
				y = Part[#,2]& /@ flatdata//N;
				erry = Last /@ flatdata//N;
				If[Length[Select[erry, # <= 0 & ]] != 0,
					Message[UnpackData::baderror];
					Return[$Failed]
				],

			4,	x = First /@ flatdata//N;
				errx = Part[#,2]& /@ flatdata//N;
				If[Length[Select[errx, # <= 0 & ]] != 0,
					Message[UnpackData::baderror];
					Return[$Failed]
				];
				y = Part[#,3]& /@ flatdata//N;
				erry = Last /@ flatdata//N;
				If[Length[Select[erry, # <= 0 & ]] != 0,
					Message[UnpackData::baderror];
					Return[$Failed]
				],

			_,	Message[UnpackData::dataformat];
				Return[$Failed];
		];
	];
	{n,nvars,x,errx,y,erry}

]

(*
 * This is copied from the now obsolete Utilities`FilterOptions`
 *)
If[$VersionNumber >= 9.0,
	FilterOptions[ command_Symbol, options___ ] :=
		FilterOptions[ First /@ Options[command], options ];
	FilterOptions[ opts_List, options___ ] :=
		Sequence @@ Select[ Flatten[{options}], MemberQ[opts, First[#]]& ];
];

End[]

SetAttributes[AdjustSignificantFigures, {ReadProtected} ]
SetAttributes[ChiSquareProbability, {ReadProtected} ]
SetAttributes[DataParameters, {ReadProtected} ]
SetAttributes[LoadData, {ReadProtected} ]
SetAttributes[UnpackData, {ReadProtected} ]
 
If[$VersionNumber >= 9.0,
	SetAttributes[FilterOptions, {ReadProtected}];
	Protect[FilterOptions];
];

Protect[
	AdjustSignificantFigures,
	ChiSquareProbability,
	DataParameters,
	LoadData,
	UnpackData
]

EndPackage[]
