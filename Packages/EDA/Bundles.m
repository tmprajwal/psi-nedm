(* $Id: Bundles.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Bundles of EDA Routines *)

(* :Context: EDA`Bundles` *)

(* :Author: David M. Harrison *)

(* :Summary:
	EDALogListPlot - Front end to Graphics`LogListPlot
	EDALogLogListPlot - Front end to Graphics`LogLogListPlot
	FitExponent - fit y = A Exp[B*x]
	FitPeaks - fit peaks such as spectra
*)

(* :Copyright: Copyright 2000 - 2009, Wolfram Research, Inc.*)

(* :Package Version: 1.3 *)

(* :Mathematica Version: 9.0 *)

(* :History:
	Version 1.0 by David M. Harrison, April 2000.
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
 * This package contains bundles of EDA and other routines
 * to perform specific tasks.
 *)

(* 
 * As of Mathematica 6.0 the Graphics`Graphics` package
 * is obsolete, and the functions LogListPlot[] and LogLogListPlot[]
 * have been replaced by ListLogPlot[] and ListLogLogPlot[] which
 * are in the kernel.
 * As of Mathematica 9.0 the Utilites`FilterOptions` package is
 * obsolete.  The syntax of the replacement FilterRules[] interacts badly
 * with much code in EDA. FilterOptions[] is now in EDA`Utilities`
 *)
Which[
	$VersionNumber < 6.0,
		BeginPackage["EDA`Bundles`", 
				"EDA`Common`",
        		"EDA`FindPeaks`",
        		"EDA`FindFit`",
				"EDA`LinearFit`",
				"EDA`Utilities`",
				"Graphics`Graphics`",
        		"Utilities`FilterOptions`"
		],
	$VersionNumber < 9.0,
		BeginPackage["EDA`Bundles`", 
				"EDA`Common`",
        		"EDA`FindPeaks`",
        		"EDA`FindFit`",
				"EDA`LinearFit`",
				"EDA`Utilities`",
        		"Utilities`FilterOptions`"
		],
	True,
		BeginPackage["EDA`Bundles`", 
				"EDA`Common`",
        		"EDA`FindPeaks`",
        		"EDA`FindFit`",
				"EDA`LinearFit`",
				"EDA`Utilities`"
		]
];

Unprotect[EDALogListPlot, EDALogLogListPlot, FitExponent, FitPeaks]

If[ $VersionNumber < 6.0,
	EDALogListPlot::usage =
		"EDALogListPlot[data] is a front end to LogListPlot from the standard
		Graphics`Graphics` package. It takes data in the usual EDA format,
		reformats it, and passes it to LogListPlot.";
	EDALogLogListPlot::usage =
		"EDALogLogListPlot[data] is a front end to LogLogListPlot from the 
		standard Graphics`Graphics` package. It takes data in the usual EDA
		format, reformats it, and passes it to LogLogListPlot.",
(* else *)
	EDALogListPlot::usage =
		"EDALogListPlot[data] is a front end to ListLogPlot. 
		It takes data in the usual EDA format,
		reformats it, and passes it to ListLogPlot.";
	EDALogLogListPlot::usage =
		"EDALogLogListPlot[data] is a front end to ListLogLogPlot.
		It takes data in the usual EDA format, reformats it, and
		passes it to ListLogLogPlot."
];

EDALogListPlot::negativey =
	"The minimum value of the dependent variable is not a positive number."
EDALogLogListPlot::negativex =
	"The minimum value of the independent variable is not a positive number."

FitExponent::usage = "FitExponent[data] fits data to the form
	y = A Exp[B*x]. Internally, it uses the EDA function LinearFit
	to estimate A and B by fitting Log[y] vs x to a straight line.
	Those estimates are then used by the EDA function EDAFindFit to do
	a fit to the exponential formula. Any options given to FitExponent
	are passed to EDAFindFit, but not to LinearFit."

A::usage = "A is the amplitude returned by FitExponent which fits data
	to the form y = A Exp[B*x]."

B::usage = "B is the decay constant returned by FitExponent which fits data
	to the form y = A Exp[B*x]."

FitPeaks::usage = "FitPeaks[data] takes data and fits it to peak functions.
	The default peak shape is a Gaussian, but may be controlled by a
	PeakShape option.  The data is expected to be in the format expected 
	by the Experimental Data Analyst (EDA) package. Internally, FitPeaks
	uses the EDA routines FindPeaks and EDAFindFit."

PeakShape::usage = "PeakShape is an option of FitPeaks that controls the
	type of peak shape is being used in the fit. The default is Gaussian,
	but other allowed values are BreitWigner and Lorentzian."

Options[FitPeaks] = {
	PeakShape -> Gaussian, EDAShowProgress -> False
};

FitPeaks::badshape = "A PeakShape of `1` is not valid. Valid values are
	Gaussian, BreitWigner or Lorentzian."
FitPeaks::nopeaks = "FindPeaks failed to find any peaks."

Begin["`Private`"]

EDALogListPlot[data_, opts___?OptionQ] := Module[
	{ n, nvars, x, errx, y, erry, work, optsLogListPlot },

	{n,nvars,x,errx,y,erry} = Check[UnpackData[data],
		Return[$Failed];
	];

	If[Min[y] <= 0.,
		Message[EDALogListPlot::negativey];
		Return[$Failed];
	];

	work = Transpose[{x,y}];

	If[ $VersionNumber < 6.0,
		optsLogListPlot = FilterOptions[LogListPlot, opts];
		LogListPlot[work, optsLogListPlot], 
	(* else *)
		optsLogListPlot = FilterOptions[ListLogPlot, opts];
		ListLogPlot[work, optsLogListPlot]
	]
]

EDALogLogListPlot[data_, opts___?OptionQ] := Module[
	{ n, nvars, x, errx, y, erry, work, optsLogLogListPlot },

	{n,nvars,x,errx,y,erry} = Check[UnpackData[data],
		Return[$Failed];
	];

	If[Min[y] <= 0.,
		Message[EDALogListPlot::negativey];
		Return[$Failed];
	];
	If[Min[x] <= 0.,
		Message[EDALogLogListPlot::negativex];
		Return[$Failed]
	];

	work = Transpose[{x,y}];

	If[ $VersionNumber < 6.0,
		optsLogLogListPlot = FilterOptions[LogLogListPlot, opts];
		LogLogListPlot[work, optsLogLogListPlot], 
	(* else *)
		optsLogLogListPlot = FilterOptions[ListLogLogPlot, opts];
		ListLogLogPlot[work, optsLogLogListPlot]
	]
]

FitExponent[data_, opts___?OptionQ] := Module[
	{
		n, nvars, x, errx, y, erry,	(* Returned from UnpackData *)
		guess,						(* the fit returned from LinearFit *)
		a,							(* parameter for fits *)
		valA, valB,					(* values of A and B from linearised fit *)
		ind,						(* placeholder independent variable *)
		work						(* working dataset to linearise *)
	},

	{n, nvars, x, errx, y, erry} = Check[UnpackData[data],
		Return[$Failed];
	];

	y = Log[y];
	work =  Transpose[ {x,y} ];

	guess = LinearFit[work, {0,1}, a,
		ReturnErrors -> False,
		ShowFit -> False ];

	valA = Exp[ a[0] /. guess ];
	valB = a[1] /. guess;

	EDAFindFit[data, A Exp[B*ind], ind, {{A, valA}, {B, valB}}, opts] 

]

FitPeaks[data_, opts___?OptionQ] := Module[
	{
		guess,
		shape,

		optPeakShape,
		optShowProgress,

		optsEDAFindFit,
		optsFindPeaks
	
	},

	optShowProgress = EDAShowProgress /. {opts} /. Options[FitPeaks];
	optPeakShape = PeakShape /. {opts} /. Options[FitPeaks];
	Switch[optPeakShape,
		Gaussian,	Identity,
		BreitWigner,Identity,
		Lorentzian,	Identity,
		_, 			Message[FitPeaks::badshape, optPeakShape];
					Return[$Failed]
	];

	(*
	 * This is a hack of Utilities`FilterOptions` that deals with
	 * two programs, EDAFindFit and ShowFitResult in this case.
	 *)
	optsEDAFindFit = Sequence @@ Select[ Flatten[{opts}], 
		MemberQ[First /@ Join[ Options[EDAFindFit], Options[ShowFitResult]],
		First[#]]& ];

	optsFindPeaks = FilterOptions[FindPeaks, opts];

	If[optShowProgress,
		Print["Calling FindPeaks"];
	];

	Check[ guess = FindPeaks[data, optPeakShape, optsFindPeaks];,
		Return[$Failed]
	];

	If[ (PeaksFound /. guess) == 0 ,
		Message[FitPeaks::nopeaks];
		Return[$Failed]
	];


	If[optShowProgress,
		Print[	"Found ", PeaksFound /. guess, " peaks.\n",
				"Using model: ", Model /. guess, "\n",
				"Parameter estimates are: ", EDAParameters /. guess];
	];

	EDAFindFit[data, Model /. guess, IndependentVariable,
		EDAParameters /. guess, optsEDAFindFit]

]

End[]

Protect[EDALogListPlot, EDALogLogListPlot, FitExponent, FitPeaks];

EndPackage[]

