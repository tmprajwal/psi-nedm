(* $Id: EDAGraphics.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: EDA Graphics*)

(* :Context: EDA`EDAGraphics` *)

(* :Author: David M. Harrison, John M. Novak *)

(* :Summary:
	Miscellaneous graphics programs for Experimental
	Data Analyst.
*)

(* :Copyright: Copyright 1995-2013, Wolfram Research, Inc.*)

(* :Package Version: 1.3 *)

(* :Mathematica Version: 9.0 *)

(* :History:
	Version 0.9: David Harrison (May 1995).
	Version 1.0: David Harrison and John Novak (June 1996).
	Version 1.1: David Harrison (December 2003).
	Version 1.2: David Harrison (May 2009).
	Version 1.3: David Harrison (February 2013).
*)

(* :Keywords:
	BoxPlot
	EDAHistogram (* Obsolete for Mathematica 6.0 and later *)
	EDAListPlot
*)

(* :Sources:
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

(*
 * Graphics routines for the EDA library.  Contents:
 *	BoxPlot
	EDAHistogram (* Obsolete for Mathematica 6.0 and later *)
 *	EDAListPlot
 *)

(*
 * As of Mathematica 9.0 Utilities`FilterOptions` is obsolete. The
 * replacement FilterRules[] interacts badly with much EDA code.
 * FilterOptions is now in EDA`Utilities`
 *)

Which[
	$VersionNumber < 6.0,
		BeginPackage["EDA`EDAGraphics`",
			"Graphics`Graphics`", (* Used by EDAHistogram *)
			"Graphics`MultipleListPlot`", (* Used by EDAListPlot *)
			"Statistics`DescriptiveStatistics`", (* Used by BoxPlot *)
    		"Utilities`FilterOptions`",
			"EDA`Utilities`"
		],
	$VersionNumber < 9.0,
		BeginPackage["EDA`EDAGraphics`",
			"ErrorBarPlots`",
    		"Utilities`FilterOptions`",
			"EDA`Utilities`"
		],
	True,
		BeginPackage["EDA`EDAGraphics`",
			"ErrorBarPlots`",
			"EDA`Utilities`"
		]
];

Unprotect[ BoxPlot, EDAHistogram, EDAListPlot]

BoxPlot::usage =
	"BoxPlot[{a1, a2, ..., aN}] displays statistics about {a1, a2, ..., aN}.
	The \"box\" spans the inner two quartiles, with an interior line
	at the median. Lines extend from the box to the minimum/maximum
	values which are greater/lesser than the cutoffs, respectively.
	The cutoffs are the lower/upper quartiles minus/plus 3/2 of the
	height of the box, respectively. Data outside the cutoffs are
	represented by points. BoxPlot[{{a1, ..., aN}, {b1, ..., bM},...}]
	displays boxplots for a, b, ... side by side. Optional final
	arguments to BoxPlot[] are passed to Show[]."

If[ $VersionNumber < 6.0,
	EDAHistogram::usage =
		"EDAHistogram[ data, nbins, nmin, nmax ] displays a histogram of data
		in nbins bins for data greater than or equal to nmin and less than or
		equal to nmax. An optional final argument \"ShowAll -> True\" will
		display data for which the value is less than nmin and data for which
		the value is greater than nmax. Otherwise, underflows and overflows are
		dropped. Other optional final arguments are passed to 
		GeneralizedBarChart[] unevaluated. EDAHistogram[ data, nbins ] 
		displays a histogram with nmin
		set to the minimum value in data and nmax set to the maximum value in 
		data.  EDAHistogram[data] set nbins equal to 50 if there are more 
		than 50 data points in data or otherwise the number of data points 
		divided by 2.";
	ShowAll::usage =
		"ShowAll is an option of EDAHistogram. If set to True, overflows
		and underflows will be displayed. Otherwise, they will not.";
	EDAListPlot::usage =
		"EDAListPlot[data] produces a plot of data. It determines 
		if there are errors associated with the data points, and
		if so displays them also. EDAListPlot[data1, data2, ... ]
		displays multiple data sets. Internally, EDAListPlot uses
		the built-in ListPlot and the standard Graphics`MultipleListPlot'
		package.";
	ListPlotThreshold::usage =
		"ListPlotThreshold is an option for EDAListPlot. If exactly one
		data set is input to EDAListPlot containing no errors in
		the data points and the number of datapoints is greater than
		ListPlotThreshold, then ListPlot is used to display the data.
		Otherwise, MultipleListPlot is used.";,
(* else *)
	EDAHistogram::usage = "EDAHistogram is obsolete. Use the Histogram[]
		function in the standard Histograms` package instead";
	EDAListPlot::usage =
		"EDAListPlot[data] produces a plot of data. It determines 
		if there are errors associated with the data points, and
		if so displays them also. EDAListPlot[data1, data2, ... ]
		displays multiple data sets. Internally, EDAListPlot uses
		the built-in ListPlot and ErrorListPlot[] from the
		standard ErrorBarPlots` package.";
];

EDAListPlot::baddata = "The input data seems to be scrambled."
EDAListPlot::nonpositive =
	"An error specified in the data was not a positive number."
EDAListPlot::mixedform = "The datasets are not of the same type."

(* set up the options *)
Options[BoxPlot] = Options[Graphics];

SetOptions[BoxPlot, Frame -> True, PlotRange -> All];

If[ $VersionNumber < 6.0,
	Options[EDAHistogram] = Join[{ShowAll -> False},
		Options[GeneralizedBarChart]];
	Options[EDAListPlot] = Union[{ListPlotThreshold -> 100},
		Options[ListPlot], Options[MultipleListPlot]];,
(* else *)
	Options[EDAListPlot] = Union[ Options[ListPlot], Options[ErrorListPlot]];
];


Begin["`Private`"]


(* catch a single data set *)
BoxPlot[data_?(VectorQ[#, NumericQ]&), opts___?OptionQ] :=
    BoxPlot[{data}, opts]

BoxPlot[data:{_?(VectorQ[#, NumericQ]&)..}, opts___?OptionQ] :=
    Module[
	{
		lowquarts, 	(* 0.25 quartiles *)
		highquarts, 	(* 0.75 quartiles *)
		meds, 		(* medians *)
		mincutoffs, 	(* lower cutoffs *)
		maxcutoffs,	(* upper cutoffs *)
       	pts, 		(* outliers *)
		newdata, 	(* data with outliers removed *)
		boxes		(* the actual "boxes" *)
	},

      (* compute quartiles and medians *)
	{lowquarts, medians, highquarts} =
	    Transpose[Map[Quartiles, data]];

      (* compute cutoffs from quartiles *)
	maxcutoffs = (5 highquarts - 3 lowquarts)/2;
	mincutoffs = (5 lowquarts - 3 highquarts)/2;

      (* separate outliers from data *)
	{pts, newdata} = Transpose[
	    MapThread[outlierseparate, {data, mincutoffs, maxcutoffs}]
	];

      (* build graphics primitives *)
	pts = MapIndexed[Point[{First[#2], #1}]&, pts, {2}];
	boxes = MapThread[makebox,
	    {Range[Length[data]],
	     lowquarts,
	     highquarts,
	     medians,
	     Map[Min, newdata],
	     Map[Max, newdata]}
	];

	Show[Graphics[{pts, boxes}],
	    FilterOptions[Graphics, ##] & @@
	        Flatten[{opts, Options[BoxPlot]}]
	]
]

(* separate a data set into outliers and the rest of the data *)
outlierseparate[data_, min_, max_] :=
    Module[{outliers},
	outliers = Select[data, (# < min || # > max)& ];
	{outliers, Complement[data, outliers]}
    ]

(* make a box and the lines, based on an index, the low quartile,
   the high quartile, the median, the min for the data set, and the
   max for the data set *)
makebox[i_, lq_, hq_, med_, min_, max_] :=
    Module[
	{
		dimple = 0.10,
		ip = i + 0.25,
		im = i - 0.25
	},

	{
		Line[{{i, lq}, {i, min}, {im, min}, {ip, min}}],
	 	Line[{{im + dimple, med}, {ip - dimple, med}}],
	 	Line[{{i, hq}, {i, max}, {im, max}, {ip, max}}],
	 	Line[{{im, lq}, {ip, lq}, {ip - dimple, med},
			{ip, hq}, {im, hq}, {im + dimple, med}, {im, lq}}]
	}
    ]

(* 
 * Despite the indedentation, EDAHistogam is only defined for
 * Mathematica < 6.0. For later versions it prints a warning.
 *)
If[ $VersionNumber < 6.0,
EDAHistogram[data_?VectorQ,opts___Rule] := Module[ {min,max,bins},
	min = Min[data];
	max = Max[data];
	If[Length[data] > 50,
	        bins = 50,
	(* else *)
	        bins = Floor[Length[data]/2]
	];

	EDAHistogram[data,bins,min,max, opts]
];

EDAHistogram[data_?VectorQ, nbins_Integer,opts___Rule] := Module[ {min, max},
	min = Min[data];
	max = Max[data];
	EDAHistogram[ data, nbins, min, max, opts]
];

EDAHistogram[data_?VectorQ, nbins_Integer, min_?NumberQ, max_?NumberQ,
	opts___Rule] /; (min < max) := Module [
	{
		binlist,
	        binwidth,
	        label,
			oldRecursionLimit,
	        result,
	        showOption
	},

	showOption = ShowAll /. {opts} /. Options[EDAHistogram];

	binwidth = (max - min)/nbins;

	(*
	 * Due to rounding errors, it is possible for a data point
	 * to be in bin number nbins + 1.  Thus, we use ReplacePart[]
	 * to set those elements to nbins.
	 * The Partition[] is to put the binlist into the form MapAt[]
	 * requires: { {x1}, {x2}, ... }.
	 *)
	binlist = Floor[
		(Select[data, (# >= min && # <= max)&] - min)/binwidth] + 1;
	binlist = ReplacePart[binlist, nbins, Position[binlist,nbins + 1]];
	binlist = Partition[binlist, 1];

	(*
 	 * Form histogram into "result".
	 *)
	result = Table[ 0, {nbins}];

	oldRecursionLimit = $RecursionLimit;
	If[Length[binlist] >= $RecursionLimit,
		$RecursionLimit = Length[binlist] + 1;
	];
	result = MapAt[ (# + 1)&, result, binlist];
	$RecursionLimit = oldRecursionLimit;

	(* Put in under/overflows if ShowAll -> True *)
	If[ showOption === True,
		result = Prepend[ result,
			Length[Select[data, (# < min)&]]];
		result = Append[result,
			Length[Select[data, (# > max)&]]];
        result = Transpose[{
                   Drop[Range[min-binwidth, max+binwidth, binwidth] +
                         binwidth/2, -1],
                   result,
                   Table[binwidth, {Length[result]}]
                  }],
    (*else*)
        result = Transpose[{
                    Drop[Range[min, max, binwidth] + binwidth/2, -1],
                    result,
                    Table[binwidth, {Length[result]}]
                 }]
	];

	GeneralizedBarChart[result, opts,
        Ticks -> {{{min, ToString[min] <> " --"},
                   {max, "-- " <> ToString[max]}},
                  Automatic},
        AxesOrigin -> {If[ TrueQ[showOption],
                           min - binwidth,
                           min - binwidth/4], 0},
        PlotRange -> All]
];,
(* else *)
	EDAHistogram[data___] := Print[
		"EDAHistogram is obsolete. Use Histogram instead"];
]


EDAListPlot[data__, opts___Rule] := Module[
	{
		a1, a2, a3, a4,		(* arguments to ErrorBar *)
		i,
		maxnvars = 0,
		minnvars = 5,
		ndatasets,
		npoints,
		nvars,
		optListPlotThreshold,
		work		(* where we will put the data to be plotted *)
	},

	(*
	 * As of Mathematica 6.0, we no longer use MultpleListPlot
	 * in any circumstances, so the ListPlotThreshold option
	 * is obsolete.
	 *)
	If[ $VersionNumber < 6.0,
		optListPlotThreshold = ListPlotThreshold /. {opts} /. 
			Options[EDAListPlot];
	];

	ndatasets = Length[{data}];
	(* allocate storage for work *)
	work = Table[0, {ndatasets}];

	For[i = 1, i <= ndatasets, i++,
		{npoints,nvars} = DataParameters[ Part[{data},i] ];
		If[nvars > maxnvars,
			maxnvars = nvars
		];
		If[nvars < minnvars,
			minnvars = nvars
		];
		If[npoints < 1,
			Message[EDAListPlot::baddata];
			Return[$Failed];
		];
		Switch[nvars,
			1,	work[[i]] = Part[{data}, i],
			2,	work[[i]] = Part[{data}, i],
			3,  work[[i]] = Apply[ {{#1, #2}, ErrorBar[{-#3,#3}]}&,
					Flatten /@ Part[{data}, i], 1];
				(* Errors must be positive numbers *)
				If[ Length[ Cases[ Last /@ work[[i]], 
						ErrorBar[{a1_, a2_}] /; a2 < 0]] != 0,
							Message[EDAListPlot::nonpositive];
							Return[$Failed]
				],
			4,  work[[i]] = Apply[ {{#1, #3}, 
					ErrorBar[{-#2,#2}, {-#4,#4}]}&,
					Flatten /@ Part[{data}, i], 1];
				If[ Length[ Cases[ Last /@ work[[i]], 
						ErrorBar[{a1_, a2_},{a3_,a4_}] /;
							(a2 < 0 || a4 < 0) ]] != 0,
								Message[EDAListPlot::nonpositive];
								Return[$Failed]
				],

			_,  Message[EDAListPlot::baddata];
				Return[$Failed];
		];
	];

	If[ $VersionNumber < 6.0,
		If[ndatasets == 1 && maxnvars < 3 && npoints > optListPlotThreshold,
			ListPlot[Flatten[work,1], FilterOptions[ListPlot, opts]],
		(* else *)
			If[maxnvars < 3,
				MultipleListPlot[Sequence @@ work,
					FilterOptions[MultipleListPlot, opts]],
			(* else *)
				MultipleListPlot[Sequence @@ work,
					FilterOptions[MultipleListPlot, opts],
					SymbolShape -> PlotSymbol[Box, 1, Filled -> False] ]
			]
		],
	(* else *)
		(* No I don't know why Sequence has changed *)
		If[ maxnvars < 3,
			ListPlot[Sequence[work], FilterOptions[ListPlot, opts]],
		(* else *)
			(* 
			 * ErrorListPlot cannot handle datasets where one has errors
			 * and the other does not.
			 *)
			If[ minnvars < 3,
				Message[EDAListPlot::mixedform];
				Return[$Failed]
			];
			ErrorListPlot[Sequence[work], FilterOptions[ErrorListPlot, opts]]
		]
	]
]

End[]

SetAttributes[BoxPlot, {ReadProtected} ]
SetAttributes[EDAHistogram, {ReadProtected} ]
SetAttributes[EDAListPlot, {ReadProtected} ]

Protect[ BoxPlot, EDAHistogram, EDAListPlot]

EndPackage[]


