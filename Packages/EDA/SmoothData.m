(* $Id: SmoothData.m,v 6.1 2009/05/23 18:04:45 harrison Exp $ *)

(* :Title: Smoothing data and nonparametric fitting *)

(* :Context: EDA`SmoothData` *)

(* :Author: David M. Harrison *)

(* :Summary:
	SmoothData - smooth a data set.
	LoessFit - do a nonparametric loess fit of a data set.
	FillData - fill in missing data.
*)

(* :Copyright: Copyright 1995 - 2009, Wolfram Research, Inc.*)

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
	William S. Cleveland, "Visualising Data" (AT&T Bell Labs, 1993).
	K.S. Riedel and A. Sidorenko, Comp. in Physics 8, (1994) 402.
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

(*
 * Contents:
 *  FillData
 *	LoessFit
 *	SmoothData
 *	SmoothTestData
 *
 * Internal programs:
 *  columnInterpolate
 *	mean
 *	median
 *	monotonicQ
 *	nearestNeighbor
 *	rowInterpolate
 *	sdParition
 *	smoothFill
 *	smoothFillAll
 *	weightedaverageSmoothData
 *	weightedfourierSmoothData
 *)

(* 
 * References:
 *
 *	William S. Cleveland, "Visualizing Data", (AT&T Bell
 *	Laboratories, 1993), passim.
 *
 *	K.S. Riedel and A. Sidorenko, Computers in Physics 8,
 *	(1994), pg. 402.
 *
 *  Brand Fortner, "The Data Handbook" (SpyGlass, 1992), 
 *  Chapter 11.
 *
 *)

BeginPackage["EDA`SmoothData`",
	"EDA`Common`",
	"EDA`Utilities`"
]

Unprotect[FillData, LoessFit, SmoothData, SmoothTestData];

FillData::usage =
	"FillData[data] fills in missing values in the matrix of values \"data\".
	The default method is EDANearestNeighbor, and the routine determines which
	values are to be filled by the definition of MissingIs, which by default
	is 0."
	
Options[FillData] = {
	MaximumIterations -> 1,
	Method -> EDANearestNeighbor,
	MissingIs -> 0,
	StructureSize -> Infinity

}

EDANearestNeighbor::usage =
	"EDANearestNeighbor is the default method for FillData. This method fills
	in missing values with duplicates of an adjacent non-missing neighbor.
	Other methods are ColumnInterpolation, RowInterpolation, SmoothFill, and
	SmoothFillAll."
	
ColumnInterpolation::usage =
	"ColumnInterpolation is an optional method for FillData. This method
	fills in missing values with interpolations from non-missing values in
	the same column."
	
RowInterpolation::usage =
	"RowInterpolation is an optional method for FillData. This method
	fills in missing values with interpolations from non-missing values in
	the same row."
		
SmoothFill::usage =
	"SmoothFill is an optional method for FillData. This method fills in
	missing values with the average of the 8 adjacent neighboring positions."
	
SmoothFillAll::usage =
	"SmoothFillAll is an optional method for FillData. This method replaces
	all values in data with the average of the non-missing values in the 3x3
	matrix surrounding the data point."
	
MissingIs::usage =
	"MissingIs defines the contents of cells in a matrix of data that 
	FillData assumes are missing data."

StructureSize::usage =
	"StructureSize is an option for FillData when the method is 
	RowInterpolation or ColumnInterpolation that specifies the size of
	structures in the data. If a block of missing values is greater than
	the value of StructureSize, that block is not interpolated."
	
FillData::badopt = "Option `1` is not known."
FillData::nomethod =
	"Method `1` is not valid for FillData."

LoessFit::usage =
	"LoessFit[data, lambda] performs a loess fit on data.
	The parameter lambda specifies the degree of the polynomial used
	in the fits. Lambda must be 1, 2, or 3. Explicit errors in
	data are ignored. The data must contain at least 4 points for
	lambda = 1, and more points than 4 for larger lambda."

EDAAlpha::usage =
	"EDAAlpha is an option for LoessFit that selects the number of points
	used in each subset of the data. Automatic (the default) uses
	2*Floor[Sqrt[Length[data]]] + 1 points. Otherwise, the points are
	equal to the value of EDAAlpha times the number of data points in the
	data, rounded to the nearest odd integer. EDAAlpha should be a positive
	number. Note that the Points option provides another way
	to choose the points. The EDAAlpha option is used to make LoessFit
	more notationally consistent with William S. Cleveland, \"Visualizing
	Data\" (AT&T Bell Laboratories, 1993)."

Options[LoessFit] = {
	EDAAlpha -> Automatic,
	Points -> Automatic,
	EDAShowProgress -> False
}

LoessFit::adjustpoints =
	"Warning: setting points to `1`."
LoessFit::badopt = "Option `1` is not known."
LoessFit::badpoints = 
	"Cannot use `1` points for LoessFit."
LoessFit::dataformat =
	"The data format appears scrambled."
LoessFit::dof =
	"Using `1` points with lambda = `2` would give no degrees
	of freedom."
LoessFit::noralpha =
	"Either the number of data points or alpha are inconsistent."
	
SmoothData::usage =
	"SmoothData[data] smooths data. A Method option allows various
	methods to be used: EDASimple (the default), SimpleMedian,
	WeightedAverage, or WeightedFourier. If 'data' contains
	explicit errors they are ignored, and SmoothData returns
	either a set of y values or {x, y} pairs."
EDASimple::usage =
	"EDASimple is the default method used by SmoothData, in which each
	data point has the value of the dependent variable replaced by
	an average of it and its nearest neighbors."
SimpleMedian::usage =
	"SimpleMedian is a method for SmoothData. When this method
	is chosen, a local median of the data is used to smooth the data."
WeightedAverage::usage =
	"WeightedAverage is a type of method for SmoothData. When this
	method is chosen, the data is weighted with a parabola whose
	maximum is 1.5 at the center of the set of points."
WeightedFourier::usage =
	"WeightedFourier is a type of method for SmoothData. The Fourier
	transform of the data is weighted with a Gaussian centered at zero
	frequency, and the inverse Fourier transform of the result is taken."
Points::usage =
	"Points is an option for SmoothData and LoessFit that specifies the
	number of points to be used in smoothing. If set to Automatic (the
	default), the number of points is 2*Floor[Sqrt[Length[data]]] + 1.
	Otherwise, the value is used for the number of points. For both programs,
	the number of points is the size of each subset, except for the
	WeightedFourier method of SmoothData, in which case it is the
	the standard deviation squared of the Gaussian weighting.
	The number of points should be an odd number. For SmoothData, its
	value should be less than the number of data points."

SmoothData::adjustpoints =
	"Warning: setting points to `1`."
SmoothData::badopt = "Option `1` is not known."
SmoothData::badpoints = "Specified number of points of `1` is incorrect."
SmoothData::badmethod = "Method -> `1` not recognized."
SmoothData::bigsigma =
	"Warning: the standard deviation of the weight function is very large."
SmoothData::dataformat =
	"The data format appears scrambled."
SmoothData::errorsindata =
	"Warning: the errors in the data are being ignored."
SmoothData::notmonotonic =
	"Warning: the independent variable is not monotonic. Results may
	have no meaning."
SmoothData::toomanyvars =
	"The data set appears to have `1` variables. Cannot proceed."

Options[SmoothData] = {
	Method -> EDASimple,
	Points -> Automatic
}

SmoothTestData::usage = 
	"SmoothTestData[number, stddev] generates number data points which are
	of the form Sin[4Pi t^2] for t between 0 and 1, with a Gaussian noise
	of standard deviation stddev added. If the option ExtraSignal is set
	to True, a sine wave of period 0.05 and amplitude 0.5 is added to the
	chirp between t = 0.4 and t = 0.45."
ExtraSignal::usage =
	"ExtraSignal is an option for SmoothTestData. If set to True, a sine 
	wave of period 0.05 and amplitude 0.5 is added to the chirp 
	between t = 0.4 and t = 0.45."

Options[SmoothTestData] = {ExtraSignal -> False}

Begin["`Private`"]

FillData[data_?MatrixQ, opts___?OptionQ] := Module[
	{
		i,					(* dummy variable *)
		iter, workdata,
		
		optMaximumIterations,
		optMethod,
		optMissingIs,
		optStructureSize
	},

	i = Complement[ First /@ {opts},
				First /@ Options[FillData] ];
	If[Length[i] != 0,
		Message[FillData::badopt, #]& /@ i;
		Return[$Failed]
	];


	optMaximumIterations = MaximumIterations /. {opts} /. Options[FillData];
	optMethod = Method /. {opts} /. Options[FillData];
	optMissingIs = MissingIs /. {opts} /. Options[FillData];
	optStructureSize = StructureSize /. {opts} /. Options[FillData];
	
	workdata = data;

	(*
	 * Mathematica is now (V4) making a distinction between integers
	 * and floats for operations like Count. The following uses N
	 * as appropriate to get everything consistent.
	 *)
	If[NumberQ[optMissingIs],
		optMissingIs = N[optMissingIs];
		workdata = N[workdata];
	];
	
	For[iter = 1, iter <= optMaximumIterations, iter++,

		If[Count[Flatten[workdata], optMissingIs] == 0,
			Break[]
		];

		Switch[ optMethod,
		
			ColumnInterpolation,
				workdata = columnInterpolate[workdata, optMissingIs,
					optStructureSize ],
				
			EDANearestNeighbor,
				workdata = nearestNeighbor[workdata, optMissingIs ],
				
			RowInterpolation,
				workdata = rowInterpolate[workdata, optMissingIs,
					optStructureSize ],
			
			SmoothFill,
				workdata = smoothFill[workdata, optMissingIs ],
			
			SmoothFillAll,
				workdata = smoothFillAll[workdata, optMissingIs ],
			
			_,	Message[FillData::nomethod, optMethod];
				Return[$Failed];
			
		];
	];
	
	workdata
	
]

columnInterpolate[data_, missing_, size_ ] := Transpose[
	rowInterpolate[ Transpose[data], missing, size ]]

nearestNeighbor[data_, missing_] := Module[
	{
		i,j,k,	(* dummy variables *)
		posns,
		row, col,
		numrows, numcols,
		parts, added, workdata
	},
	
	numrows = Length[data];
	numcols = Length[First[data]];
	
	(* We will put the filled in terms in added.  This keeps a filled
	 * point from propagating down through the rows as it would if we
	 * simply changed values in workdata.
	 *)
	added = Table[0, {numrows + 2}, {numcols + 2}];
	
	workdata = surroundIt[data, missing, 1];
	For[row = 1, row <= numrows, row++,

		(* Find positions of missing data.  Since we have surrounded the
		 * data with missing values, posns will be {1,numcols + 2} if
		 * there is no missing data in the row.
		 *)
		posns = Flatten[Position[workdata[[row + 1]], missing]];
		If[ Length[posns] == 2,
			Continue[]
		];
		posns = Take[ posns, {2,-2}];
		(* Now we form the 3x3 matrices with the missing values in
		 * the center and flatten them.
		 *)	
		parts = Take[workdata, {row, row + 2} ];
		parts = Transpose[parts];
		parts = Take[ parts, {# - 1, # + 1}]& /@ posns;
		parts = Flatten /@ parts;
	
		(* Drop the missing values *)
		parts = DeleteCases[#, i_ /; i == missing]& /@ parts;
		If[ Union[parts] === {{}},
			Continue[];
		];
		
		(* Remove any empty parts from both parts and posns *)
		{parts,posns} = Transpose[DeleteCases[Transpose[{parts,posns}], 
			{i_, j_} /; Length[i] == 0] ];

		(* The first value left in parts will replace the missing one. *)
		i = First /@ parts;
		j = Length[i];
		For[k = 1, k <= j, k++,
			added = ReplacePart[added, i[[k]],
				{row + 1, posns[[k]] }	];
		];
		
	];
	
	If[missing != 0,
		posns = Position[workdata, missing];
		workdata = ReplacePart[workdata, 0, posns];
	];
	
	workdata = workdata + added;	
	workdata = Take[workdata, {2,-2}];
	workdata = Transpose[Take[Transpose[workdata],{2,-2}]];
	
	workdata//N
	
]

rowInterpolate[data_, missing_, size_ ] := Module[
	{
		i,j,k,
		
		intOrder,
		first, last,
		numtofill,
		posnsMissing, posnsFilled,
		row, numrows, numcols,
		numToInterpolate, skipFills,
		tofill,
		workdata, workrow
	},
	
	numrows = Length[data];
	numcols = Length[First[data]];
	
	workdata = data;
	
	For[row = 1, row <= numrows, row++,
		workrow = workdata[[row]];
			
		posnsMissing = Flatten[Position[workrow, missing]];
		If[Length[posnsMissing] == 0 || Length[posnsMissing] == numcols,
			Continue[]
		];
		
		posnsFilled = Complement[ Table[i, {i,numcols}], posnsMissing];
		tofill = Partition[posnsFilled, 2, 1];

		(* We only fill when there is a missing value *)
		tofill = DeleteCases[tofill, {i_,j_} /; j - i == 1 ];
		If[Length[tofill] == 0,
			Continue[]
		];
		
		(* Now we assemble all the ranges in tofill which have
		 * too many missing values.  We will delete these from
		 * the list of posnsMissing, so they will not be filled
		 * in by the Interpolation.  We assume that interpolating
		 * across these ranges is still reasonable since Interpolation
		 * is forced to go through every data point.
		 *)
		skipFills = Cases[ tofill, {i_, j_} /; j - i > size ];
		j = Length[skipFills];
		For[ i = 1, i <= j, i++,
			posnsMissing = DeleteCases[posnsMissing, 
				k_ /;	k > First[ skipFills[[i]] ] && 
						k < Last[ skipFills[[i]] ] ];
		];

		(* Simplest case - interpolate everything and then go on to
		 * the next row. 
		 *)
		first = First[posnsFilled];
		last = Last[posnsFilled];
		i = Part[workrow, posnsFilled];
		i = Transpose[ {posnsFilled, i} ];

		(* If not enough data points for default interpolation,
		 * we will have to reduce the order.
		 *)
		intOrder = 3;
		If[Length[i] <= 3,
			intOrder = Length[i] - 1;
		];
		j = Interpolation[i, InterpolationOrder -> intOrder ];

		(* 
		 * Drop missing positions outside the range of the
		 * interpolation.
			*)
		posnsMissing = DeleteCases[ posnsMissing, 
			i_ /; i < first || i > last ];
		k = Length[posnsMissing];
			
		(* Construct the interpolated values and put in workrow *)
		For[i = 1, i <= k, i++,
			workrow[[ posnsMissing[[i]] ]] = j[ posnsMissing[[i]] ];
		];
		workdata[[row]] = workrow;

	];
	
	workdata//N
]

smoothFill[data_, missing_] := Module[
	{
		i,j,k,	(* dummy variables *)
		posns,
		row, col,
		numrows, numcols,
		parts, added, workdata
	},
	
	numrows = Length[data];
	numcols = Length[First[data]];
	
	(* We will put the filled in terms in added.  This keeps a filled
	 * point from propagating down through the rows as it would if we
	 * simply changed values in workdata.
	 *)
	added = Table[0, {numrows + 2}, {numcols + 2}];
	
	workdata = surroundIt[data, missing, 1];

	For[row = 1, row <= numrows, row++,
		(* Find positions of missing data.  Since we have surrounded the
		 * data with missing values, posns will be {1,numcols + 2} if
		 * there is no missing data in the row.
		 *)
		posns = Flatten[Position[workdata[[row + 1]], missing]];
		If[ Length[posns] == 2,
			Continue[]
		];
		posns = Take[ posns, {2,-2}];
	
		(* Now we form the 3x3 matrices with the missing values in
		 * the center and flatten them.
		 *)	
		parts = Take[workdata, {row, row + 2} ];
		parts = Transpose[parts];
		parts = Take[ parts, {# - 1, # + 1}]& /@ posns;
		parts = Flatten /@ parts;
		
		(* Drop the missing values *)
		parts = DeleteCases[#, i_ /; i == missing]& /@ parts;
		If[ Union[parts] === {{}},
			Continue[];
		];
		
		(* Remove any empty parts from both parts and posns *)
		{parts,posns} = Transpose[DeleteCases[Transpose[{parts,posns}], 
			{i_, j_} /; Length[i] == 0] ];
			
		(* The mena of values left in parts will replace the missing one. *)
		i = mean /@ parts;
		j = Length[i];
		For[k = 1, k <= j, k++,
			added = ReplacePart[added, i[[k]],
				{row + 1, posns[[k]] }	];
		];
		
	];

	If[missing != 0,
		posns = Position[workdata, missing];
		workdata = ReplacePart[workdata, 0, posns];
	];
	
	workdata = workdata + added;	
	workdata = Take[workdata, {2,-2}];
	workdata = Transpose[Take[Transpose[workdata],{2,-2}]];
	
	workdata//N
	
]

smoothFillAll[data_, missing_] := Module[
	{
		i,j,k,	(* dummy variables *)
		posns,
		row, col,
		numrows, numcols,
		parts, answer, workdata
	},
	
	numrows = Length[data];
	numcols = Length[First[data]];
	
	(* Since we will be reconstructing every data point in this method,
	 * we put the result in answer.  First allocate space.
	 *)
	answer = Table[0, {numrows}, {numcols}];
	
	workdata = surroundIt[data, missing, 1];

	For[row = 1, row <= numrows, row++,
		(* Now we form the 3x3 matrices with the missing values in
		 * the center and flatten them.
		 *)	
		For[col = 1, col <= numcols, col++,
			parts = Take[workdata, {row, row + 2} ];
			parts = Transpose[parts];
			parts = Flatten[Take[ parts, {col, col + 2}]];
		
			(* Drop the missing values *)
			parts = DeleteCases[parts, i_ /; i == missing];
			
			(* 
			 * The mean of values left in parts will replace the missing 
			 * one unless all values are missing.
			  *)
			If[Length[parts] == 0,
				i = missing,
			(* else *)
				i = mean[parts];
			];
			answer[[row,col]] = i;
		];		
	];
	
	answer//N
	
]


(* 
 * Surround the data by n levels of missing values.  The reason for
 * setting it up this way and including an input argument `n' is that
 * I anticipate adding a WeightedAverage Method to FillData in a future
 * release.
 *)
surroundIt[data_, missing_, n_] := Module[
	{
		i,j,
		numrows, numcols,
		workdata = data
	},
	
	numrows = Length[data];
	numcols = Length[First[data]];
	
	j = Table[missing, {numcols}];
	For[i = 1, i <= n, i++,
		workdata = Insert[workdata,j,{ {1}, {-1} }];
	];
	j = Table[missing, {numrows + 2*n}];
	workdata = Transpose[workdata];
	For[i = 1, i <= n, i++,
		workdata = Insert[workdata, j, {{1},{-1}}];
	];
	workdata = Transpose[workdata];
	workdata
		
	
]



LoessFit[data_, lambda_?IntegerQ, opts___?OptionQ] /;
	Length[data] >= 4 && lambda >= 1 && lambda <= 3 := Module[

	{
		ind,					(* placeholder for independent var *)
		result,					(* from Fit *)

		i,j,k,					(* dummy variables *)
		max,
		n,
		nvars, 
		points,
		w,

		x,errx,y,erry,

		(* these are the subsets *)
		xpart, ypart, errypart,
		datapart,

		xweighted,		(* The weights *)
		ynew,			(* The answer *)

		optAlpha,
		optMethod,
		optPoints,
		optShowProgress
	},

	i = Complement[ First /@ {opts},
				First /@ Options[LoessFit] ];
	If[Length[i] != 0,
		Message[LoessFit::badopt, #]& /@ i;
		Return[$Failed]
	];

	optAlpha = EDAAlpha /. {opts} /. Options[LoessFit];
	optMethod = Method /. {opts} /. Options[LoessFit];
	optPoints = Points /. {opts} /. Options[LoessFit];
	optShowProgress = EDAShowProgress /. {opts} /. Options[LoessFit];

    Check[ n = UnpackData[data];,
        Return[$Failed]
    ];
    If[n === $Failed,
        Message[LoessFit::dataformat];
        Return[$Failed]
    ];
    {n,nvars,x,errx,y,erry} = n;

	If[optShowProgress,
		Print[n, " datapoints, each with ", nvars, " variables."];
	];

	(*
	 * Make sure points is odd, and do our best to make
	 * sure it is smaller than n.
	 *)
	If[optPoints === Automatic && optAlpha === Automatic,
		points = Round[ 2*Floor[Sqrt[n]] - 1//N ],
	(* else *)
		If[optPoints =!= Automatic,
			points = optPoints,
		(* else *)
			points = 2*Round[optAlpha * n/2] + 1;
		];
	];

	If[EvenQ[points],
		points++;
		Message[LoessFit::adjustpoints,points];
	];

	If[!IntegerQ[points],
		Message[LoessFit::badpoints, points];
		Return[$Failed];
	];

	(*
	 * Note: fit doesn't care if there are zero degrees of freedom.
	 * So 3 points will give partitions with 2 entries on the edges,
	 * which Fit will gladly fit to a line.  (Actually it will "fit"
	 * two points to a second order polynomial, but *we* know better!)
	 *)
	If[ points < (lambda + 2) ,
		Message[LoessFit::dof, points, lambda ];
		Return[$Failed]
	];

	If[optShowProgress,
		Print["Points = ", points];
	];

	xpart = sdPartition[x,points];
	ypart = sdPartition[y,points];

	If[optShowProgress,
		Print["Independent variable partitions: ", xpart];
	];

	If[optShowProgress,
		Print["Dependent variable partitions: ", ypart];
	];

	xweighted = tricubeWeight[Part[#,2],Part[#,1]]& /@
		Transpose[{x,xpart}];
	xweighted = ReplacePart[xweighted, $MinMachineNumber,
		Position[xweighted, 0.]];
	If[points > n,
		i = points / n;
		xweighted /= i;
	];
	ypart = xweighted ypart;

	If[optShowProgress,
		Print["Weights: ", xweighted];
	];

	If[optShowProgress,
		Print["Weighted y-values: ", ypart];
	];

	datapart = Transpose[{Part[#,1], Part[#,2]}]& /@
		Transpose[{xpart,ypart}];

	(* 
	 * This will put a fit for each group of data into result. 
	 *)
	Switch[lambda,
		1, result = Fit[#,{1,ind},ind]& /@ datapart,
		2, result = Fit[#,{1,ind,ind^2}, ind]& /@ datapart,
		3, result = Fit[#,{1,ind,ind^2,ind^3}, ind]& /@ datapart
	];

	(* 
	 * Since the number of groups = the number of data points,
	 * this will return a square matrix whose diagonal terms are
	 * the actual values.  The line after picks out those diagonals.
	 *)
	result = result /. ind -> x;
	ynew = Table[ result[[i,i]], {i,n}];

	If[optShowProgress,
		Print["Fit results: ", result];
	];

	If[optShowProgress,
		Print["New y-values: ", ynew];
	];

	If[nvars == 1,
		ynew,
	(* else *)
		Transpose[{x,ynew}]
	]

]

(* This is the weight used by LoessFit on the data subsets. *)
tricubeWeight[x_, x0_] := Module[
	{min, max, delta},

	min = Min[x];
	max = Max[x];
	delta = Max[Abs[(x - x0)]];

	(1 - Abs[(x - x0)/delta]^3)^3

]

SmoothData[data_, opts___Rule] := Module[
	{
		i,			(* dummy variable *)
		n,				(* length of data *)
		optMethod,
		optPoints,
		points,
		nvars,			(* number of variables per data point *)
		ret,			(* number of points returned *)
		x,y,errx,erry
	},

	i = Complement[ First /@ {opts},
				First /@ Options[SmoothData] ];
	If[Length[i] != 0,
		Message[SmoothData::badopt, #]& /@ i;
		Return[$Failed]
	];

	optMethod = Method /. {opts} /. Options[SmoothData];
	optPoints = Points /. {opts} /. Options[SmoothData];

    Check[ n = UnpackData[data];,
        Return[$Failed]
    ];
    If[n === $Failed,
        Message[SmoothData::dataformat];
        Return[$Failed]
    ];
    {n,nvars,x,errx,y,erry} = n;

	If[optPoints === Automatic,
		points = Round[ 2*Floor[Sqrt[n]] - 1//N ],
	(* else *)
		points = optPoints;
	];
	If[EvenQ[points],
		points++;
		Message[SmoothData::adjustpoints,points];
	];
	If[ !IntegerQ[points] || points < 3 || points >= n,
		Message[SmoothData::badpoints, points];
		Return[$Failed]
	];

	Switch[optMethod,
		EDASimple,
			y = mean /@ sdPartition[y,points],

		SimpleMedian,
			y = median /@ sdPartition[y,points],
			
		WeightedAverage,
			y = weightedaverageSmoothData[x,y,points],

		WeightedFourier, 
			If[!monotonicQ[x],
				Message[SmoothData::notmonotonic];
			];
			y = weightedfourierSmoothData[y,points],

		_,	Message[SmoothData::badmethod, optMethod];
			Return[$Failed]
	];

	If[nvars == 1,
		y,
	(* else *)
		Transpose[{x,y}]
	]

]

(*
 * This is a generalised Partition that returns sets
 * whose number is equal to Length[x].  It pads x at the beginning
 * and end with the placeholder `foo', partitions the result
 * and then deletes all the `foo's.  Thus, "interior" partitions
 * have length q, but at the edges they will be shorter.
 *)
sdPartition[x_, q_] := Module[
	{ foo, i },

	i = Floor[q/2];

	Delete[#, Position[#,foo]]& /@
		(Partition[
			Join[	Table[foo,{i}],
					x,
					Table[foo,{i}]
			] , q,1])
]

weightedaverageSmoothData[x_, y_, points_] := Module[
	{xpart, xweighted, ypart},

	xpart = sdPartition[x,points];
	ypart = sdPartition[y,points];
	xweighted = quadWeight[Part[#,2],Part[#,1]]& /@
		Transpose[{x,xpart}];
	Apply[Plus, (xweighted ypart), 1]
]

(*
 * Given a list of x values with a center value x0,
 * this returns a set of weights normalised so the
 * sum is equal to 1.
 *)
quadWeight[x_, x0_] := Module[
	{ a, min, max, delta },

	min = Min[x];
	max = Max[x];
	delta = Max[Abs[(x - x0)]];

	a = quadNorm[ delta, min, max, x0 ];
	First[a] (delta^2 - (x - x0)^2)

]

(*
 * This is the overall normalisation for quadWeight.
 *)
quadNorm[delta_, min_, max_, x0_] := Module[
	{},

	(*  The result was computed using this commented out code

	{a, xdummy},

	a /. Solve[	Integrate[ a*(delta^2 - (xdummy - x0)^2), 
			{xdummy, min,max} ] == 1, a]

	*)

	{3/(3*delta^2*max - max^3 - 3*delta^2*min + min^3 + 3*max^2*x0 - 
		 3*min^2*x0 - 3*max*x0^2 + 3*min*x0^2)}

]

weightedfourierSmoothData[data_, points_] := Module[
	{len, t, weight},

	len = Length[data];

	(*
	 * This check is purely heuristic, but probably useful.
	 * Because of aliasing, we will have the transform
	 * reflected about Length[data]/2.  We will weight both
	 * ends.  The check is basically looking for significant
	 * overlap between the two Gaussians.
	 *)
	If[ Chop[ Exp[ -(len/points)^2/2]//N, .00001] != 0,
		Message[SmoothData::bigsigma]
	];

	(* 
	 * We set up a double Gaussian, one centered on the left,
	 * the other on the right.
	 *)
	weight = Table[
		Exp[ -(t/points)^2/2] + Exp[ -((t-len)/points)^2/2],
		{t,1,len}];

	Re[ InverseFourier[ weight Fourier[data]]]
]

(*
 * These definitions of mean and median are copied from the
 * Statistics`DescriptiveStatistics` package.
 *)
mean[list_] := Apply[Plus, list] / Length[list]  /;
                VectorQ[list] && Length[list] > 0

median[list_] := Sort[list][[(Length[list]+1)/2]] /;
            VectorQ[list] && OddQ[Length[list]]

median[list_] :=
    Block[{s, n},
        s = Sort[list] ;
        n = Length[list] ;
        (s[[n/2]] + s[[n/2 + 1]]) / 2
    ] /; VectorQ[list] && EvenQ[Length[list]]

monotonicQ[data_] := Module[
	{
		i,
		len,
		delta
	},

	len = Length[data];
	delta = data[[2]] - data[[1]];
	data == Table[ data[[1]] + delta*i, {i,0,len-1}]
]

(*
 * This generates the test data used in the Notebook.
 *)
SmoothTestData[n_Integer, stddev_?NumberQ, opts___Rule] := Module[
	{noise, num, optExtraSignal, x},

	optExtraSignal = ExtraSignal /. {opts} /. Options[SmoothTestData];
	(*
	 * First Monte Carlo n noise points.  We assume a Gaussian
	 * distribution, and generate noise within 4 standard
	 * deviations of zero.
	 *)
	num = 0;
	noise = {};
	SeedRandom[12345]; (* so we always get the same result *)
	While[num < n,
		x = Random[Real, {-4 * stddev , 4 * stddev }];
		If[ Random[] <= Exp[ - (x/stddev)^2/2],
			noise = Append[noise, x];
			num++;
		];
	];

	(*
	 * Now construct the signal and add the noise.
	 *)
	If[ optExtraSignal === False,
		Table[ Sin[4Pi t^2], {t,1/num,1,1/num}] + noise //N,
	(* else *)
		Table[ Sin[4Pi t^2] + 
			If[ t > 0.4 && t < 0.45, 0.5 Sin[2Pi t/.05], 0],
			{t, 1/num, 1, 1/num}] + noise //N
	]
]

End[]

SetAttributes[FillData , {ReadProtected}];
SetAttributes[LoessFit , {ReadProtected}];
SetAttributes[SmoothData , {ReadProtected}];
SetAttributes[SmoothTestData , {ReadProtected}];

Protect[FillData, LoessFit, SmoothData, SmoothTestData];

EndPackage[]
