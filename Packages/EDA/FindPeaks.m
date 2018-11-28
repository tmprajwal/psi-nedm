(* $Id: FindPeaks.m,v 6.1 2009/05/22 19:02:01 harrison Exp $ *)

(* :Title: Finding and characterising peaks in a spctrum *)

(* :Context: EDA`FindPeaks` *)

(* :Author: David M. Harrison *)

(* :Summary:
	FindPeaks
*)

(* :Copyright: Copyright 1995 - 2003, Wolfram Research, Inc.*)

(* :Package Version: 1.1 *)

(* :Mathematica Version: 5.0 *)

(* :History:
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
*)

(* :Keywords:
*)

(* :Sources:
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

(* User accessible functions:
 *	FindPeaks[]
 *
 * Internal functions:
 *	doGaussian[]
 *  doLorentzian[]
 *	setBkgd[]
 *
 * User accesible names:
 *	MaximumPeaks, FindBkgd, Bkgd, Model, IndependentVariable, EDAParameters,
 *  Amplitude, CenterValue FWHM, PeakArea, PeakWidth, EDASigma.
 *
 *)

BeginPackage["EDA`FindPeaks`",
	"EDA`Common`",
	"EDA`EDAGraphics`",
	"EDA`FindFit`",		(* For defs of Gaussian, Lorentzian & BreitWigner *)
	"EDA`SmoothData`",
	"EDA`Utilities`"
]
	
Unprotect[FindPeaks]

FindPeaks::usage =
	"FindPeaks[data] finds peaks in \"data\" and returns Amplitude[i],
	CenterValue[i], and PeakWidth[i] for each of the i peaks found.
	FindPeaks[data, shape] finds peaks in \"data\" and returns a 
	model for the peaks assuming the shape of the peaks is given
	by \"shape\", and also returns initial parameter estimates as
	\"EDAParameters\". FindPeaks[data, None] is a synonym for FindPeaks[data].
	Other allowed values for \"shape\" are BreitWigner, Gaussian, or
	Lorentzian. For each of these the model and parameters may be
	passed to the EDAFindFit routine."

Options[FindPeaks] = {
	FindBkgd -> Automatic,
	MaximumPeaks -> 5,
	EDAShowProgress -> False
}

MaximumPeaks::usage =
	"MaximumPeaks is an option for FindPeaks that controls the maximum
	number of peaks in the data that it will attempt to find."

FindBkgd::usage =
	"FindBkgd is an option for FindPeaks. If set to Automatic (the
	default), a linear background will be used and returned provided
	such a background accounts for over 2% of the sum of all values of
	the dependent variable in the data. If set to None, no background
	term is used or returned by FindPeaks."

Bkgd::usage =
	"Bkgd is a symbol used by FindPeaks to represent the background in
	the spectrum. When a background is found, FindPeaks returns the
	symbol Bkgd[0] as the intercept and the symbol Bkgd[1] as the slope."

Model::usage =
	"Model is the model constructed by FindPeaks."

IndependentVariable::usage =
	"IndependentVariable is the symbol used by FindPeaks to represent
	the independent variable."

EDAParameters::usage =
	"EDAParameters is the symbol used by FindPeaks to represent the
	names and initial values of the parameters."

Amplitude::usage = 
	"Amplitude[i] is the amplitude of the ith peak found by FindPeaks."

CenterValue::usage = 
	"CenterValue[i] is the center of the ith peak found by FindPeaks."

FWHM::usage = 
	"FWHM[i] is the full width at half maximum of the ith peak found
	by FindPeaks."

PeakArea::usage =
	"PeakArea[i] is the area under the ith peak found by FindPeaks."

PeakWidth::usage =
	"PeakWidth[i] is the width of the ith peak found by FindPeaks.
	It is the width from the points where the second derivative is
	zero. For a Guassian shape, this corresponds to 2 times the
	standard deviation. For a Lorentzian shape, it corresponds to
	the full width at half maximum divided by Sqrt[3]."

PeaksFound::usage =
	"PeaksFound is the number of peaks found by FindPeaks."

EDASigma::usage =
	"EDASigma[i] is the standard deviation of the ith peak found by
	FindPeaks."

FindPeaks::ampldef = "The Amplitude[i] symbol is assigned a value."
FindPeaks::badopt = "Option `1` is not known."
FindPeaks::badshape = "Peak shape of `1` is not known."
FindPeaks::bkgd = "Bkgd must be Automatic or None, not `1`."
FindPeaks::bkgddef =
	"Either Bkgd[0] or Bkgd[1] are assigned a value."
FindPeaks::centerdef = "The CenterValue[i] symbol is assigned a value."
FindPeaks::dataformat = "The data format appears to be scrambled."
FindPeaks::fwhmdef = "The FWHM[i] symbol is assigned a value."
FindPeaks::maxpeaks =
	"Warning: the number of peaks may exceed the `1` found ones. The
	maximum number of peaks found can be controlled with the MaximumPeaks
	option."
FindPeaks::peakareadef = "The PeakArea[i] symbol is assigned a value."
FindPeaks::peakwidthdef = "The PeakWidth[i] symbol is assigned a value."
FindPeaks::sigmadef = "The EDASigma[i] symbol is assigned a value."

(* Messages generated here when an Interpolation failed *)
FindPeaks::firstinterp = "The interpolation of the data failed."
FindPeaks::secondinterp = "The interpolation of the first derivative
	of the data failed."

Begin["`Private`"]

FindPeaks[data_, opts___?OptionQ] := FindPeaks[data,None,opts]

FindPeaks[data_, shape_, opts___?OptionQ] := Module[
	{
		i,j,k,l,
		dummy,

		first2nd = 0,					(* value of min in 2nd deriv of
										 * the first peak found. *)
		answer,
		counts,

		(* The interpolation, derivative of interpolation, and
		 * second derivative of interpolation.
		 *)
		int, dint, ddint,

		(* Values for the current peak *)
		curmin, curcenter, curamplitude, curwidth,
		left, right,

		n,nvars,x,errx,y,erry,
		yorig,
		xmin, xmax, dx,

		peak = 1,
		peakposn,
		positiveposns,

		intercept, slope,
		center, width, ampl,

		workdata, dworkdata, ddworkdata,

		(* Was InterpolatingFunction::dmwarn on? *)
		wasOn,

		optFindBkgd,
		optMaximumPeaks,
		optShowProgress
	},

	i = Complement[ First /@ {opts},
				First /@ Options[FindPeaks] ];
	If[Length[i] != 0,
		Message[FindPeaks::badopt, #]& /@ i;
		Return[$Failed]
	];


	optFindBkgd = FindBkgd /. {opts} /. Options[FindPeaks];
	optMaximumPeaks = MaximumPeaks /. {opts} /. Options[FindPeaks];
	optShowProgress = EDAShowProgress /. {opts} /. Options[FindPeaks];

	(* Check to see if the symbols we will be returning are assigned
	 * values. Using With[] is crucial since it evaluates outside
	 * the normal context.
	 *)
	For[i = 1, i <= optMaximumPeaks, i++,
		With[{dummy = i},
			If[ValueQ[ CenterValue[dummy] ],
				Message[FindPeaks::centerdef];
				Return[$Failed]
			];
		];
		Which[

			shape === None,
				With[{dummy = i},
					If[ValueQ[ Amplitude[dummy] ],
						Message[FindPeaks::ampldef];
						Return[$Failed]
					];
					If[ValueQ[ PeakWidth[dummy] ],
						Message[FindPeaks::peakwidthdef];
						Return[$Failed]
					];
				],

			shape === Gaussian,
				With[{dummy = i},
					If[ValueQ[ Amplitude[dummy] ],
						Message[FindPeaks::ampldef];
						Return[$Failed]
					];
					If[ValueQ[ EDASigma[dummy] ],
						Message[FindPeaks::sigmadef];
						Return[$Failed]
					];
				],

			shape === Lorentzian || shape === BreitWigner,
				With[{dummy = i},
					If[ValueQ[ PeakArea[dummy] ],
						Message[FindPeaks::peakareadef];
						Return[$Failed]
					];
					If[ValueQ[ FWHM[dummy] ],
						Message[FindPeaks::fwhmdef];
						Return[$Failed]
					];
				],

			True,
				Message[FindPeaks::badshape, shape];
				Return[$Failed]
		];
	];

	(* Unpack the data *)
	Check[n = UnpackData[data];,
		Return[$Failed]
	];
	If[ n === $Failed,
		Message[FindPeaks::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	(* Since SmoothData systematically depresses peak values,
	 * we will store the original y values in yorig and use
	 * them for estimating the amplitude.
	 *)
	yorig = y;

	center = width = ampl = Table[ None, {optMaximumPeaks}];

	xmin = Min[x]; xmax = Max[x];
	dx = (xmax - xmin)/(n - 1)//N;


	workdata = SmoothData[Transpose[ {x,y} ] ];

	(* Check if InterpolatingFunction::dmwarn is on, turn it off. *)
	wasOn = Head[InterpolatingFunction::dmwarn] =!= $Off;
	Off[InterpolatingFunction::dmwarn];

	x = First /@ workdata;
	y = Last /@ workdata;
	intercept = slope = None;
	If[optFindBkgd === Automatic,
		counts = Plus @@ (Last /@ workdata);
		{intercept, slope} = setBkgd[ {First[x],First[y]},
			{Last[x], Last[y]}, counts],
	(* else *)
		If[ optFindBkgd =!= None,
			Message[FindPeaks::bkgd, optBkgd];
			Return[$Failed];
		];
	];

	If[intercept =!= None,
		(* Check to see if Bkgd[0] or Bkgd[1] are assigned.
		 * Using With[] is crucial since it evaluates outside
		 * the normal context.
		 *)
		For[i = 0, i <= 1, i++,
			With[{dummy = i},
				If[ValueQ[ Bkgd[dummy] ],
					Message[FindPeaks::bkgddef];
					Return[$Failed]
				];
			];
		];

		(* Subtract background. *)
		x = First /@ workdata;
		y = Last /@ workdata;
		i = Table[ intercept + slope*j, {j, xmin, xmax, dx}];
		y = y - i;
		yorig = yorig - i;
	];

	workdata = Transpose[ {x,y} ];

	If[optShowProgress,
		If[ $VersionNumber < 6.0,
			EDAListPlot[ workdata,
				PlotLabel -> "Data: smoothed, bkgd subtracted",
				PlotRange -> All ];,
		(* else *)
			Print[EDAListPlot[ workdata,
				PlotLabel -> "Data: smoothed, bkgd subtracted",
				PlotRange -> All ]];
		];
	];

	Check[int = Interpolation[workdata];,
		Message[FindPeaks::firstinterp];
		Return[$Failed]
	];
	dint = D[int[dummy],dummy];

	dworkdata = SmoothData[ 
		Table[ {dummy, dint}, {dummy, xmin, xmax, dx}] ];

	If[optShowProgress,
		If[ $VersionNumber < 6.0,
			EDAListPlot[ dworkdata,
				PlotLabel -> "First derivatives",
				PlotRange -> All ];,
		(* else *)
			Print[EDAListPlot[ dworkdata,
				PlotLabel -> "First derivatives",
				PlotRange -> All ]];
		];
	];

	Check[ dint = Interpolation[dworkdata];,
		Message[FindPeaks::secondinterp];
		Return[$Failed]
	];

	ddint = D[dint[dummy], dummy];
	ddworkdata = SmoothData[ 
		Table[ {dummy, ddint}, {dummy, xmin, xmax, dx}]];

(*********************************)
(* Begin loop looking for peaks. *)
(*********************************)

While[peak <= optMaximumPeaks,

	If[optShowProgress,
		If[ $VersionNumber < 6.0,
			EDAListPlot[ ddworkdata,
				PlotLabel -> "Second derivatives",
				PlotRange -> All ];,
		(* else *)
			Print[EDAListPlot[ ddworkdata,
				PlotLabel -> "Second derivatives",
				PlotRange -> All ]];
		];
	];

	(* Find positions in 2nd derivative list that are > 0 *)
	positiveposns = Flatten[Position[ddworkdata, {i_,j_} /; j > 0]];

	(* Find the peak position, the minimum in the 2nd derivative. *)
	curmin = Min[Last /@ ddworkdata];
	If[curmin >= 0 || Abs[curmin] < Abs[0.05*first2nd], 
		(* 2nd derivative not negative or is nearly zero.
		 * There is no peak here.
		 *)
		Break[]
	];
	peakposn = First[Flatten[Position[ddworkdata, {j_, k_} /; k == curmin]]];
	curcenter = First[ ddworkdata[[peakposn]] ];

	(* Average 3 values to get the amplitude. *)
	curampl = (yorig[[peakposn - 1]] +
		yorig[[peakposn]] + yorig[[peakposn + 1]])/3 ;

	i = positiveposns - peakposn;
	(* Find the positive 2nd derivative to the left of the peak and
	 * closest to the peak. Calculate as the relative difference.
	 *)
	j = Max[Select[i, # < 0 &]];
	(* Find the positive 2nd derivative to the right of the peak
	 * and closest to the peak. Calculate as the relative difference.
	 *)
	k = Min[Select[i, # > 0 &]];
	(* `left' and `right' are the positions where the 2nd derivative
	 * are zero.
	 *)
	left = peakposn + j;
	right = peakposn + k;

	(* Some heuristics to break out. First, if we are on an edge?
	 *)
	If[ left == -Infinity || right == Infinity,
		Break[]
	];
	(* If the width is very small break out. *)
	If[right - left <= 2,
		Break[]
	];
	(* If the peak is very assymetrical break out. *)
	i = Abs[ (peakposn - left) - (right - peakposn) ] /
			(right - left - 1)//N;
	If[i > 0.25,
		Break[]
	];

	(* Note that left and right are actually the positions where the
	 * 2nd derivatives are > 0. This is on average 1/2 a position too
	 * far, so we subtract 1 to get the curwidth.
	 *)
	curwidth = (right - left - 1)*dx;


	(* Replace all the negative numbers in the peak with 0.001.
	 * First create a list of the positions of the current peak
	 * suitable for feeding to ReplacePart[].
	 *)
	l = Table[ {i}, {i,left,right}];
	i = Last /@ ddworkdata;
	i = ReplacePart[ i, 0.001, l];
	j = First /@ ddworkdata;
	ddworkdata = Transpose[ {j,i} ];

	If[peak == 1,
		(* The value of the minimum of the second derivative.
		 * This is used to help decide when there are no more
		 * peaks to be found.
		 *)
		first2nd = Min[Last /@ ddworkdata];
	];

	ampl[[peak]] = curampl;
	center[[peak]] = curcenter;
	width[[peak]] = curwidth;

	peak++;

];

(**********************************)
(* End of loop looking for peaks. *)
(**********************************)

	peak--;

	If[peak >= optMaximumPeaks,
		Message[FindPeaks::maxpeaks, peak];
	];

	Switch[shape,
		None,
			If[ intercept =!= None,
				answer = {  Bkgd[0] -> intercept,
					Bkgd[1] -> slope},
			(* else *)
				answer = {}
			];
			For[i = 1, i <= peak, i++,
				answer = Join[ answer,
					{	Rule[ Amplitude[ i ] , ampl[[i]] ],
						Rule[ CenterValue[ i ] , center[[i]] ],
						Rule[ PeakWidth[ i ] , width[[i]] ] }];
			],
		Gaussian,
			answer = doGaussian[intercept, slope, ampl, center, width, peak],

		BreitWigner,
			answer = doLorentzian[intercept, slope, ampl, center,
						width, peak],

		Lorentzian,
			answer = doLorentzian[intercept, slope, ampl, center, 
						width, peak],

		_,	Message[FindPeaks::badshape, shape];
			Return[$Failed]
	];

	If[ wasOn,
		On[InterpolatingFunction::dmwarn]
	];

	answer = Join[ answer, { Rule[PeaksFound, peak] } ];
	answer
]

doGaussian[intercept_, slope_, ampl_, center_, width_, num_ ] := Module[
	{
		i,

		answer,
		model,
		parameters
	},

	If[intercept =!= None,
		model = Bkgd[0] + Bkgd[1]*IndependentVariable;
		parameters = { {Bkgd[0] , intercept}, {Bkgd[1] , slope}},
	(* else *)
		model = 0;
		parameters = {};
	];

	For[i = 1, i <= num, i++,
		model = model + Gaussian[IndependentVariable, Amplitude[i],
			CenterValue[i], EDASigma[i] ];
		parameters = Join[ parameters,
			{	{ Amplitude[i] , ampl[[i]] },
				{ CenterValue[i] , center[[i]] },
				(* The width is roughly where the 2nd derivatives are
				 * equal to zero.  This is 2*sigma for a Guassian
				 * shape.
				 *)
				{ EDASigma[i] , width[[i]]/2 } }];
	];

	answer = { Model -> model, EDAParameters -> parameters };
	answer


]

doLorentzian[intercept_, slope_, ampl_, center_, width_, num_ ] := Module[
	{
		i,
		answer, model, parameters
	},

	If[intercept =!= None,
		model = Bkgd[0] + Bkgd[1]*IndependentVariable;
		parameters = { {Bkgd[0] , intercept}, {Bkgd[1] , slope}},
	(* else *)
		model = 0;
		parameters = {};
	];

	For[i = 1, i <= num, i++,
		model = model + Lorentzian[IndependentVariable, PeakArea[i],
			CenterValue[i], FWHM[i] ];
		(* The width is where the 2nd derivatives are equal to zero.
		 * For a Lorentzian/BreitWigner, this is Gamma/Sqrt[3].
		 *)
		parameters = Join[ parameters,
			{	{ PeakArea[i] , 
					N[ ampl[[i]]*Pi*width[[i]]*Sqrt[3]/2. ] },
				{ CenterValue[i] , center[[i]] },
				{ FWHM[i] , N[ width[[i]]*Sqrt[3] ] } }];
	];

	answer = { Model -> model, EDAParameters -> parameters };
	answer

]

(* Calculate the slope and intercept of a linear background.  If
 * the area under the  background is < 2% of the counts, set
 * answer to {None,None}, otherwise set to the intercept
 * and slope respectively.
 *)
setBkgd[ lhs_, rhs_, counts_] := Module[
	{ answer, slope, intercept, num },

	slope = (Last[rhs] - Last[lhs])/(First[rhs] - First[lhs]);
	intercept = Last[rhs] - slope*First[rhs];

	num = slope/2*(First[rhs]^2 - First[lhs]^2) +
			intercept*(First[rhs] - First[lhs]);
	If[num < 0.02*counts,
		answer = {None, None},
	(* else *)
		answer = {intercept, slope}
	];

	answer

];

End[]

SetAttributes[FindPeaks, {ReadProtected}]

Protect[FindPeaks]

EndPackage[]
