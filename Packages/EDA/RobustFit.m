(* $Id: RobustFit.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Robust Fitting *)

(* :Context: EDA`RobustFit` *)

(* :Author: David M. Harrison *)

(* :Summary:
	RobustCurveFit - fit data robustly to curves
	RobustLineFit - fit data robustly to lines
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
	William S. Cleveland, "Visualizing Data" (AT&T Bell Labs, 1993).
	I Johnstone and P.F. Velleman, 1981 Proc. Statistical Comp.
		Section, American Statistics Assoc. (1982), 218.
*)

(* :Warnings: *)

(* :Limitations:  *)

(* :Discussion:
    
*)

(*
 * As of Mathematica 9.0 Utilities`FilterOptions` is obsolete. The
 * replacement FilterRules[] interacts badly with much EDA code.
 * FilterOptions is now in EDA`Utilities`
 *)

Which[ 
	$VersionNumber < 6.0,
		BeginPackage["EDA`RobustFit`",
			"EDA`Common`",
			"EDA`ErrorPropagation`",
			"EDA`LinearFit`",
			"EDA`Utilities`",
			"Statistics`DescriptiveStatistics`",
			"Utilities`FilterOptions`"
		],
	$VersionNumber < 9.0,
		BeginPackage["EDA`RobustFit`",
			"EDA`Common`",
			"EDA`ErrorPropagation`",
			"EDA`LinearFit`",
			"EDA`Utilities`",
			"Utilities`FilterOptions`"
		],
	True,
		BeginPackage["EDA`RobustFit`",
			"EDA`Common`",
			"EDA`ErrorPropagation`",
			"EDA`LinearFit`",
			"EDA`Utilities`"
		]
];

Unprotect[RobustCurveFit, RobustLineFit]

RobustCurveFit::usage =
	"RobustCurveFit[data, powers, parameter] does a bi-squared residual
	weighted fit of data to a polynomial specified by powers. By
	default, it returns the result as a list of rules for parameter."

PseudoChiSquared::usage =
	"PseudoChiSquared is a chi-squared-like statistic returned by
	RobustCurveFit. The weights used in calculating the statistic
	are the bi-square weighted residuals. Although not interpretable
	as a regular chi-square statistic, it is the PseudoChiSquared that
	is minimized by RobustCurveFit."

Options[RobustCurveFit] = {
	MaximumIterations -> 5,
	ReturnErrors -> False,
	ReturnFunction -> False,
	ShowFit -> True,
	EDAShowProgress -> False,
	UseSignificantFigures -> False
}

RobustCurveFit::adefined =
	"At least one of the array members for the variable named
	in 'parameter' is already defined. Choose another name or
	execute Remove[\"`1`*\"]."
RobustCurveFit::badopt = "Option `1` is not known."
RobustCurveFit::maxiters =
	"Warning: the fit did not converge in `1` iterations. You may
	wish to consider using the MaximumIterations option."
RobustCurveFit::returnfunction = "The ReturnFunction option is not correct."

RobustLineFit::usage =
	"RobustLineFit[data, parameter] fits data to a straight-line using a
	stable three-group median resistant method, and by default, returns
	the result as a list of rules for parameter."

Groups::usage = 
	"Groups is an option for RobustLineFit. By default, the three group
	sizes are set as close as possible to a ratio of 1:1:1. However, the
	requirement that repeated values of the independent variable are all
	in the same partition or in a small data set may alter that ratio somewhat.
	If Groups is set to {num1, num2, num3}, n is the number of data
	points, and Mod[n, num1 + num2 + num3] = 0, those ratios are used instead."

Options[RobustLineFit] = {
	Groups -> Automatic,
	MaximumIterations -> 5,
	ReturnFunction -> False,
	ShowFit -> True,
	EDAShowProgress -> False
}

RobustLineFit::adefined = 
	"At least one of the array members for the variable named
	in 'parameter' is already defined. Choose another name or
	execute Remove[\"`1`*\"]."
RobustLineFit::badgroups =
	"The group specification of `1` is not well formed."
RobustLineFit::badopt = "Option `1` is not known."
RobustLineFit::manyduplicates =
	"Too many duplicate independent variables."
RobustLineFit::maxiters =
	"Warning: the fit did not converge in `1` iterations. You may
	wish to consider using the MaximumIterations option."
RobustLineFit::nodata = 
	"Not enough data to use RobustLineFit with Groups `1`."
RobustLineFit::nogroups =
	"Can't find a group to use."
RobustLineFit::nosize =
	"Cannot form three-groups."
RobustLineFit::uniquex =
	"The independent variable does not contain enough unique values:
	can't partition the dataset."
RobustLineFit::unbalanced =
	"Warning: the partitions are of size `1`, which is very unbalanced."

Begin["`Private`"]

RobustCurveFit[data_, powers_, a_, opts___?OptionQ] := Module[
	{
		i,
		iter,

		numbers, oldnumbers, (* used for a secondary convergence test *)

		chisq, oldchisq,
		dof, func, ind,

		m = Length[powers],
		n,nvars,x,errx,y,erry,
		res,
		result,
		reweighted,

		optMaximumIterations,
		optReturnErrors,
		optReturnFunction,
		optShowFit,
		optShowProgress,
		optUseSignificantFigures,

		optsLinearFit,
		optsToLinearFunction,
		optsShowLinearFit

	},

	i = Complement[ First /@ {opts},
				First /@ Options[LinearFit],
				First /@ Options[ToLinearFunction],
				First /@ Options[ShowLinearFit],
				First /@ Options[RobustCurveFit] ];
	If[Length[i] != 0,
		Message[RobustCurveFit::badopt, #]& /@ i;
		Return[$Failed]
	];


	optMaximumIterations = MaximumIterations /. {opts} /. Options[RobustCurveFit];
	optReturnErrors = ReturnErrors /. {opts} /. Options[RobustCurveFit];
	optReturnFunction = ReturnFunction /. {opts} /. Options[RobustCurveFit];
	optShowFit = ShowFit /. {opts} /. Options[RobustCurveFit];
	optShowProgress = EDAShowProgress /. {opts} /. Options[RobustCurveFit];
	optUseSignificantFigures = UseSignificantFigures /. {opts} /.
		Options[RobustCurveFit];

	optsLinearFit = FilterOptions[ LinearFit, opts];
	optsToLinearFunction = FilterOptions[ ToLinearFunction, opts];
	optsShowLinearFit = FilterOptions[ ShowLinearFit, opts];

	{n,nvars,x,errx,y,erry} = Check[UnpackData[data], 
		Return[$Failed];
	];

	If[optShowProgress,
		Print[n, " data points, each with ", nvars, " variables."];
		If[data != Sort[data],
			Print["Note: the data is sorted by independent"];
			Print["  variable."];
		];
	];

	(*
	 * In the following, the use of With[ ... ] is crucial
	 * since it evaluates the constant outside of the normal
	 * evaluation context.
	 *)
	For[i = 1, i <= m, i++,
		With[{dummy = powers[[i]] },
			If[ValueQ[a[dummy]],
				Message[RobustCurveFit::adefined, a];
				Return[$Failed]
			];
		];
	];

	result = LinearFit[data, powers, a, 
		Reweight -> False,
		ShowFit -> False,
		ReturnErrors -> False,
		ReturnResiduals -> True ];
	If[result === $Failed,
		Return[$Failed];
	];
	oldchisq = $MaxMachineNumber;
	oldnumbers = Table[ $MaxMachineNumber, {m}];

	(* Here is where we do the fit *)

	For[iter = 1, iter <= optMaximumIterations, iter++,
		res = Part[#,2]& /@ (Flatten /@ (Residuals /. result));
		res = bisquare[res, 6*Median[Abs[res]]];
		reweighted = Transpose[ {x,
			Transpose[ {y, 1/Sqrt[res] } ] }];

		(* Turning off significant figure adjustment is
		 * crucial here: otherwise the large fake error in
		 * y turns the residual for the data point we are
		 * trying to suppress to zero.  Also, we must not
		 * return a function here.
		 *)
		result = LinearFit[reweighted, powers, a,
			UseSignificantFigures -> False,
			ReturnFunction -> False,
			optsLinearFit,
			ShowFit -> False,
			ReturnResiduals -> True,
			ReturnErrors -> optReturnErrors ];
		If[result === $Failed,
			Return[$Failed];
		];

		numbers = ((a[#]& /@ powers) /. result);

		If[optShowProgress,
			Print["Iteration: ", iter, " ChiSquared = ", ChiSquared /. result ];
			Print["  Suppressed data points at: ",
				Position[res, $MinMachineNumber] ];
			Print["  ",
				(a[#]& /@ powers), " = ", ((a[#]& /@ powers) /. result) ];
		];

		chisq = ChiSquared /. result;
		If[ chisq <= oldchisq && (oldchisq - chisq)/chisq < 0.1,
			Break[];
		];
		oldchisq = chisq;

		If[ Max[ Abs[numbers - oldnumbers] / numbers] < 0.01,
			Break[];
		];
		oldnumbers = numbers;
	];

	If[iter > optMaximumIterations,
		Message[RobustCurveFit::maxiters, iter - 1];
	];

	(* 
	 * Will return this later: calculate now before we
	 * over-write result.
	 *)
	dof = (DegreesOfFreedom /. result) - Count[res,$MinMachineNumber];

	If[optReturnErrors && optUseSignificantFigures,
		result = Rule[Part[#,1],Part[#,2]]& /@ Transpose[
			{ (a[#]& /@ powers), 
				AdjustSignificantFigures[ ((a[#]& /@ powers) /. result)] } ],
	(* else *)
		result = Rule[Part[#,1],Part[#,2]]& /@ Transpose[
			{ (a[#]& /@ powers), ((a[#]& /@ powers) /. result) } ]
	];

	(* We will need the function whether we want to return it or not. *)
	func = ToLinearFunction[result, powers, a, ind, optsToLinearFunction ];

	chisq = Plus @@ ((res*(y - (First[func] /. ind -> x)))^2);

	result = Append[result, PseudoChiSquared -> chisq];
	result = Append[result, DegreesOfFreedom -> dof];

	If[optShowFit,
		(*
		 * In Mathematica 6.0 terminating a graphics command suppresses
		 * the output, but the scope of Print[] has been expanded to
		 * include graphics.
		 *)
		If[ $VersionNumber < 6.0,
			ShowLinearFit[data, powers, a, result,
				Evaluate @ optsShowLinearFit ];,
		(* else *)
			Print[ShowLinearFit[data, powers, a, result,
				Evaluate @ optsShowLinearFit ]];
		];
	];

	If[optReturnFunction,
		func /. ind -> a,
	(* else *)
		result
	]
]

bisquare[epsilon_?(VectorQ[#,NumericQ]&), sixs_?NumericQ] := Module[
	{
		work
	},

	(* 
	 * This is the square root of the bi-square weight,
	 * so for epsilon > sixs we get negative numbers.  We'll
	 * square it in the next line.
	 *)
	work = (1 - (epsilon/sixs)^2);
	If[# <= 0, $MinMachineNumber, #^2]& /@ work

]

RobustLineFit[data_, a_, opts___?OptionQ] := Module[
	{
		i,j,k,
		ind,								(* place-holder *)
		iter,

		n, nvars, x, errx, y, erry,
		chisq,

		xLdata,yLdata,xMdata,yMdata,xRdata,yRdata,
		(*
		 * Much of the notation below agrees with John D. Emerson and
		 * David C. Hoaglin, Chapter 5 of David C. Hoaglin, Frederick
		 * Mosteller & John W. Tukey, eds., "Understanding Robust and
		 * Exploratory Data Analysis" (Wiley Interscience, 1983).
		 *)
		b0, b1, b2,
		ucDelta0, ucDelta1, ucDelta2,		(* "upper-case Deltas" *)
		delta1,
		rL,rM,rR,
		xL,yL,xM,yM,xR,yR,

		optGroups,
		optMaximumIterations,
		optReturnFunction,
		optShowFit,
		optShowProgress,

		optsToLinearFunction,
		optsShowLinearFit
	},

	i = Complement[ First /@ {opts},
				First /@ Options[ToLinearFunction],
				First /@ Options[ShowLinearFit],
				First /@ Options[RobustLineFit] ];
	If[Length[i] != 0,
		Message[RobustLineFit::badopt, #]& /@ i;
		Return[$Failed]
	];

	optGroups = Groups /. {opts} /. Options[RobustLineFit];
	optMaximumIterations = MaximumIterations /. {opts} /. Options[RobustLineFit];
	optReturnFunction = ReturnFunction /. {opts} /. Options[RobustLineFit];
	optShowFit = ShowFit /. {opts} /. Options[RobustLineFit];
	optShowProgress = EDAShowProgress /. {opts} /. Options[RobustLineFit];

	optsToLinearFunction = FilterOptions[ ToLinearFunction, opts];
	optsShowLinearFit = FilterOptions[ ShowLinearFit, opts];

	{n,nvars,x,errx,y,erry} = Check[UnpackData[data],
	        Return[$Failed];
	];

	(*
	 * In the following, the use of With[ ... ] is crucial
	 * since it evaluates the constant outside of the normal
	 * evaluation context.
	 *)
	For[i = 0, i <= 1, i++,
		With[{dummy = i},
			If[ValueQ[a[dummy]],
				Message[LinearFit::adefined, a];
				Return[$Failed]
			];
		];
	];


	If[optGroups =!= Automatic,
		If[Length[optGroups] != 3 || Mod[n, (Plus @@ optGroups)] != 0,
			Message[RobustLineFit::badgroups, optGroups];
			Return[$Failed]
		];
		If[(Plus @@ optGroups) > n,
			Message[RobustLineFit::nodata, optGroups];
			Return[$Failed]
		],
	(* else *)
		Switch[Mod[n, 3],
			0,	optGroups = {1,1,1},
			1,	i = Floor[n/3];
				optGroups = {i, i + 1, i},
			2,	i = Floor[n/3];
				optGroups = {i + 1, i, i + 1}
		];
	];
	(* Found ratios: convert to actual numbers *)
	optGroups = optGroups * n / (Plus @@ optGroups);
	If[optShowProgress,
		Print["Initial groups sizes: ", optGroups];
	];

	(* Put into Left, Middle, and Right groups *)
	i = First[optGroups];
	xLdata = Take[x, i];
	yLdata = Take[y, i];
	j = Part[optGroups,2] + i;
	xMdata = Take[x, {i + 1,j}];
	yMdata = Take[y, {i + 1,j}];
	k = Last[optGroups];
	xRdata = Take[x, -k];
	yRdata = Take[y, -k];

	Check[{xLdata,yLdata,xMdata,yMdata} = uniqueX[xLdata,yLdata,xMdata,yMdata],
		Return[$Failed]
	];
	Check[{xMdata,yMdata,xRdata,yRdata} = uniqueX[xMdata,yMdata,xRdata,yRdata],
		Return[$Failed]
	];


	optGroups = {Length[xLdata],Length[xMdata],Length[xRdata]};
	If[Count[optGroups,0] != 0,
		Message[RobustLineFit::nosize];
		Return[$Failed]
	];
	{i,j,k} = {First[optGroups],Part[optGroups,2],Last[optGroups]};
	If[	i < .5*j || i > 2*j || j < .5*k || j > 2*k,
			Message[RobustLineFit::unbalanced, optGroups];
	];
	If[optShowProgress,
		Print["Final group sizes are: ", optGroups]
	];

	(*************************************************************)
	(* Done with all these preliminaries, now we can go to work. *)
	(*************************************************************)

	(* 
	 * Replace the data with their medians.
	 *)

	xL = Median[xLdata]//N;
	xM = Median[xMdata]//N;
	xR = Median[xRdata]//N;
	yL = Median[yLdata]//N;
	yM = Median[yMdata]//N;
	yR = Median[yRdata]//N;

	b0 = (yR - yL)/(xR - xL);
	If[optShowProgress,
		Print["First slope estimate: ", b0];
	];

	rL = Median[yLdata - b0 (xLdata - xM)]//N;
	rR = Median[yRdata - b0 (xRdata - xM)]//N;
	ucDelta0 = rR - rL;

	delta1 = (rR - rL)/(xR - xL);
	b1 = b0 + delta1;
	If[optShowProgress,
		Print["Second slope estimate: ", b1];
	];

	rL = Median[yLdata - b1 (xLdata - xM)]//N;
	rR = Median[yRdata - b1 (xRdata - xM)]//N;
	ucDelta1 = rR - rL;

	iter = 1;
	(*
	 * We adjust b1 until ucDelta1 has a different sign
	 * than ucDelta0.
	 *)
	If[Chop[ucDelta0] == 0, (* unlikely but possible *)
		b1 = b0;
		ucDelta1 = ucDelta0, (* to stop the For[ , iter .. below *)
	(* else *)
		While[Chop[ucDelta1] != 0 && Sign[ucDelta0/ucDelta1] != -1 &&
			iter <= optMaximumIterations,
				b1 = b1 + delta1;
				If[optShowProgress,
					Print["Iteration: ", iter];
					Print["  Increasing second slope estimate to: ", b1];
				];
				rL = Median[yLdata - b1 (xLdata - xM)]//N;
				rR = Median[yRdata - b1 (xRdata - xM)]//N;
				ucDelta1 = rR - rL;
				iter++;
		];
	];

	For[ NULL, iter <= optMaximumIterations && Chop[ucDelta1] != 0., iter++,

		b2 = b1 - ucDelta1* (b1 - b0) / (ucDelta1 - ucDelta0);
		If[ Abs[ (b2 - b1)/b1 ] < 0.01,
			Break[];
		];
		ucDelta2 = Median[yLdata - b2 (xLdata - xM)] -
			Median[yRdata - b2 (xRdata - xM)];

		If[optShowProgress,
			Print["Iteration: ", iter];
			Print["  New slope estimate is: ", b2];
		];
		
		(* Convergence *)
		If[Chop[ucDelta2] == 0,
			b1 = b2;
			Break[]
		];

		If[Sign[ucDelta1/ucDelta2] == 1,
			b1 = b2,
		(* else *)
			b0 = b2;
			rL = Median[yLdata - b0 (xLdata - xM)]//N;
			rR = Median[yRdata - b0 (xRdata - xM)]//N;
			ucDelta0 = rR - rL;
		];

		rL = Median[yLdata - b1 (xLdata - xM)]//N;
		rR = Median[yRdata - b1 (xRdata - xM)]//N;
		ucDelta1 = rR - rL;

	];

	(* Finished with the fit *)

	If[iter > optMaximumIterations,
		Message[RobustLineFit::maxiters, iter - 1];
	];

	If[nvars == 4,
		(* Calculate an effective variance *)
		erry = Quadrature[ erry, b1 * errx ];
	];

	(* This is the intercept a[0] *)
	i = Median[y - b1*(x - xM)] - b1*xM;

	(* 
	 * This relies on erry being {1,1, ... , 1} if no explicit errors,
	 * which is how UnpackData does it.
	 *)
	chisq = Plus @@ (((y - (i + b1*x))/erry)^2);

	If[nvars < 3,
		result = { 	Rule[ a[0], i],
	  		Rule[ a[1], b1],
			Rule[ SumOfSquares, chisq ]
		},
	(* else *)
		result = {	Rule[ a[0], i],
	  		Rule[ a[1], b1],
			Rule[ ChiSquared, chisq ]
		}
	];

	If[optShowFit,
		(*
		 * In Mathematica 6.0 terminating a graphics command suppresses
		 * the output, but the scope of Print[] has been expanded to
		 * include graphics.
		 *)
		If[ $VersionNumber < 6.0,
			ShowLinearFit[data, {0,1}, a, result,
				Evaluate @ optsShowLinearFit ];,
		(* else *)
			Print[ShowLinearFit[data, {0,1}, a, result,
				Evaluate @ optsShowLinearFit ]];
		];
	];

	If[optReturnFunction,
		result = ToLinearFunction[result, {0,1}, a, ind,
			optsToLinearFunction ];
		result = result /. ind -> a;
	];

	result

]

(*
 * This routine returns {x1,y1,x2,y2} adjusted so that
 * no values of x are in both x1 and x2.
 *)
uniqueX[x1_, y1_, x2_, y2_ ] := Module[
	{
		first,last,count1,count2,len1,len2,
		newx1, newy1, newx2, newy2
	},

	If[Length[Intersection[x1,x2]] == 0,
		Return[ {x1,y1,x2,y2} ]
	];
	last = Last[x1];
	first = First[x2];
	If[last == first,
		len1 = Length[x1];
		len2 = Length[x2];
		count1 = Count[x1, last];
		count2 = Count[x2, first];
		If[count1 == len1 && count2 == len2,
			Message[RobustLineFit::uniquex];
			Return[$Failed];
		];
		Which[
			count1 > count2,
				newx1 = Flatten[{x1, Take[x2, count2]}];
				newy1 = Flatten[{y1, Take[y2, count2]}];
				newx2 = Drop[x2, count2];
				newy2 = Drop[y2, count2],
			True,
				newx2 = Flatten[{x2, Take[x1, -count1]}];
				newy2 = Flatten[{y2, Take[y1, -count1]}];
				newx1 = Drop[x1, -count1];
				newy1 = Drop[y1, -count1];
		];
	];
	{newx1,newy1,newx2,newy2}
]

End[]

SetAttributes[ RobustCurveFit, {ReadProtected}]
SetAttributes[ RobustLineFit, {ReadProtected}]

Protect[RobustCurveFit, RobustLineFit]

EndPackage[]
