(* $Id: FindFit.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Find a fit of data to an arbitrary model *)

(* :Context: EDA`FindFit` *)

(* :Author: David M. Harrison *)

(* :Summary:
	EDAFindFit - find a fit of data to a model.
	ShowFitResult - graph the results of a fit.
	ToFitFunction - convert a fit result to a function.
*)

(* :Copyright: Copyright 1995-2013, Wolfram Research, Inc.*)

(* :Package Version: 1.3 *)

(* :Mathematica Version: 9.0 *)

(* :History:
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
	Version 1.2 by David M. Harrison, May 2009.
	Version 1.3 by David M. Harrison, February 2013.
*)

(* :Keywords:
	Levenberg-Marquardt algorithm.
*)

(* :Sources:
		Victor Adamchik et. al., "Guide to Standard Mathematica Packages,
			Version 2.2" (Wolfram Research, 1993), Statistics`NonlinearFit`
			package description.
        Janhunen, Pekka, NonlinearFit`, (a Mathematica package), 1990.
        Press, William, et. al., "Numerical Recipes in Pascal",
                pp. 572-580, Cambridge University Press (Cambridge, 1989).
        Withoff, Dave, DataAnalysis`NonlinearRegression`,
                (a Mathematica package), 1989.

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
	$VersionNumber < 9.0,
		BeginPackage["EDA`FindFit`",
			"EDA`Common`",
			"EDA`EDAGraphics`",
			"EDA`ErrorPropagation`",
			"EDA`Utilities`",
			"Utilities`FilterOptions`"
		],
	True,
		BeginPackage["EDA`FindFit`",
			"EDA`Common`",
			"EDA`EDAGraphics`",
			"EDA`ErrorPropagation`",
			"EDA`Utilities`"
		]
];

Unprotect[
	EDAFindFit,
	BreitWigner,
	Gaussian,
	Galatry,
	Lorentzian,
	PearsonVII,
	Voigt,
	ShowFitResult,
	ToFitFunction
]

EDAFindFit::usage =
	"EDAFindFit[data, model, ind, parameters] finds a fit of \"data\" to
	\"model\" that minimizes the sum of the squares of the residuals or the
	chi-squared. The model is expected to be a function of the independent
	variable \"ind\" and the variables named in \"parameters\". The parameters
	can be of the form {name1, name2, ... , nameM}, but will more usually
	be of the form { {name1, start1}, {name2, start2}, ... , {nameM, startM}},
	where \"starti\" is initial values of the parameter. In addition, each
	parameter can be specified as {name, min, start, max}, where \"name\" is
	the name, \"start\" is the starting value, and \"min\" and \"max\" specify
	the minimum and maximum values of the parameter that can be returned.
	Finally, the parameter can be specified as {name, min, max} in which
	case the starting value is (min + max)/2. By default, EDAFindFit uses
	the Levenberg-Marquardt algorithm. If there are no declared errors
	in the independent variable, other methods, chosen with a
	Method option, can be EDALM, Gradient, Newton, or QuasiNewton."

EDALM::usage = 
	"EDALM is a possible method for EDAFindFit. It causes EDAFindFit to
	not use the Mathematica built-in FindMinimum and instead use a
	simple but robust Levenberg-Marquardt algorithm. If the Reweight
	flag to EDAFindFit is set to True, or there are declared errors
	in both coordinates, the EDALM method is always used by EDAFindFit."

(*
 * Design decisions about what to include here, option names in
 * routines, etc. are the same as those for LinearFit.m. See that
 * file for the discussion.
 *)
Options[EDAFindFit] = {
	AbsoluteChiSquaredTolerance -> 0.1,
	MaximumIterations -> 30,
	Method -> LevenbergMarquardt,
	RelativeChiSquaredTolerance -> 0.005,
	ReturnCovariance -> False,
	ReturnEffectiveVariance -> False,
	ReturnErrors -> True,
	ReturnFunction -> False,
	ReturnResiduals -> False,
	Reweight -> False,
	ShowFit -> True,
	EDAShowProgress -> False,
	EDAAccuracyGoal -> Automatic,
	EDAPrecisionGoal -> Automatic,
	UseSignificantFigures -> True,
	ValueTolerance -> 0.002
}

(* TODO: the reweightflag is not being set for optimal efficiency. *)

AbsoluteChiSquaredTolerance::usage =
	"AbsoluteChiSquaredTolerance is used in the default convergence
	test of EDAFindFit. When the chi-squared of the current iteration
	is less than the chi-squared of the previous iteration and their
	difference is less than AbsoluteChiSquaredTolerance, the fit is
	judged to have converged and no more iterations are performed."
RelativeChiSquaredTolerance::usage =
	"RelativeChiSquaredTolerance is used in the default convergence
	test of EDAFindFit. When the chi-squared of the current iteration
	is less than the chi-squared of the previous iteration and their
	relative difference is less than RelativeChiSquaredTolerance, the fit
	is judged to have converged and no more iterations are performed."
ValueTolerance::usage =
	"ValueTolerance is used in the default convergence test of
	EDAFindFit. When the chi-squared of the current iteration is
	less than the chi-squared of the previous iteration and the maximum
	change in the values of the parameters being fit divided by the
	value of that parameter is less than ValueTolerance, the fit is
	judged to have converged and no more iterations are performed."
EDAAccuracyGoal::usage = 
	"When EDAFindFit fits to data with no errors in the independent
	coordinate and the Reweight option is False, by default the fit is
	performed by the Mathematica built-in FindMinimum. EDAAccuracyGoal
	is then the AccuracyGoal used by FindFit. By default,
	EDAAccuracyGoal is Automatic"
EDAPrecisionGoal::usage = 
	"When EDAFindFit fits to data with no errors in the independent
	coordinate and the Reweight option is False, by default the fit is
	performed by the Mathematica built-in FindMinimum. EDAPrecisionGoal
	is then the PrecisionGoal used by FindFit. By default,
	EDAPrecisionGoal is Automatic"

EDAFindFit::badopt = "Option `1` is not known."
EDAFindFit::badparams=
	"The parameter list `1` is not well-formed."
EDAFindFit::chisqnonumber = 
	"A non-numeric value of the chi-squared was calculated."
EDAFindFit::dataformat = "The data format appears to be incorrect."
EDAFindFit::dof = "There are no degrees of freedom."
EDAFindFit::eek = 
	"An attempt to evaluate the effective variance has not
	returned all numeric results."
EDAFindFit::effvar =
	"The effective variance only has meaning when there are errors
	in both coordinates. EDAFindFit is setting the ReturnEffectiveVariance
	flag to False."
EDAFindFit::maxiters =
	"EDAFindFit failed to converge after `1` iterations. The
	maximum number of iterations can be increased with the
	MaximumIterations option."
EDAFindFit::paramname =
	"It appears that one or more of the parameter names has been
	assigned a value. Consider executing Remove[] on one of more
	of the parameter names."
EDAFindFit::reweight =
	"Warning: the Reweight option has no effect when there are
	explicit errors in the data. Setting Reweight to False."

BreitWigner::usage =
	"BreitWigner[x, a, x0, gamma] is a nonrelativistic Breit-Wigner
	function of the independent variable x, center value x0, and full
	width at half-maximum gamma. The function is normalized
	so that its integral from -Infinity to +Infinity is equal to 
	\"a\". The maximum amplitude of the peak is 2*a/(Pi*gamma). BreitWigner
	is a synonym for Lorentzian."

Galatry::usage =
	"Galatry[y, z, tau] is a Galatry profile with collision broadening
	parameter \"y\", collision narrowing parameter \"z\", and dimensionless
	time \"tau\". The profile is normalized so that its value is one
	when tau = 0."

Gaussian::usage =
	"Gaussian[x, ampl, x0, sigma] is a Gaussian function of the independent
	variable x with amplitude ampl, center value x0, and standard
	deviation sigma. The function is normalized so that its integral
	from -Infinity to +Infinity is equal to Sqrt[2Pi]*ampl*sigma."

Lorentzian::usage =
	"Lorentzian[x, a, x0, gamma] is a nonrelativistic Lorentzian
	function of the independent variable x, center value x0, and full
	width at half-maximum gamma. The function is normalized
	so that its integral from -Infinity to +Infinity is equal to 
	\"a\". The maximum amplitude of the peak is 2*a/(Pi*gamma). Lorentzian
	is a synonym for BreitWigner."

PearsonVII::usage =
	"PearsonVII[x, a, x0, gamma, m] is a Pearson VII function of the independent
	variable x, absorbance a, center value x0, full width at half
	maximum gamma, and tailing factor m."

RelativisticBreitWigner::usage =
	"RelativisticBreitWigner[m_, mr_, gamma_, m1_, m2_] generates a
	relativistic Breit-Wigner for a resonance of nominal mass mr and total
	width gamma decaying into two particles of rest mass m1 and m2. It
	returns the fraction of such decays whose invariant mass is m. The
	function is normalized so that when m equals mr, the function returns
	1."

Voigt::usage =
	"Voigt[y, tau] is a Voigt profile with collision broadening parameter
	\"y\" and dimensionless time \"tau\". The profile is Limit[ Galatry[
	y, z, tau], z -> 0]."

ShowFitResult::usage =
	"ShowFitResult[data, model, ind, parameters, result] takes
	results from EDAFindFit and displays graphic information
	about the fit. The meaning of the first four arguments is
	identical to arguments of the same name for EDAFindFit. Note
	that if parameters contains {param, value} pairs, the values are
	ignored by ShowFitResult. ShowFitResult[data, model, ind,
	parameters, result, residuals] is the same as the first form,
	except the residuals of the fit are given instead of calculated."

(*
 * In terms of readability by humans, this should appear down with
 * the rest of the ToFitFunction stuff. But then, the setting of
 * the options to ShowFitResult immediately following gets garbled.
 *)
Options[ToFitFunction] = {
	UseFitErrors -> False
}

Options[ShowFitResult] = Union[
	{
		Extrapolate -> False,
		ResidualPlacement -> Automatic,
		UseSignificantFigures -> True
	},	
		Options[EDAListPlot], Options[Plot], Options[ToFitFunction]
]

ShowFitResult::badextrap = "The Extrapolate option is not well-formed."
ShowFitResult::badopt = "Option `1` is not known."
ShowFitResult::dataformat = "The data format appears to be incorrect."
ShowFitResult::badresult =
	"The result given ShowFitResult is not a list of Rules."
ShowFitResult::badresplacement = "A ResidualPlacement of `1` is not legal."

ToFitFunction::usage =
	"ToFitFunction[result, model, ind, parameters] takes a result,
	which is assumed to be a set of rules such as is returned by
	default by EDAFindFit, and returns a function of the independent
	variable \"ind\" for \"model\". The model is assumed to be a function
	of \"ind\" and the \"parameters\". The \"parameters\" may be a list of
	symbols, or {symbol, value} pairs, although the values are ignored
	in all cases. By default, a single function is returned, which
	is evaluated at the result of the fit. If UseFitErrors is set
	to True, the routine returns a list of two functions. The first
	is as before and the second is the estimated error in the function
	due to the errors in the fit parameters, if any."

ToFitFunction::badopt = "Option `1` is not known."
ToFitFunction::result = "The result `1` does not appear to be correct."

Begin["`Private`"]

EDAFindFit[data_, model_, ind_Symbol, parameters_?ListQ,
	opts___?OptionQ] := Module[
	{
		answer,newanswer,

		i,j,k,l,						(* dummy variables *)
		iter,							(* iteration counter *)

		alphaF,							(* alpha without full evaluation *)
		alpha, alphaprime, beta, 
		covar,

		(*
		 * This is the chisq without the parameters being evaluated.
		 *)
		chisqF,

		chisq, oldchisq,
		deltaa,
		lambda,

		paramnames,
		paramvalues, newparamvalues,
		minparamvalues, maxparamvalues,

		dof,							(* degrees of freedom *)
		m = Length[parameters],			(* number of parameters *)
		n, nvars, x, errx, y, erry,
		origerry,
		orignvars,
		residuals,
		reweightflag,
		wasOn,							(* Shut up the underflow message *)

		optAbsoluteChiSquaredTolerance,
		optMaximumIterations,
		optMethod,
		optRelativeChiSquaredTolerance,
		optReturnCovariance,
		optReturnEffectiveVariance,
		optReturnErrors,
		optReturnFunction,
		optReturnResiduals,
		optReweight,
		optShowFit,
		optShowProgress,
		optUseSignificantFigures,
		optValueTolerance,

		(* These 2 are used only by FindMinimum *)
		optAccuracyGoal,
		optPrecisionGoal,

		optsAdjustSignificantFigures,
		optsShowFitResult,
		optsToFitFunction
	},

	i = Complement[ First /@ {opts},
				First /@ Options[EDAFindFit],
				First /@ Options[AdjustSignificantFigures],
				First /@ Options[ShowFitResult],
				First /@ Options[ToFitFunction] ];
	If[Length[i] != 0,
		Message[EDAFindFit::badopt, #]& /@ i;
		Return[$Failed]
	];

	optAbsoluteChiSquaredTolerance = AbsoluteChiSquaredTolerance /. {opts} /.
		Options[EDAFindFit];
	optMethod = Method /. {opts} /.  Options[EDAFindFit];
	optMaximumIterations = MaximumIterations /. {opts} /.
		Options[EDAFindFit];
	optRelativeChiSquaredTolerance = RelativeChiSquaredTolerance /. {opts} /.
		Options[EDAFindFit];
	optReturnCovariance = ReturnCovariance /. {opts} /.  Options[EDAFindFit];
	optReturnEffectiveVariance = ReturnEffectiveVariance /. {opts} /.
		Options[EDAFindFit];
	optReturnErrors = ReturnErrors /. {opts} /.  Options[EDAFindFit];
	optReturnFunction = ReturnFunction /. {opts} /.  Options[EDAFindFit];
	optReturnResiduals = ReturnResiduals /. {opts} /.  Options[EDAFindFit];
	optReweight = Reweight /. {opts} /.  Options[EDAFindFit];
	optShowFit = ShowFit /. {opts} /.  Options[EDAFindFit];
	optShowProgress = EDAShowProgress /. {opts} /.  Options[EDAFindFit];
	optUseSignificantFigures = UseSignificantFigures /. {opts} /.
		Options[EDAFindFit];
	optValueTolerance = ValueTolerance /. {opts} /.  Options[EDAFindFit];
	optAccuracyGoal = EDAAccuracyGoal /. {opts} /. Options[EDAFindFit];
	optPrecisionGoal = EDAPrecisionGoal /. {opts} /. Options[EDAFindFit];

	optsAdjustSignificantFigures = FilterOptions[AdjustSignificantFigures,
		opts];
	optsShowFitResult = FilterOptions[ShowFitResult, opts];
	optsToFitFunction = FilterOptions[ToFitFunction, opts];


	(* Unpack the data *)
	Check[n = UnpackData[data];,
		Return[$Failed]
	];
	If[ n === $Failed,
		Message[EDAFindFit::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;
	orignvars = nvars;

	origerry = erry;
	If[optReweight && nvars >= 3,
		Message[EDAFindFit::reweight];
		optReweight = False;
	];
	If[optReturnEffectiveVariance && nvars != 4,
		Message[EDAFindFit::effvar];
		optReturnEffectiveVariance = False;
	];

	If[optShowProgress,
		Print[	n, " data points with ", nvars, " variables.\n",
				"Fitting to ", m, " parameters"];
	];

	(*
	 * Get parameter names, minimum values, start values, and maximum
	 * values.  If no start value and no minimum or maximum are given
	 * set start to 1, minimum to -Infinity, maximum to Infinity.
	 * It would be nice to check i_ for type, but then we can't
	 * have parameters names like a[1].
	 *)
	{paramnames, minparamvalues, paramvalues, maxparamvalues} = 
		Transpose[ Map[Replace[#, {
			{i_, j_?NumericQ} -> {i,-Infinity,j,Infinity},
			{i_, j:(-Infinity | _?NumericQ), k_?NumericQ, 
				l:(_?NumericQ | Infinity) } -> {i,j,k,l},
			{i_, j_?NumericQ, k_?NumericQ} -> {i,j,(j+k)/2,k},
			i_ -> {i,-Infinity,1,Infinity},
			_ -> {$Failed,$Failed,$Failed,$Failed}
		}]&, parameters] ];
	If[(Select[paramvalues,(# === $Failed)&] =!= {}) ||
		Not[And @@ Thread[N[minparamvalues <= paramvalues <=
			maxparamvalues]]],
				Message[EDAFindFit::badparams, parameters];
				Return[$Failed]
	];

	(* Replace zeroes by $MinMachineNumber *)
	paramvalues = ReplacePart[paramvalues, $MinMachineNumber,
		Join[ Position[paramvalues,0], Position[paramvalues, 0.]] ];

	dof = n - m;
	If[dof <= 0,
		Message[EDAFindFit::dof];
		Return[$Failed]
	];


	answer = Apply[Rule, Transpose[{paramnames,paramvalues}], {1} ];

	newanswer = answer;
	newparamvalues = paramvalues;

	(* 
	 * On the first iteration, lambda will be decreased by a
	 * factor of 10, so we start it higher than we will use.
	 *)
	lambda = 0.01;

	oldchisq = $MaxMachineNumber;
	deltaa = Table[ $MaxMachineNumber, {m}];

	(*
	 * Calculate alphaF:  the derivatives in
	 * all symbolic form.
	 *)
	Check[alphaF = D[model,#]& /@ paramnames,
		Message[EDAFindFit::paramname];
		Return[$Failed]
	];

	(*
	 * Check to see of the model is linear. If so, we must use the
	 * EDALM method. The alternative is to punt to LinearFit, but
	 * that misses the important pedagological illustration that
	 * a Levenberg-Marquardt algorithm works for linear models, although
	 * EDAFindFit's implementation does not work.
	 *)

	If[optMethod =!= EDALM && Union[FreeQ[alphaF, #]& /@ paramnames] == {True},
		If[optShowProgress,
			Print[	"Warning: the model appears to be linear. Setting",
					" the Method to EDALM."];
		];
		optMethod = EDALM;
	];

	If[optShowProgress,
		Print["Beginning iterations."];
	];

	(* For functions like Galatry, we can get underflow Messages,
	 * although they seem to do no harm.  Turn them off.
	 *)
	wasOn = Head[General::unfl] =!= $Off;
	Off[General::unfl];

	(*************************)
	(* BEGIN interation loop *)
	(*************************)

	For[iter = 1, iter <= optMaximumIterations, iter++,

		(*
		 * Despite the name, this might be the sum of the squares.
		 * First set it up with the parameter symbols unevaluated.
		 * Note that on the first iteration, erry does not include
		 * the effective variance contribution if any.  If there
		 * are four variables, we need to re-evaluate on every
		 * iteration since erry is changing.
		 *)
		If[iter == 1 || nvars == 4 || reweightflag,
			chisqF = Plus @@ (((y - (model /. ind -> x))/erry)^2);
		];

		(* Now evaluate the chisquared.  *)
		chisq = Evaluate[ chisqF /. newanswer ]//N;
		If[!NumericQ[chisq] || Im[chisq] != 0,
			Message[EDAFindFit::chisqnonumber];
			(* Turn on General::unfl messages if they have
			 * been turned off. *)
			If[wasOn, On[General::unfl]];
			Return[$Failed];
		];

		If[optReweight === True,
			If[optShowProgress && optMethod =!= EDALM,
				Print[	"Warning: since the Reweight option has been",
						" turned on, the Method has been set to EDALM."];
			];
			optMethod = EDALM;
		];
		If[nvars == 4,
			If[optShowProgress,
				Print[	"Warning: since there are errors in both coordinates",
						" the Method has been set to EDALM."]
			];
			optMethod = EDALM;
		];

		If[nvars < 4 && optReweight === False && optMethod =!= EDALM,

			(*
			 * Here we use the built-in FindMinimum and break out of the
			 * loop.
			 *)
			If[optShowProgress,
				Print[	"Passing control to FindMinimum.  ",
						"Method is: ", optMethod];
			];

			Check[i = FindMinimum[ chisqF, ##, 
				Evaluate[Method -> optMethod], 
				MaxIterations -> optMaximumIterations,
				AccuracyGoal -> optAccuracyGoal,
				PrecisionGoal -> optPrecisionGoal ]& @@ Transpose[
					{paramnames, paramvalues}];,
						Return[$Failed]
			];
			If[i === $Failed,
				Return[$Failed]
			];
			{chisq, answer} = i;

			paramvalues = paramnames /. answer;

			(* 
			 * Evaluate alphaF for the values of the parameters.
			 * The order of evaluation interacts strongly with
			 * the speed of the calculation.
			 *)
			alpha = alphaF /. answer;
			(* Turn it into a matrix *)
			alpha = Outer[Times,alpha, alpha];
			(* Replace all numbers by a list of n such numbers *)
			alpha = MapAt[ Table[#, {n}]&, alpha,
				Position[ alpha, i_ /; NumericQ[i], {2} ]];
			(* Evaluate at the values of the independent variable *)
			alpha = alpha /. ind -> x;
			(*
			 * Finally, divide by erry^2 and add up the terms.
			 *)
			alpha = Dot[#,1/erry^2]& /@ alpha;

			Break[];

		];



		If[optShowProgress,
			If[nvars >= 3,
				Print[	"Iteration ", iter, " Chi-squared = ", chisq,
						" Previous chi-squared: ", oldchisq],
			(* else *)
				Print[	"Iteration ", iter, " Sum of squares = ", chisq,
						" Previous sum of squares: ", oldchisq]
			];
			Print["  Current answer: ", newanswer];
		];

		If[ iter > 1 && chisq <= oldchisq &&
				( 	oldchisq - chisq < optAbsoluteChiSquaredTolerance ||
					(oldchisq - chisq)/oldchisq < 
						optRelativeChiSquaredTolerance ||
					Max[Abs[deltaa/paramvalues]] < optValueTolerance ),

			paramvalues = newparamvalues;
			answer = newanswer;

			If[optShowProgress,
				If[oldchisq - chisq < optAbsoluteChiSquaredTolerance,
					If[nvars >= 3,
						Print[	"Convergence: the previous chisquared minus",
								" the current chisquared is less than: ",
								optAbsoluteChiSquaredTolerance],
					(* else *)
						Print[	"Convergence: the previous sumofsquares minus",
								" the current sumofsquares is less than: ",
								optAbsoluteChiSquaredTolerance]
					];
				];
				If[(oldchisq - chisq)/oldchisq < 
						optRelativeChiSquaredTolerance,
					If[nvars >= 3,
						Print[	"Convergence: the relative decrease in the",
								" chisquared is less than: ", 
								optRelativeChiSquaredTolerance],
					(* else *)
						Print[	"Convergence: the relative decrease in the",
								" sumofsquares is less than: ", 
								optRelativeChiSquaredTolerance]
					];
				];
				If[Max[Abs[deltaa/paramvalues]] < optValueTolerance,
					Print[	"Convergence: the maximum change in the",
							" parameters is less than: ",
							optValueTolerance];
				];
			];

			If[optReweight === False,
				If[reweightflag,
					(* Make chisq the sum of the squares.  Note the
					 * only change from the usual chisqF is we have
					 * dropped the division by erry.
					 *)
					chisqF = Plus @@ ((y - (model /. ind -> x))^2);
					chisq = chisqF /. answer//N;
				];
				Break[];
			];
			If[optShowProgress,
				Print["  Reweighting the data with erry = ", chisq/dof];
			];
			pseudoerry = Sqrt[chisq/dof]//N;
			If[pseudoerry == 0,
				pseudoerry = $MinMachineNumber
			];
			erry = Table[pseudoerry, {n}];
			nvars = 3;
			oldchisq = $MaxMachineNumber;
			lambda = 0.01;
			optReweight = False;
			reweightflag = True;
		];

		If[iter == 1 || chisq < oldchisq,

			(* Effective variance *)
			If[nvars == 4,
				erry = Quadrature[ origerry, 
					(D[model,ind] /. Join[newanswer,{ind -> x}])*errx]//N;
			];
			If[!(Union[NumericQ /@ erry] == {True}),
				Message[EDAFindFit::eek];
				(* Turn on General::unfl messages if they have
				 * been turned off. *)
				If[wasOn, On[General::unfl]];
				Return[$Failed];
			];

			If[optShowProgress,
				Print["  Evaluating matrices for the current answer."];
			];

			beta = D[chisqF,#]& /@ paramnames;
			beta = -0.5 * (beta /. newanswer);

			(*
			 *  Evaluate alphaF for the values of the parameters.
			 *  Note that the order of evaluation is important
			 *   to get the best speed of the calculation.
			 *)
			alpha = alphaF /. newanswer;
			(* Turn it into a matrix *)
			alpha = Outer[Times,alpha, alpha];
			(* Replace all numbers by a list of n such numbers *)
			alpha = MapAt[ Table[#, {n}]&, alpha,
				Position[ alpha, i_ /; NumericQ[i], {2} ]];
			(* Evaluate at the values of the independent variable *)
			alpha = alpha /. ind -> x;
			(*
			 * Finally, divide by erry^2 and add up the terms.
			 *)
			alpha = Dot[#,1/erry^2]& /@ alpha;

			oldchisq = chisq;
			answer = newanswer;
			paramvalues = newparamvalues;
			(* Never allow a zero. *)
			paramvalues = ReplacePart[paramvalues, $MinMachineNumber,
				Join[ Position[paramvalues,0], Position[paramvalues, 0.]] ];
			lambda /= 10,

		(* else *)

			lambda *= 10;
		];

		If[optShowProgress,
			Print["  Scale for step size (lambda) = ", lambda];
		];

		alphaprime =  alpha * (Table[1, {m}, {m}] +
						DiagonalMatrix[Table[lambda,{m}]]);

		If[optShowProgress,
			Print["  Using singular value decomposition to find corrections."];
		];
		If[ $VersionNumber < 5.0,
			{i,j,k} = SingularValues[alphaprime],
		(* else *)
			{i,j,k} = CompactSVD[alphaprime];
		];
		deltaa = Transpose[k] . DiagonalMatrix[1/j] . i . beta//N;

		If[optShowProgress,
			Print["  Corrections to parameters: ", deltaa];
		];
		newparamvalues = paramvalues + deltaa;

		If[optShowProgress,
			i = Position[newparamvalues - minparamvalues,
				i_ /; i < 0];
			If[Length[i] != 0,
				Print[	"Invoking minimum values for",
						" parameter(s): ", i];
			];
			i = Position[maxparamvalues - newparamvalues ,
				i_ /; i < 0];
			If[Length[i] != 0,
				Print[	"Invoking maximum values for",
						" parameter(s): ", i];
			];
		];
		newparamvalues = Max /@ Transpose[
			{newparamvalues,minparamvalues}];
		newparamvalues = Min /@ Transpose[
			{newparamvalues,maxparamvalues}];

		newanswer = Apply[Rule, Transpose[{paramnames,newparamvalues}],
						{1}];

	];

	(*************************)
	(* END of iteration loop *)
	(*************************)

	If[iter > optMaximumIterations,
		Message[EDAFindFit::maxiters, iter - 1];
	];

	If[optReturnErrors || optReturnCovariance,
		If[optShowProgress,
			Print["Calculating covariance matrix using singular value decomposition."];
		];
		If[ $VersionNumber < 5.0,
			{i,j,k} = SingularValues[alpha],
		(* else *)
			{i,j,k} = CompactSVD[alpha];
		];
		covar = Transpose[k] . DiagonalMatrix[1/j] . i;
	];
	If[optReturnErrors,
		i = Table[Sqrt[ Abs[covar[[j,j]] ] ], {j,m}];
		j = Transpose[ {paramvalues,i} ];
		If[optUseSignificantFigures,
			j = AdjustSignificantFigures[ j, optsAdjustSignificantFigures];
		];
		answer = Apply[ Rule,
			Transpose[ {paramnames, j}], {1} ];
	];
	If[optReturnCovariance,
		answer = Append[answer, EDACovariance -> covar]
	];

	If[optShowFit || optReturnResiduals,
		If[optShowProgress,
			Print["Calculating residuals"];
		];
		i = model /. Apply[ Rule, 
			Transpose[ {paramnames, paramvalues} ], {1} ];
		j = i /. ind -> x;
		residuals = y - j;
		If[nvars > 2,
			residuals = Transpose[ {residuals,erry} ];
			If[optUseSignificantFigures,
				residuals = AdjustSignificantFigures[ residuals,
					optsAdjustSignificantFigures];
			];
		];
		residuals = Transpose[{x,residuals}];
	];

	If[optReturnResiduals,
		answer = Append[answer, Residuals -> residuals];
	];

	If[optReturnEffectiveVariance,
		answer = Append[answer, EffectiveVariance -> erry^2];
	];

	If[optShowFit,
		If[optReturnErrors && !optReturnCovariance,
			(* EDACovariance is not in answer.  Save the answer,
			 * and put EDACovariance in.
			 *)
			i = answer;
			answer = Append[answer, EDACovariance -> covar];
		];
		(* For Mathematica 6.0, terminating the graphics with a
		 * a semi-colon suppresses the graphics output, but the
		 * scope of Print[] has been extended to include graphics.
		 *)
		If[ $VersionNumber < 6.0,
			ShowFitResult[data, model, ind, parameters, answer,
				residuals, Evaluate @ optsShowFitResult ];,
		(* else *)
			Print[ShowFitResult[data, model, ind, parameters, answer,
				residuals, Evaluate @ optsShowFitResult ]];
		];
		If[optReturnErrors && !optReturnCovariance,
			(* Restore answer *)
			answer = i;
		];
	];

	If[orignvars < 3,
		If[reweightflag,
			answer = Append[answer, PseudoErrorY -> pseudoerry];
		];
		answer = Append[answer, SumOfSquares -> chisq],
	(* else *)
		answer = Append[answer, ChiSquared -> chisq]
	];
	answer = Append[answer, DegreesOfFreedom -> dof];

	If[optReturnFunction,
		If[optReturnErrors && !optReturnCovariance,
			(* EDACovariance is not in answer.  Put it in.
			 * Note that we don't bother to save the old answer
			 * since we're going to overwrite it anyhow.
			 *)
			answer = Append[answer, EDACovariance -> covar];
		];
		answer = ToFitFunction[answer,model,ind,parameters,
					optsToFitFunction];
	];

	(* Turn on General::unfl messages if they have been turned off. *)
	If[wasOn, On[General::unfl]];

	answer
]

(*
 * This form calculates the residuals and punts to the full form.
 *)
ShowFitResult[data_, model_, ind_, parameters_, result_,
	opts___?OptionQ] := Module[
	{
		i,j,
		residuals,
		n,nvars,x,errx,y,erry
	},

	If[Union[Head /@ result] =!= {Rule},
		Message[ShowFitResult::badresult];
		Return[$Failed]
	];

	Check[n = UnpackData[data];,
		Return[$Failed]
	];
	If[n === $Failed,
		Message[ShowFitResult::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	residuals = Residuals /. result;
	If[residuals === Residuals,
		(* Have to calculate them *)
		i = model /. result;
		If[Length[i] == 2,
			i = First[i]
		];
		j = i /. ind -> x;
		residuals = y - j;
		If[nvars > 2,
			residuals = Transpose[ {residuals,erry} ];
			If[optUseSignificantFigures,
				residuals = AdjustSignificantFigures[ residuals,
					optsAdjustSignificantFigures];
			];
		];
		residuals = Transpose[{x,residuals}];
	];

	ShowFitResult[data,model,ind,parameters,result,
		residuals, opts]
]

ShowFitResult[data_, model_, ind_, parameters_, result_,
	res_, opts___?OptionQ] := Module[

	{
		i,j,k,l,

		combinedplot, dataplot, fitplot, residualplot,

		func,
		quadrant,

		halfx, halfy,
		minx, maxx, miny, maxy,
		n,nvars,x,errx,y,erry,

		paramnames, 

		optExtrapolate,
		optResidualPlacement,

		optsEDAListPlot,
		optsPlot,
		optsToFitFunction
	},


	i = Complement[ First /@ {opts},
				First /@ Options[ShowFitResult],
				First /@ Options[ToFitFunction] ];
	If[Length[i] != 0,
		Message[ShowFitResult::badopt, #]& /@ i;
		Return[$Failed]
	];
	optExtrapolate = Extrapolate /. {opts} /. Options[ShowFitResult];
	optResidualPlacement = ResidualPlacement /. {opts} /. 
		Options[ShowFitResult];

	optsEDAListPlot = FilterOptions[ EDAListPlot, opts];
	optsPlot = FilterOptions[ Plot, opts];
	optsToFitFunction = FilterOptions[ ToFitFunction, opts];

	If[Union[Head /@ result] =!= {Rule},
		Message[ShowFitResult::badresult];
		Return[$Failed]
	];

	Check[n = UnpackData[data];,
		Return[$Failed]
	];
	If[n === $Failed,
		Message[ShowFitResult::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	(*
	 * Get the names of the parameters, drop any values
	 * on the floor.  Note that we return a second variable
	 * in all cases for no reason except to keep the first
	 * rule from also matching the last.
	 *)
	{paramnames,j} = Transpose[
		Map[Replace[#, {
			{i_, j_?NumericQ} -> {i,1},
			{i_, j:(-Infinity | _?NumericQ), k_?NumericQ, 
				l:(_?NumericQ | Infinity) } -> {i,1},
			{i_, j_?NumericQ, k_?NumericQ} -> {i,1},
			i_ -> {i,1},
			_ -> {$Failed,1}
		}]&, parameters]];
	If[(Select[paramnames,(# === $Failed)&] =!= {}),
				Message[EDAFindFit::badparams, parameters];
				Return[$Failed]
	];

    (*
     * If the user has explicitly asked for PlotRange -> Automatic,
     * we just remove that from the options to Plot and EDAListPlot,
     * since this routine does its own "automatic" PlotRange
     * algorithm.
     *)
    optsPlot = Sequence @@ DeleteCases[
        Apply[List, Hold[optsPlot]], PlotRange -> Automatic];
    optsEDAListPlot = Sequence @@ DeleteCases[
        Apply[List, Hold[optsEDAListPlot]], PlotRange -> Automatic];

	Switch[nvars,
		1,  minx = 1;
			maxx = n;
			miny = Min[y];
			maxy = Max[y],

		2,  minx = Min[x];
			maxx = Max[x];
			miny = Min[y];
			maxy = Max[y],

		3,  minx = Min[x];
			maxx = Max[x];
			miny = Min[y - erry];
			maxy = Max[y + erry],

		4,  minx = Min[x - errx];
			maxx = Max[x + errx];
			miny = Min[y - erry];
			maxy = Max[y + erry];
	];

	If[optExtrapolate =!= False,
		If[Length[optExtrapolate] != 2 ||
			Union[NumericQ /@ optExtrapolate] =!= {True},
				Message[ShowFitResult::badextrap];
				Return[$Failed]
		];
		minxSave = minx;
		maxxSave = maxx;
		minx = First[optExtrapolate];
		maxx = Last[optExtrapolate];
		optsPlot = {PlotRange -> {{minx, maxx}, All}, optsPlot};
	];


	dataplot = EDAListPlot[data,
		optsEDAListPlot,
		PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
			{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}},
		DisplayFunction -> Identity ];

	func = ToFitFunction[result, model, ind, paramnames,
		optsToFitFunction ];
	If[Head[func] =!= List,
		fitplot = Plot[ func, {ind, minx, maxx}, 
		Evaluate @ optsPlot,
		PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
			{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}},
		DisplayFunction -> Identity ],
	(* else *)
		fitplot = {
			Plot[ First[func],
				{ind,minx,maxx},
				DisplayFunction -> Identity,
				Evaluate @ optsPlot,
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
					{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}}],
			Plot[ (Subtract @@ func),
				{ind,minx,maxx},
				DisplayFunction -> Identity,
				Evaluate @ optsPlot,
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
					{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}}],
			Plot[ (Plus @@ func),
				{ind,minx,maxx},
				DisplayFunction -> Identity,
				Evaluate @ optsPlot,
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
					{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}}]
		};
	];

	If[optExtrapolate =!= False,
	     minx = minxSave;
		 maxx = maxxSave;
	];

	(*
	 * Figure out which quadrant has the least data, so we
	 * can put the residual plot there.  If all quadrants
	 * have at least 10% of the data, put out the residual
	 * plot separately.
	 *)
	If[optResidualPlacement === Automatic,
		halfx = (maxx - minx)/2 + minx;
		halfy = (maxy - miny)/2 + miny;

		quadrant = 0;
		i = Transpose[{x,y}];


		j = { Length[ Select[i, (Part[#,1] > halfx && Part[#,2] > halfy)&]],
			Length[ Select[i, (Part[#,1] < halfx && Part[#,2] > halfy)&]],
			Length[ Select[i, (Part[#,1] < halfx && Part[#,2] < halfy)&]],
			Length[ Select[i, (Part[#,1] > halfx && Part[#,2] < halfy)&]]};
		k = Min[j];
		If[k < 0.1 n,
			quadrant = First[Position[j, k]];
			Switch[quadrant,
				{1},	i = 0.5;
						j = 0.5,
				{2},	i = 0;
						j = 0.5,
				{3},	i = 0;
						j = 0,
				{4},	i = 0.5;
						j = 0
			],
		(* else *)
			optResidualPlacement = Separate;
		],
	(* else *)
		Switch[ optResidualPlacement,
				1,  i = 0.5;
					j = 0.5,
				2,  i = 0;
					j = 0.5,
				3,  i = 0;
					j = 0,
				4,  i = 0.5;
					j = 0,
				Separate,Null,
				None,Null,
				_,  Message[ShowFitResult::badresplacement,
						optResidualPlacement];
					Return[$Failed];
		];
	];

	If[optResidualPlacement =!= None,
		If[optResidualPlacement === Separate,
			residualplot = EDAListPlot[res,
				If[ $VersionNumber < 6.0,
					PlotJoined -> True,
				(* else *)
					Joined -> True
				],
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
					All},
				Axes -> {True, False}, Frame -> True,
				PlotLabel -> "Residuals",
				FrameTicks -> {None, Automatic},
				optsEDAListPlot,
				DisplayFunction -> Identity ],
		(* else *)
			residualplot = EDAListPlot[res,
				If[ $VersionNumber < 6.0,
					PlotJoined -> True,
				(* else *)
					Joined -> True
				],
				Axes -> {True, False}, Frame -> True,
				PlotLabel -> "Residuals",
				PlotRange -> All,
				FrameTicks -> {None, Automatic},
				optsEDAListPlot,
				DisplayFunction -> Identity ];
		];
	];


    (*
     * In an extrapolation, we have to place to origin at the x value
     * that is the miminum we produced for fitplot. Otherwise, there
     * will be a gap between the x axis and the y axis. This little
     * hack produces that effect.
     *)
    If[optExtrapolate =!= False,
        minx = First[optExtrapolate]/1.1;
        maxx = 0;
    ];

	combinedplot = Show[ {fitplot, dataplot},
		AxesOrigin -> {1.1*minx - .1maxx,1.1*miny - .1*maxy},
		DisplayFunction -> Identity];
	If[optResidualPlacement =!= Separate && optResidualPlacement =!= None,
		Show[combinedplot,
			Prolog -> Rectangle[ Scaled[{i,j}],
				Scaled[{i + .5, j + .5}], residualplot],
			DisplayFunction -> $DisplayFunction ],
	(* else *)
		If[optResidualPlacement =!= None,
			(* Then is has to be Separate *)
			If[ $VersionNumber < 6.0,
				{Show[ FullGraphics @ combinedplot, DisplayFunction ->
					$DisplayFunction],
				Show[ FullGraphics @ residualplot,
					DisplayFunction -> $DisplayFunction]},
			(* else *)
				Show[ GraphicsColumn[{combinedplot, residualplot}],
					DisplayFunction -> $DisplayFunction]
			],
		(* else *)
			Show[ combinedplot, DisplayFunction -> $DisplayFunction]
		]
	]

]

ToFitFunction[result_, model_, ind_Symbol, parameters_?ListQ,
	opts___?OptionQ] := Module[
	{
		i,j,k,l,				(* dummy variables *)

		answer,
		covar,					(* covariance matrix *)
		errlist,				(* list of errors in parameters values *)
		m,						(* number of parameters *)
		paramnames,				(* list of names from `parameters' *)
		valuelist,				(* list of values of parameters *)

		optUseFitErrors
	},

	i = Complement[ First /@ {opts}, First /@ Options[ToFitFunction] ];
	If[Length[i] != 0,
		Message[ToFitFunction::badopt, #]& /@ i;
		Return[$Failed]
	];

	optResidualPlacement = ResidualPlacement /. {opts} /. 
		Options[ShowFitResult];
	optUseFitErrors = UseFitErrors /. {opts} /.
		Options[ToFitFunction];

	(*
	 * Get the names of the parameters, drop any values
	 * on the floor.  Note that we have to return a dummy
	 * here, or the next to last rule will also match the last.
	 *)
	{paramnames,j} = Transpose[
		Map[Replace[#, {
			{i_, j_?NumericQ} -> {i,1},
			{i_, j:(-Infinity | _?NumericQ), k_?NumericQ, 
				l:(_?NumericQ | Infinity) } -> {i,1},
			{i_, j_?NumericQ, k_?NumericQ} -> {i,1},
			i_ -> {i,1},
			_ -> {$Failed,1}
		}]&, parameters]];
	If[(Select[paramnames,(# === $Failed)&] =!= {}),
				Message[EDAFindFit::badparams, parameters];
				Return[$Failed]
	];
	m = Length[paramnames];

	(* Get values from result *)
	valuelist = paramnames /. result;

	If[MatrixQ[valuelist] && optUseFitErrors,

		(* There are errors in the parameters  and we want to 
		 * use them.
		 *)
		answer = First[model /. result];

		covar = EDACovariance /. result;
		If[covar =!= EDACovariance,
			(* Use the sign of the terms in the EDACovariance matrix
			 * to find the signs of each term in the combination.
			 * Note, when the matrix has Sign -1, we set it to I
			 * so the minus sign survives after using Quadrature.
			 * Also, we must take Abs of the Quadrature since it
			 * is possible to get an imaginary answer.
			 *)
			i = D[model,#]& /@ paramnames;
			errlist = Last /@ valuelist;
			j = Sign /@ First /@ covar;
			j = j /. -1 -> I;
			errlist = errlist j;
			errlist = errlist i;
			errlist = errlist /. Apply[Rule,
				Transpose[ {paramnames, First /@ valuelist}],
					{1} ];
			errlist = Abs[ Quadrature[ errlist ]]//N;
			answer = {answer, errlist},
		(* else *)
			(* A sometimes reasonable heuristic. *)
			i = D[model,#]& /@ paramnames;
			i = i /. Apply[Rule,
				Transpose[ {paramnames, First /@ valuelist}],
					{1} ];
			errlist = i (Last /@ valuelist);
			errlist = Quadrature[ errlist];
			answer = {answer, errlist};
		],
	(* else *)
		If[MatrixQ[valuelist],
			(* There are errors in the parameters but we want
			 * to drop them on the floor. *)
			answer = First[model /. result],
		(* else *)
			answer = model /. result
		];
	];

	answer
]

(* Some convenience functions of common spectral shapes *)

BreitWigner[x_, a_, x0_, gamma_] := Module[ {answer},
	answer = a*(gamma/(2Pi) / ((x - x0)^2 + (gamma/2)^2));
	If[NumericQ[x] && NumericQ[a] && NumericQ[x0] && NumericQ[gamma],
		answer = N[answer]
	];
	answer
];

Galatry[y_, z_, tau_] := Module[ {res, wasOn},
	wasOn = Head[General::unfl] =!= $Off;
	Off[General::unfl];
	res = Exp[-y*tau + 1/(2*z^2)*(1 - z*tau - Exp[-z*tau])];
	If[NumericQ[y] && NumericQ[z] && NumericQ[tau],
		res = res//N;
	];
	If[wasOn, On[General::unfl]];
	If[res === Underflow[], 0, res]
]

Gaussian[x_,ampl_,x0_,sigma_] := Module[ {answer},
	answer = ampl*Exp[-((x - x0)/sigma)^2 /2];
	If[NumericQ[x] && NumericQ[ampl] && NumericQ[x0] && NumericQ[sigma],
		answer = N[answer];
	];
	answer
]

Lorentzian[x_,a_,x0_,gamma_] := BreitWigner[x,a,x0,gamma]

PearsonVII[x_, a_, x0_, gamma_, m_] := Module[ {answer},
	answer = a / (1 + 4*((x - x0)/gamma)^2*(2^(1/m) - 1))^m;
	If[NumericQ[x] && NumericQ[a] && NumericQ[x0] && NumericQ[gamma] &&
		NumericQ[m],
			answer = N[answer]
	];
	answer
];

RelativisticBreitWigner[m_, mr_, gamma_, m1_, m2_] := Module[

	{norm, Q, psfact, partialgamma, crosssection},

	(*  Calculates a Breit Wigner resonance		 *)

	(*  Calculate the phase space factor and the partial width first *)
	(*  First make sure we are above threshold					   *)

	If[ m < m1 + m2,
			crosssection = 0.0,
	(* else *)
		Q = Sqrt[( m^2 - (m1 + m2)^2)(m^2 - (m1 - m2)^2)]/(2m);
			psfact = Q/(2m);
			partialgamma  = psfact/(2m);

		(*  Calculate the normalisation factor   *)
			norm = 8 mr^3 gamma^2 /
				Sqrt[( mr^2 - (m1 + m2)^2)(mr^2 - (m1 - m2)^2)];
		(*  Now calculate the cross section	*)
			crosssection = norm (m mr partialgamma) /
				Abs[mr^2 - m^2 - I mr gamma]^2
	];

	If[NumericQ[m] && NumericQ[mr] && NumericQ[gamma] && NumericQ[m1] &&
		NumericQ[m2],
			crosssection = N[crosssection];
	];
	crosssection
]

Voigt[y_, tau_] := Module[ {answer},
	answer = Exp[ -y*tau - (tau/2)^2 ];
	If[NumericQ[y] && NumericQ[tau],
		answer = N[answer]
	];
	answer
]

End[]

SetAttributes[EDAFindFit, {ReadProtected}]
SetAttributes[ShowFitResult, {ReadProtected, HoldAll}]
SetAttributes[ToFitFunction, {ReadProtected}]

Protect[
	EDAFindFit,
	BreitWigner,
	Gaussian,
	Galatry,
	Lorentzian,
	PearsonVII,
	Voigt,
	ShowFitResult,
	ToFitFunction
]

EndPackage[]
