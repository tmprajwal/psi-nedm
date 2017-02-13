(* $Id: LinearFit.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Linear Least Square Fitting *)

(* :Context: EDA`LinearFit` *)

(* :Author: David M. Harrison *)

(* :Summary:
	LinearFit - fit to a linear model using least squares.
	ShowLinearFit - show a fit result.
	ToLinearFunction - convert a fit result to a function.
*)

(* :Copyright: Copyright 1995 - 2013, Wolfram Research, Inc.*)

(* :Package Version: 1.3 *)

(* :Mathematica Version: 9.0 *)

(* :History:
	Version 0.9 by David M. Harrison, Summer 1993.
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
	Version 1.2 by David M. Harrison, May 2009.
	Version 1.3 by David M. Harrison, February 2013.
*)

(* :Keywords:
*)

(* :Sources:
	Philip R. Bevington, "Data Reduction and Error Analysis" (McGraw-
		Hill, 1969), pgs. 134 ff and 204 ff.
	Jay Orear, Amer. Jour. Physics 50, (1982) 912.
	William H. Press, Brian P. Flannery, Saul A. Teukolsky and William
		T. Vetterling, "Numerical Recipes in C" (Cambridge Univ., 1988),
		Chapter 14.
	William H. Press and Saul A. Teukolsky, Computers in Physics 6,
		(1992) 274.

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
		BeginPackage["EDA`LinearFit`",
			"EDA`Common`",	
			"EDA`EDAGraphics`",
			"EDA`ErrorPropagation`",	(* Used in computing residuals *)
			"EDA`Utilities`",
			"Statistics`DescriptiveStatistics`",
			"Utilities`FilterOptions`"
		],
	$VersionNumber < 9.0,
		BeginPackage["EDA`LinearFit`",
			"EDA`Common`",	
			"EDA`EDAGraphics`",
			"EDA`ErrorPropagation`",	(* Used in computing residuals *)
			"EDA`Utilities`",
			"Utilities`FilterOptions`"
		],
	True,
		BeginPackage["EDA`LinearFit`",
			"EDA`Common`",	
			"EDA`EDAGraphics`",
			"EDA`ErrorPropagation`",	(* Used in computing residuals *)
			"EDA`Utilities`"
		]
];

Unprotect[LinearFit, ShowLinearFit, ToLinearFunction]

LinearFit::usage = 
	"LinearFit[data, factors, parameter] does least-square fits of
	data. The default model is a polynomial. The model may be
	controlled with a Basis option. For the default model, factors
	is a list of the powers of the polynomial to include in the fit.
	The parameter is a symbol and LinearFit will return a list of
	rules for an array of parameters; for the default model the index
	is equal to the corresponding power of the polynomial. The default
	method of solving the system uses singular value decomposition; 
	the method may be controlled with a Method option."

Basis::usage =
	"Basis is an option for LinearFit and ToLinearFunction that specifies
	the basis function. The default value of Basis is Power, which
	specifies that the basis functions are Power[x, n], where x is the
	independent variable and n is one of the values of the factor's
	argument to LinearFit. To change the basis function to, say, a
	sum of sines, one may define:\n
	mySin[x_, n_] := Sin[ 2Pi x n]\n
	 and add:\n
	Basis -> mySin\n
	 to the call to LinearFit. Then if the factors are {1,2,3}, the fit will
	be to the function:\n
	y = a[1] Sin[2Pi x] + a[2] Sin[4Pi x] + a[3] Sin[6Pi x]."

Brent::usage =
	"Brent is an option to LinearFit. When set to Automatic (the
	default), if the data has errors in both coordinates and the fit
	is to a straight line with powers \"{0, 1}\" and ReturnCovariance
	is set to False (its default), then a Brent minimization is used to
	perform the fit. When set to False, the standard effective variance
	method is used. If Brent minimization is used for the fit, then the
	Method option only affects the initial guess of the fit parameters.
	When set to False, the Method option controls every iteration of 
	the fit. If Brent minimization is used for the fit, the tolerance of
	the result is controlled by the BrentTolerance option."

BrentTolerance::usage =
	"BrentTolerance is an option to LinearFit. It only has an effect
	when Brent minimization is used for the fit. Then it is the
	tolerance used by the minimization procedure."

ConvergenceTest::usage =
	"ConvergenceTest is an option to LinearFit. It only has meaning
	when the data has declared errors in both coordinates and the
	Brent option is set to False. In this case, if ConvergenceTest
	is Automatic (the default), then the iterated fit is declared to have
	converged when either ChiSquared / DegreesOfFreedom is less than
	0.001 or ChiSquared is decreasing and has decreased, in the current
	iteration, by less than 10% from the previous iteration.
	If ConvergenceTest is set to a function, that function is to be
	supplied by the user and its input arguments are:\n
	func[chisq,oldchisq,dof,values,oldvalues], where \"chisq\" is the
	ChiSquared of the current iteration, \"oldchisq\"
	is the ChiSquared of the previous iteration, \"dof\" is the
	DegreesOfFreedom of the fit, \"values\" is a list of the current
	values of the fitted parameters, and \"oldvalues\" is a list of the
	values of the fitted parameters from the previous iteration. The
	function is expected to be returned True if the fit has converged,
	False otherwise. On the first iteration, oldchisq is $MaxMachineNumber
	and oldvalues is a list of lengths equal to the number of factors,
	each element of which is equal to $MaxMachineNumber."

InverseMethod::usage =
	"InverseMethod is an option to LinearFit that specifies the
	method for computing the Covariance matrix if the method is
	LUD. For this method the default is just the inverse function.
	It may also be SVD, in which case, singular value decomposition is used
	to compute the inverse. For the default method of SVD, this
	option is ignored."

LUD::usage =
	"LUD is an optional method for LinearFit. In this method
	LUDecomposition and LUFactor are used on the normal equations
	to do the fit."
SVD::usage =
	"SVD is the default method and an optional InverseMethod for
	LinearFit. For this method singular value decomposition is used
	on the design matrix to do the fit. For InverseMethod, singular
	value decomposition is
	used to compute the Covariance matrix, which has meaning only if
	the method is LUD."


ToLinearFunction::usage =
	"ToLinearFunction[result, factors, parameter, ind] takes results
	from LinearFit with factors and parameter, just as input
	to LinearFit returns a function of the independent variable
	\"ind\". If the parameters of the fit have associated errors,
	two functions are returned: the first is the result as a function
	of ind and the second is the error. In this case, the errors are
	combined."

(*
 * We don't include options to ShowLinearFit and ToLinearFunction
 * here, mostly because the list then gets too long to be read by
 * a human being.
 *)
Options[LinearFit] = {
	Basis -> Power,
	Brent -> Automatic,
	BrentTolerance -> 0.001,
	ConvergenceTest -> Automatic,
	InverseMethod -> Inverse,
	MaximumIterations -> 15,
	Method -> SVD,
	ReturnCovariance -> False,
	ReturnEffectiveVariance -> False,
	ReturnErrors -> True,
	ReturnFunction -> False,
	ReturnResiduals -> False,
	Reweight -> True,
	ShowFit -> True,
	EDAShowProgress -> False,
	UseSignificantFigures -> True
}

ShowLinearFit::usage =
	"ShowLinearFit[data, factors, parameter, result] takes the default
	form of the result from LinearFit and displays graphical information about
	the fit. The other three input parameters to ShowLinearFit are expected
	to be exactly those given to LinearFit to produce results.
	ShowLinearFit[data, factors, parameters, result, residuals] is the
	same as the first form, except the residuals of the fit are
	given as the final argument."

(*
 * Thus must be set before Options[ShowLinearFit], since the latter
 * uses Options[ToLinearFunction].
 *)
Options[ToLinearFunction] = {
	Basis -> Power,
	UseFitErrors -> True
}

Options[ShowLinearFit] = Union[
	{	Basis -> Power,
		Extrapolate -> False,
		ResidualPlacement -> Automatic,
		UseSignificantFigures -> True
	}, Options[EDAListPlot], Options[Plot], Options[ToLinearFunction]
]



LinearFit::adefined =
	"At least one of the array members for the variable named
	in 'parameter' is already defined. Choose another name or
	execute Remove[\"`1`*\"]."
LinearFit::baderror = "An error was specified as negative or zero."
LinearFit::badopt = "Option `1` is not known."
LinearFit::chisqnonumber = "A non-numeric chi-squared was found."
LinearFit::dataformat = "The data format appears to be incorrect."
LinearFit::brent = "Brent must be either True or False."
LinearFit::brentcovar =
	"For the Brent method, the covariance matrix is unavailable.
	LinearFit is turning off the ReturnCovariance option."
LinearFit::maxiterations = 
	"Warning: fit did not converge in `1` iterations. You may wish to
	use the MaximumIterations option."
LinearFit::nobrent =
	"Brent can only be True when factors are {0,1}, the Basis is equal
	to Power, and there are errors declared in both coordinates of the
	data. LinearFit is turning off the Brent option."
LinearFit::nodof = "No degrees of freedom."
LinearFit::noerrors = "Warning: returned errors may have no meaning."
LinearFit::nocovariance = "Warning: covariance matrix may have no meaning."
LinearFit::noinversemethod = "InverseMethod `1` is not known."
LinearFit::nomethod = "Method `1` is not known."
LinearFit::oneoverzero = "The design matrix contains 1/0 terms."
LinearFit::returnfunction = "The ReturnFunction option is not correct."
LinearFit::singular = "Singular matrix, cannot fit."

ShowLinearFit::badextrap = "The Extrapolate option is not well formed."
ShowLinearFit::dataformat = "The data format appears to be incorrect."
ShowLinearFit::badopt = "Option `1` is not known."
ShowLinearFit::badresplacement = 
	"ResidualPlacement of `1` is not legal."
ShowLinearFit::badresult = 
	"The result given ShowLinearFit does not contain Rules."
	

ToLinearFunction::badopt = "Option `1` is not known."
ToLinearFunction::droppingerrors =
	"Warning: for over two fit parameters or if UseFitErrors is set to
	False, a function corresponding to the errors in the fit parameters
	is not calculated."

ToLinearFunction::adefined =
	"At least one of the array members for the variable named
	in 'parameter' is already defined. Choose another name or
	execute Remove[\"`1`*\"]."

Begin["`Private`"]

(* 
 * The word "powers" below is appropriate for the default basis
 * function of Power; it may not be for user-chosen bases.
 *)
LinearFit[data_, powers_?(VectorQ[#,NumericQ]&) , a_Symbol,
	opts___?OptionQ] := Module[
	{
		answer,					(* for storing the current answer *)
		oldanswer,				(* from the previous iteration *)

		(*
		 * This group of variables are named (within upper-lower case
		 * differences) as in Press, et. al, Numerical Receipes, Section 14.3.
		 *)
		A,						(* design matrix *)
		alpha,
		b,
		beta,
		c,
		m = Length[powers],
		n,
		u,v,w,					(* for SVD Method *)

		chisq, oldchisq,
		dof,					(* degrees of freedom *)
		erra,					(* errors in the fitted parameters *)
		i,j,k,					(* dummy variables *)
		(* 
		 * ind is placeholder for the independent variable.
		 * It must never have a value assigned to it.
		 *)
		ind,					
		indBasisList,			(* list of basis funcs, function of ind *)
		iter,
		nvars, orignvars,
		residuals,
		zeroPositions,			(* where zeros exists in powers *)

		(* 
		 * These are options used by LinearFit itself.  The first
		 * group are for options named in this package, the second
		 * group for options named in Common`Common`.
		 *)
		optBasis,
		optBrent,
		optBrentTolerance,
		optConvergenceTest,
		optInverseMethod,
		optMethod,
		optReweight,

		optMaximumIterations,
		optReturnCovariance,
		optReturnEffectiveVariance,
		optReturnErrors,
		optReturnFunction,
		optReturnResiduals,
		optShowFit,
		optShowProgress,
		optUseSignificantFigures,

		(*
		 * Options used by the named routine.
		 *)
		optsAdjustSignificantFigures,
		optsToLinearFunction,
		optsShowLinearFit,

		x,y,errx,erry,origerry
	},

	i =	Complement[ First /@ {opts},
				First /@ Options[LinearFit],
				First /@ Options[AdjustSignificantFigures],
				First /@ Options[ToLinearFunction],
				First /@ Options[ShowLinearFit] ];
	If[Length[i] != 0,
		Message[LinearFit::badopt, #]& /@ i;
		Return[$Failed]
	];

	optBasis = Basis /. {opts} /. Options[LinearFit];
	optBrent = Brent /.  {opts} /. Options[LinearFit];
	optBrentTolerance = BrentTolerance /.  {opts} /. Options[LinearFit];
	optConvergenceTest = ConvergenceTest /.  {opts} /. Options[LinearFit];
	optInverseMethod = InverseMethod /. {opts} /. Options[LinearFit];
	optMaximumIterations = MaximumIterations /. {opts} /. Options[LinearFit];
	optMethod = Method /. {opts} /. Options[LinearFit];
	optReweight = Reweight /. {opts} /. Options[LinearFit];
	optReturnEffectiveVariance = ReturnEffectiveVariance /.
		{opts} /. Options[LinearFit];
	optReturnErrors = ReturnErrors /. {opts} /. Options[LinearFit];
	optReturnFunction = ReturnFunction /. {opts} /.  Options[LinearFit];
	optReturnCovariance = ReturnCovariance /. {opts} /. Options[LinearFit];
	optReturnResiduals = ReturnResiduals /. {opts} /. Options[LinearFit];
	optShowFit = ShowFit /. {opts} /. Options[LinearFit];
	optShowProgress = EDAShowProgress /. {opts} /. Options[LinearFit];
	optUseSignificantFigures = UseSignificantFigures /. {opts} /.
		Options[LinearFit];

	optsAdjustSignificantFigures = FilterOptions[AdjustSignificantFigures,
		opts];
	optsToLinearFunction = FilterOptions[ ToLinearFunction, opts];
	optsShowLinearFit = FilterOptions[ ShowLinearFit, opts];

	(*
	 * In the following, the use of With[ ... ] is crucial
	 * since it evaluates the constant outside of the normal
	 * evaluation context.
	 *)
	For[i = 1, i <= m, i++,
		With[{dummy = powers[[i]] },
			If[ValueQ[a[dummy]],
				Message[LinearFit::adefined, a];
				Return[$Failed]
			];
		];
	];

	Check[ n = UnpackData[data];,
		Return[$Failed]
	];
	If[n === $Failed,
		Message[LinearFit::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	(* 
	 *For reasons unknown, this can't be:
	 * If[optBrent === Automatic && Equal[powers,{0,1}] && 
	 *		!optReturnCovariance && nvars == 4,
	 *)
	If[optBrent === Automatic,
		If[ Equal[powers,{0,1}] && !optReturnCovariance && nvars == 4 &&
				optBasis === Power,
			optBrent = True,
		(* else *)
			optBrent = False
		];
	];
	If[optBrent,
		If[!Equal[powers,{0,1}] || nvars != 4 || !(optBasis === Power),
			Message[LinearFit::nobrent];
			optBrent = False;
		];
		If[optReturnCovariance,
			Message[LinearFit::brentcovar];
			optReturnCovariance = False;
		];
	];

	If[optShowProgress,
		Print[n, " data points, each with ", nvars, " variables."];
	];

	(* SVD blows up for this case. *)
	If[Equal[powers, {0}] && optMethod =!= LUD && optBasis === Power,
		If[optShowProgress,
			Print["Since powers are {0}, setting Method to LUD."];
		];
		optMethod = LUD;
	];

	orignvars = nvars;
	origerry = erry;

	(*
	 * Form a list of all positions in powers containing either
	 * a "0" or a "0.".
	 *)
	zeroPositions = Join[Position[powers,0.],Position[powers,0]];

	(*
	 * Form a list of basis functions of the placeholder variable
	 * 'ind'.
	 *)
	indBasisList = optBasis[ind,#]& /@ powers;

	If[ NumericQ[optBasis[ind,0]] && optBasis[ind,0] == 1 &&
		Length[zeroPositions] != 0,
			If[ Min[powers] < 0 && Length[Select[x,# == 0&]] != 0,
				Message[LinearFit::oneoverzero];
				Return[$Failed];
			];
			(*
			 * indBasisList is of, say: {1, ind, ind^2}.
			 * This will flop when we do optBasisList /. ind -> x
			 * so we change it to: { {1,1, ... , 1}, ind, ind^2}
			 * where the first term has length = Length[data].
			 *)
		 	i = Table[1, {n}];
		 	indBasisList = ReplacePart[
				indBasisList, i, zeroPositions ];
	];

	dof = n - m;
	If[dof < 1,
		Message[LinearFit::nodof];
		Return[$Failed]
	];

	oldchisq = $MaxMachineNumber;
	oldanswer = Table[ $MaxMachineNumber, {m}];

	(*********************************************************)
	(* Begin loop for iterations.  NB: for Reweight -> True  *)
	(* we will have exactly two iterations.  For errors in   *)
	(* both variables and not using the Brent method, we may *)
	(* have more iterations.  For all other cases there is   *)
	(* only one iteration ever.	                          *)
	(*********************************************************)

	For[iter = 1, iter <= optMaximumIterations, iter++,

		(* Calculate design matrix etc. *)
		A = 1/erry Transpose[ indBasisList /. ind -> x];
		b = y / erry;
		If[optMethod == LUD,
			(* the "normal equations" *)
			alpha = Transpose[A] . A;
			beta = Transpose[A] . b;
			If[Det[alpha] == 0,
				Message[LinearFit::singular];
				Return[$Failed]
			];
		];

		(* Here is where we do the fit *)
		Switch[ optMethod,

			LUD,		If[optShowProgress,
							Print["Iteration ", iter, " using LUD routines."]
						];
						answer = LUBackSubstitution[
							LUDecomposition[alpha],beta],

			SVD,		If[optShowProgress,
							Print["Iteration ", iter,
								" using singular value decomposition."]
						];

						If[$VersionNumber < 5.0,
							{u,w,v} = SingularValues[A],
						(* else *)
							{u,w,v} = CompactSVD[A];
						];

						(*  Changed: the 2nd form is more stable.
						answer = Sum[ (u[[i]] . b)/w[[i]] v[[i]],
							{i,m}],
						*)
						answer = Transpose[v] . DiagonalMatrix[1/w] .
							u . b,

			_ , Message[LinearFit::nomethod, optMethod];
				Return[$Failed]
		];

		(* 
		 * Despite the name, this might really be the sum of the squares.
		 *)
		chisq = Plus @@ (((y - (Plus @@ Times[answer,#]& /@
			Transpose[indBasisList /. ind -> x])) /
			erry)^2);

		If[!NumericQ[chisq] || Im[chisq] != 0,
			Message[LinearFit::chisqnonumber];
			Return[$Failed];
		];

		If[optShowProgress,
			If[nvars > 2,
				Print["  Chi-squared = ", chisq],
			(* else *)
				Print["  Sum-of-squares ", chisq]
			];
			Print["  Parameter values: ", answer];
		];

		If[nvars == 3,
			(* Fit is exact *)
			Break[]
		];

		If[nvars == 4,
			Switch[optBrent,
				False,
					(* 
					 * The "ind -> x" below makes this the effective
					 * variance algorithm, not Lybanon's modified
					 * version.
					 *)
					i = (D[ Plus @@ (answer Map[optBasis[ind,#]&, powers]),
							ind] /. ind -> x) * errx;
					erry = Sqrt[ origerry^2 + i^2 ],

				True,
					{answer,chisq} = brentfit[x,errx,y,erry,n,answer,
						optBrentTolerance, optShowProgress ];
					Break[],

				_,	Message[LinearFit::brent];
					Return[$Failed];
			];
			(* 
			 * I know there is a Break above for Brent, but it
			 * doesn't work!  This is a bug in Switch[], making
			 * Break[] act like in C.
			 *)
			If[optBrent,
				Break[]
			];
			If[optShowProgress,
				Print["  Calculated effective variance: ", erry ];
			],

		(* else *)

			(* Can only be 1 or 2 variables to get here *)
			If[optReweight === False,
				If[optShowProgress,
					If[optReturnErrors,
						Message[LinearFit::noerrors]
					];
					If[optReturnCovariance,
						Message[LinearFit::nocovariance]
					];
				];
				Break[]
			];

			(* Must have specified Reweight to get here. *)
			If[chisq == 0,
				i = $MinMachineNumber,
			(* else *)
				i = Sqrt[chisq/dof]
			];
			erry = Table[ i , {n}];
			If[optShowProgress,
				Print["  Re-weighting with a statistical assumption."];
				Print["  The routine will return a PseudoErrorY."];
				Print["  The error in the dependent variable is: ", 
							First[erry]];
			];
			(* This will mean that we get only one more iteration. *)
			nvars = 3;

			(* Make sure we don't bump into the convergence test below *)
			Continue[];
		];

		(* Convergence test *)
		If[optConvergenceTest === Automatic,
			If[ chisq/dof < 0.001 ||
				(chisq <= oldchisq && (oldchisq - chisq)/chisq < 0.1),
					Break[]
			],
		(* else *)
			If[ optConvergenceTest[chisq,oldchisq,dof,answer,oldanswer],
				Break[]
			];
		];

		oldchisq = chisq;
		oldanswer = answer;

	];

	(**************************)
	(* End loop of iterations *)
	(**************************)

	If[iter > optMaximumIterations,
		Message[LinearFit::maxiterations, iter - 1]
	];

	If[!optBrent,
		If[optReturnErrors || optReturnCovariance,
			If[optShowProgress,
				Print["Calculating covariance matrix."]
			];
			Switch[optMethod,
				SVD,		(* This is Eqn 14.3.20 of Press et. al. *)
							c = Inner[Times,Transpose[v], v/w^2],
				LUD,		Switch[ optInverseMethod,
								Inverse,	c = Inverse[alpha],
								SVD,		
										If[$VersionNumber < 5.0,
											{i,j,k} = SingularValues[alpha],
										(* else *)
											{i,j,k} = CompactSVD[alpha];
										];
										c = Transpose[k].DiagonalMatrix[1/j].i,
	
								_,	Message[LinearFit::noinversemethod, 
										optInverseMethod];
									Return[$Failed]
							],
	
				_ , Message[LinearFit::nomethod, optMethod];
					Return[$Failed]
			];
		];
		If[optReturnErrors,
			erra = Table[ Sqrt[c[[i,i]]], {i,m}];
			answer = Transpose[ {answer,erra} ]//N;
			If[optUseSignificantFigures,
				If[optShowProgress,
					Print["Adjusting significant figures of parameters."]
				];
				answer = AdjustSignificantFigures[answer,
					optsAdjustSignificantFigures];
			];
		],
	(* else *)
		(* Brent is true: errors are already calculated *)

		(* erry is not yet the effective variance.  Make it so. 
		 * Note that the powers must be {0,1}, NOT {1,), so we
		 * are assumred that the slope is the third term in the
		 * answer.
		 *)
		erry = Quadrature[ erry, Part[Flatten[answer],3]*errx];

		If[optReturnErrors === False,
			answer = First /@ answer,
		(* else *)
			If[optUseSignificantFigures,
				answer = AdjustSignificantFigures[answer,
					optsAdjustSignificantFigures];
			];
		];
	];

	answer = Apply[Rule,
		Transpose[ {a[#]& /@ powers, answer}], {1}];

	If[nvars <= 2,
		answer = Append[answer, SumOfSquares -> chisq],
	(* else *)
		If[orignvars <= 2,
			answer = Append[answer, PseudoErrorY -> First[erry]];
			chisq = First[erry]^2 * chisq;
			answer = Append[answer, SumOfSquares -> chisq],
		(* else *)
			answer = Append[answer, ChiSquared -> chisq];
		];
	];
	answer = Append[answer, DegreesOfFreedom -> dof];

	If[ nvars == 4 && optReturnEffectiveVariance,
		answer = Append[answer, EffectiveVariance -> erry^2];
	];

	If[optReturnCovariance,
		answer = Append[answer, EDACovariance -> c];
	];

	If[optShowFit || optReturnResiduals,

		(* 
		 * Calculate the Basis functions evaluted for each x.
		 *)

		i = (optBasis[ind,#]& /@ powers) /. ind -> x;

		(*
		 * If the basis is always 1 for power 0, put in
		 * explicit 1's.
		 *)

		If[NumericQ[optBasis[ind,0]] &&  optBasis[ind,0] == 1,
			i = ReplacePart[ i, Table[1,{n}], zeroPositions ]
		];


		(*
		 * Now we calculate f[x] for each value of x
		 *)

		If[optReturnErrors,
			j = Plus @@ (First /@ ((a[#]& /@ powers) /. answer) i),
		(* else *)
			j =  Plus @@ (((a[#]& /@ powers) /. answer) i);
		];

		residuals = y - j;
		If[nvars > 2,
			residuals = Transpose[{residuals, erry}];
			If[optUseSignificantFigures,
				residuals = AdjustSignificantFigures[residuals,
					optsAdjustSignificantFigures ];
			];
		];

		residuals = Transpose[{x,residuals}];
							
		If[optReturnResiduals,
			answer = Append[answer, Residuals -> residuals];
		];

		If[optShowFit,
			If[!optReturnCovariance && !optBrent && optReturnErrors,
				(* EDACovariance is not in answer.  Save answer
				 * in i, put in EDACovariance.
				 *)
				i = answer;
				answer = Append[answer,EDACovariance -> c];
			];
			(*
			 * For Mathematica 6.0, terminating the graphic with
			 * a semi-colon suppresses the grpahics output, but the
			 * scope of Print[] has been extended to include grpahics.
			 *)
			If[ $VersionNumber < 6.0,
				ShowLinearFit[data, powers, a, answer, residuals,
					Evaluate @ optsShowLinearFit ];,
			(* else *)
				Print[ShowLinearFit[data, powers, a, answer, residuals,
					Evaluate @ optsShowLinearFit ]];
			];
				
			If[!optReturnCovariance && !optBrent && optReturnErrors,
				(* Restore the answer without the Covariance. *)
				answer = i
			];
		];
	];

	If[optReturnFunction,
		If[!optReturnCovariance && optReturnErrors && !optBrent,
			i = answer;
			answer = Append[answer,EDACovariance -> c];
		];
		answer = ToLinearFunction[answer, powers, a, 
			ind, optsToLinearFunction ];
		answer = answer /. ind -> a;
	];


	answer
]

(*
 * The following internal routines and variables are used for
 * the Brent option to LinearFit.  Ref: W.H. Press & S.A.
 * Teukolsky, Comp. in Physics 6, (1992) pg. 274.
 *
 * Routines are:
 *	brentfit
 *	brentInterpolation
 *	chixy
 *	bracketMin
 *	vanWDBrent	(* "Van Wijngaarden-Dekker-Brent" method *)
 *)

(* The "COMMON" block (ugh!) *)
xx = 0;
yy = 0;
sx = 0;
sy = 0;
aa = 0;
offs = 0;
nn = 0;

brentfit::maxiter = "Warning: fit did not converge in `1` iterations."

(*
 * Using the results of the first standard fit causes this
 * routine to get much closer to exact solutions for PearsonYorkData
 * then the weighted fit recommended by Press & Teukolsky.
 *)
brentfit[x_,errx_,y_,erry_,n_,previous_,tolerance_,progress_] := Module[
	{
		foo,

		a,
		amx, amn,
		ang,
		answer,
		b,
		bmx, bmn,
		ch,
		chi2,
		chisq,
		d1, d2,
		pOTN = 1.571,
		q,
		scale,
		siga, sigb
	},

	nn = n;
	offs = 0;

	If[progress,
		Print["Using Brent minimisation."];
	];

	scale = Sqrt[ Variance[x] / Variance[y] ];
	If[progress,
		Print["  Scale factor = ", scale];
	];
	xx = x;
	yy = y * scale;
	sx = errx;
	sy = erry*scale;
	ww = Quadrature[sx, sy];

	ang = Table[0, {6}];
	ang[[2]] = ArcTan[ Last[previous] ];
	ang[[5]] = ang[[2]];
	ang[[6]] = pOTN;

	ch = Table[0, {6}];
	ch[[4]]  = chixy[ang[[4]] ];
	ch[[5]]  = chixy[ang[[5]] ];
	ch[[6]]  = chixy[ang[[6]] ];

	If[progress,
		Print["  Bracketing the minimum in the chi square."];
	];
	{ang[[1]],ang[[2]],ang[[3]],ch[[1]],ch[[2]],ch[[3]]} = bracketMin[
		ang[[1]],ang[[2]],ang[[3]],ch[[1]],ch[[2]],ch[[3]], chixy];
	If[progress,
		Print["  Brent fitting the bracketed minimum."];
	];

	(*
	 * The built-in FindMinimum could be used here, but in my
	 * experiments, even after upping the tolerance in the
	 * call to brentInterpolation[] below to 10^(-10),
	 * brentInterpolation is nearly * 50% faster on a 100 point
	 * data set.  Surprising but true.
	 *)
	b = brentInterpolation[ang[[1]], ang[[2]],ang[[3]],chixy,
		tolerance,progress];

	chi2 = chixy[b];
	a = aa;
	q = ChiSquareProbability[n - 2, chi2];
	r2 = 1/(Plus @@ ww);
	bmx = $MaxMachineNumber;
	bmn = $MAxMachineNumber;
	d1 = bmx;
	d2 = bmn;
	offs = chi2 + 1;
	If[Length[Position[ch, foo_ /; foo > offs]] > 0,
		d1 = Mod[
			Abs[ ang[[ Last[Position[ch, foo_ /; foo > offs]] ]] - b], Pi]//N;
		d1 = First[d1];
		d2 = Pi - d1//N;
		If[Length[Position[ang, foo_ /; foo < b]] > 0,
			{d1,d2} = {d2,d1};
		];
	];
	bmx = d1;
	bmn = d2;
	If[progress,
		Print["  Calculating errors using Brent's root method."];
	];
	If[bmx < $MaxMachineNumber,
		bmx = vanWDBrent[chixy,b,b+bmx, tolerance] - b;
		amx = aa - a;
		bmn = vanWDBrent[chixy,b,b-bmn, tolerance] - b;
		amn = aa - a;
		sigb = Sqrt[ 0.5*(bmx^2 + bmn^2)]/(scale*Cos[b]^2);
		siga = Sqrt[ 0.5*(amx^2 + amn^2)]/scale,
	(* else *)
		sigb = $MaxMachineNumber;
		siga = $MaxMachineNumber
	];

	a = a/scale;
	b = Tan[b]/scale;

	answer = { {a,siga},{b,sigb}};

	{answer,chi2}
]

brentInterpolation[ax_, bx_, cx_, f_, tol_,progress_] := Module[
	{
		iter,
		a,b,d,etemp,fu,fv,fw,p,q,r,tol1,tol2,u,v,w,x,xm,
		e = 0.0,

		iterMaximum = 100,
		brentMagic = 0.3819660,
		zEPS = 10^(-10),

		xmin
	},
	a = Min[ax,cx];
	b = Max[ax,cx];
	x = w = v = bx;
	fw = fv = fx = f[x];
	For[iter = 1, iter <= iterMaximum, iter++,
		xm = (a + b)/2;
		tol1 = tol*Abs[x] + zEPS;
		tol2 = 2*tol1;
		If[Abs[x - xm] <= (tol2 - 0.5(b - a)),
			xmin = x;
			Break[];
		];
		If[Abs[e] > tol1,
			r = (x - w)*(fx - fv);
			q = (x - v)*(fx - fw);
			p = (x - v)*q - (x - w)*r;
			q = 2.*(q - r);
			If[q > 0,
				p = -p;
			];
			q = Abs[q];
			etemp = e;
			e = d;
			If[ Abs[p] >= Abs[q*etemp/2] || p <= q*(a - x) || p >= q*(b-x),
				If[x >= xm,
					e = a - x,
				(* else *)
					e = b - x
				];
				d = brentMagic*e,
			(* else *)
				d = p/q;
				u = x + d;
				If[u - a < tol2 || b - u < tol2,
					If[xm - x > 0,
						d = Abs[tol1],
					(* else *)
						d = - Abs[tol1]
					];
				];
			],
		(* else *)
			If[x >= xm,
				e = a - x,
			(* else *)
				e = b - x
			];
			d = brentMagic * e;
		];
		If[Abs[d] >= tol1,
			u = x + d,
		(* else *)
			If[d > 0,
				u = x + Abs[tol1],
			(* else *)
				u = x - Abs[tol1]
			];
		];
		fu = f[u];
		If[fu <= fx,
			If[ u >= x,
				a = x,
			(* else *)
				b = x
			];
			v = w;
			w = x;
			x = u;
			fv = fw;
			fw = fx;
			fx = fu,
		(* else *)
			If[u < x,
				a = u,
			(* else *)
				b = u
			];
			Which[
				fu <= fw || w == x,
					v = w;
					w = u;
					fv = fw;
					fw = fu,

				fu <= fv || v == x || v == w,
					v = u;
					fv = fu;
			];
		];

		If[progress,
			Print["  Brent iteration: ", iter];
			Print["	ArcTan[scale*slope] =", x];
		];

	];  (* end "For[iter = ..." *)

	If[iter > iterMaximum,
		Message[brentfit::maxiter, itMAX];
		xmin = x;
	];

	xmin 
]

chixy[bang_] := Module[
	{
		answer,
		avex = 0, avey = 0,
		b,
		sumw = 0
	},

	b = Tan[bang];
	ww = (b * sx)^2 + sy^2;
	ww = ReplacePart[ww, $MinMachineNumber,
		Join[ Position[ww, 0], Position[ww,0.]] ];
	ww = 1/ww;
	sumw = Plus @@ ww;
	avex = ww . xx / sumw;
	avey = ww . yy / sumw;
	aa = avey - b*avex;
	answer = -offs + (Plus @@ (ww * (yy - aa - b*xx)^2));

	answer
]

bracketMin[axorig_, bxorig_,cxorig_, faorig_, fborig_, fcorig_,
	func_] := Module[
	{
		ax, bx, cx,
		fa, fb, fc,

		magicNumber = 1.618034,
		gLIMIT = 100.,

		ulim, u, r, q, fu
	},

	ax = axorig;
	bx = bxorig;
	cx = cxorig;

	fa = func[ax];
	fb = func[bx];
	If[fb > fa,
		Reverse[{ax,bx}];
		Reverse[{fb,fa}];
	];
	cx = bx + magicNumber*(bx - ax);
	fc = func[cx];
	While[fb > fc,
		r = (bx - ax)*(fb - fc);
		q = (bx - cx)*(fb - fa);
		Which[
			q - r == 0,
				u = $MaxMachineNUmber,
			True,
				u = bx - ((bx - cx)*q - (bx - ax)*r)/
						(2*(q - r));
		];
		ulim = bx + gLIMIT*(cx - bx);
		Which[
			(bx - u)*(u - cx) > 0,
				fu = func[u];
				Which[
					fu < fc,
						ax = bx;
						bx = u;
						fa = fb;
						fb = fu;
						Break[],
					fu > fb,
						cx = u;
						fc = fu;
						Break[]
				];
				u = cx + magicNumber*(cx - bx);
				fu = func[u],

			(cx - u)*(u - ulim) > 0,
				fu = func[u];
				If[fu < fc,
					bx = cx;
					cx = u;
					u = cx + magicNumber*(cx - bx);
					fb = fc;
					fc = fu;
					fu = func[u];
				],

			(u - ulim)*(ulim - cx) >= 0,
				u = ulim;
				fu = func[u],

			True,
				u = cx + magicNumber*(cx - bx);
				fu = func[u];


		];	(* end Which *)
		ax = bx;
		bx = cx;
		cx = u;
		fa = fb;
		fb = fc;
		fc = fu;

	]; (* end While *)

	{ax, bx, cx, fa, fb, fc}
]

vanWDBrent[func_, x1_, x2_, tol_] := Module[
	{
		iterMaximum = 100,
		ePS = 3*10^(-8),

		iter,
		a,b,c,d,e,min1,min2,
		fa, fb, fc, p,q,r,s,tol1,xm
	},
	a = x1; b = x2;
	fa = func[a];
	fb = func[b];
	fc = fb;
	For[iter = 1, iter <= iterMaximum, iter++,
		If[fb*fc > 0,
			c = a;
			fc = fa;
			e = d = b - a;
		];
		If[Abs[fc] < Abs[fb],
			a = b;
			b = c;
			c = a;
			fa = fb;
			fb = fc;
			fc = fa;
		];
		tol1 = 2*ePS*Abs[b] + tol/2;
		xm = (c - b)/2;
		If[Abs[xm] <= tol1 || fb == 0,
			Break[]
		];
		If[Abs[e] >= tol1 && Abs[fa] > Abs[fb],
			s = fb/fa;
			If[a == c,
				p = 2*xm*s;
				q = 1.0 - s,
			(* else *)
				q = fa/fc;
				r = fb/fc;
				p = s*(2*xm*q*(q-r) - (b-a)*(r - 1.0));
				q = (q - 1.0)*(r - 1.0)*(s - 1.0);
			];
			If[p > 0.0,
				q = -q;
			];
			p = Abs[p];
			min1 = 3*xm*q - Abs[tol1*q];
			min2 = Abs[e*q];
			If[2.0*p < Min[min1,min2],
				e = d;
				d = p/q,
			(* else *)
				d = xm;
				e = d;
			],
		(* else *)
			d = xm;
			e = d;
		];
		a = b;
		fa = fb;
		If[Abs[d] > tol1,
			b += d,
		(* else *)
			If[xm > 0,
				b += Abs[tol1],
			(* else *)
				b -= Abs[tol1]
			];
		];
		fb = func[b];

	]; (* end "For[iter = ..." *)

	If[iter > iterMaximum,
		Message[brentfit::maxiter, iterMaximum];
	];

	b

]

(*
 * This version just calculates the residuals and punts to
 * the other form of ShowLinearFit.
 *)
ShowLinearFit[data_, powers_?(VectorQ[#,NumericQ]&) , a_Symbol, result_,
	opts___?OptionQ ] := Module[

	{
		i,j,k,

		ind,
		n, nvars,
		res,
		zeroPositions,

		x,errx,y,erry,

		optBasis,
		optUseSignificantFigures,

		optsAdjustSignificantFigures
	},

	optBasis = Basis /. {opts} /. Options[ShowLinearFit];
	optsAdjustSignificantFigures = FilterOptions[AdjustSignificantFigures,
		opts];
	optUseSignificantFigures = UseSignificantFigures /. {opts} /.
		Options[ShowLinearFit];


	Check[ n = UnpackData[data];,
		Return[$Failed]
	];
	If[n === $Failed,
		Message[ShowLinearFit::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	If[Union[Head /@ result] =!= {Rule},
		Message[ShowLinearFit::badresult];
		Return[$Failed]
	];


	res = Residuals /. result;
	If[res === Residuals,

		(* Have to calculate them *)
		zeroPositions = Join[Position[powers,0.], Position[powers,0]];

		i = (optBasis[ind,#]& /@ powers) /. ind -> x;

		(*
		 * If the basis is always 1 for power 0, put in
		 * explicit 1's.
		 *)

		If[NumericQ[optBasis[ind,0]] &&  optBasis[ind,0] == 1,
			i = ReplacePart[ i, Table[1,{n}], zeroPositions ]
		];

		(* Set the number of terms in the first parameter. *)
		k = Length[First[ (a[#]& /@ powers) /. result]];
		If[k == 2,
			(* There are errors in the parameters *)
			j = Plus @@ (First /@ ((a[#]& /@ powers) /. result) i),
		(* else *)
			j = Plus @@ (((a[#]& /@ powers) /. result) i);
		];
		res = y - j;
		If[nvars > 2,
			res = Transpose[{res, erry}];
			If[optUseSignificantFigures,
				res = AdjustSignificantFigures[res,
					optsAdjustSignificantFigures ];
			];
		];
		res = Transpose[{x,res}];
	];

	ShowLinearFit[data, powers, a, result, res, opts]

];

ShowLinearFit[data_, powers_?(VectorQ[#,NumericQ]&) , a_Symbol, result_,
	res_, opts___?OptionQ ] := Module[
	{
		combinedplot, dataplot, fitplot, residualplot,
		func,
		ind,
		n, nvars,
		quadrant,
		zeroPositions,

		i,j,k,

		optBasis,
		optExtrapolate,
		optResidualPlacement,
		optUseSignificantFigures,

		optsAdjustSignificantFigures,
		optsEDAListPlot,
		optsPlot,
		optsToLinearFunction,

		halfx, halfy,
		minx, maxx, miny, maxy,
		minxSave, maxxSave,
		x, y, errx, erry
	},

	i =	Complement[ First /@ {opts},
				First /@ Options[ShowLinearFit],
				First /@ Options[AdjustSignificantFigures],
				First /@ Options[ToLinearFunction] ];
	If[Length[i] != 0,
		Message[ShowLinearFit::badopt, #]& /@ i;
		Return[$Failed]
	];

	optBasis = Basis /. {opts} /. Options[ShowLinearFit];
	optExtrapolate = Extrapolate /. {opts} /. Options[ShowLinearFit];
	optResidualPlacement = ResidualPlacement /. {opts} /. Options[ShowLinearFit];
	optUseSignificantFigures = UseSignificantFigures /. {opts} /.
		Options[ShowLinearFit];

	optsEDAListPlot = FilterOptions[EDAListPlot, opts];
	optsPlot = FilterOptions[Plot, opts];
	optsToLinearFunction = FilterOptions[ToLinearFunction, opts];

	Check[ n = UnpackData[data];,
		Return[$Failed]
	];
	If[n === $Failed,
		Message[ShowLinearFit::dataformat];
		Return[$Failed]
	];
	{n,nvars,x,errx,y,erry} = n;

	If[Union[Head /@ result] =!= {Rule},
		Message[ShowLinearFit::badresult];
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
		1,	minx = 1;
			maxx = n;
			miny = Min[y];
			maxy = Max[y],

		2,	minx = Min[x];
			maxx = Max[x];
			miny = Min[y];
			maxy = Max[y],

		3,  minx = Min[x];
			maxx = Max[x];
			miny = Min[y - erry];
			maxy = Max[y + erry],

		4,	minx = Min[x - errx];
			maxx = Max[x + errx];
			miny = Min[y - erry];
			maxy = Max[y + erry];
	];


	If[optExtrapolate =!= False,
		If[Length[optExtrapolate] != 2 ||
			Union[NumericQ /@ optExtrapolate] =!= {True},
				Message[ShowLinearFit::badextrap];
				Return[$Failed]
		];
		minxSave = minx;
		maxxSave = maxx;
		minx = First[optExtrapolate];
		maxx = Last[optExtrapolate];
		optsPlot = {PlotRange -> {{minx, maxx}, All}, optsPlot};
	];

	(*
	 * For the data plot and the fit plot, we let any options
	 * override our default settings of the PlotRange.
	 * This is the opposite of what we do below for the residual plot.
	 *)
	dataplot = EDAListPlot[data, 
		optsEDAListPlot,
		PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
			{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}},
		DisplayFunction -> Identity ];

	func = ToLinearFunction[result, powers, a, ind,
		optsToLinearFunction ];

	If[Length[func] == 1,
		fitplot = Plot[ func, {ind, minx, maxx}, DisplayFunction -> Identity,
			Evaluate @ optsPlot,
			PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
			 	{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}}],
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
				PlotStyle -> Dashing[{0.03}],
				Evaluate @ optsPlot,
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1*minx},
				 	{1.1*miny - .1*maxy, 1.1maxy - 0.1*miny}}],
			Plot[ (Plus @@ func), 
				{ind,minx,maxx},
				DisplayFunction -> Identity,
				PlotStyle -> Dashing[{0.03}],
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
				1,	i = 0.5;
					j = 0.5,
				2,	i = 0;
					j = 0.5,
				3,	i = 0;
					j = 0,
				4,	i = 0.5;
					j = 0,
				Separate,Null,
				None,Null,
				_,	Message[ShowLinearFit::badresplacement, 
						optResidualPlacement];
					Return[$Failed];
		];
	];

	If[optResidualPlacement =!= None,
		If[optResidualPlacement === Separate,
			(* scale the x axis to match the combinedplot *)
			residualplot = EDAListPlot[res,
				PlotRange -> { {1.1*minx - .1*maxx, 1.1maxx - 0.1minx},
					All},
				If[ $VersionNumber < 6.0,
					PlotJoined -> True,
				(* else *)
					Joined -> True
				],
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
			(* Then it has to be Separate *)

			If[ $VersionNumber < 6.0,
				(* FullGraphics forces the alignment of the two graphs to
				 * be on the vertical axis, not on the leftmost position
				 * of the Tick labels.
			 	 *)
				{Show[FullGraphics @ combinedplot,
					DisplayFunction -> $DisplayFunction],
			 	Show[FullGraphics @ residualplot,
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

ToLinearFunction[result_, powers_List, param_Symbol,
	ind_Symbol, opts___?OptionQ ] := Module[
	{
		answer,
		covar,
		errlist,
		indlist,
		m = Length[powers],
		valuelist,

		i,j,

		optBasis,
		optUseFitErrors
	},


	i =	Complement[ First /@ {opts}, First /@ Options[ToLinearFunction] ];
	If[Length[i] != 0,
		Message[ToLinearFunction::badopt, #]& /@ i;
		Return[$Failed]
	];

	optBasis = Basis /. {opts} /. Options[ToLinearFunction];
	optUseFitErrors = UseFitErrors /. {opts} /. Options[ToLinearFunction];

	(*
	 * In the following, the use of With[ ... ] is crucial
	 * since it evaluates the constant outside of the normal
	 * evaluation context.
	 *)
	For[i = 1, i <= m, i++,
		With[{dummy = powers[[i]] },
			If[ValueQ[param[dummy]],
				Message[ToLinearFunction::adefined, param];
				Return[$Failed]
			];
		];
	];

	indlist = optBasis[ind,#]& /@ powers;
	valuelist = (param[#]& /@ powers) /. result;

	If[MatrixQ[valuelist] && optUseFitErrors,
		(* 
		 * There are errors in the parameters and we
		 * want to use them.
		 *)
		covar = EDACovariance /. result;
		If[covar =!= EDACovariance,
			(* We use the sign of the terms in the first row
			 * of the covariance matrix to assign the signs of
			 * the terms in the error function.  This is the
			 * right thing to do since it is the covariance
			 * matrix that determines the signs of the linear
			 * correlation coefficients.
			 *)
			errlist = Last /@ valuelist;
			errlist = errlist (Sign /@ First /@ covar);
			errlist = errlist . indlist;
			answer = {First /@ valuelist . indlist};
			answer = Flatten[{answer, errlist}],
		(* else *)
			(* A heuristic: we evaluate the terms at an arbitrary
			 * value of the independent variable = 10, then
			 * we sort by the values and alternate the signs
			 * of the terms.
			 *)
			i = (First /@ valuelist) indlist /. ind -> 10;
			(*
			 * Now a sorted list of {value,position} pairs.
			 *)
			i = Last /@ Sort[Table[ {Part[i,j], j}, {j,m}]];
			(*
			 * Now we put the errors in with alternating signs
			 *)
			errlist = Last /@ ((-1)^(# + 1)*Part[valuelist,#]& /@ i); 
			i = Part[indlist,#]& /@ i;
			(*
			 * i will now be the function of the independent variable
			 * for the errors.
			 *)
			i = errlist . i;

			answer = {First /@ valuelist . indlist};
			answer = Flatten[{answer, i}]
		],
	(* else *)
		If[MatrixQ[valuelist],
			answer = { (First /@ valuelist) . indlist},
		(* else *)
			answer = {valuelist . indlist};
		];
	];

	answer

]

End[]

SetAttributes[LinearFit, {ReadProtected}]
SetAttributes[ShowLinearFit, {ReadProtected,HoldAll}]
SetAttributes[ToLinearFunction, {ReadProtected}]

Protect[LinearFit, ShowLinearFit, ToLinearFunction]

EndPackage[]
