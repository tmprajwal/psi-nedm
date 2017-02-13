(* $Id: Common.m,v 6.0 2009/05/13 11:52:39 harrison Exp $ *)

BeginPackage["EDA`Common`"]

(* There *is* an order below. All options and stand-alone symbols are
 * in alphabetical order, and possible values of options follow the
 * usage message for that option.
 *
 * At the end is the usage message and definition of CompactSVD[]
 * used to make the SingularValueDecomposition package of MMA
 * Version >= 5.0 compatible with the SingularValue[] function
 * or earlier versions.
 *)

DegreesOfFreedom::usage =
	"DegreesOfFreedom is a parameter returned by various fitting
	routines. It is the number of degrees of freedom of the fit,
	which is the number of data points minus the number of parameters
	of the fit."

ChiSquared::usage =
	"ChiSquared is the name of the chi-squared statistic returned
	by various fitting routines."

Extrapolate::usage =
	"Extrapolate is an option for ShowLinearFit and ShowFitResult.
	If set to False (the default), the graph of the fit results spans
	the data. If set to {xmin, xmax}, the graph of the fit results
	uses a plot range in the independent variable of {xmin, xmax}."

MaximumIterations::usage =
	"MaximumIterations is an option for various routines that use	iterative techniques. It specifies the maximum number
	of iterations to perform before quitting."

PseudoErrorY::usage =
	"PseudoErrorY is the value of the error in the dependent variable
	used by LinearFit and EDAFindFit when the number of variables in the
	data is 1 or 2 and Reweight has been specified as true."

ResidualPlacement::usage =
	"ResidualPlacement is an option for ShowLinearFit and ShowFitResult.
	If set to Automatic, they try to place the plot of residuals as a small
	graph inside the graph of the data and results of the fit. If
	a quadrant cannot be found, then the residual plot is displayed
	separately. If the option is set to Separate, then the residual
	plot is always displayed separately. If the option is set to
	an integer between 1 and 4, that quadrant is used to display
	the residuals. If the option is set to None, no residuals
	are displayed."

Separate::usage =
	"Separate is a possible value for the ResidualPlacement option."

Residuals::usage =
	"Residuals is the residuals of a fit, defined as the value
	of the dependent variable minus the value of the fit."

ReturnCovariance::usage =
	"ReturnCovariance is an option for various fitting routines.
	If set to True, then the routine returns the full covariance matrix.	Otherwise, it does not."

EDACovariance::usage =
	"EDACovariance is a parameter which is returned by various fitting	routines. It is the full covariance matrix of the fit."

ReturnEffectiveVariance::usage =
	"ReturnEffectiveVariance is an option for various fitting routines.	If set to True, the routine returns the effective variance as part of	the result of the fit."

EffectiveVariance::usage = 
	"EffectiveVariance is a parameter returned by various fitting 
	routines if ReturnEffectiveVariance is set to True."

ReturnErrors::usage =
	"ReturnErrors is an option for various fitting routines. If set to	True, then the routine returns errors in the fitted parameters.	Otherwise, it does not."

ReturnFunction::usage =
	"ReturnFunction is an option for various fitting routines. If
	set to False, then the fit will be returned as a set of rules	involving the \"parameter\" given in the call to the routine.
	If set to True, then the fit will be returned as a function
	and the independent variable is taken to be \"parameter\"."

ReturnResiduals::usage =
	"ReturnResiduals is an option for various fitting routines. If
	set to True, the residuals of the fit are returned along with other
	information about the fit. The residuals returned always include
	a value for the independent variable. If none is in the data, the
	values are {1, 2, ... , N}."

Reweight::usage =
	"Reweight is an option for LinearFit and EDAFindFit, which controls
	whether or not to re-weight the data if it contains no explicit
	errors. The default is True for LinearFit and False for EDAFindFit.
	If set to True, the data is weighted using a \"statistical assumption\",
	where the error in the dependent variables is the square root of
	the sum of the squares divided by the number of degrees of freedom.
	See Taylor, \"An Introduction to Error Analysis,\" Eqn 8.14 on pg.
	158 for further information. When set to True, all subsequent
	processing assumes that the generated errors in the dependent
	variable are real."

ShowFit::usage =
	"ShowFit is an option for various fitting routines. If set to True,
	then a graphical display of the results of the fit is displayed."

EDAShowProgress::usage =
	"EDAShowProgress is an option for various routines. When set to True, the
	routine will print messages showing its progress in performing
	its tasks. If set to False, no such information is printed."

SumOfSquares::usage =
	"SumOfSquares is the name of the sum of the squares of the
	residuals returned by various fitting routines."

UseFitErrors::usage =
	"UseFitErrors is an option for ToLinearFunction and ToFitFunction.
	If set to True (the default for ToLinearFunction) and the result
	contains errors in the fitted parameters, then two functions are
	returned. The first is the function for the values of the parameters
	and the second is the function for the values of the errors in the
	parameters. A heuristic method is used to choose the sign of each
	term in the function of the errors in the parameters. If UseFitErrors
	is set to False (the default for ToFitFunction), only a single function
	evaluated at the values of the parameters is used."

UseSignificantFigures::usage =
	"UseSignificantFigures is an option for various routines. When set to
	True, the routine will use AdjustSignificantFigures on numbers with
	associated errors, so that the error determines the number of
	significant figures in the number itself. If set to False, no
	such adjustment is performed."

CompactSVD::usage =
	"CompactSVD[mat, opts] calls SingularValueDecomposition from
	Mathematica Version >= 5 and returns a form compatible with
	the SingularValues function from earlier versions."

Unprotect[CompactSVD]

Begin["`Private`"]

(*
 * This was actually written by WRI's Applications Testing Manager
 * and is used by LinearFit and FindFit, except DH has taken the
 * Transpose of the first and last matrices returne.
 *)
CompactSVD[mat_, opts___] :=
	Module[{sv, U, W, V, n},

    (* Compute SVD *)
    sv = SingularValueDecomposition[mat, Min[Dimensions[mat]], opts];
	If[!ListQ[sv], Return[$Failed]];

    {U, W, V} = sv;
    (*extract the diagonal vector*)
    sv = Tr[W, List];
    (* determine the number of positive singular values *)
    n = Length[sv] - Count[sv, 0 | 0.];
    If [n == 0,
        {Transpose[U], W, Transpose[V]},
        {Transpose[Take[U, All, n]], Take[sv, n], Transpose[Take[V, All, n]]}
    ]
] 

End[]

SetAttributes[CompactSVD, {ReadProtected}]

EndPackage[]
