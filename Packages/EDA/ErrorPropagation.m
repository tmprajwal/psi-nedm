(* $Id: ErrorPropagation.m,v 6.4 2011/05/13 13:51:33 harrison Exp $ *)

(* :Title: Error Propagation *)

(* :Context: EDA`ErrorPropagation` *)

(* :Author: Todd Gayley and David M. Harrison *)

(* :Summary:
	A collection of routines and constructs for propagation of errors.
*)

(* :Copyright: Copyright 1995-2009, Wolfram Research, Inc.*)

(* :Package Version: 1.2 *)

(* :Mathematica Version: 6.0 *)

(* :History:
	Version 0.8 by Todd Gayley and David M. Harrison, July 1994.
	Version 0.9 by David M. Harrison, October 1994.
	Version 1.0 by David M. Harrison, May 1995.
	Version 1.1 by David M. Harrison, December 2003.
	Version 1.2 by David M. Harrison, May 2009.
*)

(* :Keywords:
*)

(* :Sources:
*)

(* :Warnings: *)

(* :Limitations:  
	The routines assume that the errors are independent and
	probabilistic.  The Data and Datum constructs assume that
	the errors are "small".
*)

(* :Discussion:
    
*)
BeginPackage["EDA`ErrorPropagation`",
	"EDA`Common`",		(* defines UseSignificantFigures and ErrorDigits *)
	"EDA`Utilities`"	(* for AdjustSignificantFigures *)
]

Unprotect[{
	Quadrature,
	CombineWithError,
	Datum,
	Data,
	DivideWithError,
	PlusWithError,
	PowerWithError,
	TimesWithError,
	SubtractWithError
}]

Quadrature::usage =
	"Quadrature[x1, x2, ... , xN] returns Sqrt[x1^2 + x2^2 + ... +
	xN^2].  Quadrature[ {x1,x2, ... , xN} ] returns the same as
	the first form."

CombineWithError::usage =
	"CombineWithError[expr] computes expr and combines the errors in
	quadrature. 'expr' is assumed to be an expression involving
	variables, arithmetic operations, and constants, and the variables are
	assumed to be of the form {{val1, errva1}, {val2, errval2}, ... ,
	{valN,errvalN}} or of the form {value, error}. The length of all
	variables must be the same. There must be at least two different
	variable names in the expression. An optional final argument
	UseSignificantFigures, if set to False, suppresses the adjustment of
	significant figures based on the error in the result. If
	UseSignificantFigures is set to True, the number of digits in the error
	considered to be significant can be adjusted with an ErrorDigits
	option."

(*
 * Some messages in case things go wrong.
 *)
CombineWithError::unequallength =
	"The number of datapoints in the variables are not all equal or
	an unexpected specification of variable names was encountered."
CombineWithError::notanumber =
	"A non-numeric datum or unexpected data format was encountered."
CombineWithError::numvars =
	"The number of variables in the expression is less than 2."

Datum::usage =
	"Datum[{val, err}] is a data type where val is the value of some
	variable and err is the error associated with that value. If
	the error is statistical, then different Datum constructs can be combined
	using the usual arithmetic operations, + - * / ^, and the
	errors will be propagated correctly. In addition, a Datum expression can
	have a constant number added or multiplied to it, or it can be
	evaluated as the argument to one of the functions specified in
	DataFunctions. In StandardForm, Datum will typeset as PlusMinus[a,
	b]."

Data::usage =
	"Data[ {{val1, err1}, ... , {valN, errN}} ] is a data type that declares
	that the argument is a list containing a set of value/error pairs.
	Declaring lists as Data allows them to be combined
	using the usual arithmetic operations, + - * / ^, and the errors will
	be propagated correctly. In addition, a Data expression can have a constant
	number added or multiplied to it, or it can be evaluated as the argument
	to one of the functions specified in DataFunctions."

DataFunctions::usage =
	"DataFunctions is the set of functions whose argument can be
	either Data or Datum type (or PlusMinus[a, b] with a and b both
	numeric), and can have the errors propagated correctly. Note that 
	\"correctly\" in this context means that the errors are
	small compared to the values and that the function is defined for
	the values being used."

DatumQ::usage =
	"DatumQ[a] returns True if 'a' can be interpreted as Datum,
	False otherwise."

DataQ::usage =
	"DataQ[a] returns True if 'a' can be interpreted as Data,
	False otherwise."

DivideWithError::usage =
	"DivideWithError[a, b] divides a by b and combines their fractional
	errors in quadrature. 'a' must be of the form {a, erra} or {{a1, erra1},
	{a2, erra2}, ... , {aN, erraN}} and similarly for b. Both a and b must
	be of the same length. An optional final argument, UseSignificantFigures,
	if set to False, suppresses the adjustment of significant figures based
	on the error in the result. If UseSignificantFigures is True, the
	number of digits in the error considered significant can be adjusted
	by adding an ErrorDigits option."

PlusMinus::usage = 
	"An infix and prefix operator. x \[PlusMinus] y is by default
	interpreted as \!\(PlusMinus[x, y]\). 
	(\[PlusMinus] x is by default interpreted as \!\(PlusMinus[x]\).)
	When EDA is loaded, if the value and the error are both numeric,
	propagation of errors of precision is calculated automatically."

PlusWithError::usage = 
	"PlusWithError[a, b] adds a and b together and combines their errors
	in quadrature. 'a' must be of the form {a, erra} or {{a1, erra1},
	{a2, erra2}, ... , {aN, erraN}} and similarly for b. Both a and b must
	be of the same length. An optional final argument, UseSignificantFigures,
	if set to False, suppresses the adjustment of significant figures based
	on the error in the result. If UseSignificantFigures is set to True, the
	number of digits in the error considered significant can be adjusted
	by adding an ErrorDigits option. PlusWithError[a, b, c, ...] adds
	all of the numbers."

PowerWithError::usage =
	"PowerWithError[a, n] raises a to the nth power and calculates the
	errors. 'a' must be of the form {a, erra} or {{a1, erra1}, {a2, erra2},
	... , {aN, erraN}}. An optional final argument, UseSignificantFigures,
	if set to False, suppresses the adjustment of significant figures based
	on the error in the result. If UseSignificantFigures is set to True, the
	number of digits in the error considered significant can be adjusted
	by using the ErrorDigits option."

SubtractWithError::usage = 
	"SubtractWithError[a, b] subtracts b from a and combines their errors
	in quadrature. 'a' must be of the form {a, erra} or {{a1, erra1},
	{a2, erra2}, ... , {aN, erraN}} and similarly for 'b'. Both a and b
	must be of the same length. An optional final argument,
	UseSignificantFigures, if set to False, suppresses the adjustment
	of significant figures based on the error in the result. If
	UseSignificantFigures is set to True, the number of digits in the error
	considered significant can be adjusted by adding an ErrorDigits
	option."

TimesWithError::usage =
	"TimesWithError[a, b] multiplies a and b together and combines their
	fractional errors in quadrature. 'a' must be of the form {a, erra} or
	{{a1, erra1}, {a2, erra2}, ... , {aN, erraN}} and similarly for b. Both
	a and b must be of the same length. An optional final argument,
	UseSignificantFigures, if set to False, suppresses the adjustment of
	significant figures based on the error in the result. If
	UseSignificantFigures is set to True, the number of digits in the error
	considered significant can be adjusted by adding an ErrorDigits option.
	TimesWithError[a, b, c, ...] multiples all of the numbers."

Options[CombineWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}
Options[DivideWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}
Options[PlusWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}
Options[PowerWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}
Options[TimesWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}
Options[SubtractWithError] = {UseSignificantFigures -> True,
	ErrorDigits -> 2}

DivideWithError::zerodivisor =
	"Found a divisor of zero in the input."

Begin["`Private`"]

Quadrature[x_List] := Sqrt[Plus @@ (Power[#,2]& /@ x)]//N


Quadrature[x__] := Sqrt[Plus @@ Power[{x}, 2]]//N

(*
 * holdSymbol is used by CombineWithError.
 *)
SetAttributes[holdSymbol, {HoldFirst}]
holdSymbol[x_Symbol] := Hold[x] /; !NumericQ[x]
holdSymbol[x_] := x

(*
 * Much of CombineWithError was written by Todd Gayley. The
 * notation is very similar to the "Combining Errors" section
 * of the CombiningData.ma notebook.
 *)
SetAttributes[CombineWithError, {HoldFirst}]
CombineWithError[expr_, opts___?OptionQ] := Module[
	{e,syms,form,errors, sigfitOpt},

	sigfigOpt = UseSignificantFigures /. 
		{opts} /. Options[CombineWithError];

	(* Get the atoms in the expression *)
	e = Map[holdSymbol, Unevaluated[expr], {-1}];

	(* Get the symbols. Need Union to drop multiple cases of the
	   same var *)
	syms = Union[ Cases[Level[e, {-2}], Hold[_]] ];
	If[ Length[Union[Length /@ ReleaseHold[syms]]] != 1,
		Message[CombineWithError::unequallength];
		Return[$Failed]
	];

	If[Length[syms] < 2,
		Message[CombineWithError::numvars];
		Return[$Failed]
	];

	(*
	 * Make sure all syms point to matrices or vectors of numbers.
	 *)
	If[ Union[MatrixQ[#, NumericQ]& /@ ReleaseHold[syms]] =!= {True} &&
	    Union[VectorQ[#, NumericQ]& /@ ReleaseHold[syms]] =!= {True},
			Message[CombineWithError::notanumber];
			Return[$Failed]
	];

	(* Here is the form of the error *)
	form = Quadrature @@ (Dt[e] /. (List /@ Thread[Dt[syms] -> 0]));

	(* 
	 * Evaluate the errors for the datapoints and calculate the
	 * answer.  The first form is for a list of datapoints, the
	 * second for a single datapoint.  Code above here insures
	 * that one or the other will be True.
	 *)
	If[ Union[MatrixQ[#, NumericQ]& /@ ReleaseHold[syms]] === {True},
		errors = form /. Join[
			Dt[syms] /. a:Dt[Hold[z_]] :> (a :> (Last /@ z)),
			syms /. a:Hold[z_] :> (a :> (First /@ z))
		];
		answer = Transpose[{First /@ expr, errors}];
	];
	If[ Union[VectorQ[#, NumericQ]& /@ ReleaseHold[syms]] === {True},
		errors = form /. Join[
			Dt[syms] /. a:Dt[Hold[z_]] :> (a :> Last[z]),
			syms /. a:Hold[z_] :> (a :> First[z])
		];
		answer = {First[expr], errors};
	];

	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

(*
 * Special forms for if given three or more input numbers.
 *)

TimesWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	c__?(VectorQ[#, NumericQ]&), opts___?OptionQ] /;
	(Length[a] == Length[b] && Length[b] == Length[c]) := Module[
		{ oldopts, answer },
		oldopts = Options[TimesWithError];
		SetOptions[TimesWithError, opts];
		answer = Fold[TimesWithError, a, Join[{b},{c}]];
		SetOptions[TimesWithError,#]& /@ oldopts;
		answer
]

PlusWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	c__?(VectorQ[#, NumericQ]&), opts___?OptionQ/;
	(Length[a] == Length[b] && Length[b] == Length[c]) ] := Module[
		{ oldopts, answer },
		oldopts = Options[PlusWithError];
		SetOptions[PlusWithError, opts];
		answer = Fold[PlusWithError, a, Join[{b},{c}]];
		SetOptions[PlusWithError,#]& /@ oldopts;
		answer
]

TimesWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	c__?(MatrixQ[#, NumericQ]&), opts___?OptionQ] /;
	(Length[a] == Length[b] && Length[b] == Length[c]) := Module[
		{ oldopts, answer },
		oldopts = Options[TimesWithError];
		SetOptions[TimesWithError, opts];
		answer = Fold[TimesWithError, a, Join[{b},{c}]];
		SetOptions[TimesWithError,#]& /@ oldopts;
		answer
]

PlusWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	c__?(MatrixQ[#, NumericQ]&), opts___?OptionQ/;
	(Length[a] == Length[b] && Length[b] == Length[c]) ] := Module[
		{ oldopts, answer },
		oldopts = Options[PlusWithError];
		SetOptions[PlusWithError, opts];
		answer = Fold[PlusWithError, a, Join[{b},{c}]];
		SetOptions[PlusWithError,#]& /@ oldopts;
		answer
]

(*
 * Special forms for if given a single datum.
 *)
DivideWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	(Length[a] == 2 && Length[b] == 2) :=
	Flatten[DivideWithError[ {a}, {b}, opts]]

PlusWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	(Length[a] == 2 && Length[b] == 2) :=
	Flatten[PlusWithError[ {a}, {b}, opts]]

PowerWithError[a_?(VectorQ[#, NumericQ]&), n_?NumericQ,
	opts___?OptionQ ] /;
	(Length[a] == 2) :=
	Flatten[PowerWithError[ {a}, n, opts]]

TimesWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	(Length[a] == 2 && Length[b] == 2) :=
	Flatten[TimesWithError[ {a}, {b}, opts]]

SubtractWithError[a_?(VectorQ[#, NumericQ]&), b_?(VectorQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	(Length[a] == 2 && Length[b] == 2) :=
	Flatten[SubtractWithError[ {a}, {b}, opts]]

(*
 * Forms for if given lists of data.  These are the definitions
 * that actually do the work.
 *)
DivideWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	( Length[a] == Length[b]) := Module[
	{sigfigOpt,answer,awork,bwork},

	sigfigOpt = UseSignificantFigures /. {opts} /. Options[DivideWithError];

	If[Count[First /@ b, 0] != 0 || Count[First /@ b, 0.] != 0,
		Message[DivideWithError::zerodivisor];
		Return[$Failed]
	];

	(*
	 * If an error in either a or b is zero, that is acceptable
	 * but will generate a useless General::dbyz message below.
	 * If a value in either a or b is zero, that should be acceptable,
	 * but of course will cause, say, erra/a to explode uselessly.
	 * Thus, we turn all zeros in the input into $MinMachineNumber.
	 *)
	awork = ReplacePart[ a, $MinMachineNumber,
			Join[ Position[a, 0], Position[a, 0.] ]];
	bwork = ReplacePart[ b, $MinMachineNumber,
			Join[ Position[b, 0], Position[b, 0.] ]];

	answer = Divide[ First /@ awork, First /@ bwork];
	answer = Transpose[ 
		{	answer,
			answer * Quadrature[ 1. / (Divide @@ Transpose[awork]),
						1. / (Divide @@ Transpose[bwork])
			]
		}
	];
	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

PlusWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	( Length[a] == Length[b]) := Module[
	{sigfigOpt,answer},

	sigfigOpt = UseSignificantFigures /. {opts} /. Options[PlusWithError];
	answer = Transpose[
		{	Plus[ First /@ a, First /@ b],
			Quadrature[ Last /@ a, Last /@ b]
		}
	];
	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

PowerWithError[a_?(MatrixQ[#, NumericQ]&), n_?NumericQ,
	opts___?OptionQ ] := Module[
	{sigfigOpt,answer},

	sigfigOpt = UseSignificantFigures /. {opts} /. Options[PowerWithError];
	answer = Apply[
	({Power[#1,n], Abs[n*#1^(n-1)*#2]})&, a, 2] ;
	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

TimesWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	( Length[a] == Length[b]) := Module[
	{sigfigOpt,answer,awork,bwork},

	sigfigOpt = UseSignificantFigures /. {opts} /. Options[TimesWithError];

	(*
	 * If an error in either a or b is zero, that is acceptable
	 * but will generate a useless General::dbyz message below.
	 * If a value in either a or b is zero, that should be acceptable,
	 * but of course will cause, say, erra/a to explode uselessly.
	 * Thus, we turn all zeros in the input into $MinMachineNumber.
	 *)
	awork = ReplacePart[ a, $MinMachineNumber,
			Join[ Position[a, 0], Position[a, 0.] ]];
	bwork = ReplacePart[ b, $MinMachineNumber,
			Join[ Position[b, 0], Position[b, 0.] ]];

	answer = Times[ First /@ awork, First /@ bwork];
	answer = Transpose[ 
		{	answer,
			answer * Quadrature[ 1. / (Divide @@ Transpose[awork]),
						1. / (Divide @@ Transpose[bwork])
			]
		}
	];
	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

SubtractWithError[a_?(MatrixQ[#, NumericQ]&), b_?(MatrixQ[#, NumericQ]&),
	opts___?OptionQ ] /;
	( Length[a] == Length[b]) := Module[
	{sigfigOpt,answer},

	sigfigOpt = UseSignificantFigures /. {opts} /. 
		Options[SubtractWithError];
	answer = Transpose[
		{	Subtract[ First /@ a, First /@ b],
			Quadrature[ Last /@ a, Last /@ b]
		}
	];
	If[sigfigOpt === True,
		answer = AdjustSignificantFigures[answer, opts];
	];
	answer //N
]

(*
 * Rulesets for the Data and Datum constructs.
 *)

(* 
 * These are the functions that can directly use Data and Datum
 * and PlusMinus.
 * The reason for the existence of this at all is without it
 * the Notebook wrappers for expr being sent to the kernel get
 * mangled up with the defs.
 *)
DataFunctions = Log | Exp | Sin | Cos | Tan | Csc | Sec | Cot | Sinh | \
	Cosh | Tanh | Csch | Sech | Coth | ArcSin | ArcCos | ArcTan | \
	ArcCsc | ArcSec | ArcCot | ArcSinh | ArcCosh | ArcTanh | \
	ArcCsch | ArcSech | ArcCoth
	
(*
 * We don't want to restrict ourselves to just numeric inputs
 * for these constructs.  Thus, no "NumericQ" tests.
 *)
DataQ[a_] := If[ MatrixQ[a /. Data -> Identity ] && 
	Length[First[a] /. Data -> Identity ] == 2, True, False]

DatumQ[a_] := If[ ListQ[a /. Datum -> Identity] && 
	Length[a /. Datum -> Identity] == 2, True, False]

Data/: Data[a_?DataQ] + Data[b_?DataQ] := Data[ PlusWithError[a,b] ]

Data/: Data[a_?DataQ] - Data[b_?DataQ] := Data[ SubtractWithError[a,b] ]

Data/: Data[a_?DataQ] * Data[b_?DataQ] := Data[ TimesWithError[a,b] ]

Data/: Data[a_?DataQ] / Data[b_?DataQ] := Data[ DivideWithError[a,b] ]

Data/: n_?NumericQ + Data[a_?DataQ] := Data[ AdjustSignificantFigures[
	Transpose[ {n + First /@ a, Last /@ a} ]]]

Data/: n_?NumericQ * Data[a_?DataQ] := Data[ AdjustSignificantFigures[
	Transpose[ {n * (First /@ a), Abs[ n * (Last /@ a)]} ]]]

Data/: Data[a_?DataQ] ^ n_?NumericQ := Data[ PowerWithError[a,n]]

Data/: f_[ Data[a_?DataQ]] := Data[ AdjustSignificantFigures[
	Transpose[ {f[First /@ a], f'[First /@ a] * (Last /@ a)  }]]] /;
		MatchQ[f, DataFunctions]

Datum/: n_?NumericQ + Datum[{a1_,a2_}] := Datum[
		AdjustSignificantFigures[ { n + a1, a2 } ]
]

Datum/: n_?NumericQ * Datum[a_?DatumQ] := Datum[
	AdjustSignificantFigures[ {n * First[a], Abs[n*Last[a]]} ]
]

Datum/: Datum[{a1_,a2_}] + Datum[{b1_,b2_}] := Datum[
	AdjustSignificantFigures[
		{ a1 + b1,
		Quadrature[ a2 , b2 ]}
	]
]

Datum/: Datum[{a1_,a2_}] - Datum[{b1_,b2_}] := Datum[
	AdjustSignificantFigures[
		{ a1 - b1,
		Quadrature[ a2 , b2 ]} 
	]
]

Datum/: Datum[{a1_,a2_}] * Datum[{b1_,b2_}] := Datum[
	AdjustSignificantFigures[
		{ a1 * b1,
		Abs[a1 * b1 * Quadrature[ a2 / a1 ,
			b2 / b1 ]]}
	]
] /; (a1 != 0 && b1 != 0)

Datum/: Datum[{a1_,a2_}] / Datum[{b1_,b2_}] := Datum[
	AdjustSignificantFigures[
		{ a1 / b1 ,
		Abs[a1 * b1 * Quadrature[ a2 / a1 ,
			b2 / b1 ]]}
	]
] /; (a1 != 0 && b1 != 0)

Datum/: Datum[{a1_,a2_}] ^ n_?NumericQ := Datum[
	AdjustSignificantFigures[ {a1^n,
		Abs[n*(a1 ^ (n-1))* a2 ] }
	]
]

Datum/: f_[ Datum[a_?DatumQ]] := Datum[ AdjustSignificantFigures[
	{f[First[a]], f'[First[a]] * Last[a]  }]] /; MatchQ[f, DataFunctions]

(*
 * Error propagation for PlusMinus.
 *)

PlusMinus/: PlusMinus[a_?NumericQ,b_?NumericQ] + PlusMinus[c_?NumericQ,d_?NumericQ] :=
	Apply[PlusMinus,PlusWithError[{a,b},{c,d}]
]

PlusMinus/: PlusMinus[a_?NumericQ,b_?NumericQ] - PlusMinus[c_?NumericQ,d_?NumericQ] :=
	Apply[PlusMinus,SubtractWithError[{a,b},{c,d}]
]

PlusMinus/: PlusMinus[a_?NumericQ,b_?NumericQ] * PlusMinus[c_?NumericQ,d_?NumericQ] :=
	Apply[PlusMinus,TimesWithError[{a,b},{c,d}]
]

PlusMinus/: PlusMinus[a_?NumericQ,b_?NumericQ] / PlusMinus[c_?NumericQ,d_?NumericQ] :=
	Apply[PlusMinus,DivideWithError[{a,b},{c,d}]
]

PlusMinus/: n_?NumericQ + PlusMinus[a_?NumericQ, b_?NumericQ] :=
	Apply[PlusMinus, AdjustSignificantFigures[{n + a, b}]
]

PlusMinus/: n_?NumericQ * PlusMinus[a_?NumericQ, b_?NumericQ] :=
	Apply[PlusMinus, AdjustSignificantFigures[ {n*a, Abs[n*b]}] 
] 

PlusMinus/: PlusMinus[a_?NumericQ, b_?NumericQ] ^ n_?NumericQ := Apply[PlusMinus,
	AdjustSignificantFigures[ {a^n,
		Abs[n*(a ^ (n-1))* b ] }
	]
]

PlusMinus/: f_[ PlusMinus[a_?NumericQ, b_?NumericQ]] := Apply[PlusMinus,
	 AdjustSignificantFigures[{f[a], f'[a] * b  }]
] /; MatchQ[f, DataFunctions]


End[]

(*
 * Now we have Datum formatted as PlusMinus.  This *must* be after the above
 * End[] statement in order to work.
 *)
Format[Datum[{a_,b_}], StandardForm] := PlusMinus[a,b]
(* Now format Data as a list of Datums *)
Format[Data[a_],StandardForm] := Datum /@ a

SetAttributes[ Data, ReadProtected ]
SetAttributes[ Datum, ReadProtected ]
SetAttributes[ Quadrature, ReadProtected ]
SetAttributes[ CombineWithError, ReadProtected ]
SetAttributes[ DivideWithError, ReadProtected ]
SetAttributes[ PlusWithError, ReadProtected ]
SetAttributes[ PowerWithError, ReadProtected ]
SetAttributes[ TimesWithError, ReadProtected ]
SetAttributes[ SubtractWithError, ReadProtected ]

(* 
 * We don't protect DataFunctions to make it simple for the
 * expert user to add to the list.
 *)
Protect[{
	Quadrature,
	CombineWithError,
	Datum,
	Data,
	DivideWithError,
	PlusWithError,
	PowerWithError,
	TimesWithError,
	SubtractWithError
}]

EndPackage[]
