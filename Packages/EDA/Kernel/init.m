(* $Id: init.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

(* :Title: Master Declarations File for EDA *)

(* :Summary:
	This file contains declarations of all the major symbols
	contained in files in this directory.  When loaded, it sets
	up the symbols with attribute Stub, so the correct package
	will be loaded when the symbol is called.  *)

(* :Author: This file was created by an automated system written in
	Mathematica by John M. Novak *)

(* :History: File created on 10 June 1996 at 8:56 *)
(* :History: File edited by hand by David Harrison on 17 June 1997 *)
(* :History: File edited by hand by David Harrison on 2 Sept 1999 *)
(* :History: File handed over to SCCS by David Harrison, Sept 13/99 *)
(* :History: Added FilterOptions by David Harrison, February 2013 *)

BeginPackage["EDA`"]
EndPackage[]

(* Declarations for file EDA`Common` *)

DeclarePackage["EDA`Common`",{"ChiSquared", "EDACovariance", "DegreesOfFreedom", 
	"EffectiveVariance", "Extrapolate", "MaximumIterations", "PseudoErrorY", 
	"ResidualPlacement", "Residuals", "ReturnCovariance",
	"ReturnEffectiveVariance", "ReturnErrors", "ReturnFunction",
	"ReturnResiduals", "Reweight", "Separate", "ShowFit", "EDAShowProgress",
	"SumOfSquares", "UseFitErrors", "UseSignificantFigures"}];


(* Declarations for file EDA`Data`AbsZero` *)

DeclarePackage["EDA`Data`AbsZero`",{"AbsoluteZeroData"}];


(* Declarations for file EDA`Data`Anscombe` *)

DeclarePackage["EDA`Data`Anscombe`",{"AnscombeData"}];


(* Declarations for file EDA`Data`Boyle` *)

DeclarePackage["EDA`Data`Boyle`",{"BoyleData"}];


(* Declarations for file EDA`Data`Cobalt60` *)

DeclarePackage["EDA`Data`Cobalt60`",{"Cobalt60Data"}];


(* Declarations for file EDA`Data`Chwirut` *)

DeclarePackage["EDA`Data`Chwirut`",{"ChwirutData"}];

(* Declarations for file EDA`Data`CThDatng` *)

DeclarePackage["EDA`Data`CThDatng`",{"CThDatingData"}];


(* Declarations for file EDA`Data`Darwin` *)

DeclarePackage["EDA`Data`Darwin`",{"CrossFertilizedData",
	"SelfFertilizedData"}];


(* Declarations for file EDA`Data`FreeFall` *)

DeclarePackage["EDA`Data`FreeFall`",{"FreeFallData"}];


(* Declarations for file EDA`Data`Ganglion` *)

DeclarePackage["EDA`Data`Ganglion`",{"GanglionData"}];


(* Declarations for file EDA`Data`Intrnron` *)

DeclarePackage["EDA`Data`Intrnron`",{"InterneuronData"}];


(* Declarations for file EDA`Data`KPiPi` *)

DeclarePackage["EDA`Data`KPiPi`",{"KPiPiData"}];


(* Declarations for file EDA`Data`MassSpec` *)

DeclarePackage["EDA`Data`MassSpec`",{"MassSpecData"}];


(* Declarations for file EDA`Data`MssSpec2` *)

DeclarePackage["EDA`Data`MssSpec2`",{"MassSpec2Data"}];


(* Declarations for file EDA`Data`Ozone` *)

DeclarePackage["EDA`Data`Ozone`",{"OzoneData"}];


(* Declarations for file EDA`Data`Popltion` *)

DeclarePackage["EDA`Data`Popltion`",
     {"ArgentinaData", "BrazilData", "CanadaData", "ChinaData",
     "EnglandData" , "FranceData", "IndiaData", "ItalyData", "JapanData",
     "MexicoData", "NetherlandsData", "PopulationData", "SovietUnionData",
     "SpainData", "SwedenData", "UnitedStatesData", "WestGermanyData"}];


(* Declarations for file EDA`Data`PrsnYork` *)

DeclarePackage["EDA`Data`PrsnYork`",{"PearsonYorkData"}];


(* Declarations for file EDA`Data`Rctnce` *)

DeclarePackage["EDA`Data`Rctnce`",{"ReactanceData"}];


(* Declarations for file EDA`Data`Siegel` *)

DeclarePackage["EDA`Data`Siegel`",{"SiegelData"}];


(* Declarations for file EDA`Data`Thrmcple` *)

DeclarePackage["EDA`Data`Thrmcple`",{"ThermocoupleData"}];

(* Declarations for file EDA`Data`Wampler` *)

DeclarePackage["EDA`Data`Wampler`",{"WamplerData"}];


(* Declarations for file EDA`EDAGraphics` *)

DeclarePackage["EDA`EDAGraphics`",
     {"BoxPlot", "EDAHistogram", "EDAListPlot", "ListPlotThreshold", "ShowAll"}];


(* Declarations for file EDA`ErrorPropagation` *)

DeclarePackage["EDA`ErrorPropagation`",
     {"CombineWithError", "Data", "DataFunctions", "DataQ", "Datum",
     "DatumQ", "DivideWithError", "PlusMinus", "PlusWithError", "PowerWithError",
     "Quadrature", "SubtractWithError", "TimesWithError"}];


(* Declarations for file EDA`FindFit` *)

DeclarePackage["EDA`FindFit`",{"AbsoluteChiSquaredTolerance",
	"BreitWigner", "EDAFindFit", "Galatry", "Gaussian", "Lorentzian",
	"PearsonVII", "RelativeChiSquaredTolerance",
	"RelativisticBreitWigner", "ShowFitResult", "ToFitFunction",
	"ValueTolerance", "Voigt"}];


(* Declarations for file EDA`FindPeaks` *)

DeclarePackage["EDA`FindPeaks`",{"Amplitude", "Bkgd", "FindBkgd",
	"FindPeaks", "CenterValue", "FWHM", "IndependentVariable", "MaximumPeaks",
	"Model", "EDAParameters", "PeakArea", "PeakWidth", "EDASigma", "PeaksFound"}];


(* Declarations for file EDA`ImportExport` *)

DeclarePackage["EDA`ImportExport`",
     {"AllNumeric", "DataForm", "ExportData", "FormatData", "HeaderLines",
     "ImportData", "InputVariables", "NumData", "OutputVariables",
     "Overwrite", "Padding", "Separator", "TrailerLines", "UseVariables"}];


(* Declarations for file EDA`LinearFit` *)

DeclarePackage["EDA`LinearFit`",{"Basis", "Brent", "BrentTolerance",
	"ConvergenceTest", "InverseMethod", "LinearFit", "LUD",
	"ShowLinearFit", "SVD", "ToLinearFunction"}];


(* Declarations for file EDA`RobustFit` *)

DeclarePackage["EDA`RobustFit`",{"Groups", "PseudoChiSquared",
	"RobustCurveFit", "RobustLineFit"}];


(* Declarations for file EDA`SmoothData` *)

DeclarePackage["EDA`SmoothData`",
     {"EDAAlpha", "ColumnInterpolation", "ExtraSignal", "FillData",
     "LoessFit", "MissingIs", "EDANearestNeighbor", "Points",
     "RowInterpolation", "EDASimple", "SimpleMedian", "SmoothData",
     "SmoothFill", "SmoothFillAll", "SmoothTestData", "StructureSize",
     "WeightedAverage", "WeightedFourier"}];


(* Declarations for file EDA`Utilities` *)

If[ $VersionNumber < 9.0,
	DeclarePackage["EDA`Utilities`",{"AbsoluteZero",
		"AdjustSignificantFigures", "Anscombe", "Boyle",
		"ChiSquareProbability", "Cobalt60", "CThDating", "Darwin",
		"DataParameters", "ErrorDigits", "FreeFall", "Ganglion",
		"Interneuron", "KPiPi", "LoadData", "MassSpec", "MassSpec2",
	 	"Ozone", "PearsonYork", "Population", "EDAReactance",
		"Siegel", "Thermocouple", "UnpackData"}],
	(* else *)
	DeclarePackage["EDA`Utilities`",{"AbsoluteZero",
		"AdjustSignificantFigures", "Anscombe", "Boyle",
		"ChiSquareProbability", "Cobalt60", "CThDating", "Darwin",
		"DataParameters", "ErrorDigits", "FreeFall", "Ganglion",
		"Interneuron", "KPiPi", "LoadData", "MassSpec", "MassSpec2",
	 	"Ozone", "PearsonYork", "Population", "EDAReactance",
		"Siegel", "Thermocouple", "UnpackData", "FilterOptions"}]
];

(* Declarations for file EDA`Bundles` *)

DeclarePackage["EDA`Bundles`", {"EDALogListPlot", "EDALogLogListPlot", 
	"FitExponent", "FitPeaks", "PeakShape"}];

DeclarePackage["EDA`EDARelease`", {"$EDAVersionNumber", "$EDAReleaseNumber",
	"$EDAVersion"}];

(* End of Master Package *)
