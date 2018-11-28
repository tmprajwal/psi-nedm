BeginPackage["EDA`Data`AbsZero`"]

AbsoluteZeroData::usage =
	"AbsoluteZeroData is student-collected data on the pressure and
	temperature of a fixed volume of gas, reported in John R.
	Taylor, \"An Introduction to Error Analysis: The Study of
	Uncertainties in Physical Measurements\" (University Science
	Books, Mill Valley California, 1982), pg 160. The format
	of each data point is {p, t}, where 'p' is the pressure in
	mm of mercury and 't' the temperature in degrees Centigrade.
	The student judged that his measurements of p had neglible
	uncertainty and those of t were all equally uncertain, with
	an uncertainty of \"a few degrees\"."

AbsoluteZeroData = { {65, -20}, {75, 17}, {85, 42}, {95, 94},
	{105, 127}};

EndPackage[]
