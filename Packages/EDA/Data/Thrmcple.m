BeginPackage["EDA`Data`Thrmcple`"]

ThermocoupleData::usage =
	"ThermocoupleData is experimental data of a thermocouple calibration
	given by Philip R. Bevington's \"Data Reduction and Error Analysis\"
	(McGraw Hill, 1969), pg 138. The format of each data point is
	{T, {V, dV}}, where T is the temperature in degrees Celsius, V the
	voltage in mV, and dV the error in V in mV."

ThermocoupleData = {
	{0.0,	{-0.89,	.05}},
	{5.0,	{-0.69,	.05}},
	{10.0,	{-0.53,	.05}},
	{15.0,	{-0.34,	.05}},
	{20.0,	{-0.15,	.05}},
	{25.0,	{0.02,	.05}},
	{30.0,	{0.20,	.05}},
	{35.0,	{0.42,	.05}},
	{40.0,	{0.61,	.05}},
	{45.0,	{0.82,	.05}},
	{50.0,	{1.03,	.05}},
	{55.0,	{1.22,	.05}},
	{60.0,	{1.45,	.05}},
	{65.0,	{1.68,	.05}},
	{70.0,	{1.88,	.05}},
	{75.0,	{2.10,	.05}},
	{80.0,	{2.31,	.05}},
	{85.0,	{2.54,	.05}},
	{90.0,	{2.78,	.05}},
	{95.0,	{3.00,	.05}},
	{100.0,	{3.22,	.05}}};

EndPackage[]
