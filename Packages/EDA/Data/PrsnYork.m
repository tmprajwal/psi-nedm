BeginPackage["EDA`Data`PrsnYork`"]

PearsonYorkData::usage =
	"PearsonYorkData is based on a data set of x, y pairs due to K.
	Pearson, Philos, Mag 2, (1901) pg 2. It has been weighted by
	Derek York, Canadian Journal of Physics 44, (1966) pg 1079. The format
	of the data is { {x, errx}, {y, erry} }."

PearsonYorkData = {
	{{0.0,	0.0316},	{5.9,	1.000}},
	{{0.9,	0.0316},	{5.4,	.746}},
	{{1.8,	0.0447},	{4.4,	.500}},
	{{2.6,	0.0354},	{4.6,	.354}},
	{{3.3,	0.0707},	{3.5,	.224}},
	{{4.4,	0.1118},	{3.7,	.224}},
	{{5.2,	0.1291},	{2.8,	.120}},
	{{6.1,	0.2236},	{2.8,	.120}},
	{{6.5,	0.7454},	{2.4,	.100}},
	{{7.4,	1.0000},	{1.5,	.045}}};

EndPackage[]
