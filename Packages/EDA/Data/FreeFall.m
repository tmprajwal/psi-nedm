BeginPackage["EDA`Data`FreeFall`"]

FreeFallData::usage =
	"FreeFallData is free-fall data taken by Professor A.W. Key,
	Department of Physics, University of Toronto using an apparatus from the
	undergraduate laboratory (1994, unpublished).  The dropped
	object was a round plastic ball.  The data format is
	{ {t, errt}, {s, errs}}, where t is the time, errt is the
	error in the time, both in ms, s is the distance, and errs is the
	error in the distance, both in meters."

FreeFallData = {
	{{685, 0.05}, {3.3, 0.00014}},
	{{645.3, 0.05}, {3, 0.00014}},
	{{603.5, 0.05}, {2.7, 0.00014}},
	{{559.3, 0.05}, {2.4, 0.00014}},
	{{512.2, 0.05}, {2.1, 0.00014}},
	{{461.4, 0.05}, {1.8, 0.00014}},
	{{405.9, 0.05}, {1.5, 0.00014}},
	{{344, 0.05}, {1.2, 0.00014}},
	{{273, 0.05}, {0.9, 0.00014}},
	{{186.4, 0.05}, {0.6, 0.00014}}
};

EndPackage[]
