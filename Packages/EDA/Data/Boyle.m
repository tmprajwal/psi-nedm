BeginPackage["EDA`Data`Boyle`"]

BoyleData::usage =
	"BoyleData is based on data for testing Boyle's law taken by a 
	first-year student at the University of Toronto in the Department of
	Physics undergraduate laboratories in 1983. A fixed quantity of dry air
	at room temperature had its pressure and volume measured for a
	number of different values of the pressure. The format of each
	data point is { {p, errp}, {v, errv} }, where p and errp are
	the value and error in the pressure, measured in cm of Hg, and v
	and errv are the value and error in the volume, measured in cm^3."

BoyleData = {{{85.82, 0.02}, {3.74, 0.02}}, {{95.12, 0.02}, {3.36, 0.02}}, 
  {{119.12, 0.02}, {2.65, 0.02}}, {{137.02, 0.02}, {2.29, 0.02}}, 
  {{151.62, 0.02}, {2.05, 0.02}}, {{106.32, 0.02}, {2.99, 0.02}}, 
  {{115.72, 0.02}, {2.72, 0.02}}, {{131.52, 0.02}, {2.38, 0.02}}, 
  {{147.52, 0.02}, {2.1, 0.02}}};

EndPackage[]
