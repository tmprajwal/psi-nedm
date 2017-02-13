BeginPackage["EDA`Data`Anscombe`"]

AnscombeData::usage =
	"AnscombeData is a quartet of made-up data devised by F.J.
	Anscombe, American Statistician 27 (Feb. 1973), pg 17.
	All four data sets, AnscombeData[[1]], AnscombeData[[2]],
	AnscombeData[[3]], and AnscombeData[[4]], have almost identical
	averages and fit to almost identical straight lines. The format
	of each data set is {x, y}, where neither coordinate has units."

AnscombeData = {{{10., 8.04}, {8., 6.95}, {13., 7.58}, {9., 8.81}, 
   {11., 8.33}, {14., 9.96}, {6., 7.24}, {4., 4.26}, 
   {12., 10.84}, {7., 4.82}, {5., 5.68}}, 
  {{10., 9.14}, {8., 8.14}, {13., 8.74}, {9., 8.77}, {11., 9.26}, 
   {14., 8.1}, {6., 6.13}, {4., 3.1}, {12., 9.13}, {7., 7.26}, 
   {5., 4.74}}, {{10., 7.46}, {8., 6.77}, {13., 12.74}, 
   {9., 7.11}, {11., 7.81}, {14., 8.84}, {6., 6.08}, 
   {4., 5.39}, {12., 8.15}, {7., 6.42}, {5., 5.73}}, 
  {{8., 6.58}, {8., 5.76}, {8., 7.71}, {8., 8.84}, {8., 8.47}, {8., 7.04}, 
   {8., 5.25}, {19., 12.5}, {8., 5.56}, {8., 7.91}, {8., 6.89}}};

EndPackage[]
