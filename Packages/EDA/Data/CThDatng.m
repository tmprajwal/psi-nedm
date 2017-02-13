BeginPackage["EDA`Data`CThDatng`"]

CThDatingData::usage =
	"CThDatingData compares the dating of samples of coral using two
	different techniques: carbon-14 and thorium.  It can be viewed
	as a calibration of carbon-14 dating.  The data is from Edouard Bard,
	Bruno Hamelin, Richard G. Fairbanks, and Alan Zindler, Nature 345,
	(May, 1990), pg 405. The format of the data is {C14 age,
	{thorium age minus C14 age, error in thorium age minus C14 age}}, where
	all numbers are in years before the present. The errors are one standard
	deviation, not the two standard deviation errors given in the original
	source."

CThDatingData = {{6400, {1060., 110.}}, {7780, {670., 110.}}, 
  {8200, {1080., 110.}}, {9050, {680., 130.}}, {9400, {1690., 110.}}, 
  {10100, {1490., 100.}}, {9800, {1730., 110.}}, {10300, {1960., 110.}}, 
  {10900, {2320., 110.}}, {11850, {1850., 130.}}, {11800, {2000., 120.}}, 
  {11800, {2430., 210.}}, {12500, {2160., 130.}}, {14700, {3540., 210.}}, 
  {15400, {3490., 240.}}, {17085, {1900., 260.}}, {16700, {3910., 310.}}, 
  {18200, {3729., 210.}}, {27120, {3350., 770.}}};

EndPackage[]
