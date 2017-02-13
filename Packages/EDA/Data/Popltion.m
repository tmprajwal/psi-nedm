BeginPackage["EDA`Data`Popltion`"]

PopulationData::usage =
	"PopulationData is the population of the 10 largest cities
	of 16 countries as reported by the 1967 \"World Almanac\".
	The countries listed are, in order: Sweden, the Netherlands,
	Canada, France, Mexico, Argentina, Spain, England, Italy,
	West Germany, Brazil, the Soviet Union, Japan, the United
	States, India, and China. Populations are in 100,000s. The
	countries are sorted by the value of the median of the population.
	The data for each individual country is available as, for example,
	\"SwedenData\"."

SwedenData = {7.87,4.22, 2.49, 0.94, 0.89, 0.87,
        0.81, 0.78, 0.71, 0.69};
NetherlandsData = {8.68, 7.31, 6.02, 2.64, 1.75, 1.72,
        1.51, 1.42, 1.31, 1.29};
CanadaData = {11.91, 6.72, 3.84, 2.81, 2.73, 2.68,
        2.65, 2.49, 1.71, 1.69};
FranceData = {28.11, 7.83, 5.35, 3.30, 2.94, 2.54,
        2.46, 2.33, 2.03, 1.99};
MexicoData = {31.18, 10.12, 8.06, 3.79, 3.46, 2.91,
        2.71, 2.17, 2.06, 1.86};
ArgentinaData = {29.66, 7.61, 6.35, 4.10, 3.80,
        2.75, 2.70, 2.69, 2.51, 2.44};
SpainData = {25.99, 16.96, 5.01, 4.74, 3.57,
        3.34, 3.12, 2.64, 2.14, 1.69};
EnglandData = {79.86, 11.02, 7.22, 6.38, 5.09,
        4.88, 4.30, 3.30, 3.10, 2.99};
ItalyData = { 25.39, 15.80, 11.82, 11.14, 7.84,
        5.90, 4.54, 4.44, 3.61, 3.36};
WestGermanyData = {21.92, 18.56, 11.42, 8.27, 7.28,
        7.02, 6.94, 6.53, 5.84, 5.66};
BrazilData = {49.81, 38.57, 9.68, 9.52, 8.08,
        8.03, 6.99, 5.02, 4.95, 2.78};
SovietUnionData = {63.34, 36.36, 13.32, 11.37, 10.90,
        10.84, 10.70, 10.27, 9.50, 9.17};
JapanData = {110.21, 32.14, 18.88, 16.39, 13.37,
        11.95, 10.70, 7.89, 7.71, 7.04};
UnitedStatesData = {77.81, 35.50, 24.79, 20.02,
        16.70, 9.39, 9.38, 8.76, 7.63, 7.50};
IndiaData = {45.37, 30.03, 22.98, 20.62, 17.25, 16.11,
        11.49, 9.47, 9.07, 7.21};
ChinaData = {69.0, 40.10, 36.92, 32.20, 24.11,
        21.46, 21.21, 16.50, 15.00, 11.13};

PopulationData = {SwedenData, NetherlandsData, CanadaData,
        FranceData, MexicoData, ArgentinaData, SpainData,
        EnglandData, ItalyData, WestGermanyData,
        BrazilData, SovietUnionData, JapanData,
        UnitedStatesData,IndiaData, ChinaData};

EndPackage[]

