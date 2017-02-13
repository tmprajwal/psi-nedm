BeginPackage["EDA`Data`Ganglion`"]

GanglionData::usage =
	"GanglionData is data for the ratio of the central ganglion cell density
	to the peripheral density versus retinal area for 14 cat fetuses.
	The fetuses ranged in age from 35 to 62 days, and the retinal area
	is approximately monotonic in the age of the fetus. The data is from
	Barry Lia, Robert W. Williams, and Leo M Chalupa, Science 236, (1987)
	pg 848. The format of the data is {area, CP}, where area is in mm^2
	and CP is dimensionless."

GanglionData = {
	{ 11.1, 2.56 },
	{ 13.6, 2.87 },
	{ 22.5, 2.96 },
	{ 31.4, 3.75 },
	{ 32.7, 3.42 },
	{ 34.0, 3.50 },
	{ 53.8, 4.55 },
	{ 63.0, 4.68 },
	{ 67.0, 6.98 },
	{ 81.0, 6.85 },
	{ 101., 11.25 },
	{ 107., 10.91 },
	{ 114., 11.10 },
	{ 141., 18.29}};

EndPackage[]
