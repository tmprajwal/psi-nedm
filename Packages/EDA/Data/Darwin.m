BeginPackage["EDA`Data`Darwin`"]

(*
 * Data on growth rates of Zea Mays, taken by Darwin in 1878.  From
 * B.A. Barnard, "Darwin's Data on Growth Rates of Plants" in
 * D.F. Andres and A.M. Herzberg, "Data: A Collection of Problems from
 * Many Fields for the Student and Research Worker" (Springer-Verlag,
 * 1985) pgs 912.  ISBN: 0-387-96125-9.
 *
 * Note that the data from Pot 1, plants 1-3 in each, were discarded by
 * Darwin because the plants got sick.
 *
 * Units are inches.
 *)

CrossFertilizedData::usage =
	"CrossFertilizedData is the height of 15 Zea Mays plants that were
	cross-fertilized, measured in inches. The data was taken by Darwin
	in 1878."

SelfFertilizedData::usage =
	"SelFertilizedData is the height of 15 Zea Mays plants that were
	self-fertilized, measured in inches. The data was taken by Darwin
	in 1878."

CrossFertilizedData = { 23.5, 12, 21, 22, 19.125, 21.5, 22.125, 20.375,
18.25, 21.625, 23.25, 21, 22.125, 23, 12};

SelfFertilizedData = {17.375, 20.375, 20, 20, 18.375, 18.625, 18.625,
15.25, 16.5, 18, 16.25, 18, 12.75, 15.5, 18};


EndPackage[]
