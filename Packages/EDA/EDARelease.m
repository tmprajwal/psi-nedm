(* $Id: EDARelease.m,v 9.0 2013/02/17 16:58:55 harrison Exp $ *)

BeginPackage["EDA`EDARelease`"]

$EDAVersionNumber::usage = "$EDAVersion is a real number that
	gives the current Experimental Data Analyst version number,
	and increases in successive versions."

$EDAReleaseNumber::usage = "$EDAReleaseNumber is an integer
	that gives the current Experimental Data Analyst release number,
	and increases in successive releases."

$EDAVersion::usage = "$EDAVersion is a string that represents
	the version of Experimental Data Analyst."

$EDAVersionNumber = 1.3
$EDAReleaseNumber = 1
$EDAVersion = "Experimental Data Analyst Version 1.3.1 (February 2013)"

EndPackage[]
