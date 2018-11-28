BeginPackage["EDA`Data`MssSpec2`"]

MassSpec2Data::usage =
	"MassSpec2Data is ionization data for neon from a mass
	spectrometer. The data was collected by an apparatus in the
	undergraduate laboratories of the Dept. of Physics, Univ.
	of Toronto, during the Winter of 1993, by Mr. Youri Zabbal,
	when he was an undergraduate student. The format is 
	{voltage, current}, where the voltage is in volts and the
	current is in mA."

(*
 * {voltage, current} data for Neon from the Mass Spectrometer
 * experiment.  Data taken by Youri Zabbal as a student in the
 * III & IV Year Lab, winter 1993.
 *)
MassSpec2Data = {
{20.5, 0.004}, {20.6, 0.005}, {20.7, 0.006}, {20.8, 0.008}, 
{20.9, 0.010}, {21.0, 0.014}, {21.1, 0.018}, {21.2, 0.026},
{21.3, 0.035}, {21.4, 0.048}, {21.5, 0.068}, {21.6, 0.087},
{21.7, 0.110}, {21.8, 0.138}, {21.9, 0.172}, {22.0, 0.204},
{22.1, 0.237}, {22.2, 0.278}, {22.3, 0.313}, {22.4, 0.348},
{22.5, 0.39}};

EndPackage[]
