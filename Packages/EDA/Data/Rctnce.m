BeginPackage["EDA`Data`Rctnce`"]

ReactanceData::usage =
	"ReactanceData is data from an LRC circuit being driven by
	an AC voltage (V). The phase shift of the voltage (Vc) across
	the capacitor, with respect to V, is theta, and y = Cot[theta].
	The variable omega is the frequency of the supplied voltage,
	in radians per second. The format of the data is { {omega, erromega},
	{y, erry} }. The data was taken by Jay Orear, American Journal of 
	Physics 50, (1982) pg 912."

(* 
 * Phase shift data, from Orear, Am.J.Phys. 50, (1982) 912.
 *  x = 2Pi*a[[j]f/ omega0 and y = Cot[ theta[[j]].
 *)
ReactanceData = {
	{{22000,440}, {-4.017,.50}},
	{{22930,470}, {-2.742,.25}},
	{{23880,500}, {-1.1478,.08}},
	{{25130,530}, {1.491,.09}},
	{{26390,540}, {6.873,1.90}}
};

EndPackage[]
