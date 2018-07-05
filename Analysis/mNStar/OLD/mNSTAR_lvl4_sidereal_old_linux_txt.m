(* ::Package:: *)

(* ::Input::Initialization:: *)
OS="axion";(*or,linux*)
If[OS=="win",
Print["Working Directory: ",descDir="C:\\Users\\tmpra\\Dropbox\\nEDM\\analysis-mirror_neutrons\\nstar_online"];Print["(AscDir) Rawdata Directory: ",AscDir="C:\\Users\\tmpra\\Dropbox\\nEDM\\rawdata"];Print["(ScrDir) Scratch Directory: ",curDir=SetDirectory["C:\\Users\\tmpra\\Dropbox\\Scratch"]];Print["(PicDir) Picture Directory: ",PicDir="C:\\Users\\tmpra\\Dropbox\\nEDM\\Pictures\\nstar\\runs2"];Print["(SysDir) System Directory: ",SysDir="C:\\Users\\tmpra\\Dropbox\\nEDM\\system"];
Print["Rawdata on mpc1636:",mpcDir="/xdata/nedm_data/RawData"];
seperator="\\";
];
If[OS=="axion",
Print["Working Directory: ",descDir="/home/prajwal/Dropbox/nEDM/analysis-mirror_neutrons/nstar_online"];Print["(AscDir) Rawdata Directory: ",AscDir="/home/prajwal/Dropbox/nEDM/rawdata"];Print["(ScrDir) Scratch Directory: ",curDir=SetDirectory["/home/prajwal/Dropbox/Scratch"]];Print["(PicDir) Picture Directory: ",PicDir="/home/prajwal/Dropbox/nEDM/Pictures/nstar/runs"];Print["(SysDir) System Directory: ",SysDir="/home/prajwal/Dropbox/nEDM/system"];
Print["Rawdata on mpc1636:",mpcDir="/xdata/nedm_data/RawData"];
seperator="/";
];
metaStructure={Word,Real,Real,Word,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real};
metadesc={Number,Number,Number,Number,Number,Number,Number,Word,Real,Real,Real,Real};
descdat=ReadList[StringJoin[descDir,seperator,"desctf_c.dat"],metadesc];
dimdesc=Dimensions[descdat][[1]];
(*LS-Periodogram*)
LS[tx_, nP_] := 
 LS[tx, nP] = 
  Block[{t, x, xbar, std, \[Tau], P, \[CapitalOmega], diff, lim, 
     periodogram},
    t = Table[tx[[i, 1]], {i, 1, Length[tx]}];
    x = Table[tx[[i, 2]], {i, 1, Length[tx]}];
    xbar = Mean[x];
    std = 1/(Length[x] - 1.) Sum[(x[[i]] - xbar)^2, {i, 1, Length[x]}];
    \[Tau][\[Omega]_] := \[Tau][\[Omega]] = ArcTan[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(Sin[2. \ \[Omega]\ x[\([i]\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]Cos[2. \ \[Omega]\ x[\([i]\)]]\)\)\)]/(2. \[Omega]);
    P[\[Omega]_] := P[\[Omega]] = (0.5/std) ((\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(\((x[\([i]\)] - xbar)\)\ Cos[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\)\))^2/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]
\*SuperscriptBox[\(Cos[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\), \(2\)]\) + (\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(\((x[\([i]\)] - xbar)\)\ Sin[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\)\))^2/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]
\*SuperscriptBox[\(Sin[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\), \(2\)]\));
    \[CapitalOmega][T_] := \[CapitalOmega][T] = 2. \[Pi]/T;
    diff = Table[t[[i + 1]] - t[[i]], {i, 1, Length[t] - 1}]; 
    lim = \[CapitalOmega] /@ {Last[t] - First[t], 2 Min[diff]};
    periodogram = 
     ParallelTable[{\[Omega], P[\[Omega]]}, {\[Omega], lim[[1]], 
       lim[[2]], (lim[[2]] - lim[[1]])/nP}]
    ];
LSCos[tx_, nP_] := 
 LSCos[tx, nP] = 
  Block[{t, x, xbar, std, \[Tau], P, \[CapitalOmega], diff, lim, 
     periodogram},
    t = Table[tx[[i, 1]], {i, 1, Length[tx]}];
    x = Table[tx[[i, 2]], {i, 1, Length[tx]}];
    xbar = Mean[x];
    std = 1/(Length[x] - 1.) Sum[(x[[i]] - xbar)^2, {i, 1, Length[x]}];
    \[Tau][\[Omega]_] := \[Tau][\[Omega]] = ArcTan[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(Cos[2. \ \[Omega]\ x[\([i]\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]Sin[2. \ \[Omega]\ x[\([i]\)]]\)\)\)]/(2. \[Omega]);
    P[\[Omega]_] := P[\[Omega]] = (0.5/std) ((\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(\((x[\([i]\)] - xbar)\)\ Sin[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\)\))^2/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]
\*SuperscriptBox[\(Sin[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\), \(2\)]\) + (\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]\(\((x[\([i]\)] - xbar)\)\ Cos[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\)\))^2/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[tx]\)]
\*SuperscriptBox[\(Cos[\[Omega]\ \((t[\([i]\)] - \[Tau][\[Omega]])\)]\), \(2\)]\));
    \[CapitalOmega][T_] := \[CapitalOmega][T] = 2. \[Pi]/T;
    diff = Table[t[[i + 1]] - t[[i]], {i, 1, Length[t] - 1}]; 
    lim = \[CapitalOmega] /@ {Last[t] - First[t], 2 Min[diff]};
    periodogram = 
     ParallelTable[{\[Omega], P[\[Omega]]}, {\[Omega], lim[[1]], 
       lim[[2]], (lim[[2]] - lim[[1]])/nP}]
    ];
T[periodogram_,rankmax_]:=T[periodogram,rankmax]=Block[{power,\[Omega]max,period},power=Table[periodogram[[i,2]],{i,1,Length[periodogram]}];
\[Omega]max=periodogram[[Position[power,RankedMax[power,rankmax]][[1,1]],1]];
{period=2 \[Pi]/\[Omega]max,RankedMax[power,rankmax]}];
p[z_,n_]:=p[z,n]=1-(1-Exp[-z])^n;
(*************************************************************************************************)
Quiet[
rundat={};
starttime=0;
pta10=pta20=fta10=fta20=erra10=erra20=fta102=fta202={};
bpt10=bft10=errb10={};
bpt20=bft20=errb20={};
bpt0=bft0=errb0={};
ptarun=Table[{},{k,1,dimdesc}];ptarun2=Table[{},{k,1,dimdesc}];
ftarun=Table[{},{k,1,dimdesc}];ftarun2=Table[{},{k,1,dimdesc}];
errarun=Table[{},{k,1,dimdesc}];errarun2=Table[{},{k,1,dimdesc}];
tsadd=2*PlusMinus[11.306,0.397];
For[i=1,i<=dimdesc,i++,
pmb0run=pmbbrun={};
If[(descdat[[i]][[5]]>20&&descdat[[i]][[5]]<9000(*&&MemberQ[{1,2,3,4,5,39,40,41},i]*))
,
(*Print[descdat[[i]][[4]]];*)
rundat=ReadList[StringJoin[AscDir,seperator,IntegerString[descdat[[i]][[4]],10,6],seperator,IntegerString[descdat[[i]][[4]],10,6],"_lvl2.dat"],{Real,Real,Real,Real,Real,Real,Real,Real,Real}];
dimrundat=Dimensions[rundat][[1]];
If[i==1,starttime=rundat[[1]][[1]]];
(*For-A*tf/ts*)
ptarun[[i]]=Table[
{
N[(rundat[[;;,1]][[k]]-starttime)/(86164.09054)](*Mod[(rundat[[;;,1]][[k]]-starttime)/(86164.09054),1]*),
{
((PlusMinus[rundat[[;;,{2,3}]][[k]][[1]],rundat[[;;,{2,3}]][[k]][[2]]])*10^6*((PlusMinus[descdat[[i]][[9]],descdat[[i]][[10]]])/(PlusMinus[descdat[[i]][[7]],0.1]+tsadd)))[[1]],
Abs[((PlusMinus[rundat[[;;,{2,3}]][[k]][[1]],rundat[[;;,{2,3}]][[k]][[2]]])*10^6*((PlusMinus[descdat[[i]][[9]],descdat[[i]][[10]]])/(PlusMinus[descdat[[i]][[7]],0.1]+tsadd)))[[2]]]
}
},
{k,1,Dimensions[rundat[[;;,{2,3}]]][[1]]}
];
ftarun[[i]]=Transpose[{ptarun[[i]][[;;,1]],ptarun[[i]][[;;,2]][[;;,1]]}];
errarun[[i]]=Abs[ptarun[[i]][[;;,2]][[;;,2]]];
(*For-A*)
ptarun2[[i]]=Table[{N[(rundat[[;;,1]][[k]]-starttime)/(86164.09054)],{rundat[[;;,{2,3}]][[k]][[1]],Abs[rundat[[;;,{2,3}]][[k]][[2]]]}},{k,1,Dimensions[rundat[[;;,{2,3}]]][[1]]}];
ftarun2[[i]]=Transpose[{ptarun2[[i]][[;;,1]],ptarun2[[i]][[;;,2]][[;;,1]]}];
errarun2[[i]]=Abs[ptarun2[[i]][[;;,2]][[;;,2]]];
(**)
If[descdat[[i]][[6]]==10,
{
bpt10=Join[bpt10,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,{8,9}]]}]];
bft10=Join[bft10,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,8]]}]];
errb10=Join[errb10,rundat[[;;,9]]];
pmbbrun=Join[pmbbrun,Table[PlusMinus[rundat[[k]][[8]],rundat[[k]][[9]]],{k,1,dimrundat}]];
(*A*)
pta10=Join[pta10,ptarun[[i]]];
fta10=Join[fta10,ftarun[[i]]];
erra10=Join[erra10,errarun[[i]]];
},
{
bpt20=Join[bpt20,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,{8,9}]]}]];
bft20=Join[bft20,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,8]]}]];
errb20=Join[errb20,rundat[[;;,9]]];
pmbbrun=Join[pmbbrun,Table[PlusMinus[rundat[[k]][[8]],rundat[[k]][[9]]],{k,1,dimrundat}]];
(*A*)
pta20=Join[pta20,ptarun[[i]]];
fta20=Join[fta20,ftarun[[i]]];
erra20=Join[erra20,errarun[[i]]];
}
];
bpt0=Join[bpt0,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,{6,7}]]}]];
bft0=Join[bft0,Transpose[{N[(rundat[[;;,1]]-starttime)/(86164.09054)],rundat[[;;,6]]}]];
errb0=Join[errb0,rundat[[;;,7]]];
];
lvl3={{}};
(*Export[StringJoin[AscDir,seperator,IntegerString[descdat[[i]][[4]],10,6],seperator,IntegerString[descdat[[i]][[4]],10,6],"_lvl3sidereal.dat"],lvl3];*)
];
(**)
cutoff=3;
cleanpta10=Select[pta10,Abs[#[[2]][[1]]]<cutoff&];
cleanpta20=Select[pta20,Abs[#[[2]][[1]]]<cutoff*1.5&];
cleanfta10=Table[{cleanpta10[[k]][[1]],cleanpta10[[k]][[2]][[1]]},{k,1,Dimensions[cleanpta10][[1]]}];
cleanerra10=Table[cleanpta10[[k]][[2]][[2]],{k,1,Dimensions[cleanpta10][[1]]}];
cleanfta20=Table[{cleanpta20[[k]][[1]],cleanpta20[[k]][[2]][[1]]},{k,1,Dimensions[cleanpta20][[1]]}];
cleanerra20=Table[cleanpta20[[k]][[2]][[2]],{k,1,Dimensions[cleanpta20][[1]]}];
(**)
(**************************************************************************************************************)
mcruns=25;
np=160000;
gramftamc=Table[{},{k,1,mcruns}];
gramftamc2=Table[{},{k,1,mcruns}];
totdat=Join[cleanpta10,cleanpta20];
totdatft=Join[cleanfta10,cleanfta20];
Print["Begin Heavy Computation"];
Monitor[
Parallelize[
For[j=1,j<=mcruns,j++,
cleanftamc={};
For[i=1,i<=Dimensions[totdat][[1]],i++,
RandomSeed[Total[Date[]]];
cleanftamc=AppendTo[cleanftamc,{totdat[[i]][[1]],RandomVariate[NormalDistribution[totdat[[i]][[2]][[1]],totdat[[i]][[2]][[2]]]]}];
];
gramftamc[[j]]=LS[cleanftamc,np];
gramftamc2[[j]]=LSCos[cleanftamc,np];
];
gramfta=LS[totdatft,np];
gramfta2=LSCos[totdatft,np];
gramftamcm=Mean[gramftamc];
gramftamcm2=Mean[gramftamc2];
gramftamcs=StandardDeviation[gramftamc];
gramftamcs2=StandardDeviation[gramftamc2];
];,ProgressIndicator[j/mcruns,ImageSize->{1000,100}]]
ProgressIndicator[1,ImageSize->{1000,100}]
Export[StringJoin[descDir,seperator,"LS-Sin-data2.dat"],gramfta]
Export[StringJoin[descDir,seperator,"LS-Cos-data2.dat"],gramfta2]
Export[StringJoin[descDir,seperator,"LS-Sin-mc_mean2.dat"],gramftamcm]
Export[StringJoin[descDir,seperator,"LS-Cos-mc_mean2.dat"],gramftamcm2]
Export[StringJoin[descDir,seperator,"LS-Sin-mc_sigma2.dat"],gramftamcs]
Export[StringJoin[descDir,seperator,"LS-Cos-mc_sigma2.dat"],gramftamcs2]];//AbsoluteTiming



