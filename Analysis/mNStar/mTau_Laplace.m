#!/usr/local/bin/wolframscript
OS="mpc1636";
If[OS=="mpc1636",
Print["Working Directory:",curDir=SetDirectory["/home/prajwal"]];
Print["Analysis Directory:",anDir="/home/prajwal/psi-nedm/Analysis/mNStar"];
Print["(ASCII) Rawdata Directory:",AscDir="/xdata/nedm_data/RawData/LiveData"];
Print["Scratch Directory: ",ScrDir="/home/prajwal/Scratch"];
Print["Picture Directory: ",PicDir="/dev/shm/prajwal"];
];
ftallseries3=Import[StringJoin[anDir, "/", "ftallseries3.dat"]];
(**)
stt=50;
endt=2000;
scnftst=-N[1/(stt/Log[2])];
scnend=-N[1/(endt/Log[2])];
numsamples=1000000;
scnint=Abs[scnend-scnftst]/numsamples;
dimcctvals=Dimensions[Table[cct,{cct,scnftst,scnend,scnint}]][[1]]
parameters=Array[a,{dimcctvals}];
variables=Array[x,{dimcctvals}];
guess=Table[1,{k,1,dimcctvals}];
bbini = 1/RandomReal[{1/scnftst, 1/scnend}, {numsamples + 1}];
bb = Sort[bbini, #1 > #2 &];
f[x_,a_]:=a.Exp[x bb];
nlmfitchk=NonlinearModelFit[ftallseries3,f[xx,parameters],Transpose[{parameters,guess}],xx];
basechi2=Total[(nlmfitchk["FitResiduals"]/errallseries3)^2];
Unprotect[$ProcessorCount];
$ProcessorCount=16;
LaunchKernels[16];
nlmfitchktab=ParallelTable[Total[(NonlinearModelFit[ftallseries3,f[xx,parameters]-f[xx,Table[If[k==kk,parameters[[k]],0],{k,1,dimcctvals}]],Transpose[{parameters,guess}],xx]["FitResiduals"]/errallseries3)^2],{kk,1,dimcctvals}];
Export["mTau_Laplace_info.dat",{stt,endt,numsamples,basechi2,"\n"}];
(**)
ptnlmfitchktab=Table[{-Log[2]/bb[[k]],Log[nlmfitchktab[[k]]]},{k,1,dimcctvals}];
Export["mTau_Laplace_fit.dat",ptnlmfitchktab];
(*EOF*)
