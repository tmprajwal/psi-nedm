#!/usr/local/bin/WolframScript -script
Print["Working Directory:",curDir=SetDirectory["/home/prajwal/Dropbox/nEDM/psi-nedm/Import"]];
Print["Meta file Directory:",metaDir="/home/prajwal/Dropbox/nEDM/rawdata"];
Print["Rawdata (faster) Directory:",fasDir="/home/prajwal/Dropbox/nEDM/rawdata"];
Print["Rawdata (ASCII) Directory:",AscDir="/home/prajwal/Dropbox/nEDM/rawdata"];
metaStructure={Word,Real,Real,Word,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real};
runNum=Round[ToExpression[$ScriptCommandLine[[2]]]];
Print["Run #",runNum];
maxCy=ReadList[StringJoin[AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta3.edm"],metaStructure][[-1]][[2]];
(*Creating ASCII faster files*)
For[i=1,i<=maxCy,i++,
Run[StringJoin["faster_disfast ",fasDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.public -t 43 > ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Run[StringJoin["sed -i -e '1,3d' -e 's/ns//' -e 's/q1=//' -e 's/q2=//' -e 's/q3=//' -e 's/mV.ns//' -e 's/mV.ns//' -e 's/mV.ns//' ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Print["Run #",runNum," with ",i," cycles, completed.\n"];
];
Print["Run #",runNum," with ",i-1," cycles, completed.\n"]
(*EOF*)
