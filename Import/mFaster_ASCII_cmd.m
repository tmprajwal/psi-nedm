(* ::Package:: *)

Print["Working Directory:",curDir=SetDirectory["/home/prajwal/Dropbox/nEDM/psi-nedm/Import"]];
Print["(faster) Rawdata Directory:",fasDir="/home/prajwal/nEDM/rawdata"];
Print["(ASCII) Rawdata Directory:",AscDir="/home/prajwal/Dropbox/nEDM/rawdata"];
year=2016;
month=06;
day=13;
runNum=11162;
maxCy=181;
For[i=1,i<=maxCy,i++,
Run[StringJoin["faster_disfast ",fasDir,"/",ToString[year],"/",IntegerString[month,10,2],"/",IntegerString[day,10,2],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.public -t 43 > ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Run[StringJoin["sed -i -e '1,3d' -e 's/ns//' -e 's/q1=//' -e 's/q2=//' -e 's/q3=//' -e 's/mV.ns//' -e 's/mV.ns//' -e 's/mV.ns//' ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Print["Run #",runNum,"with ",i," cycles, completed.\n"];
];
Print["Run #",runNum,"with ",i-1," cycles, completed.\n"](*;*)
