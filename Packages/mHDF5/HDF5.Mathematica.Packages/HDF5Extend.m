(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
BeginPackage["HDF5Extend`",{"HDF5`","HDF5Basic`","NETLink`"}]


(* ::Input::Initialization:: *)
$HDF5ExtendPackageVersion="V2.00"


(* ::Input::Initialization:: *)
H5ExtendGetUlibVersion::usage=
"Function is deprecated (V2.00). Use instead H5getUlibversion[]."


(* ::Input::Initialization:: *)
H5ExtendGetUlibVersion::deprecated=
"Function is deprecated (V2.00). Use instead H5getUlibversion[ ]."


(* ::Input::Initialization:: *)
H5AExtendGetNumberAttributes::usage=
"H5AExtendGetNumberAttributes[\"LocationID\"] is functional but deprecated in V2.00. Use instead H5OExtendGetInfo[\"NumberAttributes\"][\"ObjectID\"]."


(* ::Input::Initialization:: *)
H5AExtendGetNumberAttributes::deprecated=
"H5AExtendGetNumberAttributes[\"LocationID\"] is functional but deprecated in V2.00. Use instead H5OExtendGetInfo[\"NumberAttributes\"][\"ObjectID\"]."


(* ::Input::Initialization:: *)
H5AExtendGetName::usage=
"H5AExtendGetName[\"AttributeID\"] returns the String name for the indicated AttributeID. In the case of error, it returns a negative number instead of a string."


(* ::Input::Initialization:: *)
H5AExtendRead::usage=
"H5AExtendRead[\"AttributeID\"] reads the data of an attribute.\n\nThe function takes the same options as H5DExtendRead. See there."


(* ::Input::Initialization:: *)
H5AExtendRead::format=
"H5AExtendRead[\"AttributeID\", \"MemoryTypeID\", \"Dimensions\"] is deprecated (V2.00). Use instead H5AExtendRead[\"AttributeID\"]."


(* ::Input::Initialization:: *)
H5DExtendOpen::usage=
"H5DExtendOpen[{\"dataSetIDsymbol\" = H5Dopen[....]}, body] sets the dataSetIDsymbol and then runs the body. The purpose of this function is that it provides an automatic H5Dclose[dataSetIDsymbol] after executing body. The return value is that of the body."


(* ::Input::Initialization:: *)
H5DExtendRead::usage=
"H5DExtendRead[\"DataSetID\", \"FileSpaceID\", \"xferPListID\"] reads data from a file. For \"FileSpaceID\" of \"H5SuALL\" or of a single hyperslab, the return is a dimensionalized matrix. For \"FileSpaceID\" of multiple hyperslabs or a list of elementary points, the returns is a list of rank 1. \n\nThe function takes the option \"ReturnAsNETObject\", which is False by default. If set to True, the return is a NETObject instead of Mathematica expression.\n\nThe function takes the option \"StringByAnsi\", which is False by default. If set to True, the H5TgetCSet[ ] is ignored and instead System`Runtime`InteropServices`Marshal`PtrToStringAnsi of .NET is used. This operation may be faster for long strings.\n\nThe function takes the option \"ByteConversionFunction\" which should be the function that takes bytes as input and returns the desired data. The default is \"Function[{##}]\", i.e., an identity operation which then returns all bytes read for each item. User needs to take care if some of these bytes correspond to an IntPtr in terms of memory release after call, i.e., the function does not do this memory release so the user must do it."


(* ::Input::Initialization:: *)
H5DExtendRead::memorySpaceError=
"Use instead H5DExtendRead[\"DataSetID\", \"FileSpaceID\", \"xferPListID\"] for full \"MemorySpaceID\". Alternatively, for partial or complex read, selection of \"MemorySpaceID\" cannot be a hyperslab for H5DExtendRead since this functions makes the NETObject buffer for the read; instead, use H5DBasicRead[ ] and preload NETObject with memory data."


(* ::Input::Initialization:: *)
H5DExtendRead::dimensionsError=
"H5DExtendRead[\"DataSetID\", \"MemoryTypeID\", \"MemorySpaceID\", \"FileSpaceID\", \"xferPListID\", \"Dimensions\"] is deprecated (V2.00). Use instead H5DExtendRead[\"DataSetID\", \"MemorySpaceID\", \"FileSpaceID\", \"xferPListID\"]."


(* ::Input::Initialization:: *)
H5DExtendWrite::usage=
"H5DExtendWrite[\"DataSetID\", \"MemoryTypeID\", \"MemorySpaceID\", \"FileSpaceID\", \"xferPListID\", \"Data\"] writes data to a file. The first 5 arguments are as defined for H5Dwrite. The \"Data\" argument should be a list or a list of lists, depending on dimensions of data. This function calls H5TExtendGetSystemType[ ] to determine the \"System.XXX\" data type to use based on \"MemoryTypeID\"; as such, this function fails if the \"System.XXX\" signature fails from the call to H5TExtendGetSystemType[ ]. Further note: In V2.00, this function is much less developed compared to H5DExtendRead. It works for basic numerical datatypes but would need updating for enums, strings (fixed and variable length), compound datatypes, and so on."


(* ::Input::Initialization:: *)
H5FExtendOpen::usage=
"H5FExtendOpen[{\"fileIDsymbol\" = H5Fopen[....]}, body] sets the fileIDsymbol and then runs the body. The purpose of this function is that it provides an automatic H5Fclose[fileIDsymbol] after executing body. The return value is that of the body."


(* ::Input::Initialization:: *)
H5GExtendOpen::usage=
"H5GExtendOpen[{\"groupIDsymbol\" = H5Gopen[....]}, body] sets the groupIDsymbol and then runs the body. The purpose of this function is that it provides an automatic H5Dclose[dataSetIDsymbol] after executing body. The return value is that of the body."


(* ::Input::Initialization:: *)
H5GExtendGetNumObjs::usage=
"H5GExtendGetNumObjs[\"GroupID\"] returns the number of objects at one node of the GroupID."


(* ::Input::Initialization:: *)
H5GExtendGetObjNameByIndex::usage=
"H5GExtendGetObjNameByIndex[\"GroupID\", \"Index\"] is functional but deprecated in V2.00. Use instead H5LExtendGetNameByIdx[\"LocationID\", \"Index\"]."


(* ::Input::Initialization:: *)
H5GExtendGetObjNameByIndex::deprecated=
"H5GExtendGetObjNameByIndex[\"GroupID\", \"Index\"] is functional but deprecated in V2.00. Use instead H5LExtendGetNameByIdx[\"LocationID\", \"Index\"]."


(* ::Input::Initialization:: *)
H5LExtendGetNameByIdx::usage=
"H5LExtendGetNameByIdx[\"LocationID\", \"Index\"] returns the String name for the indicated Index of the LocationID. For simplification, the type of index is H5uINDEXuNAME, the traverse order is H5uITERuINC, and lapl is H5PuDEFAULT."


(* ::Input::Initialization:: *)
H5OExtendGetInfo::usage=
"H5OExtendGetInfo[\"Item\"]][\"ObjectID\"] to get information. \"Item\" of \"NumberAttributes\" returns the number of attributes for the indicated ObjectID. \"Item\" of \"TypeAsString\" returns the type of object as \"UNKNOWN\", \"GROUP\", \"DATASET\", NAMED_DATATYPE\", or \"NTYPES\". \"Item\" of \"TypeAsValues\" returns the type of object as -1, 0, 1, 2, or 3. \"Item\" of \"ObjectAddressInFile\" returns the address of the object in the file (i.e., unique identifier)."


(* ::Input::Initialization:: *)
H5OExtendGetInfoByIndex::usage=
"H5OExtendGetInfoByIndex[\"Item\"]][\"LocationID\",\"ObjectPosition\"] to get information. \"Item\" is same as for H5OExtendGetInfo[ ]."


(* ::Input::Initialization:: *)
H5TExtendGetSystemType::usage="H5TExtendGetSystemType[\"DataTypeID\"] returns a .NET string of the data type, such as \"System.Int32[]\". Alternatively, if not identified by the algorithm, it returns Missing[\"NotSupported\"] along with a warning message."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeCountInOneDatum::usage="H5TExtendGetSystemTypeCountInOneDatum[\"DataTypeID\"] returns the number of system types in one datum element (i.e., it could be the size of a compound data type or a fixed length string expressed as System.Byte[] or simply the number 1 for an integer expressed as System.Int32[])."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize::usage="H5TExtendGetSystemTypeAndSize[\"DataTypeID\"] returns a list of {\"SystemType\"-> ..., \"SystemTypeCountInOneDatum\"-> ...}, where \"SystemType\" is a .NET string of the data type, such as \"System.Int32[]\", and \"SystemTypeCountInOneDatum\" is the number of system type elements that make up one datum. See more at H5TExtendGetByteSize[] about size. Alternatively, if not identified by the algorithm, it returns Missing[\"NotSupported\"] along with a warning message."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize::badtype="Identified class of `1` is not one of INTEGER, FLOAT, STRING, BITFIELD, ENUM, OPAQUE, COMPOUND, or ARRAY."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize`SystemVariables::missing="\"System.XXXX\" not identified for class of `1`, byte size of `2`, and sign of `3`."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize`SystemVariables::badtype="Identified class of `1` is not one of INTEGER, FLOAT, BITFIELD, or ENUM."


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize`Array::badtype="For ARRAY datatype, contents of `1` is not one of INTEGER, FLOAT, BITFIELD, or ENUM. This class is not implemented in algorithm. Access instead using low level commands."


(* ::Input::Initialization:: *)
H5TExtendGetAllMemberNames::usage=
"H5TExtendGetAllMemberNames[\"DataTypeID\"] returns a list of String names for the indicated DataTypeID of \"ENUM\" or \"COMPOUND\". In the case of error, it returns a negative number instead of a string."


(* ::Input::Initialization:: *)
Begin["Private`"]


(* ::Input::Initialization:: *)
optionListForReadFunctions={"ReturnAsNETObject"->False,"StringByAnsi"->False,"ByteConversionFunction"->Function[{##}] (*Identity*)}


(* ::Input::Initialization:: *)
Options[convert]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert[memoryTypeID:_Integer,matrixOfValues:_List,opts:OptionsPattern[]]:=

Which[

(* is it a fixed length String ?*)
(H5TBasicGetClass["String"][memoryTypeID]=="STRING")&&(H5TisVariableStr[memoryTypeID]==0),
(* yes: convert matrix entries to strings using fixed length algorithm *)
convert`FixedStrings[memoryTypeID,matrixOfValues,opts],

(* is it a variable length String ?*)
(H5TBasicGetClass["String"][memoryTypeID]=="STRING")&&(H5TisVariableStr[memoryTypeID]==1),
(* yes: convert matrix entries to strings using variable length algorithm *)
convert`VariableStrings[memoryTypeID,matrixOfValues,opts],

(* is it an enum ?*)
H5TBasicGetClass["String"][memoryTypeID]=="ENUM",
(* yes: convert matrix entries to strings using enum algorithm *)
convert`Enums[memoryTypeID,matrixOfValues,opts],

(* is it a compound data type ?*)
H5TBasicGetClass["String"][memoryTypeID]=="COMPOUND",
(* yes: convert matrix entries to sublist of bytes using compound algorithm and then apply "ByteConversionFunction" to those sublists *)
convert`CompoundDatatypes[memoryTypeID,matrixOfValues,opts],

(* otherwise, use native return values in matrix *)
True,
matrixOfValues
]



(* ::Input::Initialization:: *)
Options[convert`FixedStrings]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert`FixedStrings[memoryTypeID:_Integer,matrixOfValues:_List,opts:OptionsPattern[]]:=

CompoundExpression[

With[
{
(* HDF5 to Mathematica encoding *)
encoding=<|"ASCII"->"ASCII","UTF8"->"UTF-8"|>[H5TBasicGetCSet["String"][memoryTypeID]]
},

With[
{
(* in case Fortran form*)
removeRightSpacesIfFortranForm=
If[
(* Fortran form? *)
H5TBasicGetStrPad["String"][memoryTypeID]=="SPACEPAD",
(* yes; drop any right padding in spaces *)
StringDrop[StringTrim[StringJoin["LeftEdge",#]],8]&,
(* no *)
Identity[#]&]
},

(* list of bytes to a String that is possibly Null terminated (0) *)
Map[
removeRightSpacesIfFortranForm[FromCharacterCode[TakeWhile[#,Positive],encoding]]&,
matrixOfValues,
{-2}
]
]
]
]



(* ::Input::Initialization:: *)
Options[convert`VariableStrings]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert`VariableStrings[memoryTypeID:_Integer,matrixOfValues:_List,opts:OptionsPattern[]]:=

NETBlock[

Module[{intPtrToIndividualStrings,return},

CompoundExpression[

(* get IntPtr of each string *)
intPtrToIndividualStrings=NETObjectToExpression@matrixOfValues,

(* get the strings by Ansi or alternative by ASCII/UTF8 encoding *)
If[
OptionValue["StringByAnsi"],

(* use System`Runtime`InteropServices`Marshal`PtrToStringAnsi *)
return=
Map[
System`Runtime`InteropServices`Marshal`PtrToStringAnsi[#]&,
intPtrToIndividualStrings,
{-1}
],

(* read each byte and use FromCharacterCode *)
return=
Map[
convert`StringByReadingBytesUntilZero[memoryTypeID,#,opts]&,
intPtrToIndividualStrings,
{-1}
]
],

(* clear up memory; all of these IntPtr that we're clearing were created by H5Dread *)
H5freeMemory/@intPtrToIndividualStrings,

return
]
]
]




(* ::Input::Initialization:: *)
Options[convert`StringByReadingBytesUntilZero]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert`StringByReadingBytesUntilZero[memoryTypeID:_Integer,intPtr:_Symbol,opts:OptionsPattern[]]:=

(* HDF5 to Mathematica encoding *)
With[{encoding=<|"ASCII"->"ASCII","UTF8"->"UTF-8"|>[H5TBasicGetCSet["String"][memoryTypeID]]},

Module[
{byteList,i=0},
CompoundExpression[
(* read at one IntPtr until zero obtained *)
byteList=Reap[While[Positive[Sow[System`Runtime`InteropServices`Marshal`ReadByte[intPtr,i]]],i++]]//Last//Last//Most,
(* convert to a string of correct encoding*)
FromCharacterCode[byteList,encoding]
]
]
]



(* ::Input::Initialization:: *)
Options[convert`Enums]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert`Enums[memoryTypeID:_Integer,matrixOfValues:_List,opts:OptionsPattern[]]:=
Map[H5TBasicGetMemberName[memoryTypeID,#]&,matrixOfValues,{-1}]



(* ::Input::Initialization:: *)
Options[convert`CompoundDatatypes]=optionListForReadFunctions


(* ::Input::Initialization:: *)
convert`CompoundDatatypes[compoundMemoryTypeID:_Integer,matrixOfValues:_List,opts:OptionsPattern[]]:=

Module[
{numMembers,memorySize,memberOffsets,splitPoints,datumedRawData,return},

CompoundExpression[

(* example: return 4 for a compuond datatype having 4 members*)
numMembers=H5TgetNMembers[compoundMemoryTypeID],

(* example: return 32 for a compound data type having 4 members of 8 bytes each *)
memorySize=H5TBasicGetSize[compoundMemoryTypeID],

(* example: return {8,8,8,8} if each member has size of 8 bytes *)
memberOffsets=Table[H5TBasicGetMemberOffset[compoundMemoryTypeID,memberIndex],{memberIndex,0,numMembers-1}],

(* example: return {{1,8},{9,16},{17,24},{25,32}} for memberOffsets of {8,8,8,8} *)
splitPoints={#1,#2-1}&@@@Partition[memberOffsets~Join~{memorySize}+1,2,1],

(* example: split list of 32 bytes into sublists of 8 bytes each based on the splitPoints *)
datumedRawData=
Apply[
Take,
Map[
(* use Thread[ ] to construct sublists, each suitable for a call by Take[ ] *)
Thread[List[#,splitPoints],List,-1]&,
matrixOfValues,
{-2}
],
{-3}
],

(* apply the user function to each of the datums *)
 return=Apply[OptionValue["ByteConversionFunction"],datumedRawData,{-3}]
]
]


(* ::Input::Initialization:: *)
Options[formattedReturn]=optionListForReadFunctions



(* ::Input::Initialization:: *)
formattedReturn[return:_Integer|_Symbol,memoryTypeID:_Integer,reshapeDimensions:_List,opts:OptionsPattern[]]:=

Which[

(* error code *)
NumericQ[return],
return,

(* requested by user as NETObject instead of list of values*)
OptionValue["ReturnAsNETObject"],
return,

(* otherwise *)
True,

Module[{returnFormatted,listOfValues,matrixOfValues},

CompoundExpression[

(* get list of read values*)
listOfValues=NETObjectToExpression[return],

(* reshape matrix as needed *)
matrixOfValues=
If[
Length@reshapeDimensions>=2,
ArrayReshape[listOfValues,reshapeDimensions],
listOfValues
],

(* converts values, e.g., strings if needed for fixed length strings, variable length strings, or enum strings and formatted returns for compound data types *)
returnFormatted=convert[memoryTypeID,matrixOfValues,opts],

(* for Dimensions of {1}, return a scalar for value instead of list around value, i.e., as {value},  *)
If[Dimensions[returnFormatted]=={1},First@returnFormatted,returnFormatted]
] (* close Compound Expression *)

] (* close Module *)
] (* close Which *)



(* ::Input::Initialization:: *)
H5ExtendGetUlibVersion[]:=

CompoundExpression[
Message[H5ExtendGetUlibVersion::deprecated],
H5getUlibversion[]
]


(* ::Input::Initialization:: *)
H5AExtendGetNumberAttributes[locationID:_Integer]:=
CompoundExpression[
Message[H5AExtendGetNumberAttributes::deprecated],
H5OExtendGetInfo["NumberAttributes"][locationID]
]



(* ::Input::Initialization:: *)
H5AExtendGetName[attributeID:_Integer]:=

NETBlock[
Module[
{return,size},
CompoundExpression[
size=H5ABasicGetName["Size"][attributeID,0],

If[
(* check for error code *)
Negative[size],size,
(* otherwise *)
CompoundExpression[
return=H5ABasicGetName[attributeID,size+1],
(*return*) If[Negative[First@return],First@return,Last@return]
]
](*close If*)

]
]
]


(* ::Input::Initialization:: *)
Options[H5AExtendRead]=optionListForReadFunctions



(* ::Input::Initialization:: *)
H5AExtendRead[attributeID:_Integer,opts:OptionsPattern[]]:=

NETBlock[
Module[{dataTypeID,memoryTypeID,attributeSpaceID,dataElementsDimensions,systemType,systemCountOfOneDatum,systemElementsDimensions,numberSystemElements,bufferForReadData,status,return1,return2},

Catch[

CompoundExpression[

(* PART 1: START: MAKE THE BUFFER AS NETOBJECT *)

(* get type of data in the attribute *)
dataTypeID=H5AgetType[attributeID],
memoryTypeID=H5TgetNativeType[dataTypeID,H5TuDEFAULT],
H5Tclose[dataTypeID],

(* check for any error and, if error, close everything and exit with -1 *)
If[Negative[Min[memoryTypeID,dataTypeID]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1a: get the SystemType for the buffer *)

(* use the correct system type (e.g., "System.Int32[]" for the type of data *)
systemType=H5TExtendGetSystemType[memoryTypeID],
(* check for known system type and exit with -1 if needed *)
If[MatchQ[systemType,Missing[_]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1b: get the system count for one datum of the buffer *)

(* use the correct system count in one datum (e.g., 1 for "System.Int32[]" *)
systemCountOfOneDatum=H5TExtendGetSystemTypeCountInOneDatum[memoryTypeID],
(* on error exit with -1 if needed *)
If[MatchQ[systemCountOfOneDatum,Missing[_]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1c: get the system dimensions for full buffer *)

(* dimensions of attributes data set *)
attributeSpaceID=H5AgetSpace[attributeID],
dataElementsDimensions=H5SBasicGetSimpleExtentDims[attributeSpaceID],
H5Sclose[attributeSpaceID],

(* check for any error and, if error, close everything and exit with -1 *)
If[Negative[attributeSpaceID],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* add in extra dimensions of datum is not atomic, e.g., for fixed string, compound data type, array, and so on*)
systemElementsDimensions=
If[systemCountOfOneDatum!=1,
Append[dataElementsDimensions,systemCountOfOneDatum],
dataElementsDimensions
],

(* in case of scalar, still set to {1}*)
If[systemElementsDimensions=={},systemElementsDimensions={1}],

(* Part 1d: make the NETObject buffer *)

numberSystemElements=Times@@systemElementsDimensions,

bufferForReadData=NETNew[systemType,numberSystemElements],

(* PART 1: STOP: WE NOW HAVE THE NETOBJECT BUFFER WE NEED *)

(* PART 2: START: FILL THE NETOBJECT BUFFER WITH THE FILE CONTENTS *)

(* 'return': NETObject if successful, -1 if fail *)
status=H5ABasicRead[attributeID,memoryTypeID,bufferForReadData],

(* choose the NETOjbect if successful for return, otherwise -1 if fail *)
If[Negative[status],return1=status,return1=bufferForReadData],

(* PART 2: STOP: THE NETOJBECT BUFFER IS NOW POPULATED WITH FILE CONTENTS *)

(* PART 3: START: RETRIEVE THE NETOBJECT INTO MATHEMATICA AND DO ANY NEEDED CONVERSIONS *)(* example conversions: (1) from a single long list to a formatted list of appropriate dimensions; (2) convert byte lists such as strings or compound data types into meaningful Mathematica expressions  *)

(* perhaps just return the matrix if INTEGER, FLOAT, and so; if fixed string, then concatenate to a string; similarly, concatenate if a compound data type and there is a "ByteConversionFunction" as an option; might also just return the NETObject if "ReturnAsNETObject" is True; return -1 if entry is -1 (i.e., error code) *)
return2=formattedReturn[return1,memoryTypeID,systemElementsDimensions,opts],

(* PART 3: STOP: THE NETOBJECT CONTENTS ARE NOW FORMATTED AND READY IN MATHEMATICA FOR RETURN TO USER *)

(* memory management *)
H5Tclose[memoryTypeID],

return2
]
]
]
]



(* ::Input::Initialization:: *)
H5AExtendRead[attributeID:_Integer,memoryTypeID:_Integer,dimensions:_List,opts:OptionsPattern[]]:=

CompoundExpression[
Message[H5AExtendRead::format],
-1
]



(* ::Input::Initialization:: *)
Options[H5DExtendRead]=optionListForReadFunctions



(* ::Input::Initialization:: *)
H5DExtendRead[dataSetID:_Integer,fileSpaceID:_Integer,xferPListID:_Integer,opts:OptionsPattern[]]:=

NETBlock[
Module[{dataTypeID,memoryTypeID,dataElementsDimensions,systemType,systemCountOfOneDatum,selectionType,numberOfSelectedHyperSpaces,numberOfSelectedDatum,systemElementsDimensions,numberSystemElements,bufferForReadData,memorySpaceID,status,return1,return2},

Catch[

CompoundExpression[

(* PART 1: START: MAKE THE BUFFER AS NETOBJECT *)

(* get type of data in the dataset *)
dataTypeID=H5DgetType[dataSetID],
memoryTypeID=H5TgetNativeType[dataTypeID,H5TuDEFAULT],
H5Tclose[dataTypeID],

(* check for any error and, if error, close everything and exit with -1 *)
If[Negative[Min[memoryTypeID,dataTypeID]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1a: get the SystemType for the buffer *)

(* use the correct system type (e.g., "System.Int32[]" for the type of data *)
systemType=H5TExtendGetSystemType[memoryTypeID],
(* check for known system type and exit with -1 if needed *)
If[MatchQ[systemType,Missing[_]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1b: get the system count for one datum of the buffer *)

(* use the correct system count in one datum (e.g., 1 for "System.Int32[]" *)
systemCountOfOneDatum=H5TExtendGetSystemTypeCountInOneDatum[memoryTypeID],
(* on error exit with -1 if needed *)
If[MatchQ[systemCountOfOneDatum,Missing[_]],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

(* Part 1c: get the system dimensions for full buffer *)

(* "ALL", "HYPERSLABS", "POINTS" for fileSpaceID *)
selectionType=H5SBasicGetSelectType["String"][fileSpaceID],

(* number of selected points *)
numberOfSelectedDatum=H5SgetSelectNPoints[fileSpaceID],

(* -1 if not of "HYPERSLABS" type; otherwise, number of hyperslabs *)
numberOfSelectedHyperSpaces=H5SgetSelectHyperNBlocks[fileSpaceID],

(* dimensions of data set *)
Which[

(* start case 1: total transfer *)
fileSpaceID==H5SuALL,
With[
{dataSpaceID=H5DgetSpace[dataSetID]},
CompoundExpression[
dataElementsDimensions=H5SBasicGetSimpleExtentDims[dataSpaceID],
H5Sclose[dataSpaceID]
]
] (* end case 1 *),

(* case 2: one specific hyperslab in file *)
(selectionType=="HYPERSLABS")&&(numberOfSelectedHyperSpaces==1),
CompoundExpression[
(* raw return of {{0,0},{7,9}} transformed to {8,10} for dimensions *)
dataElementsDimensions=(-Subtract@@H5SBasicGetSelectBounds[fileSpaceID]) +1,
H5Sclose[dataSpaceID]
] (* end case 2 *),

(* case 3: multiple hyperslabs in file; list of rank1 returned instead of matrix *)
(selectionType=="HYPERSLABS")&&(numberOfSelectedHyperSpaces>1),
dataElementsDimensions={numberOfSelectedDatum},

(* case 4: elementary points from file *)
selectionType=="POINTS",
dataElementsDimensions={numberOfSelectedDatum},

(* case 5: not recognized *)
True,
dataElementsDimensions=-1 (* case not implemented in V2.00 *)

] (* close Which *),

(* check for any error and, if error, close everything and exit with -1 *)
If[Negative[dataElementsDimensions],Throw[CompoundExpression[H5Tclose[memoryTypeID],-1]]],

memorySpaceID=H5ScreateUsimple[Length@dataElementsDimensions,dataElementsDimensions,Null],

(* add in extra dimensions of datum is not atomic, e.g., for fixed string, compound data type, array, and so on*)
systemElementsDimensions=
If[(*is ARRAY class*)(Length@systemCountOfOneDatum>1)||(* or not ATOMIC class*)(systemCountOfOneDatum!=1),
Flatten[Append[dataElementsDimensions,systemCountOfOneDatum]] (* FLATTEN in case of ARRAY class *),
dataElementsDimensions
],

(* in case of scalar, still set to {1}*)
If[systemElementsDimensions=={},systemElementsDimensions={1}],

(* Part 1d: make the NETObject buffer *)

numberSystemElements=Times@@systemElementsDimensions,

bufferForReadData=NETNew[systemType,numberSystemElements],

(* PART 1: STOP: WE NOW HAVE THE NETOBJECT BUFFER WE NEED *)

(* PART 2: START: FILL THE NETOBJECT BUFFER WITH THE FILE CONTENTS *)

(* 'return': NETObject if successful, -1 if fail *)
status=H5DBasicRead[dataSetID,memoryTypeID,memorySpaceID,fileSpaceID,xferPListID,bufferForReadData],

H5Sclose[memorySpaceID],

(* choose the NETOjbect if successful for return, otherwise -1 if fail *)
If[Negative[status],return1=status,return1=bufferForReadData],

(* PART 2: STOP: THE NETOJBECT BUFFER IS NOW POPULATED WITH FILE CONTENTS *)

(* PART 3: START: RETRIEVE THE NETOBJECT INTO MATHEMATICA AND DO ANY NEEDED CONVERSIONS *)(* example conversions: (1) from a single long list to a formatted list of appropriate dimensions; (2) convert byte lists such as strings or compound data types into meaningful Mathematica expressions  *)

(* perhaps just return the matrix if INTEGER, FLOAT, and so; if fixed string, then concatenate to a string; similarly, concatenate if a compound data type and there is a "ByteConversionFunction" as an option; might also just return the NETObject if "ReturnAsNETObject" is True; return -1 if entry is -1 (i.e., error code) *)
return2=formattedReturn[return1,memoryTypeID,systemElementsDimensions,opts],

(* PART 3: STOP: THE NETOBJECT CONTENTS ARE NOW FORMATTED AND READY IN MATHEMATICA FOR RETURN TO USER *)

(* memory management *)
H5Tclose[memoryTypeID],

return2
]
]
]
]



(* ::Input::Initialization:: *)
H5DExtendRead[dataSetID:_Integer,memorySpaceID:_Integer,fileSpaceID:_Integer,xferPListID:_Integer,opts:OptionsPattern[]]:=

CompoundExpression[
Message[H5DExtendRead::memorySpaceError],
-1
]




(* ::Input::Initialization:: *)
H5DExtendRead[dataSetID:_Integer,memoryTypeID:_Integer,memorySpaceID:_Integer,fileSpaceID:_Integer,xferPListID:_Integer,dimensions:_List,opts:OptionsPattern[]]:=

CompoundExpression[
Message[H5DExtendRead::dimensionsError],
-1
]




(* ::Input::Initialization:: *)
H5DExtendWrite[dataSetID:_Integer,memoryTypeID:_Integer,memorySpaceID:_Integer,fileSpaceID:_Integer,xferPListID:_Integer,data:_List]:=

NETBlock[
(* use the correct system type (e.g., "System.Int32[]" for the type of data *)
With[{systemType=H5TExtendGetSystemType[memoryTypeID]},
With[{bufferToWrite=MakeNETObject[Flatten@data,systemType]},
H5DBasicWrite[dataSetID,memoryTypeID,memorySpaceID,fileSpaceID,xferPListID,bufferToWrite]
]
]
]


(* ::Input::Initialization:: *)
SetAttributes[H5DExtendOpen,HoldAll]


(* ::Input::Initialization:: *)
H5DExtendOpen[groupOrFileIDspecification:_List,body_]:=

Module[
{output},

With[
(* e.g., dataSetID = H5DOpen[....]*)
groupOrFileIDspecification,
With[
{
(* pickout 'dataSetID' from user input and insert it into the statement for H5Dclose *)
dataSetIDsymbol=Replace[Hold[groupOrFileIDspecification],Hold[List[Set[symbol:_,_]]]:> symbol]
},
(* execute the main body and then close the file *)
CompoundExpression[output=body,H5Dclose[dataSetIDsymbol],output]
]
]
]


(* ::Input::Initialization:: *)
SetAttributes[H5FExtendOpen,HoldAll]


(* ::Input::Initialization:: *)
H5FExtendOpen[fileIDspecification:_List,body:_]:=

Module[
{output},

With[
(* e.g., fileID = H5FOpen[....]*)
fileIDspecification,
With[
{
(* pickout 'fileID' from user input and insert it into the statement for H5Fclose *)
fileIDsymbol=Replace[Hold[fileIDspecification],Hold[List[Set[symbol:_,_]]]:> symbol]
},
(* execute the main body and then close the file *)
CompoundExpression[output=body,H5Fclose[fileIDsymbol],output]
]
]
]


(* ::Input::Initialization:: *)
SetAttributes[H5GExtendOpen,HoldAll]


(* ::Input::Initialization:: *)
H5GExtendOpen[groupOrFileIDspecification:_List,body:_]:=

Module[
{output},

With[
(* e.g., dataSetID = H5DOpen[....]*)
groupOrFileIDspecification,
With[
{
(* pickout 'dataSetID' from user input and insert it into the statement for H5Dclose *)
groupIDsymbol=Replace[Hold[groupOrFileIDspecification],Hold[List[Set[symbol:_,_]]]:> symbol]
},
(* execute the main body and then close the file *)
CompoundExpression[output=body,H5Gclose[groupIDsymbol],output]
]
]
]


(* ::Input::Initialization:: *)
H5GExtendGetNumObjs[GroupID:_Integer]:=

NETBlock[
Module[{groupInformationNETObject},
CompoundExpression[
H5GBasicGetInfo[GroupID,groupInformationNETObject],
NETObjectToExpression[groupInformationNETObject@nlinks]
]
]
]



(* ::Input::Initialization:: *)
H5GExtendGetObjNameByIndex[LocationID:_Integer,index:_Integer]:=

CompoundExpression[
Message[H5GExtendGetObjNameByIndex::deprecated],
H5LExtendGetNameByIdx[LocationID,index]
]


(* ::Input::Initialization:: *)
H5LExtendGetNameByIdx[locationID:_Integer,namePosition:_Integer]:=

NETBlock[
With[
{typeOfIndex=H5uINDEXuNAME,traverseOrder=H5uITERuINC,lapl=H5PuDEFAULT},
Module[
{return,size},
CompoundExpression[
size=H5LBasicGetNameByIdx["Size"][locationID,typeOfIndex,traverseOrder,namePosition,0,lapl],

If[
(* check for error code *)
Negative[size],size,
(* otherwise *)
CompoundExpression[
return=H5LBasicGetNameByIdx[locationID,typeOfIndex,traverseOrder,namePosition,size+1,lapl],
(*return*) If[Negative[First@return],First@return,Last@return]
]
](*close If*)

]
]
]
]


(* ::Input::Initialization:: *)
H5OExtendGetInfo[item:_String][objectID:_Integer]:=

NETBlock[
Module[{status,objectInformationNETObject},
CompoundExpression[
status=H5OBasicGetInfo[objectID,objectInformationNETObject],
If[Negative[status],status,H5OExtendGetInfo`Conversion[item,objectInformationNETObject]]
]
]
]


(* ::Input::Initialization:: *)
H5OExtendGetInfoByIndex[item:_String][locationID:_Integer,objectPosition:_Integer]:=

NETBlock[
Module[{status,objectInformationNETObject},
CompoundExpression[
(* Evaluate since H5OgetInfoByIdx has both HoldRest and type checking (e.g., _Integer) *)
status=H5OBasicGetInfoByIdx[Evaluate[locationID,H5uINDEXuNAME,H5uITERuINC,objectPosition,objectInformationNETObject,H5PuDEFAULT]],
If[Negative[status],status,H5OExtendGetInfo`Conversion[item,objectInformationNETObject]]
]
]
]


(* ::Input::Initialization:: *)
H5OExtendGetInfo`Conversion[item:_String,objectInformationNETObject:_Symbol]:=

Switch[
item,

"NumberAttributes",
NETObjectToExpression[objectInformationNETObject@numUattrs],

"TypeAsString",
NETObjectToExpression[objectInformationNETObject@type@ToString[]],

"TypeAsValue",
NETObjectToExpression[objectInformationNETObject@type@GetHashCode[]],

"ObjectAddressInFile",
NETObjectToExpression[objectInformationNETObject@addr]

]


(* ::Input::Initialization:: *)
H5TExtendGetSystemType[dataTypeID:_Integer]:=

Replace["SystemType",H5TExtendGetSystemTypeAndSize[dataTypeID]]


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeCountInOneDatum[dataTypeID:_Integer]:=

Replace["SystemTypeCountInOneDatum",H5TExtendGetSystemTypeAndSize[dataTypeID]]


(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize[dataTypeID:_Integer]:=

With[
{
classAsValue=H5TBasicGetClass["Value"][dataTypeID],
size=H5TBasicGetSize[dataTypeID],
sign=H5TBasicGetSign["Value"][dataTypeID],

(* variable length string gives a System.IntPtr *)
queryVariableLengthString=H5TisVariableStr[dataTypeID],

classAsString=H5TBasicGetClass["String"][dataTypeID],

missingReturn={"SystemType"->Missing["NotSupported"],"SystemTypeCountInOneDatum"->Missing["NotSupported"]}
},

Which[

!StringMatchQ[classAsString,"INTEGER"|"FLOAT"|"STRING"|"BITFIELD"|"ENUM"|"OPAQUE"|"COMPOUND"|"ARRAY"],
CompoundExpression[
Message[H5TExtendGetSystemTypeAndSize::badtype,classAsString],
missingReturn
],

(* one of the standard system types? *)
StringMatchQ[classAsString,"INTEGER"|"FLOAT"|"BITFIELD"|"ENUM"],
H5TExtendGetSystemTypeAndSize`SystemVariables[dataTypeID],

(* fixed length string, read as Bytes[] of a certain size *)
classAsString=="STRING"&&queryVariableLengthString==0,
{"SystemType"->"System.Byte[]","SystemTypeCountInOneDatum"->size},

(* variable length string, read as IntPtr of a certain size *)
classAsString=="STRING"&&queryVariableLengthString==1,
{"SystemType"->"System.IntPtr[]","SystemTypeCountInOneDatum"->1},

(* compound datatype, read as Bytes[] of a certain size *)
classAsString=="COMPOUND",
{"SystemType"->"System.Byte[]","SystemTypeCountInOneDatum"->size},

(* opaque datatype, read as Bytes[] of a certain size *)
classAsString=="OPAQUE",
{"SystemType"->"System.Byte[]","SystemTypeCountInOneDatum"->size},

(* array datatype, read as SystemType elements implemented for the basic classes of INTEGER, FLOAT, *)
classAsString=="ARRAY",
H5TExtendGetSystemTypeAndSize`Array[dataTypeID],

True,
missingReturn

](* close If[] *)
] 



(* ::Input::Initialization:: *)
With[
{
database=
<|
{0,1,0}->{"SystemType"->"System.Byte[]","SystemTypeCountInOneDatum"->1},
{0,2,0}->{"SystemType"->"System.UInt16[]","SystemTypeCountInOneDatum"->1},
{0,4,0}->{"SystemType"->"System.UInt32[]","SystemTypeCountInOneDatum"->1},
{0,8,0}->{"SystemType"->"System.UInt64[]","SystemTypeCountInOneDatum"->1},
{0,1,1}->{"SystemType"->"System.SByte[]","SystemTypeCountInOneDatum"->1},
{0,2,1}->{"SystemType"->"System.Int16[]","SystemTypeCountInOneDatum"->1},
{0,4,1}->{"SystemType"->"System.Int32[]","SystemTypeCountInOneDatum"->1},
{0,8,1}->{"SystemType"->"System.Int64[]","SystemTypeCountInOneDatum"->1},
{1,4,-1}->{"SystemType"->"System.Single[]","SystemTypeCountInOneDatum"->1} (* float *),
{1,8,-1}->{"SystemType"->"System.Double[]","SystemTypeCountInOneDatum"->1} (* float *),
{4,1,-1}->{"SystemType"->"System.Byte[]" ,"SystemTypeCountInOneDatum"->1}(* bit field *),
{4,2,-1}->{"SystemType"->"System.UInt16[]","SystemTypeCountInOneDatum"->1} (* bit field *),
{4,4,-1}->{"SystemType"->"System.UInt32[]" ,"SystemTypeCountInOneDatum"->1}(* bit field *),
{4,8,-1}->{"SystemType"->"System.UInt64[]" ,"SystemTypeCountInOneDatum"->1}(* bit field *),
{8,1,1}->{"SystemType"->"System.SByte[]","SystemTypeCountInOneDatum"->1} (* enum *),
{8,2,1}->{"SystemType"->"System.Int16[]","SystemTypeCountInOneDatum"->1} (* enum *),
{8,4,1}->{"SystemType"->"System.Int32[]","SystemTypeCountInOneDatum"->1} (* enum *),
{8,8,1}->{"SystemType"->"System.Int64[]","SystemTypeCountInOneDatum"->1} (* enum *)
|>
},

H5TExtendGetSystemTypeAndSize`SystemVariables[dataTypeID:_Integer]:=

With[
{
classAsValue=H5TBasicGetClass["Value"][dataTypeID],
size=H5TBasicGetSize[dataTypeID],
sign=H5TBasicGetSign["Value"][dataTypeID],
classAsString=H5TBasicGetClass["String"][dataTypeID],
missingReturn={"SystemType"->Missing["NotSupported"],"SystemTypeCountInOneDatum"->Missing["NotSupported"]}
},

Which[

!StringMatchQ[classAsString,"INTEGER"|"FLOAT"|"BITFIELD"|"ENUM"],

CompoundExpression[
Message[H5TExtendGetSystemTypeAndSize`SystemVariables::badtype,classAsString],
missingReturn
],

True,
Lookup[
(* {class [e.g., 0 = integer], size [e.g., 4 bytes], sign [1 = signed], *)
database,
{{classAsValue,size,sign}},
CompoundExpression[
Message[H5TExtendGetSystemTypeAndSize`SystemVariables::missing,classAsString,size,sign],
missingReturn
]
]//First

]
]
] 



(* ::Input::Initialization:: *)
H5TExtendGetSystemTypeAndSize`Array[dataTypeID:_Integer]:=

Module[{returnType,returnSize,arraySize},

With[
{
arrayDataTypeID=H5TgetSuper[dataTypeID],

missingReturn={"SystemType"->Missing["NotSupported"],"SystemTypeCountInOneDatum"->Missing["NotSupported"]}
},

With[
{
arrayClassAsString=H5TBasicGetClass["String"][arrayDataTypeID]
},

Which[

!StringMatchQ[arrayClassAsString,"INTEGER"|"FLOAT"|"BITFIELD"|"ENUM"],

CompoundExpression[
Message[H5TExtendGetSystemTypeAndSize`Array::badtype,arrayClassAsString],
H5Tclose[arrayDataTypeID],
missingReturn
],

True,
CompoundExpression[
returnType=H5TExtendGetSystemType[arrayDataTypeID],
returnSize=H5TExtendGetSystemTypeCountInOneDatum[arrayDataTypeID],
arraySize=H5TBasicGetArrayDims[dataTypeID],
H5Tclose[arrayDataTypeID],
{"SystemType"->returnType,"SystemTypeCountInOneDatum"->Times[returnSize,arraySize]}
]

]
] 
]
]


(* ::Input::Initialization:: *)
H5TExtendGetAllMemberNames[datatypeID:_Integer]:=

NETBlock[
Module[
{numMembers},
CompoundExpression[
numMembers=H5TgetNMembers[datatypeID],

If[
(* check for error code *)
Negative[numMembers],numMembers,
(* otherwise *)
H5TBasicGetMemberName[datatypeID,#]&/@Range[0,numMembers-1]
](*close If*)

]
]
]


(* ::Input::Initialization:: *)
End[]


(* ::Input::Initialization:: *)
SetAttributes[
{H5ExtendGetUlibVersion,H5AExtendGetNumberAttributes,H5AExtendGetName,H5AExtendRead,H5DExtendRead,H5DExtendWrite,H5DExtendOpen,H5FExtendOpen,H5GExtendOpen,H5GExtendGetNumObjs,H5GExtendGetObjNameByIndex,H5LExtendGetNameByIdx,H5OExtendGetInfo,H5OExtendGetInfoByIndex,H5TExtendGetSystemType,H5TExtendGetSystemTypeCountInOneDatum,H5TExtendGetSystemTypeAndSize,H5TExtendGetAllMemberNames},
ReadProtected
]


(* ::Input::Initialization:: *)
EndPackage[]
