#!/usr/local/bin/wolframscript
origin = FileNameJoin[{Directory[], "EDA"}];
copyto = FileNameJoin[{$BaseDirectory, "Applications", "EDA"}];
CopyDirectory[origin, copyto];
origin = FileNameJoin[{Directory[], "CustomTicks"}];
copyto = FileNameJoin[{$BaseDirectory, "Applications", "CustomTicks"}];
CopyDirectory[origin, copyto];
origin = FileNameJoin[{Directory[], "h5mma"}];
copyto = FileNameJoin[{$BaseDirectory, "Applications", "h5mma"}];
CopyDirectory[origin, copyto];
(*EOF*)
