#!/bin/bash
runnum=( 10094 )
for i in "${runnum[@]}"
do
	./mFaster_ASCII_cmd_text.m $i
done
