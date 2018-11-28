#!/bin/bash
################################################################
#                                                              #
# Author      : Prajwal Mohan Murthy (prajwal@mohanmurthy.com) #
# Description : Creates ASCII faster files for all the runs    #
#               in 'runnum' on all available cores.            #
# Warning     : Occupies all processors keep computer idle.    #
#                                                              #
################################################################
runnum=( 011433 011605 011616 011625 011645 )
for i in "${runnum[@]}"
do
	./mFaster_ASCII_cmd_text.m $i
done
