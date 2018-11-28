#!/bin/bash
################################################################
#                                                              #
# Author      : Prajwal Mohan Murthy (prajwal@mohanmurthy.com) #
# Description : Creates ASCII faster files for all the runs    #
#               in 'runnum' on all available cores.            #
# Warning     : Occupies all processors keep computer idle.    #
#                                                              #
################################################################
runnum=( 012666 012667 012668 012669 )
for i in "${runnum[@]}"
do
	./mFaster_ASCII_cmd_text_NStar.m $i
done
