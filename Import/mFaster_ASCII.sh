#!/bin/bash
################################################################
#                                                              #
# Author      : Prajwal Mohan Murthy (prajwal@mohanmurthy.com) #
# Description : Creates ASCII faster files for all the runs    #
#               in 'runnum' on all available cores.            #
# Warning     : Occupies all processors keep computer idle.    #
#                                                              #
################################################################
runnum=( 11159 11166 11208 11239 11277 11305 11357 11363 11409 11412 11445 11448 )
for i in "${runnum[@]}"
do
	./mFaster_ASCII_cmd_text.m $i
done
