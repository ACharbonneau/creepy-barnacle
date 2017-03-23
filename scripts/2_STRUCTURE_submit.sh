#! /bin/bash

#Launches 20 different randomizations of the dataset (seq 1 20) with a K of ( -t <number range>)

#Starts in STRUCTURE directory

for i in `seq 1 20`
    do less ../scripts/2.1_STRUCTURE_submit.qsub | sed "s/nacho/..\/MungedData\/${i}rand.txt/" > ../scripts/withtemp.qsub

    qsub ../scripts/withtemp.qsub -N ${i}_STRUCTURE -t 1-24

done
