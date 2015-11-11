#! /bin/bash

#Launches 20 different randomizations of the dataset (seq 1 20) with a K of ( -t <number range>)		

for i in `seq 1 20`
    do less nacho.qsub | sed s/nacho/${i}rand.txt/ > withtemp.qsub
    
    qsub withtemp.qsub -N ${i}_NZIL_alpha -t 1-24

done
