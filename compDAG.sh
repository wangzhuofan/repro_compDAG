#!/bin/bash
mkdir -p log
mkdir -p output/simulation_results
# noisetype=Dirichlet
px=30
py=40
# n=100
# signal=1
# nc=8
# nr=8
# method=compDAG

for n in 100 150
do
    for method in compDAG COLP bQCD PC 
    do
        for noisetype in Dirichlet Additive
        do 
            for signal in 0.3 0.5 1 3 5
            do 
                nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
                wait
            done 
        done
    done
done  
