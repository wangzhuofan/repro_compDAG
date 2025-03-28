#!/bin/bash
mkdir -p log
mkdir -p output/simulation_results
noisetype=Cyclic
px=30
py=40
# n=150
# signal=5
nc=32
nr=30
# method=compDAG
rparam=0.09
pgammaa=1
pgammab=1
pdir=0.025
param_verbose=0
hyper_verbose=0
prop_verbose=0
for n in 150
do
    for method in compDAG PC
    do
        for noisetype in Cyclic
        do
            for signal in 2 3 5
            do
                nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
                wait
            done
        done
    done
done
