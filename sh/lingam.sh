#!/bin/bash
mkdir -p log
mkdir -p output/simulation_results
noisetype=LiNGAM
px=30
py=40
n=150
# signal=5
nc=32
nr=30
method=compDAG
# rparam=0.06
pgammaa=1
pgammab=1
pdir=0.025
param_verbose=0
hyper_verbose=0
prop_verbose=0

signal = 0.5
rparam = 0.5
for method in compDAG PC
do
  nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
        wait
done
signal = 1
rparam = 0.3
for method in compDAG PC
do
  nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
        wait
done

signal = 5
rparam = 0.05
for method in compDAG PC
do
  nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
        wait
done

