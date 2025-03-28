#!/bin/bash
mkdir -p log
mkdir -p output/simulation_results
noisetype=Dirichlet
px=30
py=40
n=150
# signal=5
nc=32
nr=30
method=compDAG
rparam=0.09
pgammaa=0.1
pgammab=1
pdir=0.025
param_verbose=0
hyper_verbose=1
prop_verbose=0

nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &

pgammaa=10
pgammab=1
pdir=0.025
nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &

pgammaa=1
pgammab=0.1
pdir=0.025
nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &

pgammaa=1
pgammab=10
pdir=0.025
nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &

pgammaa=1
pgammab=1
pdir=0.01
nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &

pgammaa=1
pgammab=1
pdir=0.1
nohup Rscript ./code/main.R $noisetype $px $py $n $signal $nc $nr $method $rparam $pgammaa $pgammab $pdir $prop $param_verbose $hyper_verbose $prop_verbose > ./log/simu-$noisetype-$signal-$n-$method.log 2>&1 </dev/null &
