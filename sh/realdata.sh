#!/bin/bash
mkdir -p output/log
mkdir -p output/realdata_results

nohup Rscript ./output/realdata_moms_pi_code.R > ./output/log/realdata_moms_pi.log 2>&1 </dev/null &
nohup Rscript ./output/realdata_aml_code.R > ./output/log/realdata_aml.log 2>&1 </dev/null &
nohup Rscript ./output/moms_all.R > ./output/log/moms_all.log 2>&1 </dev/null &
nohup Rscript ./output/moms_params.R > ./output/log/moms_params.log 2>&1 </dev/null &
nohup Rscript ./output/moms_sens.R > ./output/log/moms_sens.log 2>&1 </dev/null &
nohup Rscript ./output/moms_pi_allcommunity.R > ./output/log/moms_allcommunity.log 2>&1 </dev/null &
nohup Rscript ./output/moms_pi_microbe.R > ./output/log/moms_microbe.log 2>&1 </dev/null &
