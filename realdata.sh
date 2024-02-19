#!/bin/bash
mkdir -p output/log
mkdir -p output/realdata_results

nohup Rscript ./output/realdata_moms_pi_code.R > ./output/log/realdata_moms_pi.log 2>&1 </dev/null &
nohup Rscript ./output/realdata_aml_code.R > ./output/log/realdata_aml.log 2>&1 </dev/null &