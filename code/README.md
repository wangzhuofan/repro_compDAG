This directory contains R files to conduct Bayesian posterior sampling.

* 02-1coalescence.stan: stan file to conduct posterior sampling for causal direction $y_1 \rightarrow y_2$ ot $y_2 \rightarrow y_1$.
* 02-2nocausal.stan: stan file to conduct posterior sampling if there is no causal relationship between $y_1$ and $y_2$.
* 02compDAG.R :R file to conduct compDAG method.
* 00load_all.E : R file to load all the required packages and functions.
* 03bQCD.R: R file to conduct bQCD method.
* main.R: R file to conduct simulation studies in the paper.
* PARAMS.R R file to read parameter settings from bash file.
