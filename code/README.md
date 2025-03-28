This directory contains R files and stan files to conduct Bayesian posterior sampling.

* 02-5-(0-4)compDAG.stan: stan file to conduct posterior sampling for the parameters of $y_b$ with (0-4) parents.
* 02-2nocausal.stan: stan file to conduct posterior sampling if there is no causal relationship between $y_1$ and $y_2$.
* 02-1coalescence-hyper.stan: stanfile to conduct posterior sampling for different choices of hyperparameters.
* 02compDAG.R :R file to conduct compDAG method for two communities.
* 02compDAG_param.R: R file to conduct compDAG method and save the chain.
* 02-3multiCompDAG.R: R file to conduct compDAG method and estimate the community-level causal relationships for three communities.
* 02-5compDAG.R: R file to conduct compDAG method and estimate the community-level causal relationships for five communities.
* 02-3microbe.R: R file to conduct compDAG method and estimate the microbe-level causal relationships for three communities.
* 02-5microbe.R: R file to conduct compDAG method and estimate the microbe-level causal relationships for five communities.
* 03bQCD.R: R file to conduct bQCD method.
* 00load_all.R : R file to load all the required packages and functions.
* 03bQCD.R: R file to conduct bQCD method.
* main.R: R file to conduct simulation studies in the paper.
* PARAMS.R R file to read parameter settings from bash file.
