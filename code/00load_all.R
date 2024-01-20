library(loo)
library(gtools)
library(doParallel)
library(foreach)
library(iterators)
library(rstan)
source("./data/01generate_simulated_data.R")
source("./data/01generate_misspecified_data.R")
source("./code/02compDAG.R")
source("./code/03bQCD.R")


