# noise.type = "Dirichlet"
# simu_px = 30
# simu_py = 40
# simu_n = 100
# signal = 5
# nCluster = 8
# nRepli = 8
# method = "compDAG"
# 
args <- commandArgs(trailingOnly = TRUE)
noise.type = (args[1])
simu_px = as.numeric(args[2])
simu_py = as.numeric(args[3])
simu_n = as.numeric(args[4])
signal = as.numeric(args[5])
nCluster = as.numeric(args[6])
nRepli = as.numeric(args[7])
method = args[8]
