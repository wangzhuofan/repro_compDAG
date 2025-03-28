rm(list = ls())
library(reshape2)
library(ggplot2)
load("./output/realdata_results/AML_results.RData")
load("./data/realdata_aml/preprocessed_aml_data.RData")
sam_mest <- amlResult$mEst
rownames(sam_mest) <- unlist(lapply(strsplit(rownames(oral_scale),split = ";"), function(x){x[6]}))
colnames(sam_mest) <- unlist(lapply(strsplit(rownames(stool_scale),split = ";"), function(x){x[6]}))
t = tail(sort(sam_mest),10)
cn = matrix(0,nrow = 10,ncol = 2)
for (i in 1:10) {
  cn[i,1] = rownames(sam_mest)[which(sam_mest==t[11-i],arr.ind = T)[1]]
  cn[i,2] = colnames(sam_mest)[which(sam_mest==t[11-i],arr.ind = T)[2]]
}
colnames(cn) <- c("oral","stool")
outputTable <- function(cn){
  cat("\\toprule\n")
  cat("& \\textbf{Stool} & \\textbf{Oral} \\\\ \n")
  for (i in 1:nrow(cn)) {
    cat(i," &  ",cn[i,2]," & ", cn[i,1],"\\\\ \n")
  }
  cat("\\bottomrule\n")
}
outputTable(cn)
