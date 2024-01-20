rm(list = ls())
library(reshape2)
library(ggplot2)
load("./output/realdata_results/MOMS_results.RData")
load("./data/realdata_moms_pi/preprocessed_moms_pi_data.RData")
sam_mest3 <- momsResult$mEst
cn = rownames(cervix_of_uterus_scale)
cn[1] = 'Prevotella'
colnames(sam_mest3) <- cn
rownames(sam_mest3) <- rownames(vagina_scale)
sam_mest2 = sam_mest3
t = tail(sort(sam_mest2),20)
cn = matrix(0,nrow = 20,ncol = 2)
for (i in 1:20) {
  cn[i,1] = rownames(sam_mest2)[which(sam_mest2==t[21-i],arr.ind = T)[1]]
  cn[i,2] = colnames(sam_mest2)[which(sam_mest2==t[21-i],arr.ind = T)][2]
}
colnames(cn) <- c("vagina","uterus")
cnn=cn
cnn[,1] = cn[,2]
cnn[,2]=cn[,1]
colnames(cnn) <- c("uterus","vagina")
outputTable <- function(cn){
  cat("\\toprule\n")
  cat("& \\textbf{Cervix} & \\textbf{Vagina} \\\\ \n")
  for (i in 1:nrow(cn)) {
    cat(i," &  ",cnn[i,1]," & ", cnn[i,2],"\\\\ \n")
  }
  cat("\\bottomrule\n")
}
outputTable(cn)