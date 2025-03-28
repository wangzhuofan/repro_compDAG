rm(list = ls())
library(reshape2)
library(ggplot2)
load("./output/realdata_results/MOMS_all_results.RData")
load("./data/realdata_moms_pi/preprocessed_moms_pi_all_data.RData")
sam_mest3 <- apply(paramsxy$mt, c(2,3), mean)
cn = rownames(cervix_of_uterus_scale)
cn[1] = 'Eubacterium'
cn[2] = 'Prevotella'
cn[3] = 'Ruminococcus'

colnames(sam_mest3) <- cn
rn = rownames(vagina_scale)
rn[1] = 'Eubacterium'
rn[2] = 'Prevotella'
rn[3] = 'Ruminococcus'

rownames(sam_mest3) <- rn

load("./data/realdata_moms_pi/preprocessed_moms_pi_data.RData")

cn_sub = rownames(cervix_of_uterus_scale)
rn_sub = rownames(vagina_scale)
cn_sub[1] = 'Prevotella'
sam_mest_sub = sam_mest3[rn_sub,cn_sub]

sam_mest_save = sam_mest3



# axis_colors_x <- setNames(ifelse(cn %in% cn_sub, "black", "blue"), cn)
# axis_colors_y <- setNames(ifelse(rn %in% rn_sub, "black", "blue"), rn)
per <- function(sam_mest3){
  cn = rownames(cervix_of_uterus_scale)
  cn[1] = 'Prevotella'
  colnames(sam_mest3) <- cn
  rownames(sam_mest3) <- rownames(vagina_scale)
  sam_mest_save = sam_mest3
  ######do some permutation to make the heatmap look nicer#####
  temp = sam_mest3[,17]
  sam_mest3 = sam_mest3[,-17]
  sam_mest3 = cbind(sam_mest3,temp)
  colnames(sam_mest3)[33] = 'Leptotrichia'
  temp = sam_mest3[,30]
  sam_mest3[,30] = sam_mest3[,19]
  sam_mest3[,19] = temp
  colnames(sam_mest3)[30] = 'Neisseria'
  colnames(sam_mest3)[19] = 'Sneathia'
  rowindexnei = which(rownames(sam_mest3)=='Moryella')
  rowindexsne =  which(rownames(sam_mest3)=='Sneathia')
  temp = sam_mest3[rowindexnei,]
  sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
  sam_mest3[rowindexsne,] = temp
  rownames(sam_mest3)[rowindexnei] = 'Sneathia'
  rownames(sam_mest3)[rowindexsne] = 'Moryella'
  ##########do some permutation########
  colindexnei = which(colnames(sam_mest3)=='Dialister')
  colindexsne = which(colnames(sam_mest3)=='Fusobacterium')
  temp = sam_mest3[,colindexnei]
  sam_mest3[,colindexnei] = sam_mest3[,colindexsne]
  sam_mest3[,colindexsne] = temp
  colnames(sam_mest3)[colindexnei] = 'Fusobacterium'
  colnames(sam_mest3)[colindexsne] = 'Dialister'
  rowindexnei = which(rownames(sam_mest3)=='Dialister')
  rowindexsne =  which(rownames(sam_mest3)=='Fusobacterium')
  temp = sam_mest3[rowindexnei,]
  sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
  sam_mest3[rowindexsne,] = temp
  rownames(sam_mest3)[rowindexnei] = 'Fusobacterium'
  rownames(sam_mest3)[rowindexsne] = 'Dialister'


  ############3
  ##########do some permutation########
  colindexnei = which(colnames(sam_mest3)=='Bifidobacterium')
  colindexsne = which(colnames(sam_mest3)=='Faecalibacterium')
  temp = sam_mest3[,colindexnei]
  sam_mest3[,colindexnei] = sam_mest3[,colindexsne]
  sam_mest3[,colindexsne] = temp
  colnames(sam_mest3)[colindexnei] = 'Faecalibacterium'
  colnames(sam_mest3)[colindexsne] = 'Bifidobacterium'
  rowindexnei = which(rownames(sam_mest3)=='Bifidobacterium')
  rowindexsne =  which(rownames(sam_mest3)=='Faecalibacterium')
  temp = sam_mest3[rowindexnei,]
  sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
  sam_mest3[rowindexsne,] = temp
  rownames(sam_mest3)[rowindexnei] = 'Faecalibacterium'
  rownames(sam_mest3)[rowindexsne] = 'Bifidobacterium'


  ############3
  ##########do some permutation########
  colindexnei = which(colnames(sam_mest3)=='Bifidobacterium')
  colindexsne = which(colnames(sam_mest3)=='Fusobacterium')
  temp = sam_mest3[,colindexnei]
  sam_mest3[,colindexnei] = sam_mest3[,colindexsne]
  sam_mest3[,colindexsne] = temp
  colnames(sam_mest3)[colindexnei] = 'Fusobacterium'
  colnames(sam_mest3)[colindexsne] = 'Bifidobacterium'
  rowindexnei = which(rownames(sam_mest3)=='Bifidobacterium')
  rowindexsne =  which(rownames(sam_mest3)=='Fusobacterium')
  temp = sam_mest3[rowindexnei,]
  sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
  sam_mest3[rowindexsne,] = temp
  rownames(sam_mest3)[rowindexnei] = 'Fusobacterium'
  rownames(sam_mest3)[rowindexsne] = 'Bifidobacterium'
  return(sam_mest3)
}


############3
# sam_mest3 = sam_mest_sub
sam_mest3 = per(sam_mest_sub)
sam_melt = melt(sam_mest3)
# ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
#   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
#   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
###########
###################
# print to pdf
pdf("./output/Figures/MOMS_sub.pdf",width=6.6,height=6)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
dev.off()
