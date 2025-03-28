rm(list = ls())
library(reshape2)
library(ggplot2)

load("./output/realdata_results/MOMS_all_community_microbe_results.RData")
load("./output/realdata_results/MOMS_all_community_results.RData")
load("./data/realdata_moms_pi/preprocessed_moms_pi_allcommunity_data.RData")
sam_mest3 = apply(microResult[[2]]$mt1, c(2,3), mean)
rownames(sam_mest3) = rownames(scalev)
colnames(sam_mest3) = rownames(scaleu)


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
##################
rowindexnei = which(rownames(sam_mest3)=='Bifidobacterium')
rowindexsne =  which(rownames(sam_mest3)=='Dialister')
temp = sam_mest3[rowindexnei,]
sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
sam_mest3[rowindexsne,] = temp
rownames(sam_mest3)[rowindexnei] = 'Dialister'
rownames(sam_mest3)[rowindexsne] = 'Bifidobacterium'

#######
rowindexnei = which(rownames(sam_mest3)=='Sneathia')
rowindexsne =  which(rownames(sam_mest3)=='Neisseria')
temp = sam_mest3[rowindexnei,]
sam_mest3[rowindexnei,] = sam_mest3[rowindexsne,]
sam_mest3[rowindexsne,] = temp
rownames(sam_mest3)[rowindexnei] = 'Neisseria'
rownames(sam_mest3)[rowindexsne] = 'Sneathia'
##########do some permutation########
colindexnei = which(colnames(sam_mest3)=='Sneathia')
colindexsne = which(colnames(sam_mest3)=='Neisseria')
temp = sam_mest3[,colindexnei]
sam_mest3[,colindexnei] = sam_mest3[,colindexsne]
sam_mest3[,colindexsne] = temp
colnames(sam_mest3)[colindexnei] = 'Neisseria'
colnames(sam_mest3)[colindexsne] = 'Sneathia'



############3
##########do some permutation########

##################

sam_melt = melt(sam_mest3)
# ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
#   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
#   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
# ###########
###################
# print to pdf
pdf("./output/Figures/MOMS_all_uv.pdf",width=6.6,height=6)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
dev.off()

#############33

sam_mest3 = apply(microResult[[3]]$mt1, c(2,3), mean)
rownames(sam_mest3) = rownames(scaler)
rownames(sam_mest3)[1:3] = c("Eubacterium","Prevotella","Ruminococcus")
colnames(sam_mest3) = rownames(scaleu)






############3
##########do some permutation########

##################

sam_melt = melt(sam_mest3)
# ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
#   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
#   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
# ###########
###################
# print to pdf
pdf("./output/Figures/MOMS_all_ru.pdf",width=5.4,height=6)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 7),axis.text.y = element_text(size = 6)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
dev.off()


sam_mest3 = apply(microResult[[3]]$mt2, c(2,3), mean)
rownames(sam_mest3) = rownames(scaler)
rownames(sam_mest3)[1:3] = c("Eubacterium","Prevotella","Ruminococcus")
colnames(sam_mest3) = rownames(scalev)


############3
##########do some permutation########

##################

sam_melt = melt(sam_mest3)
# ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
#   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
#   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
# ###########
###################
# print to pdf
pdf("./output/Figures/MOMS_all_rv.pdf",width=5.4,height=6)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 7),axis.text.y = element_text(size = 6)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
dev.off()
