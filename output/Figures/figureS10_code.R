rm(list = ls())
library(reshape2)
library(ggplot2)
load("./output/realdata_results/AML_results.RData")
load("./data/realdata_aml/preprocessed_aml_data.RData")
sam_mest <- amlResult$mEst
rownames(sam_mest) <- unlist(lapply(strsplit(rownames(oral_scale),split = ";"), function(x){x[6]}))
colnames(sam_mest) <- unlist(lapply(strsplit(rownames(stool_scale),split = ";"), function(x){x[6]}))
############
#permute rows and columns to make the heatmap look nicer
sam_save = sam_mest
############
sam_mest=sam_save
temp = sam_mest[15:19,]
sam_mest[15:19,] = sam_mest[20:24,]
sam_mest[20:24,] = temp
name15 = rownames(sam_mest)[15:19]
name20 = rownames(sam_mest)[20:24]
rownames(sam_mest)[15:19] = name20
rownames(sam_mest)[20:24] = name15

temp = sam_mest[,3:10]
sam_mest[,3:10] = sam_mest[,15:22]
sam_mest[,15:22] = temp
name15 = colnames(sam_mest)[3:10]
name20 = colnames(sam_mest)[15:22]
colnames(sam_mest)[3:10] = name20
colnames(sam_mest)[15:22] = name15
###################
#################
sam_melt = melt(sam_mest)
# print to pdf
pdf("./output/Figures/AML.pdf",width=7,height=6.8)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
dev.off()
