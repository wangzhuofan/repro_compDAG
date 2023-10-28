load("AML_results.RData")
sam_mest <- m_est
rownames(sam_mest) <- unlist(lapply(strsplit(on,split = ";"), function(x){x[6]}))
colnames(sam_mest) <- unlist(lapply(strsplit(sn,split = ";"), function(x){x[6]}))
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
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
