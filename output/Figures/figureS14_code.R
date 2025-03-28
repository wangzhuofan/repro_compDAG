rm(list = ls())
load("./output/realdata_results/MOMS_sens.RData")
load("./data/realdata_moms_pi/preprocessed_moms_pi_data.RData")
library(reshape2)
library(ggplot2)
library(patchwork)
library(ggpubr)
hypers = c("(a,b,r)=(0.1,1,0.025)","(a,b,r)=(10,1,0.025)","(a,b,r)=(1,0.1,0.025)","(a,b,r)=(1,10,0.025)","(a,b,r)=(1,1,0.01)","(a,b,r)=(1,1,0.1)")
simu_plot <- function(){

  for (i in 1:6) {
    result <- get(paste0("momsResult",i))
    m <- result$mEst
    sam_mest3 = m
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
    m_melt = melt(sam_mest3)
    assign(paste0("p",i),
           ggplot2::ggplot(get("m_melt"), aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
             scale_fill_gradient(low = "black", high = "green")+
             theme(panel.grid = element_blank(), axis.text.y = element_text(size=5.5),axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 7),plot.title = element_text(hjust = 0.5,size=20)) +
             scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
             # expand_limits(fill = c(-0.8, 0.8)) +
             ggtitle(hypers[i]))
  }
  gp <- ggpubr::ggarrange(plotlist = list(p1,p2,p3,p4,p5,p6),ncol = 3,nrow = 2,common.legend = TRUE,legend = "right",font.label = list(size=20))

  filename = paste0("./output/Figures/MOMS_sens.pdf")
  ggsave(gp, file = filename,
         width = 14, height = 10, device='pdf')
}

simu_plot()
