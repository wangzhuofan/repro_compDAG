ci_plot <- function(allN,allnoise){
  allMethod = c("compDAG")#
  allSignal = c(0.3,0.5,1,3,5)
  # allnoise = c("Dirichlet")
  count = 1
  for (noisetype in allnoise) {
    for (signal in allSignal) {
      for (simu_n in allN) {
        fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                          "-signal-",signal,
                          "-n-", simu_n,
                          "-method-","compDAG",
                          ".RData")
        load(fileList)
        paramsxy = list()
        paramsxy$c = unlist(lapply(result, function(x){x$c}))
        paramsxy$m = array(unlist(lapply(result, function(x){aperm(x$mt,c(2,3,1))})),dim=c(40,30,length(paramsxy$c)))

        library(ggplot2)

         # 模拟后验分布
         posterior <- paramsxy$c
         ci <- quantile(posterior, probs = c(0.025, 0.975))
         meanci <- mean(posterior)
         # 绘图
         gc <- ggplot(data.frame(posterior), aes(x = posterior)) +
           geom_density(fill = "skyblue", alpha = 0.5) +
           geom_vline(xintercept = ci, linetype = "dashed", color = "red") +
           geom_vline(xintercept = meanci, color = "red")+
           labs(title = bquote(pi[21] == .(signal)),
                x = expression(pi[21]),
                y = "Density") +
           theme_classic()+
           theme(panel.grid = element_blank(),plot.title = element_text(hjust = 0.5))  # 去掉网格线
         assign(paste0("gc",count),gc)
         # count = count+1
        # filename = paste0("./output/ci_c_",noisetype,"_",simu_n,"_",signal,".pdf")
        # ggsave(gc, file = filename,
          #      width = 4, height = 4, device='pdf')
        # 将后验样本的均值和区间提取为数据框
        mean_values <- apply(paramsxy$m, c(1, 2), mean)
        #
        # 计算每个位置的 2.5% 和 97.5% 分位数
        credible_intervals <- apply(paramsxy$m, c(1, 2), function(x) {
        quantile(x, probs = c(0.025, 0.975))
        })

        lower_bounds <- credible_intervals[1,,]
        upper_bounds <- credible_intervals[2,,]
        library(reshape2)
        mean_melt = melt(mean_values)
        lower_melt = melt(lower_bounds)
        upper_melt = melt(upper_bounds)
        #
        p1 <- ggplot2::ggplot(mean_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
        scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
        scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
        ggtitle(bquote(pi[21] == .(signal)))

        p2 <- ggplot2::ggplot(lower_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
        scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
        scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
        ggtitle("")

        p3 <- ggplot2::ggplot(upper_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
        scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
        scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
        ggtitle("")

        assign(paste0("pm",count),p1)
        assign(paste0("pl",count),p2)
        assign(paste0("pu",count),p3)
        count = count+1
        # gp <- ggpubr::ggarrange(plotlist = list(p1,p2,p3),ncol = 3,nrow = 1,common.legend = TRUE,legend = "right")
        # filename = paste0("./output/ci_",noisetype,"_",simu_n,"_",signal,".pdf")
        # ggsave(gp, file = filename,
        #        width = 10, height = 4, device='pdf')
      }
    }

  }
  plotlist = list()

    for (i in 1:5) {
	        plotlist[[i]] = get(paste0("gc",i))
    }
    gc = ggpubr::ggarrange(plotlist = plotlist,ncol = 5,nrow = 1)
      filename = paste0("./output/Figures/ci_c_",noisetype,"_",simu_n,".pdf")
      ggsave(gc, file = filename,
	              width = 20, height = 4, device='pdf')
      plotlist = list()
  for (i in 1:5) {
  plotlist[[i]] = get(paste0("pm",i))
  plotlist[[i+5]] = get(paste0("pl",i))
  plotlist[[i+10]] = get(paste0("pu",i))
  }
  plot_labels <- c("pi[21] == 0.3",
                   "pi[21] == 0.5",
                   "pi[21] == 1",
                   "pi[21] == 3",
                   "pi[21] == 5",
                   "", "", "", "", "",
                   "", "", "", "", "")
  gm <- ggpubr::ggarrange(plotlist = plotlist,ncol = 5,nrow = 3,common.legend = TRUE,legend = "right")
  filename = paste0("./output/Figures/ci_m_",noisetype,"_",simu_n,".pdf")
  ggsave(gm, file = filename,
        width = 20, height = 12, device='pdf')
}
ci_plot(c(150),c("Dirichlet"))
ci_plot(c(100),c("Dirichlet"))
ci_plot(c(150),c("Additive"))
ci_plot(c(100),c("Additive"))

#####################Real data#####################
load("./output/realdata_results/MOMS_params.RData")
load("./data/realdata_moms_pi/preprocessed_moms_pi_data.RData")
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


###########################################

# 模拟后验分布
posterior <- paramsxy$c
ci <- quantile(posterior, probs = c(0.025, 0.975))
meanci <- mean(posterior)
# 绘图
gc <- ggplot(data.frame(posterior), aes(x = posterior)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = ci, linetype = "dashed", color = "red") +
  geom_vline(xintercept = meanci, color = "red")+
  labs(
    x = expression(pi[cervix%->%vagina]),
    y = "Density") +
  theme_classic()+
  theme(panel.grid = element_blank(),plot.title = element_text(hjust = 0.5))  # 去掉网格线
filename = paste0("./output/Figures/ci_c_moms",".pdf")
ggsave(gc, file = filename,
       width = 4, height = 4, device='pdf')
# 将后验样本的均值和区间提取为数据框
mean_values <- apply(paramsxy$mt, c(2, 3), mean)
#
# 计算每个位置的 2.5% 和 97.5% 分位数
credible_intervals <- apply(paramsxy$mt, c(2, 3), function(x) {
  quantile(x, probs = c(0.025, 0.975))
})

lower_bounds <- credible_intervals[1,,]
upper_bounds <- credible_intervals[2,,]
library(reshape2)
library(ggplot2)
library(ggpubr)
mean_values <- per(mean_values)
lower_bounds <- per(lower_bounds)
upper_bounds <- per(upper_bounds)
mean_melt = melt(mean_values)
lower_melt = melt(lower_bounds)
upper_melt = melt(upper_bounds)
#
p1 <- ggplot2::ggplot(mean_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.y = element_text(size = 5),axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 5),plot.title = element_text(hjust = 0.5,size=20)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  ggtitle("Mean")

p2 <- ggplot2::ggplot(lower_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.y = element_text(size = 5),axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 5),plot.title = element_text(hjust = 0.5,size=20)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  ggtitle("Lower bound")

p3 <- ggplot2::ggplot(upper_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") + theme(axis.text.y = element_text(size = 5),axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size = 5),plot.title = element_text(hjust = 0.5,size=20)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  ggtitle("Upper bound")
gp <- ggpubr::ggarrange(plotlist = list(p1,p2,p3),ncol = 3,nrow = 1,common.legend = TRUE,legend = "right")
# gp
# library(cowplot)
#
# legend <- get_legend(p1 + theme(legend.position = "right"))

# # 组合主图和图例
# gp <- plot_grid(
#   plot_grid(p1, p2, p3, ncol = 3, align = "hv", axis = "tblr"), # 主图
#   legend,  # 右侧图例
#   ncol = 2, rel_widths = c(3, 0.4)  # 调整图形和图例的宽度比例
# )
# gp
filename = paste0("./output/Figures/ci_moms",".pdf")
ggplot2::ggsave(gp, file = filename,
                width = 11, height = 4, device='pdf')
