rm(list=ls())
library(reshape2)
library(ggplot2)
library(patchwork)
library(ggpubr)
set.seed(1)
# n = 100; px = 30; py = 40;signal = 5
px = 30; py = 40
m = matrix(0,py,px)
for (colind in 1:px) {
  randNum = sample(1:3,1,replace = FALSE)
  randLoc = sample(1:py,randNum,replace = FALSE)
  m[randLoc,colind] = 1/randNum
}

allSignal = c(0.3,0.5,1,3,5)
simu_plot <- function(noisetype,samplesize){
  allMethod = c("compDAG")#
  # allMethod = c("COLP","bQCD" ,"PC")
  allN = samplesize
  # allSignal =c(1,3,5)
  # allNoisetype = c("Dirichlet","Additive")
  # noisetype = "Dirichlet"
  # signal = 3
  # simu_n = 150
  # method = "compDAG"
  count=1
  for (signal in allSignal) {
    for (simu_n in allN) {
      fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                        "-signal-",signal,
                        "-n-", simu_n,
                        "-method-","compDAG",
                        ".RData")
      load(fileList)
      m_est = matrix(0,py,px)
      for (i in 1:30) {
        m_est = m_est+result[[i]]$mEst
      }
      assign(paste0("m_est",count),m_est/30)
      assign(paste0("mest_melt",count),melt(m_est/30))
      assign(paste0("m_diff",count),abs(m_est/30-m))
      assign(paste0("mdiff_melt",count),melt(abs(m_est/30-m)))
      count=count+1
      # m_est = m_est/30
    }
  }

  #print(fileList)

  # m_est = result[[1]]$mEst
  # heatmap(m,Rowv = NA,Colv = NA,scale = "none")
  # heatmap(m_est,Rowv = NA,Colv = NA,scale = "none")
  # barplot(m)
  m_melt = melt(m)

  p0 <- ggplot2::ggplot(m_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
    scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20),legend.key.size = unit(0.8,"cm")) +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
    ggtitle(expression(bold(M)[21]))
  for (i in 1:5) {
    assign(paste0("p",i),
           ggplot2::ggplot(get(paste0("mdiff_melt",i)), aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "white")+
             # scale_fill_gradient(low = "white", high = "green")+
             # scale_fill_gradientn(
             #   colors = c("blue", "white", "#FF6666", "#B22222"),  # 自定义颜色顺序
             #   values = scales::rescale(c(-0.8, 0, 0.2, 0.8)),  # 设置颜色映射值
             #   limits = c(-0.8, 0.8)
             #   # trans = "log"# 显示范围设置
             # ) +
             scale_fill_gradientn(
               colors = c("white", "#FF6666"),  # 自定义颜色顺序
               values = scales::rescale(c(0,0.8)),  # 设置颜色映射值
               limits = c(0, 0.8),
               oob = scales::squish
               # trans = "log"# 显示范围设置
             ) +
             theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
             scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
             # expand_limits(fill = c(-0.8, 0.8)) +
             ggtitle(substitute(pi[21] == value, list(value = allSignal[i]))))
  }
  # p1 <- ggplot2::ggplot(mest_melt1, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  #   ggtitle(expression(pi[21] == 0.3))
  # p2 <- ggplot2::ggplot(mest_melt2, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  #   ggtitle(expression(pi[21] == 0.5))
  # p3 <- ggplot2::ggplot(mest_melt3, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  #   ggtitle(expression(pi[21] == 1))
  # p4 <- ggplot2::ggplot(mest_melt4, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  #   ggtitle(expression(pi[21] == 3))
  # p5 <- ggplot2::ggplot(mest_melt5, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),plot.title = element_text(hjust = 0.5,size=20)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")+
  #   ggtitle(expression(pi[21] == 5))
  # p6 <- ggplot2::ggplot(mest_melt6, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
  #   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  #   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
  # # p0+p1+p2+p3+p4+p5+p6+plot_layout(ncol = 7)
  # ann0 <- ggplot() +
  #   geom_text(aes(x=0, y=0, label = "(a)true graph"),
  #             parse = TRUE, size = 6, hjust = -1) +
  #   theme_void()
  # ann1 <- ggplot() +
  #   geom_text(aes(x=0, y=0, label = "(b)n=150"),
  #             parse = TRUE, size = 6, hjust = -1) +
  #   theme_void()
  #
  # ann2 <- ggplot() +
  #   geom_text(aes(x=0, y=0, label = "(c)n=100"),
  #             parse = TRUE, size = 6, hjust = -1) +
  #   theme_void()
  gp <- ggpubr::ggarrange(plotlist = list(p1,p2,p3,p4,p5),ncol = 5,nrow = 1,common.legend = TRUE,legend = "right",font.label = list(size=20))
  # annotate_figure(gp,
  #                 # top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
  #                 bottom = text_grob("(a) true graph                                 (b) n=150                                                              (c) n=100 ", size = 10,x=0.39),
  #                 # left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
  #                 # right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
  #                 # fig.lab = "Figure 1", fig.lab.face = "bold"
  # )
  filename = paste0("./output/Figure1-2_S.1-2/simu_diff",noisetype,"_",samplesize,".pdf")
  ggsave(gp, file = filename,
         width = 20, height = 4, device='pdf')
}
# simu_plot("Dirichlet",100)
# simu_plot("Additive",100)
simu_plot("Dirichlet",150)
simu_plot("Additive",150)


# simu_plot <- function(noisetype,samplesize){
#   allMethod = c("compDAG")#
#   # allMethod = c("COLP","bQCD" ,"PC")
#   allN = samplesize
#   allSignal = c(0.3)
#   # allSignal =c(1,3,5)
#   # allNoisetype = c("Dirichlet","Additive")
#   # noisetype = "Dirichlet"
#   # signal = 3
#   # simu_n = 150
#   # method = "compDAG"
#   count=1
#   for (signal in allSignal) {
#     for (simu_n in allN) {
#       fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
#                         "-signal-",signal,
#                         "-n-", simu_n,
#                         "-method-","compDAG",
#                         ".RData")
#       load(fileList)
#       mat = matrix(0,nrow = 30,ncol = px*py)
#       for (i in 1:30) {
#         mat[i,] = as.vector(result[[i]]$mEst)
#       }
#       # assign(paste0("m_est",count),m_est/30)
#       # assign(paste0("mest_melt",count),melt(m_est/30))
#       # count=count+1
#       # m_est = m_est/30
#     }
#   }
#   boxplot(mat)
#   #print(fileList)
#
#   # m_est = result[[1]]$mEst
#   # heatmap(m,Rowv = NA,Colv = NA,scale = "none")
#   # heatmap(m_est,Rowv = NA,Colv = NA,scale = "none")
#   # barplot(m)
#
# }
# mean = colMeans(mat)
# sd = apply(mat, 2, sd)
# df = data.frame(est = mean,true = as.vector(m),sd = sd)
# p = ggplot(data = df, aes(x = true)) +
#   geom_point(aes(x = true, y = est)) +
#   geom_errorbar(aes(x = true, ymin = est-sd, ymax = est+sd),width = 0.05) +
#   # stat_function(fun = cos_func, args = list(a = amplitude, phase = phase, omega = omega, intercept = 0), size = 0.7, color = col) +
#   # geom_ribbon(data = ci, aes(x = month, ymin = lower_ci, ymax = upper_ci), fill = col, alpha = 0.3) +
#   # scale_x_continuous(breaks=c(1, 3, 5, 7, 9, 11)) +
#   # ggtitle(title) +
#   # xlab("Month") +
#   geom_abline(intercept=0,slope=1,linetype="dashed")+
#   theme_classic() +
#   theme(legend.position="none",
#         plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
#         axis.text = element_text(size = 10),
#         axis.title.x = element_text(size = 11),
#         axis.title.y = element_blank()
#   )
# p
