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
sam_mest_save = sam_mest3

load("./data/realdata_moms_pi/preprocessed_moms_pi_data.RData")

cn_sub = rownames(cervix_of_uterus_scale)
rn_sub = rownames(vagina_scale)

axis_colors_x <- setNames(ifelse(cn %in% cn_sub, "black", "blue"), cn)
axis_colors_y <- setNames(ifelse(rn %in% rn_sub, "black", "blue"), rn)

############3
sam_melt = melt(sam_mest3)
# ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) + geom_tile(aes(fill = value), color = "black")+
#   scale_fill_gradient(low = "black", high = "green") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
#   scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + labs(x = "", y = "")
###########
###################
# print to pdf
pdf("./output/Figures/MOMS_all_split.pdf",width=10,height=9)
ggplot2::ggplot(sam_melt, aes(y =Var1, x = Var2)) +
  geom_tile(aes(fill = value), color = "black")+
  scale_fill_gradient(low = "black", high = "green") +
  theme(axis.text.x = element_markdown(angle = 90, hjust = 1, vjust = 0.5,size=5),axis.text.y = element_markdown(size=5))+
  scale_x_discrete(
    labels = function(x) {
      sapply(x, function(label) {
        paste0("<span style='color:", axis_colors_x[label], "'>", label, "</span>")
      })
    }
  ) +
  scale_y_discrete(
    labels = function(x) {
      sapply(x, function(label) {
        paste0("<span style='color:", axis_colors_y[label], "'>", label, "</span>")
      })
    }
  ) +
  labs(x = "", y = "")
dev.off()
