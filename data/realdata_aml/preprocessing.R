datagenus <- read.csv("./data/realdata_aml/rawdata/Genus.csv",header = FALSE)
ds <- as.character(datagenus[1,-1])
rownames(datagenus) <- c("",datagenus$V1[-1])
colnames(datagenus) <- c("",ds)
datagenus <- datagenus[-1,-1]
metadata <- read.csv("./data/realdata_aml/rawdata/metadata.csv")
ns <- metadata$Sample
ns_oral <- ns[metadata$Material=="Oral"]
ns_stool <- ns[metadata$Material=="Stool"]
meta_oral <- subset(metadata,Material=="Oral")
meta_stool <- subset(metadata,Material=="Stool")
pairsm <- data.frame(oral=ns_oral,stool=NA,patient=NA,timepoint=NA)
templ <- list()
for (i in 1:768) {
  pairsm$oral[i] <- ns_oral[i]
  pairsm$patient[i] <- metadata$Patient[i]
  pairsm$timepoint[i] <- metadata$Timepoint[i]
  temp <- which((meta_stool$Patient==metadata$Patient[i])&(meta_stool$Timepoint==metadata$Timepoint[i]))
  templ[[i]] <- temp
  if(length(temp)==1){
    pairsm$stool[i] <- ns_stool[temp]
  }
  if(length(temp)==2){
    pairsm$stool[i] <- ns_stool[min(temp)]
  }
}
pairsm <- na.omit(pairsm)
pairs_record <- pairsm
pairs_record$oral <- match(pairsm$oral,ds)
pairs_record$stool <- match(pairsm$stool,ds)
pairs_record <- na.omit(pairs_record)
pair_max <- tapply(seq_along(pairs_record$patient), pairs_record$patient, max)
pairs_select <- pairs_record[pair_max,]
#oral_index <- na.omit(match(pairsm$oral,ds))
#stool_index <- na.omit(match(pairsm$stool,ds))
# genus_oral <- datagenus[,pairs_record$oral]
# genus_stool <- datagenus[,pairs_record$stool]
genus_oral <- datagenus[,pairs_select$oral]
genus_stool <- datagenus[,pairs_select$stool]
ratio0 <- data.frame(oral=apply(genus_oral,1,function(x){sum(x==0)/ncol(genus_oral)}),stool=apply(genus_stool,1,function(x){sum(x==0)/ncol(genus_stool)}))
library(dplyr)
select_ratio_oral  <- which(ratio0$oral<0.8)
select_ratio_stool <- which(ratio0$stool<0.8)
#select_ratio <- which(ratio0$oral<0.9&ratio0$stool<0.9)
genus_oral <- genus_oral[select_ratio_oral,]
genus_stool <- genus_stool[select_ratio_stool,]
oral_mat <- matrix(as.numeric(as.matrix(genus_oral)),nrow = nrow(genus_oral))
stool_mat <- matrix(as.numeric(as.matrix(genus_stool)),nrow = nrow(genus_stool))
library(robCompositions)
imp_oral <- t(oral_mat+0.5)
imp_stool <- t(stool_mat+0.5)

imp_oral <- as.matrix(impRZilr(t(oral_mat))$x)
imp_stool <- as.matrix(impRZilr(t(stool_mat))$x)
oral_scale <- t((imp_oral)/rowSums(imp_oral))
stool_scale <- t((imp_stool)/rowSums(imp_stool))