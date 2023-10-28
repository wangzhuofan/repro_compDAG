ls()
rm(list=ls())
library(HMP2Data)
library(phyloseq)
library(SummarizedExperiment)
library(MultiAssayExperiment)
library(dplyr)
library(ggplot2)
library(UpSetR)
set.seed(1)
data("momspi16S_mtx")
data("momspi16S_tax")
data("momspi16S_samp")
tmp_data <- split(momspi16S_samp, momspi16S_samp$subject_id)
momspi_upset <- lapply(tmp_data, function(x) {
  table(x$sample_body_site)
})
momspi_upset <- bind_rows(momspi_upset)
tmp <- as.matrix(momspi_upset[, -1])
tmp <- (tmp > 0) *1
tmp[is.na(tmp)] <- 0
momspi_upset <- data.frame(patient = names(tmp_data), tmp)


sam = momspi16S_samp$file_name
samcervix_of_uterus  <- sam[momspi16S_samp$sample_body_site=="cervix of uterus"]
mscervix_of_uterus  <- subset(momspi16S_samp,sample_body_site=="cervix of uterus")
msvagina <- subset(momspi16S_samp,sample_body_site=="vagina")
pairsm <- data.frame(cervix_of_uterus  = samcervix_of_uterus ,vagina=NA)
templ <- list()
for (i in 1:length(samcervix_of_uterus )) {
  temp <- which((msvagina$subject_id==mscervix_of_uterus $subject_id[i])&(msvagina$visit_number==mscervix_of_uterus $visit_number[i]))
  templ[[i]] <- temp
  if(length(temp)==1){
    pairsm$vagina[i] <- msvagina$file_name[temp]
  }
  if(length(temp)>1){
    pairsm$vagina[i] <- msvagina$file_name[min(temp)]
  }
}
pairsm <- na.omit(pairsm)
pairs_record <- pairsm
filename = colnames(momspi16S_mtx)
pairs_record$cervix_of_uterus  <- match(pairsm$cervix_of_uterus ,filename)
pairs_record$vagina <- match(pairsm$vagina,filename)
pairs_record <- na.omit(pairs_record)

moms_cervix_of_uterus  <- momspi16S_mtx[,pairs_record$cervix_of_uterus]
moms_vagina <- momspi16S_mtx[,pairs_record$vagina]

gu <- aggregate(moms_cervix_of_uterus,by=list(as.vector(momspi16S_tax[,6])),sum)
gv <- aggregate(moms_vagina,by=list(as.vector(momspi16S_tax[,6])),sum)

rownames(gu) <- as.vector(gu[,1])
gu <- gu[,-1]
rownames(gv) <- as.vector(gv[,1])
gv <- gv[,-1]

moms_cervix_of_uterus <- gu
moms_vagina <- gv
ratio0 <- data.frame(cervix_of_uterus =apply(moms_cervix_of_uterus ,1,function(x){sum(x==0)/ncol(moms_cervix_of_uterus )}),vagina=apply(moms_vagina,1,function(x){sum(x==0)/ncol(moms_vagina)}))
library(dplyr)
select_ratio_cervix_of_uterus  <- which(ratio0$cervix_of_uterus<0.9)
select_ratio_vagina <- which(ratio0$vagina<0.9)
moms_cervix_of_uterus  <- moms_cervix_of_uterus [select_ratio_cervix_of_uterus ,]
moms_vagina <- moms_vagina[select_ratio_vagina,]
cervix_of_uterus_mat <- matrix(as.numeric(as.matrix(moms_cervix_of_uterus )),nrow = length(select_ratio_cervix_of_uterus ))
vagina_mat <- matrix(as.numeric(as.matrix(moms_vagina)),nrow = length(select_ratio_vagina))

del <- which(colSums(vagina_mat)==0|colSums(cervix_of_uterus_mat)==0)
if(length(del)>0){
  cervix_of_uterus_mat <- cervix_of_uterus_mat[,-del]
  vagina_mat <- vagina_mat[,-del]
}

library(robCompositions)
imp_cervix_of_uterus  <- as.matrix(impRZilr(t(cervix_of_uterus_mat))$x)
imp_vagina <- as.matrix(impRZilr(t(vagina_mat))$x)
cervix_of_uterus_scale <- t((imp_cervix_of_uterus )/rowSums(imp_cervix_of_uterus ))
vagina_scale <- t((imp_vagina)/rowSums(imp_vagina))