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

sum(rowSums(momspi_upset[,2:6])==5)
newupset = momspi_upset[which(rowSums(momspi_upset[,2:6])==5),]
allpatients = newupset$patient
sam = momspi16S_samp$file_name
samcervix_of_uterus  <- sam[momspi16S_samp$sample_body_site=="cervix of uterus"]
mscervix_of_uterus  <- subset(momspi16S_samp,sample_body_site=="cervix of uterus")
msvagina <- subset(momspi16S_samp,sample_body_site=="vagina")
msfeces <- subset(momspi16S_samp,sample_body_site=="feces")
msrectum <- subset(momspi16S_samp,sample_body_site=="rectum")

allcervix = subset(mscervix_of_uterus,subject_id %in% allpatients)
allvagina = subset(msvagina,subject_id %in% allpatients)
allfeces = subset(msfeces,subject_id %in% allpatients)
allrectum = subset(msrectum,subject_id %in% allpatients)

alldata <- data.frame(cervix_of_uterus  = rownames(allcervix),vagina=NA,rectum = NA)
templ <- list()
for (i in 1:nrow(allcervix)) {
  temp <- which((allvagina$subject_id==allcervix$subject_id[i])&(allvagina$visit_number==allcervix$visit_number[i]))
  print("vagina")
  print(temp)
  templ[[i]] <- temp
  if(length(temp)==1){
    alldata$vagina[i] <- allvagina$file_name[temp]
  }
  if(length(temp)>1){
    alldata$vagina[i] <- allvagina$file_name[min(temp)]
  }
  print(alldata$vagina[i] )
  # temp <- which((allfeces$subject_id==allcervix$subject_id[i]))
  # print("feces")
  # print(temp)
  # # templ[[i]] <- temp
  # if(length(temp)==1){
  #   alldata$feces[i] <- allfeces$file_name[temp]
  # }
  # if(length(temp)>1){
  #   alldata$feces[i] <- allfeces$file_name[min(temp)]
  # }
  # print(alldata$feces[i] )
  temp <- which((allrectum$subject_id==allcervix$subject_id[i])&(allrectum$visit_number==allcervix$visit_number[i]))
  print("rectum")
  print(temp)
  # templ[[i]] <- temp
  if(length(temp)==1){
    alldata$rectum[i] <- allrectum$file_name[temp]
  }
  if(length(temp)>1){
    alldata$rectum[i] <- allrectum$file_name[min(temp)]
  }
  print(alldata$rectum[i] )
}
alldata <- na.omit(alldata)
alldata <- alldata[-which(alldata$cervix_of_uterus=="EP557758_K20_MCVD"),]
alldf <- alldata
filename = colnames(momspi16S_mtx)
alldf$cervix_of_uterus <- match(alldf$cervix_of_uterus ,filename)
alldf$vagina <- match(alldf$vagina ,filename)
alldf$rectum <- match(alldf$rectum ,filename)
data_cervix  <- momspi16S_mtx[,alldf$cervix_of_uterus]
data_vagina  <- momspi16S_mtx[,alldf$vagina]
data_rectum  <- momspi16S_mtx[,alldf$rectum]


gu <- aggregate(data_cervix,by=list(as.vector(momspi16S_tax[,6])),sum)
gv <- aggregate(data_vagina,by=list(as.vector(momspi16S_tax[,6])),sum)
gr <- aggregate(data_rectum,by=list(as.vector(momspi16S_tax[,6])),sum)


rownames(gu) <- as.vector(gu[,1])
gu <- gu[,-1]
rownames(gv) <- as.vector(gv[,1])
gv <- gv[,-1]
rownames(gr) <- as.vector(gr[,1])
gr <- gr[,-1]



ratio0 <- data.frame(cervix =apply(gu ,1,function(x){sum(x==0)/ncol(gu)}),vagina=apply(gv,1,function(x){sum(x==0)/ncol(gv)}),rectum =apply(gr,1,function(x){sum(x==0)/ncol(gr)}))
library(dplyr)
select_ratio_cervix_of_uterus  <- which(ratio0$cervix<0.9)
select_ratio_vagina <- which(ratio0$vagina<0.9)
select_ratio_rectum <- which(ratio0$rectum<0.9)
delete_ratio_cervix_of_uterus  <- which(ratio0$cervix>=0.9)
delete_ratio_vagina <- which(ratio0$vagina>=0.9)
cd <- gu[delete_ratio_cervix_of_uterus ,]
vd <- gv[delete_ratio_vagina,]
gu <- gu[select_ratio_cervix_of_uterus ,]
gv <- gv[select_ratio_vagina,]
gr <- gr[select_ratio_rectum,]

matu <- matrix(as.numeric(as.matrix(gu)),nrow = length(select_ratio_cervix_of_uterus ))
matv <- matrix(as.numeric(as.matrix(gv)),nrow = length(select_ratio_vagina))
matr <- matrix(as.numeric(as.matrix(gr)),nrow = length(select_ratio_rectum))

library(robCompositions)
impu  <- as.matrix(impRZilr(t(matu))$x)
impv <- as.matrix(impRZilr(t(matv))$x)
impr <- as.matrix(impRZilr(t(matr))$x)

scaleu<- t((impu)/rowSums(impu))
scalev <- t((impv)/rowSums(impv))
scaler <- t((impr)/rowSums(impr))

rownames(scaleu)<- rownames(gu)
rownames(scalev) <- rownames(gv)
rownames(scaler) <- rownames(gr)

save(scaleu,scalev,scaler,file = "./data/realdata_moms_pi/preprocessed_moms_pi_allcommunity_data.RData")




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
delete_ratio_cervix_of_uterus  <- which(ratio0$cervix_of_uterus>=0.9)
delete_ratio_vagina <- which(ratio0$vagina>=0.9)
cd <- moms_cervix_of_uterus [delete_ratio_cervix_of_uterus ,]
vd <- moms_vagina[delete_ratio_vagina,]
moms_cervix_of_uterus  <- moms_cervix_of_uterus [select_ratio_cervix_of_uterus ,]
moms_vagina <- moms_vagina[select_ratio_vagina,]


cervix_of_uterus_mat <- matrix(as.numeric(as.matrix(moms_cervix_of_uterus )),nrow = length(select_ratio_cervix_of_uterus ))
vagina_mat <- matrix(as.numeric(as.matrix(moms_vagina)),nrow = length(select_ratio_vagina))

del <- which(colSums(vagina_mat)==0|colSums(cervix_of_uterus_mat)==0)
if(length(del)>0){
  cervix_of_uterus_mat <- cervix_of_uterus_mat[,-del]
  vagina_mat <- vagina_mat[,-del]
}



cervix_of_uterus_scale <- t((imp_cervix_of_uterus )/rowSums(imp_cervix_of_uterus ))
vagina_scale <- t((imp_vagina)/rowSums(imp_vagina))
rownames(cervix_of_uterus_scale)<- rownames(moms_cervix_of_uterus)
rownames(vagina_scale) <- rownames(moms_vagina)
save(cervix_of_uterus_scale,vagina_scale,file = "./data/realdata_moms_pi/preprocessed_moms_pi_rf_data.RData")
