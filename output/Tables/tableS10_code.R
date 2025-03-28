# Generate latex table for the simulation results.

rm(list = ls())

compute_implied_fdr <- function(threshold, s) {
  indicator = (s>threshold)
  if(sum(indicator)==0)
    return(0)
  pip = (s - min(s))/diff(range(s))
  return(sum((1-pip)*indicator)/sum(indicator))
}

search_threshold <- function(s,fdr){
  for (threshold in seq(0,1,0.005)){
    if(compute_implied_fdr(threshold,s)<fdr)
      break
  }
  return(threshold)
}

outputAccuracyTable = function(Accuracy, rk = 3){
  cat("\\midrule\n")
  colNum = 1+length(allN)*length(allSignal)
  for(rowI in 1:nrow(Accuracy)){
    if(rowI==1) cat("compDAG")
    if(rowI==2) cat("COLP")
    if(rowI==3) cat("bQCD")
    if(rowI==4) cat("PC")
    for(colJ in 1:(colNum-1)){
      cat(" & ")
      fmt = paste0("%.",rk,"f")
      if(!is.na(Accuracy[rowI,colJ])&&(Accuracy[rowI,colJ]==max(na.omit(Accuracy[,colJ])))){
        cat("\\textbf{")
        cat(sprintf(fmt,Accuracy[rowI,colJ]))
        cat("}")
      } else
        cat(sprintf(fmt,Accuracy[rowI,colJ]))
    }
    cat("\\\\", "\n")
  }# rowI
  cat("\\bottomrule\n")
}

computeAccuracy = function(noisetype,allSignal,allMethod,allN){
  nMethods = length(allMethod)
  mergeAccuracy = matrix(0,nMethods,0)
  for (signal in allSignal) {
    for (simu_n in allN) {
      finalMean = rep(0,nMethods)
      for(m in 1:nMethods){
        method = allMethod[m]
        fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                          "-signal-",signal,
                          "-n-", simu_n,
                          "-method-",method,
                          ".RData")
        load(fileList)
        if(method=="compDAG"|method=="PC"){
          result = lapply(result, function(x){return(x$cd)})
        }
        result = unlist(result)
        finalMean[m] = sum(na.omit(result==1))/length(result)
      }
      mergeAccuracy = cbind(mergeAccuracy,finalMean)
    }
  }
  return(mergeAccuracy)
}

computeMetrics = function(noisetype,allSignal,method,allN,threshold){
  mergeMean = matrix(0,3,0)
  mergeSD = matrix(0,3,0)
  for (signal in allSignal) {
    for (simu_n in allN) {
      finalMean = rep(0,3)
      # rownames(finalMean) = c("TPR","FDR","MCC")
      finalSD = rep(0,3)
      # rownames(finalSD) = c("TPR","FDR","MCC")
      # for(m in 1:length(allMethod)){
      # method = allMethod[m]
      fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                        "-signal-",signal,
                        "-n-", simu_n,
                        "-method-",method,
                        ".RData")
      #print(fileList)
      load(fileList)

      # if(method=="compDAG"){

      # thres = lapply(result,function(x){search_threshold(x$mEst,0.1)})
      result = lapply(result, function(x){return(x$mEst>threshold)})
      # }
      set.seed(1)
      # n = 100; px = 30; py = 40;signal = 5
      m = matrix(0,py,px)
      for (colind in 1:px) {
        randNum = sample(1:3,1,replace = FALSE)
        randLoc = sample(1:py,randNum,replace = FALSE)
        m[randLoc,colind] = 1/randNum
      }
      B0 = (m>0)
      TP = unlist(lapply(result, function(mat){length(which(((B0)==1)&(mat==1)))}))
      TN = unlist(lapply(result, function(mat){length(which(((B0)==0)&(mat==0)))}))
      FP = unlist(lapply(result, function(mat){length(which(((B0)==0)&(mat==1)))}))
      FN = unlist(lapply(result, function(mat){length(which(((B0)==1)&(mat==0)))}))

      TPR = TP/(TP+FN)
      FDR = FP/(FP+TP)
      MCC = (TP*TN-FP*FN)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN))

      finalMean[1] = mean(TPR)
      finalMean[2] = mean(FDR)
      finalMean[3] = mean(MCC)
      finalSD[1] = sd(TPR)
      finalSD[2] = sd(FDR)
      finalSD[3] = sd(MCC)
      # }
      mergeMean = cbind(mergeMean,finalMean)
      mergeSD = cbind(mergeSD,finalSD)
    }
  }
  return(list(finalMean = mergeMean, finalSD = mergeSD))
}
outputMetricsTable = function(finalMean, finalSD, method, rk = 3){
  cat("\\midrule\n")
  colNum = 1+length(allN)*length(allSignal)

  for(rowI in 1:3){
    if(rowI==1) cat("\\multirow{5}{*}{", method,"}&TPR",sep="")
    if(rowI==2) cat("&FDR")
    if(rowI==3) cat("&MCC")

    for(colJ in 1:(colNum-1)){
      cat(" & \\tabincell{c}{")
      fmt = paste0("%.",rk,"f")
      if(method=="compDAG")
        cat("\\textbf{",sprintf(fmt,finalMean[rowI,colJ]),"}",sep="")
      else
        cat(sprintf(fmt,finalMean[rowI,colJ]))
      cat("\\\\(",sprintf(fmt,finalSD[rowI,colJ]),")}", sep="")
    }
    cat("\\\\", "\n")
  }# rowI
}

#------ Accuracy Table (Table 1-2) ------
# # allMethod = c("VI")
# allMethod = c("compDAG","COLP","bQCD" ,"PC")#
# # allMethod = c("COLP","bQCD" ,"PC")
# allN = c(100,150)
# allSignal =c(0.3,0.5,1,3,5)
# allNoisetype = c("Dirichlet","Additive")
# # allNoisetype = c("Dirichlet")
# px = 30
# py = 40
# # allP = c(10)
# # allP = c(20,30)
# # simu_n = 500
# count = 1
# for(noisetype in allNoisetype){
#   finalList = computeAccuracy(noisetype,allSignal,allMethod,allN)
#   cat(paste0("--------------Table",count,"-------------\n"))
#   outputAccuracyTable(finalList, rk = 3)
#   count = count+1
# }

#------ Metrics Table(Table 3-4) ------
# # allMethod = c("VI")
# allMethod = c("compDAG","PC")#
# allN = c(100,150)
# allSignal =c(0.3,0.5,1,3,5)
# threshold = c(0.1)
#
# for(noisetype in allNoisetype){
#   cat(paste0("--------------Table",count,"-------------\n"))
#
#
#   for (method in allMethod) {
#     for (thres in threshold) {
#       finalList = computeMetrics(noisetype,allSignal,method,allN,thres)
#       outputMetricsTable(finalList$finalMean, finalList$finalSD, method, rk = 3)
#     }
#   }
#   count = count+1
# }

#------ Metrics Table(Table S.2,S.3) ------
# allMethod = c("VI")
allNoisetype = c("Dirichlet","Additive")
outputMetricsTable_supp = function(finalMean, finalSD, method, rk = 3,thres){
  cat("\\midrule\n")
  colNum = 1+length(allN)*length(allSignal)
  for(rowI in 1:3){
    if(rowI==1) cat("\\multirow{5}{*}{\\tabincell{c}{", method,"\\\\(threshold = ",thres,")}}&TPR",sep="")
    if(rowI==2) cat("&FDR")
    if(rowI==3) cat("&MCC")

    for(colJ in 1:(colNum-1)){
      cat(" & \\tabincell{c}{")
      fmt = paste0("%.",rk,"f")
      cat(sprintf(fmt,finalMean[rowI,colJ]))
      cat("\\\\(",sprintf(fmt,finalSD[rowI,colJ]),")}", sep="")
    }
    cat("\\\\", "\n")
  }# rowI
}
allMethod = c("compDAG")#
allN = c(100,150)
allSignal =c(0.3,0.5,1,3,5)
threshold = c(0.06,0.16)
px = 30
py = 40
count = 10
for(noisetype in allNoisetype){
  cat(paste0("--------------TableS",count,"-------------\n"))
  for (method in allMethod) {
    for (thres in threshold) {
      finalList = computeMetrics(noisetype,allSignal,method,allN,thres)
      outputMetricsTable_supp(finalList$finalMean, finalList$finalSD, method, rk = 3,thres)
    }
  }
  count = count+1
}




