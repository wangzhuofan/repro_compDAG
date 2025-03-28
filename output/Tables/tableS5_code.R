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
  colNum = 1+length(allSettings)
  for(rowI in 1:nrow(Accuracy)){
    if(rowI==1) cat("compDAG")
    if(rowI==2) cat("PC")
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

computeAccuracy = function(allMethod,allSettings){
  nMethods = length(allMethod)
  mergeAccuracy = matrix(0,nMethods,0)
  for (hyper in 1:1) {
    pgammaa = allhyper[hyper,1]
    pgammab = allhyper[hyper,2]
    pdir = allhyper[hyper,3]
    for(setting in allSettings){
      # signal = 5
      noisetype = setting[1]
      simu_n = 150
      px = 30
      py = 40
      signal = as.numeric(setting[2])
      # print(noisetype)
      # for (prop in allprop)
      # rparam = allparam[which(allSignal==signal)]
      {
        # for (simu_n in allN) {
        finalMean = rep(0,nMethods)
        for(m in 1:nMethods){
          method = allMethod[m]
          fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                            "-signal-",signal,
                            "-n-", simu_n,
                            "-method-",method,
                            # "-param-",rparam,
                            # "-pgammaa-",pgammaa,
                            # "-pgammab-",pgammab,
                            # "-pdir-",pdir,
                            # "-prop-",prop,
                            ".RData")
          load(fileList)
          if(method=="compDAG"|method=="PC"|method=="PW"|method=="PCPW"){
            result = lapply(result, function(x){return(x$cd)})
          }

          result = unlist(result)
          finalMean[m] = sum(na.omit(result==1))/length(result)
        }
        mergeAccuracy = cbind(mergeAccuracy,finalMean)
      }
    }
  }
  # }


  return(mergeAccuracy)
}

computeMetrics = function(method,allSettings){
  mergeMean = matrix(0,3,0)
  mergeSD = matrix(0,3,0)
  for (hyper in 1:1) {
    pgammaa = allhyper[hyper,1]
    pgammab = allhyper[hyper,2]
    pdir = allhyper[hyper,3]
    for (setting in allSettings) {
      # signal = 5
      noisetype = setting[1]
      simu_n = 150
      px = 30
      py = 40
      signal = as.numeric(setting[2])
      # rparam = allparam[which(allSignal==signal)]
      # for (prop in allprop)
      {
        # for (simu_n in allN) {
        finalMean = rep(0,3)
        # rownames(finalMean) = c("TPR","FDR","MCOR")
        finalSD = rep(0,3)
        # rownames(finalSD) = c("TPR","FDR","MCOR")
        # for(m in 1:length(allMethod)){
        # method = allMethod[m]
        fileList = paste0("./output/simulation_results/simulation-noisetype-",noisetype,
                          "-signal-",signal,
                          "-n-", simu_n,
                          "-method-",method,
                          # "-param-",rparam,
                          # "-pgammaa-",pgammaa,
                          # "-pgammab-",pgammab,
                          # "-pdir-",pdir,
                          # "-prop-", prop,
                          ".RData")
        #print(fileList)
        load(fileList)

        result_thres = lapply(result, function(x){return(x$mEst>threshold)})
        if(px==30){
          set.seed(1)
          # n = 100; px = 30; py = 40;signal = 5
          m = matrix(0,py,px)
          for (colind in 1:px) {
            randNum = sample(1:3,1,replace = FALSE)
            randLoc = sample(1:py,randNum,replace = FALSE)
            m[randLoc,colind] = 1/randNum
          }

        }
        if(px==100){
          set.seed(1)
          m = matrix(0,py,px)
          for (colind in 1:px) {
            randNum = 1
            m[(py/2),colind] = 1/randNum
          }

        }
        B0 = (m>0)
        result= result_thres
        TP = unlist(lapply(result, function(mat){length(which(((B0)==1)&(mat==1)))}))
        TN = unlist(lapply(result, function(mat){length(which(((B0)==0)&(mat==0)))}))
        FP = unlist(lapply(result, function(mat){length(which(((B0)==0)&(mat==1)))}))
        FN = unlist(lapply(result, function(mat){length(which(((B0)==1)&(mat==0)))}))

        TPR = TP/(TP+FN)
        FDR = FP/(FP+TP)
        MCOR = (TP*TN-FP*FN)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN))

        finalMean[1] = mean(TPR)
        finalMean[2] = mean(FDR)
        finalMean[3] = mean(MCOR)
        finalSD[1] = sd(TPR)
        finalSD[2] = sd(FDR)
        finalSD[3] = sd(MCOR)
        # }
        mergeMean = cbind(mergeMean,finalMean)
        mergeSD = cbind(mergeSD,finalSD)
      }
      # }
    }
  }


  return(list(finalMean = mergeMean, finalSD = mergeSD))
}
outputMetricsTable = function(finalMean, finalSD, method, rk = 3){
  cat("\\midrule\n")
  colNum = 1+length(allSettings)

  for(rowI in 1:3){
    if(rowI==1) cat("\\multirow{5}{*}{", method,"}&TPR",sep="")
    if(rowI==2) cat("&FDR")
    if(rowI==3) cat("&MCOR")

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
# allMethod = c("VI")
allMethod = c("compDAG","PC")#
allSettings = list(c("LiNGAM",0.5),c("LiNGAM",1),c("LiNGAM",5))
# allNoisetype = c("zero-inflated")
# px = 30
# py = 40
allhyper = matrix(0,1,3)
# allhyper[1,] = c(1,1,0.025)
allhyper[1,] = c(1,1,0.025)

finalList = computeAccuracy(allMethod,allSettings)
outputAccuracyTable(finalList, rk = 3)


threshold = c(0.1)

for (method in allMethod) {
  for (thres in threshold) {
    finalList = computeMetrics(method,allSettings)
    outputMetricsTable(finalList$finalMean, finalList$finalSD, method, rk = 3)
  }
}
# for(setting in allSettings){print(setting[1])}
