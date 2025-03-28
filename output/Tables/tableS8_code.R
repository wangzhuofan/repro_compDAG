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


computeMetrics = function(verbose,allNoisetype,allSignal,method,allN,threshold,allparam,allprop,allhyper){
  if(verbose==1){
    mergeMean = matrix(0,3,0)
    mergeSD = matrix(0,3,0)
    for (hyper in 1:1) {
      pgammaa = allhyper[hyper,1]
      pgammab = allhyper[hyper,2]
      pdir = allhyper[hyper,3]
      for (signal in allSignal) {
        # rparam = allparam[which(allSignal==signal)]
        for (noisetype in allNoisetype)
        {
          for (simu_n in allN) {
            finalMean = rep(0,3)
            finalSD = rep(0,3)
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
            # if(noisetype=="Multi3")
              {
              B0 = matrix(0,3,3)
              B0[2,1] = 1
              B0[3,2] = 1
              B0[3,1] = 1
              if(method=="compDAG"){

                gam_list = array(0, c(3, 3, 25))
                gam_list[1, 2, 2] = 1
                gam_list[1, 3, 3] = 1
                gam_list[2, 3, 4] = 1
                gam_list[2, 1, 5] = 1
                gam_list[3, 1, 6] = 1
                gam_list[3, 2, 7] = 1
                gam_list[1, 2, 8] = gam_list[1, 3, 8] = 1
                gam_list[2, 3, 9] = gam_list[2, 1, 9] = 1
                gam_list[3, 1, 10] = gam_list[3, 2, 10] = 1

                gam_list[2, 1, 11] = gam_list[3, 1, 11] = 1
                gam_list[3, 2, 12] = gam_list[1, 2, 12] = 1
                gam_list[1, 3, 13] = gam_list[2, 3, 13] = 1

                gam_list[1, 2, 14] = gam_list[2, 3, 14] = 1
                gam_list[1, 3, 15] = gam_list[3, 2, 15] = 1
                gam_list[2, 3, 16] = gam_list[3, 1, 16] = 1
                gam_list[2, 1, 17] = gam_list[1, 3, 17] = 1
                gam_list[3, 2, 18] = gam_list[2, 1, 18] = 1
                gam_list[3, 1, 19] = gam_list[1, 2, 19] = 1

                gam_list[1, 2, 20] = gam_list[1, 3, 20] = gam_list[2, 3, 20] = 1
                gam_list[1, 2, 21] = gam_list[1, 3, 21] = gam_list[3, 2, 21] = 1
                gam_list[2, 1, 22] = gam_list[2, 3, 22] = gam_list[1, 3, 22] = 1
                gam_list[2, 1, 23] = gam_list[2, 3, 23] = gam_list[3, 1, 23] = 1
                gam_list[3, 1, 24] = gam_list[3, 2, 24] = gam_list[1, 2, 24] = 1
                gam_list[3, 1, 25] = gam_list[3, 2, 25] = gam_list[2, 1, 25] = 1
                res = (lapply(result, function(x){gam_list[,,which.min(x)]}))
              }
              if(method=="PC"){
                res = lapply(result, function(x){
                  max_indices <- order(x$edge, decreasing = TRUE)[1:2]

                  # 创建与原矩阵相同大小的零矩阵
                  re <- matrix(0, nrow = nrow(x$edge), ncol = ncol(x$edge))

                  # 将最大的两个元素对应的位置设置为 1
                  re[max_indices] <- 1

                  # 输出结果
                  return(re)
                })
              }
            }


            result = res
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
        }
      }
    }


    return(list(finalMean = mergeMean, finalSD = mergeSD))
  }else{
    mergeMean = matrix(0,3,0)
    mergeSD = matrix(0,3,0)
    for (hyper in 1:1) {
      pgammaa = allhyper[hyper,1]
      pgammab = allhyper[hyper,2]
      pdir = allhyper[hyper,3]
      for (signal in allSignal) {
        # rparam = allparam[which(allSignal==signal)]
        for (noisetype in allNoisetype)
        {
          for (simu_n in allN) {
            finalMean = rep(0,3)
            finalSD = rep(0,3)
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
            # if(noisetype=="Multi3")
              {
              set.seed(1)
              ncomm = 3
              m = list()
              p = c(30,30,30)
              for (nc in 1:(ncomm-1)) {
                m[[nc]] = matrix(0,p[ncomm],p[nc])
                for (colind in 1:p[nc]) {
                  randNum = sample(1:3,1,replace = FALSE)
                  randLoc = sample(1:p[ncomm],randNum,replace = FALSE)
                  m[[nc]][randLoc,colind] = 1/randNum
                }
              }
              B0 = c(m[[1]]>0,m[[1]],m[[2]]>0)
              if(method=="compDAGPW"){
                result_m = lapply(result, function(x){
                  lapply(x, function(y){
                    if(length(y)==7)
                      return(apply(y$mt1,c(2,3),mean))
                    if(length(y)==11)
                      return(c(apply(y$mt1,c(2,3),mean),apply(y$mt2,c(2,3),mean)))
                    if(length(y)==15)
                      return(apply(y$mt3,c(2,3),mean))
                  })
                })

                result_mm = lapply(result_m, function(x){
                  return(c(x[[2]],x[[3]]))
                })
                # thres = lapply(result,function(x){search_threshold(x$mEst,0.1)})
                res = lapply(result_mm, function(x){return(x>threshold)})
              }
              if(method=="PCPW"){
                res = lapply(result, function(x){
                  i=2
                  j=1
                  a = x$adj[(px*(i-1)+1):(px*i),(px*(j-1)+1):(px*j)]
                  re <- a

                  # 输出结果
                  return(re)
                })
              }
            }

            result = res
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
        }
      }
    }


    return(list(finalMean = mergeMean, finalSD = mergeSD))
  }
}
outputMetricsTable = function(finalMean, finalSD, method, rk = 3){
  cat("\\midrule\n")
  colNum = 1+length(allNoisetype)

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
# allMethod = c("COLP","bQCD" ,"PC")
allN = c(150)
# allSignal =c(0.3,0.5,1,3,5)
allSignal = c(5)
allNoisetype = c("Multi3","Multi5")
px = 30
py = 40
allhyper = matrix(0,1,3)
# allhyper[1,] = c(1,1,0.025)
allhyper[1,] = c(1,0.1,0.025)
allprop = c(0.2,0.4,0.6)

rparam = 0.5
thres=0.1

# for(noisetype in allNoisetype)
{
  for (method in allMethod) {
    # for (thres in threshold)
    {
      finalList = computeMetrics(1,allNoisetype,allSignal,method,allN,thres,allparam,allprop,allhyper)
      cat("--------------community level---------------\n")
      outputMetricsTable(finalList$finalMean, finalList$finalSD, method, rk = 3)
    }
  }
}

# allMethod = c("microbe","PC")#
# # for(noisetype in allNoisetype)
#   {
#   for (method in allMethod) {
#     for (thres in threshold) {
#       finalList = computeMetrics(0,allNoisetype,allSignal,method,allN,thres,allparam,allprop,allhyper)
#       cat("--------------microbe level---------------\n")
#       outputMetricsTable(finalList$finalMean, finalList$finalSD, method, rk = 3)
#     }
#   }
# }
#




