rm(list = ls())
computeMetrics = function(allSignal,allN){
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
      fileList = paste0("./output/simulation_results/simulation-noisetype-Dirichlet",
                        "-signal-",signal,
                        "-n-", simu_n,
                        "-method-compDAG",
                        ".RData")
      #print(fileList)
      load(fileList)
      
      # if(method=="compDAG"){
      mResult = lapply(result, function(x){return(x$mEst)})
      sigResult = unlist(lapply(result, function(x){return(x$signalEst)}))
      mu1Result = unlist(lapply(result, function(x){return(x$mu1Est)}))
      # }
      set.seed(1)
      # n = 100; px = 30; py = 40;signal = 5
      m = matrix(0,py,px)
      for (colind in 1:px) {
        randNum = sample(1:3,1,replace = FALSE)
        randLoc = sample(1:py,randNum,replace = FALSE)
        m[randLoc,colind] = 1/randNum
      }
      mRe= unlist(lapply(mResult, function(x){norm(x-m,type = "F")^2/norm(m,"F")^2}))
      sigRe = (sigResult-signal)^2/(signal^2)
      # murr = mur^2/(0.1^2)
      mu1Re = (mu1Result-0.1)^2/(0.1^2)
      finalMean[1] = mean(mRe)
      finalMean[2] = mean(sigRe)
      finalMean[3] = mean(mu1Re)
      finalSD[1] = sd(mRe)
      finalSD[2] = sd(sigRe)
      finalSD[3] = sd(mu1Re)
      # }
      mergeMean = cbind(mergeMean,finalMean)
      mergeSD = cbind(mergeSD,finalSD)
    }
  }
  return(list(finalMean = mergeMean, finalSD = mergeSD))
}
outputMetricsTable = function(finalMean, finalSD,rk = 3){
  cat("\\midrule\n")
  colNum = 1+length(allN)*length(allSignal)
  
  for(rowI in 1:3){
    if(rowI==1) cat("Error of $\\Mb_{21}$")
    if(rowI==2) cat("Error of $\\pi_{21}$")
    if(rowI==3) cat("Error of $\\mu_1$")
    
    for(colJ in 1:(colNum-1)){
      cat(" & \\tabincell{c}{")
      fmt = paste0("%.",rk,"f")
      
      cat(thres(sprintf(fmt,finalMean[rowI,colJ])))
      cat("\\\\(",thres(sprintf(fmt,finalSD[rowI,colJ])),")}", sep="")
    }
    cat("\\\\", "\n")
  }# rowI
}
thres <- function(x){
  if(x=="0.000")
    x = "$\\leq 0.001$"
  return(x)
}
allN = c(150,100)
allSignal =c(0.3,0.5,1,3,5)
px = 30
py = 40
finalList = computeMetrics(allSignal,allN)
outputMetricsTable(finalList$finalMean, finalList$finalSD,  rk = 3)

# 
# for (signal in c(0.3,0.5,1,3,5)) {
#   for (n in c(100,150)) {
#     # rm(list = ls())
#     load(paste0("./output/simulation_results/simulation-noisetype-Dirichlet",
#                 "-signal-",signal,
#                 "-n-", n,
#                 "-method-compDAG",
#                 ".RData"))
#     mr = array(0,dim = c(40,30,30))
#     cr = rep(0,30)
#     ar = rep(0,30)
#     # mur = rep(0,30)
#     for (i in 1:30) {
#       t2 = sum2031[[i]]
#       m = t2$m
#       me = t2$me
#       ce = t2$ce
#       ae = t2$ae
#       mue = t2$mue
#       mr[,,i] = m-me
#       cr[i] = signal-ce
#       ar[i] = 0.1-ae
#       # mur[i] = 0.1-mue
#     }
#     mrr = apply(mr, 3, function(x){norm(x,type = "F")^2})/norm(m,"F")^2
#     crr = cr^2/(signal^2)
#     # murr = mur^2/(0.1^2)
#     arr = ar^2/(0.1^2)
#     cat(paste0('The relative error of compDAG when n = ',n,' and pi = ',signal,' are: M:',mean(mrr),'(',sd(mrr),'); pi:',mean(crr),'(',sd(crr),'); mu_1:',mean(arr),'(',sd(arr),'); mu_2:',mean(murr),'(',sd(murr),').'))
#   }
# }