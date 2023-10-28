
for (signal in c(0.3,0.5,1,3,5)) {
  for (n in c(100,150)) {
    rm(list = ls())
    
    ###competing methods
    library("doParallel")   
    library("foreach") 
    cl<- makeCluster(8) 
    registerDoParallel(cl) 
    
    comp <- function(i){
      ####################
      set.seed(1)
      px = 30; py = 40
      m = matrix(0,py,px)
      for (colind in 1:px) {
        randNum = sample(1:3,1,replace = FALSE)
        randLoc = sample(1:py,randNum,replace = FALSE)
        m[randLoc,colind] = 1/randNum
      }
      # m = t(rdirichlet(px,rep(1/py,py)))
      set.seed(i)
      x = t(rdirichlet(n,rep(0.1,px)))
      y <- matrix(0,py,n)
      e = t(rdirichlet(n,rep(1,py)))
      y = (signal/(1+signal)) * m %*% x+(1/(1+signal))*e
      ####################
      
      ######PC######################################################
      data = rbind(x,y)
      p = px+py
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      xtoy = sum(adj[(px+1):p,1:px])
      ytox = sum(adj[1:px,(px+1):p])
      if(xtoy>ytox) respc = 1 else respc = 0
      
      ##########
      pcmat = adj[(px+1):p,1:px]
      mmat = matrix(m>0,py,px)
      pc_spec = norm(pcmat-mmat,"F")^2/(norm(mmat,"F")^2)
      pc_tp = sum((mmat==1)&(pcmat==1))
      pc_fp = sum((mmat==0)&(pcmat==1))
      pc_tn = sum((mmat==0)&(pcmat==0))
      pc_fn = sum((mmat==1)&(pcmat==0))
      return(list(tp = pc_tp,fp = pc_fp,tn = pc_tn,fn = pc_fn))
      
    }
    
    comp200101 = foreach(i=1:30,.packages = c("rstan","gtools","loo","quantregForest","rvinecopulib","statmod","qrnn","COLP","cluster")) %dopar%
      {
        comp(i)
      }
    tp_pc = rep(0,30)
    fp_pc = rep(0,30)
    tn_pc = rep(0,30)
    fn_pc = rep(0,30)
    for (i in 1:30) {
      tp_pc[i] = comp200101[[i]]$tp
      fp_pc[i] = comp200101[[i]]$fp
      tn_pc[i] = comp200101[[i]]$tn
      fn_pc[i] = comp200101[[i]]$fn
    }
    
    tpr_pc = tp_pc/(tp_pc+fn_pc)
    fdr_pc = fp_pc/(fp_pc+tp_pc)
    mcc_pc = (tp_pc*tn_pc-fp_pc*fn_pc)/(sqrt(tp_pc+fp_pc)*sqrt(tp_pc+fn_pc)*sqrt(tn_pc+fp_pc)*sqrt(tn_pc+fn_pc))
    
    load(paste0("misspecified_n=',n,'_pi=',signal,'_results.RData"))
    tp = rep(0,30)
    fp = rep(0,30)
    tn = rep(0,30)
    fn = rep(0,30)
    for (i in 1:30) {
      px = 30; py = 40
      t2 = sum2031[[i]]
      m = t2$m
      me = t2$me
      mmat = matrix(m>0,py,px)
      compDAGmat = matrix(me>(1/px),py,px)
      compDAG_tp = sum((mmat==1)&(compDAGmat==1))
      compDAG_fp = sum((mmat==0)&(compDAGmat==1))
      compDAG_tn = sum((mmat==0)&(compDAGmat==0))
      compDAG_fn = sum((mmat==1)&(compDAGmat==0))
      tp[i] = compDAG_tp
      fp[i] = compDAG_fp
      tn[i] = compDAG_tn
      fn[i] = compDAG_fn
    }
    
    tpr = tp/(tp+fn)
    fdr = fp/(fp+tp)
    mcc = (tp*tn-fp*fn)/(sqrt(tp+fp)*sqrt(tp+fn)*sqrt(tn+fp)*sqrt(tn+fn))
    cat(paste0('The results of compDAG in misspecified simulation setting when n = ',n,' and pi = ',signal,' are: TPR:',mean(tpr),'(',sd(tpr),'); FDR:',mean(fdr),'(',sd(fdr),'); MCC:',mean(mcc),'(',sd(mcc),').'))
    cat(paste0('The results of PC in misspecified simulation setting when n = ',n,' and pi = ',signal,' are: TPR:',mean(tpr_pc),'(',sd(tpr_pc),'); FDR:',mean(fdr_pc),'(',sd(fdr_pc),'); MCC:',mean(mcc_pc),'(',sd(mcc_pc),').'))
  }
}
