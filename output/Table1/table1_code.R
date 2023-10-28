library(quantregForest)
library(rvinecopulib)
library(statmod)
library(qrnn)
library(gtools)
library(COLP)
library(cluster)
## Proper scoring rule for predicted quantiles
quantileScoring <- function(actual, pred, prob = 0.95) {
  mean((as.numeric(actual <= pred) - prob) * (pred - actual))
}

## Quantile Causal discovery method nonparametric copula
QCCD <- function(pair, m=1) {
  
  # to get reproducible jittering results for discreet data
  set.seed(0)
  
  n <- nrow(pair)
  # Recover pseudo-observations and estimate the copula non-parametrically
  u <- apply(pair, 2, function(x) rank(x, ties.method = "random")/(n + 1))
  cop <- bicop(data = u,
               family_set = "tll",
               nonpar_method = "constant")
  
  # deal with discrete data
  pair_scaled <- qnorm(u)
  
  # integrate over quantiles
  if(n < 200){
    uw <- gauss.quad.prob(1)
  } else {
    uw <- gauss.quad.prob(m)
  }
  
  cls <- sapply(uw$nodes, function(uu) {
    u_pred <- cbind(predict(object = cop,
                            newdata = cbind(uu, u[, 2]),
                            what = "hinv2"),
                    predict(object = cop,
                            newdata = cbind(u[, 1], uu),
                            what = "hinv1"))
    
    # marginal and conditional quantiles
    marg_q <- sapply(1:2, function(i) quantile(pair_scaled[,i], uu))
    cond_q <- sapply(1:2, function(i) quantile(pair_scaled[,i], u_pred[, i]))
    
    # code lengths
    cl_marginal <- sapply(1:2, function(i)
      quantileScoring(pair_scaled[, i], marg_q[i], uu))
    cl_conditional <- sapply(1:2, function(i)
      quantileScoring(pair_scaled[, i], cond_q[, i], uu))
    
    c(cl_marginal, cl_conditional)
  })
  
  sel <- !apply(is.na(cls), 2, any)
  uw$weights <- uw$weights[sel] / sum(uw$weights[sel])
  cls <- apply(na.omit(t(cls)) * uw$weights, 2, sum)
  
  dx_to_y <- (cls[1] + cls[4])/sum(cls[1:2])
  dy_to_x <- (cls[2] + cls[3])/sum(cls[1:2])
  
  cd <- ifelse(dy_to_x > dx_to_y, 1, 0)
  
  epsilon <-  (-dx_to_y + dy_to_x )
  
  return(list(cd = cd, epsilon = epsilon))
}


for (signal in c(0.3,0.5,1,3,5)) {
  for (n in c(100,150)) {
    # rm(list = ls())
    ###competing methods
    library("doParallel")   
    library("foreach") 
    cl<- makeCluster(8) 
    registerDoParallel(cl) 
    
    comp <- function(i){
      set.seed(1)
      px = 30; py = 40
      # m = t(rdirichlet(px,rep(1/py,py)))
      m = matrix(0,py,px)
      for (colind in 1:px) {
        randNum = sample(1:3,1,replace = FALSE)
        randLoc = sample(1:py,randNum,replace = FALSE)
        m[randLoc,colind] = 1/randNum
      }
      set.seed(i)
      x = t(rdirichlet(n,rep(0.1,px)))
      y <- matrix(0,py,n)
      temp = signal * m %*% x+0.1
      for (i in 1:n) {
        y[,i] = rdirichlet(1,temp[,i])
      }
      ####################
      ######COLP#############################################
      asw <- numeric(5)
      for (k in 2:5)
        asw[[k]] <- pam(t(x), k) $ silinfo $ avg.width
      kx.best <- which.max(asw)
      asw <- numeric(5)
      for (k in 2:5)
        asw[[k]] <- pam(t(y), k) $ silinfo $ avg.width
      ky.best <- which.max(asw)
      
      kx = as.factor(kmeans(t(x),kx.best)$cluster)
      ky = as.factor(kmeans(t(y),ky.best)$cluster)
      
      fit = COLP(kx,ky,algo="E")
      rescat = fit$cd
      
      #######bQCD#####################################################
      
      pcax = prcomp((x))
      pcax = pcax$rotation[,1]
      pcay = prcomp((y))
      pcay = pcay$rotation[,1]
      resbqcd = QCCD(cbind(pcax,pcay),7)
      resbqcd = resbqcd$cd
      
      
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
      return(list(cat=rescat,bqcd=resbqcd,pc=respc,spec_error = pc_spec,tp = pc_tp,fp = pc_fp,tn = pc_tn,fn = pc_fn))
      
    }
    
    comp200101 = foreach(i=1:30,.packages = c("rstan","gtools","loo","quantregForest","rvinecopulib","statmod","qrnn","COLP","cluster")) %dopar%
      {
        comp(i)
      }
    comp200101mat = rep(0,3)
    for (i in 1:30) {
      if(is.na(comp200101[[i]][1]))
        comp200101[[i]][1]=0
      comp200101mat[1] = comp200101mat[1]+as.numeric(comp200101[[i]][1])
      comp200101mat[2] = comp200101mat[2]+as.numeric(comp200101[[i]][2])
      comp200101mat[3] = comp200101mat[3]+as.numeric(comp200101[[i]][3])
    }
    load(paste0("simulation_n=',n,'_pi=',signal,'_results.RData"))
    looic = rep(0,30)
    for (i in 1:30) {
      t2 = sum2031[[i]]
      x2yloo = t2$x2yloo
      y2xloo = t2$y2xloo
      looic[i] = ((x2yloo<y2xloo)&(x2yloo<xy0loo))
    }
    cat(paste0('The accuracy of compDAG in the first simulation setting when n = ',n,' and pi = ',signal,' is:', sum(looic)/30))
    cat(paste0('The accuracy of COLP in the first simulation setting when n = ',n,' and pi = ',signal,' is:', comp200101mat[1]/30))
    cat(paste0('The accuracy of bQCD in the first simulation setting when n = ',n,' and pi = ',signal,' is:', comp200101mat[2]/30))
    cat(paste0('The accuracy of PC in the first simulation setting when n = ',n,' and pi = ',signal,' is:', comp200101mat[3]/30))
  }
}