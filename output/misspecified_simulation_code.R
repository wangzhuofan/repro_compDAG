library("doParallel")   
library("foreach") 
cl<- makeCluster(8) 
registerDoParallel(cl) 

coal <- function(i,signal,n){
  model_dd = stan_model('coalescence.stan')
  set.seed(1)
  px = 30; py = 40
  m = matrix(0,py,px)
  for (colind in 1:px) {
    randNum = sample(1:3,1,replace = FALSE)
    randLoc = sample(1:py,randNum,replace = FALSE)
    m[randLoc,colind] = 1/randNum
  }
  set.seed(i)
  x = t(rdirichlet(n,rep(0.1,px)))
  y <- matrix(0,py,n)
  e = t(rdirichlet(n,rep(1,py)))
  y =(signal/(1+signal)) * m %*% x+(1/(1+signal))*e

  dd_xy <- list(n = n, px = px, py = py, x = x, y = y)
  dd_yx <- list(n = n, px = py, py = px, x = y, y = x)
  
  fitxy = sampling(model_dd,dd_xy,iter=5000,chains=1,seed=1)
  fityx = sampling(model_dd,dd_yx,iter=5000,chains=1,seed=1)
  model_dd = stan_model('nocausal.stan')
  fitxy0 = sampling(model_dd,dd_xy,iter=5000,chains=1,seed=1)
    
  paramsxy = extract(fitxy)
  paramsyx = extract(fityx)
  paramsxy0 = extract(fitxy0)
  
  
  # 
  sam_mestxy <- apply(paramsxy$mt,c(2,3),mean)
  cestxy <- mean(paramsxy$c)
  muestxy <- mean(paramsxy$mu)
  # 
  llxy = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    llxy[i,] = log(ddirichlet(t(x),rep(paramsxy$a[i],nrow(x))))+(log(ddirichlet(t(y),t(paramsxy$c[i]*paramsxy$mt[i,,]%*%x+paramsxy$mu[i]))))
  }
  
  x2yloo = loo(llxy)
  llyx = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    llyx[i,] = log(ddirichlet(t(y),rep(paramsyx$a[i],nrow(y))))+(log(ddirichlet(t(x),t(paramsyx$c[i]*paramsyx$mt[i,,]%*%y+paramsyx$mu[i]))))
  }
  
  y2xloo = loo(llyx)
  
  ll0 = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    ll0[i,] = log(ddirichlet(t(x),rep(paramsxy0$a[i],nrow(x))))+(log(ddirichlet(t(y),rep(paramsxy0$mu[i],nrow(y)))))
  }
  
  xy0loo = loo(ll0)
  return(list(fitxy=fitxy,fityx=fityx,paramsxy=paramsxy,paramsyx=paramsyx,sam_mestxy=sam_mestxy,cestxy=cestxy,muestxy=muestxy,x2yloo=x2yloo,y2xloo=y2xloo,xy0loo = xy0loo))
}
for (signal in c(0.3,0.5,1,3,5)) {
  for (n in c(100,150)) {
    simulation30501 = foreach(i=1:30,.packages = c("rstan","gtools","loo")) %dopar%
      {
        coal(i=i,signal=signal,n = n)
      }
    sum2031 = list()
    for (i in 1:30) {
      t1 = simulation30501[[i]]
      set.seed(1)
      n = 100; px = 30; py = 40
      m = matrix(0,py,px)
      for (colind in 1:px) {
        randNum = sample(1:3,1,replace = FALSE)
        randLoc = sample(1:py,randNum,replace = FALSE)
        m[randLoc,colind] = 1/randNum
      }
      ae = mean(t1$paramsxy$a)
      res = list(m=m,me=t1$sam_mestxy,ae = ae,ce = t1$cestxy,mue = t1$muestxy,x2yloo=t1$x2yloo$looic,y2xloo=t1$y2xloo$looic,xy0loo=t1$xy0loo$looic)
      sum2031[[i]]=res
    }
    save(sum2031,paste0("misspecified_n=',n,'_pi=',signal,'_results.RData"))
  }
}

