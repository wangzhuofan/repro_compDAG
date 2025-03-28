# compile stan file
# library(rstan)

# prepare data
compDAG_param <- function(x,y,paramx,paramy){
  px = nrow(x)
  py = nrow(y)
  n = ncol(x)
  model0 = stan_model('./code/02-5-0compDAG.stan')
  model1 = stan_model('./code/02-5-1compDAG.stan')
  #fit baseline
  datax <- list(n=n,p1=px,y1=x)
  fitx <- sampling(model0,datax,iter=5000,chains=1,seed=1,thin = 1)
  paramsx <- extract(fitx)
  datay <- list(n=n,p1=py,y1=y)
  fity <- sampling(model0,datay,iter=5000,chains=1,seed=1,thin = 1)
  paramsy <- extract(fity)

  #fit regression
  dataxy <- list(n=n,p1=px,p2=py,y1=x,y2=y,mu2=paramx)
  fitxy <- sampling(model1,dataxy,iter=5000,chains=1,seed=1,thin = 1)
  paramsxy = extract(fitxy)
  datayx <- list(n=n,p1=py,p2=px,y1=y,y2=x,mu2=paramy)
  # fityx <- sampling(model1,datayx,iter=5000,chains=1,seed=1,thin = 1)
  # paramsyx = extract(fityx)
  #
  #calculate likelihood
  # loglx = paramsx$log_lik
  # logly = paramsy$log_lik
  # logxy = paramsxy$log_lik
  # logyx = paramsyx$log_lik
  #
  # logx2y = loglx+logxy
  # logy2x = logly+logyx
  # logx0y = loglx+logly
  #
  # x2yloo = loo(logx2y)$looic
  # y2xloo = loo(logy2x)$looic
  # x0yloo = loo(logx0y)$looic
  #
  # # cd=1:causal direction is from x to y, cd=-1:causal direction is from y to x, cd=0:there is no causal relationship between y and x.
  # if (x2yloo==min(c(x2yloo,y2xloo,x0yloo)))
  #   cd = 1
  # if (y2xloo==min(c(x2yloo,y2xloo,x0yloo)))
  #   cd = -1
  # if (x0yloo==min(c(x2yloo,y2xloo,x0yloo)))
  #   cd = 0
  #
  # # get estimations
  # mEstxy <- apply(paramsxy$mt1,c(2,3),mean)
  # signalEstxy <- mean(paramsxy$pi1)
  # mu1Estxy <- mean(paramsxy$a)
  # calculate LOOIC for two directions


  return(paramsxy)
}

