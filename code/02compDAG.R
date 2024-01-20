# compile stan file
# library(rstan)

# prepare data
compDAG <- function(x,y,param1,param2){
  px = nrow(x)
  py = nrow(y)
  n = ncol(x)
  dd_xy <- list(n = n, px = px, py = py, x = x, y = y, param = param1)
  dd_yx <- list(n = n, px = py, py = px, x = y, y = x, param = param2)
  #  HMC posterior sampling
  model_dd = stan_model('./code/02-1coalescence.stan')
  fitxy = sampling(model_dd,dd_xy,iter=5000,chains=1,seed=1)
  fityx = sampling(model_dd,dd_yx,iter=5000,chains=1,seed=1)
  model_dd = stan_model('./code/02-2nocausal.stan')
  dd_xy0 <- list(n = n, px = px, py = py, x = x, y = y)
  fit0 = sampling(model_dd,dd_xy0,iter=5000,chains=1,seed=1)
  # get posterior samples
  paramsxy = extract(fitxy)
  paramsyx = extract(fityx)
  params0 = extract(fit0)
  # mu_x = mean(params0$a)
  # mu_y = mean(params0$mu)
  # paramy = as.integer(mu_y*100)/100
  # paramx = as.integer(mu_x*100)/100
  # get estimations
  mEstxy <- apply(paramsxy$mt,c(2,3),mean)
  signalEstxy <- mean(paramsxy$c)
  mu1Estxy <- mean(paramsxy$a)
  # calculate LOOIC for two directions
  llxy = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    llxy[i,] = log(ddirichlet(t(x),rep(paramsxy$a[i],nrow(x))))+(log(ddirichlet(t(y),t(paramsxy$c[i]*paramsxy$mt[i,,]%*%x+0.1))))
  }
  x2yloo = loo(llxy)$looic
  llyx = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    llyx[i,] = log(ddirichlet(t(y),rep(paramsyx$a[i],nrow(y))))+(log(ddirichlet(t(x),t(paramsyx$c[i]*paramsyx$mt[i,,]%*%y+0.1))))
  }
  y2xloo = loo(llyx)$looic
  ll0 = matrix(0,nrow = 2500,ncol = ncol(x))
  for (i in 1:2500) {
    ll0[i,] = log(ddirichlet(t(x),rep(params0$a[i],nrow(x))))+(log(ddirichlet(t(y),rep(params0$mu[i],nrow(y)))))
  }
  x0yloo = loo(ll0)$looic
  # cd=1:causal direction is from x to y, cd=-1:causal direction is from y to x, cd=0:there is no causal relationship between y and x.
  if (x2yloo==min(c(x2yloo,y2xloo,x0yloo)))
    cd = 1
  if (y2xloo==min(c(x2yloo,y2xloo,x0yloo)))
    cd = -1
  if (x0yloo==min(c(x2yloo,y2xloo,x0yloo)))
    cd = 0
  return(list(cd = cd,mEst = mEstxy,signalEst = signalEstxy,mu1Est = mu1Estxy,x2y=x2yloo,y2x=y2xloo,x0y = x0yloo))
}

