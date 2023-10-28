# compile stan file
library(rstan)

# prepare data
dd_xy <- list(n = n, px = px, py = py, x = x, y = y)
dd_yx <- list(n = n, px = py, py = px, x = y, y = x)
#  HMC posterior sampling
model_dd = stan_model('coalescence.stan')
fitxy = sampling(model_dd,dd_xy,iter=5000,chains=1,seed=1)
fityx = sampling(model_dd,dd_yx,iter=5000,chains=1,seed=1)
model_dd = stan_model('nocausal.stan')
fit0 = sampling(model_dd,dd_xy,iter=5000,chains=1,seed=1)
# get posterior samples
paramsxy = extract(fitxy)
paramsyx = extract(fityx)
params0 = extract(fit0)
# get estimations
sam_mestxy <- apply(paramsxy$mt,c(2,3),mean)
cestxy <- mean(paramsxy$c)
muestxy <- mean(paramsxy$mu)
# calculate LOOIC for two directions
llxy = matrix(0,nrow = 2500,ncol = ncol(x))
for (i in 1:2500) {
  llxy[i,] = log(ddirichlet(t(x),rep(paramsxy$a[i],nrow(x))))+(log(ddirichlet(t(y),t(paramsxy$c[i]*paramsxy$mt[i,,]%*%x+paramsxy$mu[i]))))
}
x2yloo = loo(llxy)$looic
llyx = matrix(0,nrow = 2500,ncol = ncol(x))
for (i in 1:2500) {
  llyx[i,] = log(ddirichlet(t(y),rep(paramsyx$a[i],nrow(y))))+(log(ddirichlet(t(x),t(paramsyx$c[i]*paramsyx$mt[i,,]%*%y+paramsyx$mu[i]))))
}
y2xloo = loo(llyx)$looic
ll0 = matrix(0,nrow = 2500,ncol = ncol(x))
for (i in 1:2500) {
  ll0[i,] = log(ddirichlet(t(x),rep(params0$a[i],nrow(x))))+(log(ddirichlet(t(y),rep(params0$mu[i],nrow(y)))))
}
x0yloo = loo(ll0)$looic
# cd=1:causal direction is from x to y, cd=-1:causal direction is from y to x, cd=0:there is no causal relationship between y and x.
if (x2yloo==max(c(x2yloo,y2xloo,x0yloo)))
  cd = 1
if (y2xloo==max(c(x2yloo,y2xloo,x0yloo)))
  cd = -1
if (x0yloo==max(c(x2yloo,y2xloo,x0yloo)))
  cd = 0