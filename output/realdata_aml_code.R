load("preprocessed_aml_data.RData")
dd_data <- list(n = ncol(oral_scale), px = nrow(stool_scale), py = nrow(oral_scale), x = stool_scale, y = oral_scale)
#dd_data <- list(n = 515, px = 14, py = 14, x = oral_scale, y = stool_scale)
model_dd = stan_model('coalescence.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
# heatmap(x,Rowv = NA, Colv = NA)
# heatmap(y,Rowv = NA, Colv = NA)
params = extract(fit)
on <- rownames(oral_scale)
sn <- rownames(stool_scale)

library(gtools)
ll = matrix(0,nrow = 2500,ncol = ncol(stool_scale))
for (i in 1:2500) {
  ll[i,] = log(ddirichlet(t(stool_scale),rep(params$a[i],nrow(stool_scale))))+(log(ddirichlet(t(oral_scale),t(params$c[i]*params$mt[i,,]%*%stool_scale+params$mu[i]))))
}

dd_data <- list(n = ncol(oral_scale), px = nrow(oral_scale), py = nrow(stool_scale), x = oral_scale, y = stool_scale)
#dd_data <- list(n = 515, px = 14, py = 14, x = oral_scale, y = stool_scale)
model_dd = stan_model('coalescence.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
params2 = extract(fit)

library(gtools)
ll2 = matrix(0,nrow = 2500,ncol = ncol(stool_scale))
for (i in 1:2500) {
  ll2[i,] = log(ddirichlet(t(oral_scale),rep(params2$a[i],nrow(oral_scale))))+(log(ddirichlet(t(stool_scale),t(params2$c[i]*params2$mt[i,,]%*%oral_scale+params2$mu[i]))))
}
library(loo)
o2s = waic(ll2)
loo(ll2)
model_dd = stan_model('nocausal.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
# heatmap(x,Rowv = NA, Colv = NA)
# heatmap(y,Rowv = NA, Colv = NA)
params0 = extract(fit)
library(gtools)
ll0 = matrix(0,nrow = 2500,ncol = ncol(stool_scale))
for (i in 1:2500) {
  ll0[i,] = log(ddirichlet(t(stool_scale),rep(params0$a[i],nrow(stool_scale))))+(log(ddirichlet(t(oral_scale),rep(params0$mu[i],nrow(oral_scale)))))
}
m_est = apply(params$mt,c(2,3),mean)
o2slooic = loo(ll2)$looic
s2olooic = loo(ll)$looic
o0slooic = loo(ll0)$looic
save(m_est,on,sn,o2slooic,s2olooic,o0slooic,file = "AML_results.RData")
