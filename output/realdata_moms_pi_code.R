ls()
rm(list=ls())
library(HMP2Data)
library(phyloseq)
library(SummarizedExperiment)
library(MultiAssayExperiment)
library(dplyr)
library(ggplot2)
library(UpSetR)
load("preprocessed_moms_pi_data.RData")
dd_data <- list(n = ncol(vagina_scale), px = nrow(vagina_scale), py = nrow(cervix_of_uterus_scale), x = vagina_scale, y = cervix_of_uterus_scale)
library(rstan)
model_dd = stan_model('coalescence.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
params = extract(fit)

library(gtools)
ll = matrix(0,nrow = 2500,ncol = ncol(vagina_scale))
for (i in 1:2500) {
  ll[i,] = log(ddirichlet(t(vagina_scale),rep(params$a[i],nrow(vagina_scale))))+(log(ddirichlet(t(cervix_of_uterus_scale),t(params$c[i]*params$mt[i,,]%*%vagina_scale+params$mu[i]))))
}

dd_data <- list(n = ncol(vagina_scale), py = nrow(vagina_scale), px = nrow(cervix_of_uterus_scale), y = vagina_scale, x = cervix_of_uterus_scale)
library(rstan)
model_dd = stan_model('coalescencezhuofan2.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
params2 = extract(fit)

library(gtools)
ll2 = matrix(0,nrow = 2500,ncol = ncol(vagina_scale))
for (i in 1:2500) {
  ll2[i,] = log(ddirichlet(t(cervix_of_uterus_scale),rep(params2$a[i],nrow(cervix_of_uterus_scale))))+(log(ddirichlet(t(vagina_scale),t(params2$c[i]*params2$mt[i,,]%*%cervix_of_uterus_scale+params2$mu[i]))))
}
dd_data <- list(n = ncol(vagina_scale), px = nrow(vagina_scale), py = nrow(cervix_of_uterus_scale), x = vagina_scale, y = cervix_of_uterus_scale)
library(rstan)
model_dd = stan_model('nocausal.stan')
fit = sampling(model_dd,dd_data,iter=5000,chains=1,cores = parallel::detectCores(),seed=1)
params5 = extract(fit)
library(gtools)
ll5 = matrix(0,nrow = 2500,ncol = ncol(vagina_scale))
for (i in 1:2500) {
  ll5[i,] = log(ddirichlet(t(vagina_scale),rep(params5$a[i],nrow(vagina_scale))))+(log(ddirichlet(t(cervix_of_uterus_scale),rep(params5$mu[i],nrow(cervix_of_uterus_scale)))))
}

llu0v = ll5
llu2v = ll2
llv2u = ll
paramsu0v = params5
paramsu2v = params2
paramsv2u = params
u2vlooic = loo(llu2v)$looic
v2ulooic = loo(llv2u)$looic
u0vlooic = loo(llu0v)$looic
m_est <- apply(paramsu2v$mt,c(2,3),mean)
save(u2vlooic,v2ulooic,u0vlooic,m_est,file = "MOMS_PI_results.RData")
