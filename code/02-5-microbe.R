model0 = stan_model('./code/02-5-0compDAG.stan')
model1 = stan_model('./code/02-5-1compDAG.stan')
model2 = stan_model('./code/02-5-2compDAG.stan')
model3 = stan_model('./code/02-5-3compDAG.stan')
model4 = stan_model('./code/02-5-4compDAG.stan')
compRegm = function(y,x,iters,rparam) {
  number = length(x)
  if(number==0){
    tryCatch({
      model = model0
      data = list(n = ncol(y),
                  p1 = nrow(y),
                  y1=y)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  if(number==1){
    tryCatch({
      model = model1
      data = list(n = ncol(y),
                  p1 = nrow(x[[1]]),
                  p2 = nrow(y),
                  y1=x[[1]],
                  y2=y,
                  mu2=rparam)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  if(number==2){
    tryCatch({
      model = model2
      data = list(n = ncol(y),
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]),
                  p3 = nrow(y),
                  y1=x[[1]],
                  y2=x[[2]],
                  y3 = y,
                  mu3=rparam)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  if(number==3){
    tryCatch({
      model = model3
      data = list(n = ncol(y),
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]),
                  p3 = nrow(x[[3]]),
                  p4 = nrow(y),
                  y1 = x[[1]],
                  y2 = x[[2]],
                  y3 = x[[3]],
                  y4 = y,
                  mu4=rparam)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  if(number==4){
    tryCatch({
      model = model4
      data = list(n = ncol(y),
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]),
                  p3 = nrow(x[[3]]),
                  p4 = nrow(x[[4]]),
                  p5 = nrow(y),
                  y1 = x[[1]],
                  y2 = x[[2]],
                  y3 = x[[3]],
                  y4 = x[[4]],
                  y5 = x[[5]],
                  mu5=rparam)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  return(pars)
}



microbe_fit <- function(y,i,rparam){
  pars_list = list()
  if(length(y)==3){
    gam = matrix(0,3,3)
    gam[2,1] = 1
    gam[3,2] = 1
    q=3

    iters=5000
    for (i in 1:q) {
      pars_list[[i]] = compRegm(y[[i]], y[which(gam[i,]!=0)],iters = iters,rparam)
    }
  }
  if(length(y)==5){
    load("./output/simulation_results/simulation-noisetype-Multi5-signal-5-n-150-method-Multi5.RData")
    gam = result[[i]]$gam
    q=5
    iters=2000

    for (i in 1:q) {
      pars_list[[i]] = compRegm(y[[i]], y[which(gam[i,]!=0)],iters = iters,rparam)
    }
  }


  return(pars_list)
}
