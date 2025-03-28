model0 = stan_model('./code/02-5-0compDAG.stan')
model1 = stan_model('./code/02-5-1compDAG.stan')
model2 = stan_model('./code/02-5-2compDAG.stan')
compReg3 = function(y,x,iters,mu1,mu2,mu3) {
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
                  mu2=mu2)
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
                  mu3=mu3)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {

    }, finally = {

    })
  }
  return(logl)
}

calLOOIC = function(logll){
  sumlogll = apply(logll, c(2,3), sum)
  tryCatch({looic = loo(sumlogll)$looic
  }, error = function(e) {

  }, finally = {

  })
  return(looic)
}

multiCompDAG3 <- function(nComm,pdata,y,mu1=0.09,mu2=0.09,mu3=0.09){
  if(nComm ==3){
    gam_list = array(0, c(3, 3, 25))
    gam_list[1, 2, 2] = 1
    gam_list[1, 3, 3] = 1
    gam_list[2, 3, 4] = 1
    gam_list[2, 1, 5] = 1
    gam_list[3, 1, 6] = 1
    gam_list[3, 2, 7] = 1
    gam_list[1, 2, 8] = gam_list[1, 3, 8] = 1
    gam_list[2, 3, 9] = gam_list[2, 1, 9] = 1
    gam_list[3, 1, 10] = gam_list[3, 2, 10] = 1

    gam_list[2, 1, 11] = gam_list[3, 1, 11] = 1
    gam_list[3, 2, 12] = gam_list[1, 2, 12] = 1
    gam_list[1, 3, 13] = gam_list[2, 3, 13] = 1

    gam_list[1, 2, 14] = gam_list[2, 3, 14] = 1
    gam_list[1, 3, 15] = gam_list[3, 2, 15] = 1
    gam_list[2, 3, 16] = gam_list[3, 1, 16] = 1
    gam_list[2, 1, 17] = gam_list[1, 3, 17] = 1
    gam_list[3, 2, 18] = gam_list[2, 1, 18] = 1
    gam_list[3, 1, 19] = gam_list[1, 2, 19] = 1

    gam_list[1, 2, 20] = gam_list[1, 3, 20] = gam_list[2, 3, 20] = 1
    gam_list[1, 2, 21] = gam_list[1, 3, 21] = gam_list[3, 2, 21] = 1
    gam_list[2, 1, 22] = gam_list[2, 3, 22] = gam_list[1, 3, 22] = 1
    gam_list[2, 1, 23] = gam_list[2, 3, 23] = gam_list[3, 1, 23] = 1
    gam_list[3, 1, 24] = gam_list[3, 2, 24] = gam_list[1, 2, 24] = 1
    gam_list[3, 1, 25] = gam_list[3, 2, 25] = gam_list[2, 1, 25] = 1

    looicList = rep(0,25)
    # parsList = list()
    n = ncol(y[[1]])
    q=3
    iters = 5000
    for (graph in 1:dim(gam_list)[3]) {
      # looic=0
      logll = array(0,dim = c(q,0.5*iters,n))
      for (i in 1:q) {
        logll[i,,] = compReg3(y[[i]], y[which(gam_list[i,,graph]!=0)],iters = iters,mu1,mu2,mu3)
      }
      ic = calLOOIC(logll)
      looicList[graph] = ic
      # parsList[[graph]] = pars
    }
  }
  return(looicList)
}
# getmodemax <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
