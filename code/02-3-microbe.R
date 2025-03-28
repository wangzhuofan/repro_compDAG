model1 = stan_model('./code/02-5-1compDAG.stan')

compReg = function(y,x,iters) {
  number = length(x)
  if(number==1){
    tryCatch({
      model = model1
      data = list(n = ncol(y), 
                  p1 = nrow(x[[1]]),
                  p2 = nrow(y),
                  y1=x[[1]],
                  y2=y,
                  mu2=0.09)
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
microbe_fit <- function(nComm,pdata,y,rparam){
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
  pars_21 <- compReg(y[[2]], y[which(gam_list[2,,18]!=0)],iters = iters)
  pars_32 <- compReg(y[[3]], y[which(gam_list[3,,18]!=0)],iters = iters)
  return(list(pars_21 =pars_21,pars_32=pars_32))
}