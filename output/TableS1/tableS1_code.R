
for (signal in c(0.3,0.5,1,3,5)) {
  for (n in c(100,150)) {
    # rm(list = ls())
    load(paste0("misspecified_n=',n,'_pi=',signal,'_results.RData"))
    mr = array(0,dim = c(40,30,30))
    cr = rep(0,30)
    ar = rep(0,30)
    mur = rep(0,30)
    for (i in 1:30) {
      t2 = sum2031[[i]]
      m = t2$m
      me = t2$me
      ce = t2$ce
      ae = t2$ae
      mue = t2$mue
      mr[,,i] = m-me
      cr[i] = signal-ce
      ar[i] = 0.1-ae
      mur[i] = 0.1-mue
    }
    mrr = apply(mr, 3, function(x){norm(x,type = "F")^2})/norm(m,"F")^2
    crr = cr^2/(signal^2)
    murr = mur^2/(0.1^2)
    arr = ar^2/(0.1^2)
    cat(paste0('The relative error of compDAG when n = ',n,' and pi = ',signal,' are: M:',mean(mrr),'(',sd(mrr),'); pi:',mean(crr),'(',sd(crr),'); mu_1:',mean(arr),'(',sd(arr),'); mu_2:',mean(murr),'(',sd(murr),').'))
  }
}