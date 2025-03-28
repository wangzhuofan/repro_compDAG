# library(gtools)
generate_confounder_data<- function(n,p,pi,seed){
  set.seed(1)
  # n = 100; px = 30; py = 40;signal = 5
  ncomm = length(p)
  m = list()
  y = list()
  z = matrix(0,p[ncomm],n)
  for (nc in 1:(ncomm-1)) {
    m[[nc]] = matrix(0,p[ncomm],p[nc])
    for (colind in 1:p[nc]) {
      randNum = sample(1:3,1,replace = FALSE)
      randLoc = sample(1:p[ncomm],randNum,replace = FALSE)
      m[[nc]][randLoc,colind] = 1/randNum
    }
  }
  set.seed(seed)
  y[[1]] = t(rdirichlet(n,rep(0.1,p[1])))
  for (nc in 2:2) {
    y[[nc]] <- matrix(0,p[nc],n)
    z = pi[nc-1]*m[[nc-1]]%*%y[[nc-1]]+0.1
    for (i in 1:n) {
      y[[nc]][,i] = rdirichlet(1,z[,i])
    }
    # y[[nc]] = t(rdirichlet(n,rep(0.1,p[nc])))
  }
  for (nc in 3:3) {
    y[[nc]] <- matrix(0,p[nc],n)
    z = pi[nc-1]*m[[nc-1]]%*%y[[nc-1]]+pi[nc-2]*m[[nc-2]]%*%y[[nc-2]]+0.1
    for (i in 1:n) {
      y[[nc]][,i] = rdirichlet(1,z[,i])
    }
    # y[[nc]] = t(rdirichlet(n,rep(0.1,p[nc])))
  }
  
  # y[[ncomm]] <- matrix(0,p[ncomm],n)
  # z =z+0.1
  
  return(list(m=m,y=y))
}


