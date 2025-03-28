generate_sparse_data<- function(n,px,py,signal,seed){
  set.seed(1)
  # n = 100; px = 30; py = 40;signal = 5
  m = matrix(0,py,px)
  # prob = 0.15
  for (colind in 1:px) {
    randNum = 1
    # u = runif(1)
    # if (u<prob)
    #   randLoc = sample((py/2):py,randNum,replace = FALSE)
    # else
    #   randLoc = sample(1:(py/2),randNum,replace = FALSE)
    m[(py/2),colind] = 1/randNum
  }
  set.seed(seed)
  x = t(rdirichlet(n,rep(0.1,px)))
  y <- matrix(0,py,n)
  temp =signal * m %*% x+0.1
  for (i in 1:n) {
    y[,i] = rdirichlet(1,temp[,i])
  }
  return(list(x=x,y=y,m=m))
}
