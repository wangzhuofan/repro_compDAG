library(ExtDist)
generate_lingam_comp_data <- function(n,px,py,signal,seed){
  set.seed(1)
  # n = 100; px = 30; py = 40;signal = 5
  m = matrix(0,py,px)
  for (colind in 1:px) {
    randNum = sample(1:3,1,replace = FALSE)
    randLoc = sample(1:py,randNum,replace = FALSE)
    m[randLoc,colind] = 1/randNum
  }
  set.seed(seed)
  # x = t(rdirichlet(n,rep(0.1,px)))
  y <- matrix(0,py,n)
  x <- matrix(rLaplace(px*n),px,n)
  noises <- matrix(rLaplace(py*n),py,n)
  
  y =signal * m %*% x+noises
  x = exp(x)
  y = exp(y)
  y = t(t(y)/colSums(y))
  x = t(t(x)/colSums(x))
  return(list(x=x,y=y))
}