generate_latent_comp_data <- function(n,px,py,signal,missing,prop,seed){
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
  x <- matrix(0,px,n)
  for (i in 1:n) {
    ind = sample(1:px,floor(prop*px))
    x[ind,i] = rdirichlet(1,rep(1,length(ind)))
    # x[-ind,n] = 0
  }

  if (missing=="truncate"){
    temp =signal * m %*% x+0.1
    for (i in 1:n) {
      y[,i] = rdirichlet(1,temp[,i])
      y[,i][which(y[,i]<quantile(y[,i],1-prop))] = 0
    }
  }else{
    for (i in 1:n) {
      indy = sample(1:py,floor(prop*py))
      temp = signal*m[indy,]%*%x[,i]+0.1
      y[indy,i] = rdirichlet(1,temp)
      y[-indy,i] = 0
    }
  }
  x = as.matrix(impRZilr(t(10000*x),dl = rep(0.5, nrow(x)))$x)
  x = t((x)/rowSums(x))
  y = as.matrix(impRZilr(t(10000*y),dl = rep(0.5, nrow(y)))$x)
  y = t((y)/rowSums(y))
  return(list(x=x,y=y))
}
