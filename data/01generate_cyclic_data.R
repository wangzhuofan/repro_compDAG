# library(gtools)
generate_cyclic_data<- function(n,px,py,signalxy,signalyx,seed){
  set.seed(1)
  # n = 100; px = 30; py = 40;signal = 5
  myx = matrix(0,py,px)
  for (colind in 1:px) {
    randNum = sample(1:3,1,replace = FALSE)
    randLoc = sample(1:py,randNum,replace = FALSE)
    myx[randLoc,colind] = 1/randNum
  }
  mxy = matrix(0,px,py)
  for (colind in 1:py) {
    randNum = sample(1:3,1,replace = FALSE)
    randLoc = sample(1:px,randNum,replace = FALSE)
    mxy[randLoc,colind] = 1/randNum
  }
  m = cbind(matrix(0,px,px),(signalxy/(signalxy+1)) * mxy)
  m = rbind(m,cbind((signalyx/(signalyx+1)) * myx,matrix(0,py,py)))
  set.seed(seed)
  xm = matrix(0,px,n)
  ym = matrix(0,py,n)
  for (i in 1:n) {
    ey = (1/(signalyx+1))*t(rdirichlet(1,rep(1,py)))
    ex = (1/(signalxy+1))*t(rdirichlet(1,rep(1,px)))
    e = c(ex,ey)
    xy = solve((diag(px+py)-m),e)
    xm[,i] = xy[1:px]
    ym[,i] = xy[-(1:px)]
  }
  # y = (signal/(signal+1)) * m %*% x+(1/(signal+1))*e
  return(list(x=xm,y=ym))
}
