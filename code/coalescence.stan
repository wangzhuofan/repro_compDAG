data {
  int<lower = 0> n;
  int<lower = 0> px;
  int<lower = 0> py;
  
  matrix[px,n] x;
  matrix[py,n] y;
}

parameters {
  real a;
  real c;
  real mu;
  //vector<lower=0>[py] b;
  simplex[py] m[px];
}
transformed parameters {
  // real<lower=0,upper=1> cstar;
  matrix[py,px] mt;matrix[py,n] mx;
  for(i in 1:px){
    mt[,i] = m[i];
  }
  mx = mt*x;
  // bound = y ./mx;
  // cstar = c;
  // if(cstar>min(bound)) cstar = c*min(bound);
  // for(i in 1 : n) {
  //   rr[i] = y[,i]-cstar*mx[,i];
  //   rr[i]=rr[i]/(1-cstar);
  // }
}
model {
  //b~gamma(1,1);
  a~gamma(1,1);
  c~gamma(1,1);
  mu~gamma(1,1);
  for(j in 1:px) m[j]~dirichlet(rep_vector(1.0/py,py));
  for(i in 1:n){
    x[,i] ~ dirichlet(rep_vector(a,px));
    target+= dirichlet_lpdf( y[,i] |(c*mx[,i]+mu));
    // target+= 0-py*log(1-cstar);
  } //rr[i]~dirichlet(rep_vector(1,py));
     
}

