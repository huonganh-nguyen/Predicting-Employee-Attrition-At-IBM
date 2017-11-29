#  
#  A collection of functions that are useful 
#  for lasso type of estimators
## 

### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}


### Penalty choice for Quantile Regression
lambda.BC<- function(X, R = 1000, tau = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}
