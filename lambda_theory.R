#  
#  Computed the value of lambda for Lasso
#
# X is the design matrix 
# family = leastsquares, binomial, quantile
## 
lambda.LS <- function(X, alpha = .05, c=1, R = 1000){
  n <- nrow(X)
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}

lambda.qr<- function(X, R = 1000, tau=0.5, tauList = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  ntau <- length(tauList)
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  r <- rep(0,R)
  for k = 1:ntau{ 
  R <- (t(X) %*% (tauList[k] - (U < tauList[k])))/(sigs*sqrt(tauList[k]*(1-tauList[k])))
  rtmp <- apply(abs(R),2,max)
  r <- apply( cbind(rtmp,r),2,max)
  }
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}
