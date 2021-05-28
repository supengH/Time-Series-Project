##This function caculates the exponential decay covariance matrix
exp_decay_cov <- function(index,lambda=0.94){

  data=StockData[1:208,2:p]
  data=rbind(data,stockpred[1:index,])
  days.diff = c(dim(data)[1]:1)
  wgt = lambda^(days.diff)
  wgt = wgt/sum(wgt)
  head(wgt)
  ret.demean = t(t(data)-colMeans(data))
  cov.adj=matrix(rep(0,ncol(ret.demean)*ncol(ret.demean)),ncol(ret.demean))## Initialise a all zero 10¡Á10 matrix.
  for(t in 1:nrow(ret.demean)){
    r.demean.t = matrix(ret.demean[t,],1)
    sigma.t = t(r.demean.t) %*% r.demean.t
    sigma.t = wgt[t] * sigma.t
    cov.adj = cov.adj + sigma.t
  }
  
  return(cov.adj)
}


##This function finds the optimal weights for portfolio
get_max_ratio <- function(cov,rf,index){
  library(Rsolnp)
  mu=stockpred[index,]
  initialw=rep(1/10,10)
  weight <- solnp(
    pars = initialw,
    fun=function(w) ((- t(w) %*% mu)-rf / sqrt( t(w) %*% cov %*% w ))*100,
    ##objective function
    eqfun = function(w) sum(w),
    eqB   = 1,
    LB = rep(-0.1,10),
    UB = rep(0.5,10),
    control = list((outer.iter=5),inner.iter=5)
  )
  return(weight$pars)
}
