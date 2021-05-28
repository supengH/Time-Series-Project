###Below are the functions for different models:
### Most of them are traditional ARIMA models and their extensions,
### The performance is not so good. 

##### Execute here #####
##Exponential Smoothing 
sesforcast=function(x,index,period){
  ses1=ses(StockData[index,x],alpha = 0.2,h=3)
  return (ses1$mean[1]) 
}

### Rolling###
##auto-arima

model=arima(StockData[,2],order=c(2,0,0))
model
for (i in 2:11){
  print(auto.arima(StockData[,i]),ic = "aic")
}

summary(model$ttable)



## Kernalsmoothing 
kerneltsforecast=function(x,index,period){
  kernalsmooth=ksmooth(time(StockData[index,x]), StockData[index,x],"normal", bandwidth=4)$y
  pred=sarima.for(kernalsmooth,n.ahead = period,p=2,d=0,q=0)$pred[period]
  return (pred) 
}

## ARIMA model
tsforecast=function(x,index,period){
  pred=sarima.for(StockData[index,x],n.ahead = period,p=1,d=0,q=0)$pred[period]
  return (pred) 
}

## ARMA-GARCH model
tsforecastga=function(x,index,period){
  garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                          variance.model=list(garchOrder=c(1,1)))
  garch.norm.1 = ugarchfit(data=StockData[index,x], spec=garch.norm,solver.control = list(tol = 1e-12),solver = "hybrid")
  pred1 = ugarchforecast(garch.norm.1, data = StockData[index,x],
                         n.ahead = period)
  return ((fitted(pred1)[period])) 
}

## ARIMA model with extra more independent variables (sp500 index)
tsforecastsp500=function(index){
  sp=StockData[index,1]
  sp = data.frame(matrix(unlist(sp), nrow=length(sp), byrow=TRUE))
  pred=sarima.for(StockData[index,1],n.ahead = 1,p=0,d=0,q=1)$pred[1]
  return (rbind(sp,pred))
}

tsforecastwr=function(x,index,period){
  sppred=sarima.for(StockData[index,1],n.ahead = period,p=0,d=0,q=1)$pred[period]
  pred=sarima.for(StockData[index,x],n.ahead = period,p=0,d=0,q=1,xreg=StockData[index,1],newxreg = sppred)$pred[period]
  return (pred) 
}

## A search for best p,d,q based on AIC
arima.model.selection <- function (rets, p.max, d.max, q.max)
{
  best.aic <- 1e9
  best.model <- NA
  best.p <- NA
  best.d <- NA
  best.q <- NA
  
  aics <- array(NA, dim = c(p.max+1, d.max+1, q.max+1) )
  for (p in 0:p.max) {	
    for (d in 0:d.max) {
      for (q in 0:q.max) {
        #print(sprintf("Model selection... p = %d, d = %d, q = %d", p, d, q))
        flush.console()
        
        r <- list(aic=1e9)
        
        try( r <- arima(rets, order=c(p,d,q)) )
        
        aics[p+1, d+1, q+1] <- r$aic
        
        if (r$aic < best.aic)
        {
          best.aic <- r$aic
          best.model <- r
          best.p <- p
          best.d <- d
          best.q <- q
          print(sprintf("aic=%f, p=%d, d=%d, q=%d", r$aic, p, d, q))
        }
      }
    }
  }
  
  list(aics = aics, best.aic = best.aic, 
       best.model = best.model, 
       best.p = best.p, best.d = best.d, best.q = best.q)
}
#for (i in 2:11){
#  arima.model.selection(StockData[0:208,i],5,1,5)
#}

