### Feature engineering for SVM
##

svmFeatures = function(index,ret)
{
  sp500=StockData[index,1]
  
  
  
  # lag returns
  res = cbind(ret,lag(ret, type="discrete", n=1)#,lag(ret, type="discrete", n=2)#,lag(ret, type="discrete", n=3)
              #lag(ret, type="discrete", n=4)
              )
  res=res[-c(1),]

  
  colnames(res) = c(
                    "lag1", 
                    "lag2" 
                    #"lag3"
                    #"lag4"#,"lag5"

                    )
  res <- scale(res)
  return(res)
}

## SVM model selection function

hybrid.model.selection <- function(	x, y, 
                                    gamma=10^(-3:0), 
                                    cost=10^(-3:2), 
                                    sampling="boot",  nboot = 15, 
                                    kernel="radial")
{
  
  newSvm = tune( svm,
                 train.x=x,
                 train.y=y,
                 ranges=list( gamma=gamma, cost=cost ),
                 tunecontrol=tune.control( sampling=sampling, nboot=nboot ),
                 kernel=kernel )
  newSvm
  ## The sample size is rather small, we shall use the bootstrap sampling
}

hybridforecast<-function(x,index,period) {

  
  stock.history.rets	<- StockData[index,x]
  stock.history.data	<- svmFeatures(index,stock.history.rets)
  # Do model selection
  print(index[1])
  
  try({

    garchfit <- garchFit(~arma(1,0)+garch(1,1), data=StockData[index,x],
                       cond.dist='std')   
    
    
    
    ## note that only have fitted value till 208
    stock.fitted <- fitted(garchfit)
    stock.res=residuals(garchfit) ## The residual of fitted ARIMA-GARCH model, consider to contain nonlinear patterns

    stock.today.data <- stock.history.data[dim(stock.history.data)[1]-period+1,] 
    stock.today.data <- data.frame(t(unlist(stock.today.data)))
    
    stock.history.data	<- stock.history.data[-c(dim(stock.history.data)[1]-period+1:dim(stock.history.data)[1]),]##
    stock.fitted<-stock.fitted[-c(0:(1+period))] ## 
    stock.res<-stock.res[-c(0:(period))]
    stock.restoday <- stock.res[length(stock.res)]
    stock.res <- stock.res[-c(length(stock.res))]
    stock.history.data <- cbind(stock.history.data,stock.fitted,stock.res)
    arima_prediction <- predict(garchfit, n.ahead = 3)[1,period]
    stock.today.data <- cbind(stock.today.data,arima_prediction,stock.restoday)
    stock.history.rets <- stock.history.rets[-c(0:(1+period))]
    ##Delete one more, since the feature should be the data before residual

    ##Tune the SVM model for the first period input (only once) 
    if (!tune){
      model_selection_result <<- hybrid.model.selection(stock.history.data , stock.history.rets) ##Data 可能是有feature的，搞清楚input output
      print('tuned!')
    }
    
    # Train a SVM model
    stock.svm<-svm(stock.history.data, stock.history.rets,
                   gamma	= model_selection_result$best.parameters$gamma,
                   cost	= model_selection_result$best.parameters$cost,
                   kernel	= "radial")

    # Make prediction

    svm_prediction <- predict(stock.svm,stock.today.data)
    #stock.pred <- arima_prediction + svm_prediction ##Adding up we get the pred
      
    print(svm_prediction)
  })
  return (svm_prediction)##
}



 