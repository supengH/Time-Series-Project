
#### Read the data, which contain weekly log return of SP500 index and 10 stocks
require(astsa)
require(tseries)
require(forecast)
require(stlplus)
require(tibbletime)
require(tibble)
require(dplyr)
require(broom)
require(MASS)
require(zoo)
require(e1071)
require(PerformanceAnalytics)
require(MASS)
require(tree)
require(randomForest)
require(fGarch)
require(rugarch)
set.seed(205)


### NOTE ###
##This project use the SVM to create the model
##In the result part of the report, the SVM need to be tuned everytime newdata comes in
## Running the program will take 25-30 minutes!!
## Therefore, I only tune the SVM in this code for simplicity,
## However, this may lead to different result from the report.
## In case this does not work, all the functions are in the rest R files.


setwd("D:/STA")
load("StockData.Rdata")

#StockData <-StockReturn

source('q2.R')
source('hybrid.R')
source('arima.R')

n1 <- 208 #the length of train set
n2 <- 52  
p <- dim(StockData)[2]  # The first column is the log return for SP500


# Testing periods for recording your forecasts
index.test.Step1 <- (n1+1):(n1+n2)
index.test.Step2 <- (n1+2):(n1+n2)
index.test.Step3 <- (n1+3):(n1+n2)

# Variables for mean squared errors
MSE <- rep(0,3) # The mean squared errors for 1-Step, 2-Step, 3-Step forecasts

MSE.Stock <- matrix(rep(0,(p-1)*3),3,(p-1)) # MSE for each stock
########## Your code to build the time series model  
stockpred <- matrix(rep(0,(p-1)*n2),n2,(p-1))

directionerr1= 0
directionerr2=0
directionerr3=0
for (j in 2:p)
{
  Stock  <- StockData[,j]
  step1.forecasts <- rep(0,n2);
  step2.forecasts <- rep(0,n2);
  step3.forecasts <- rep(0,n2);
  tune=FALSE
  for (i in 1:n2)
  {
    index.train <- i:(i+n1-1)  # Moving training windows
      # Your code for building model
      
    step1.forecasts[i] <-hybridforecast(j,index.train,1) # Your 1-step forecast for Stock j on Week i 
    step2.forecasts[i] <-tsforecastga(j,index.train,2)# Your 2-step forecast for Stock j on Week i
    step3.forecasts[i] <-tsforecastga(j,index.train,3)
    tune=TRUE
    # Your 3-step forecast for Stock j on Week i
  }
  
  stockpred[,(j-1)]=step1.forecasts
  
  ##Plotting 
  #png(paste(j,".png",sep=""))
  #plot(tail(StockData[,j],52), cex=0.3, ylab = paste("Stock", toString(j-1))  )
  #lines(tail(StockData[,j],52), )
  #lines(step1.forecasts, col='red')
  #lines( step2.forecasts , col='blue')
  #lines( step3.forecasts, col='green')
  #title(main = "Predicted Weekly Log Return")
  #legend("topright",lty = rep(1,3), col=c("black","red","blue","green"),
  #       c("true","1-step forecast","2-step forecast","3-step forecast"),
  #       cex = 0.7)
  #dev.off()
  MSE.Stock[1,(j-1)] <- mean(( Stock[index.test.Step1]- step1.forecasts)^2) 
  
  MSE.Stock[2,(j-1)] <- mean(( Stock[index.test.Step2]- step2.forecasts[1:(n2-1)])^2) 
  
  MSE.Stock[3,(j-1)] <- mean(( Stock[index.test.Step3]- step3.forecasts[1:(n2-2)])^2)
}

error=StockData[(n1+1):(n1+n2)]-stockpred

######## Your code for Trading stratedy
wgt <- matrix(rep(0,(p-1)*n2),n2,(p-1))
return.stock <- matrix(rep(0,1*n2),n2,1)

for (i in 1:n2){
  wgt[i,] <-get_max_ratio(exp_decay_cov(i,lambda = 0.98),tsforecast(1,1:(n1+i),1),i)
  print(tsforecast(1,1:(n1+i),1))
  return.stock[i,] <- wgt[i,]%*%StockData[(n1+i),2:11]
  
}

######### Output
MSE <- rowMeans(MSE.Stock)
Ex.Return <- return.stock - StockData[(n1+1):(n1+n2),1] # excessive returns over SP500
Total.Return <- sum(Ex.Return)
Var.Return <- var(Ex.Return)
Ratio.Return <- mean(Ex.Return)/sqrt(Var.Return)

print(MSE)
print(Total.Return)
print(Var.Return)
print(Ratio.Return)
