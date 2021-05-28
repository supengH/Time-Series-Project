# Time-Series-Project
## This is the project of STA 4003 Time Series from the Chinese University of Hong Kong, Shenzhen.
### 1. Introduction
In this project, a dataset which contains weekly log
returns of SP500 index and 10 stocks for n1 + n2 = 260 weeks is provided, n1 is for trainning and n2 is for testing.
In this project, I need to finish two main tasks. First is forecasting and the second is to construct a portfolio with maximum sharpe ratio.

### 2. Exploratory analysis
In this part, we first plot the return as well as its ACF/PACF. Since we need a stationary time series for the ARMA and the following hybrid model, adf test is used to determine the order of differencing.
### 3. ARMA model
In this part, we first use the AIC standard to choose the order of p,q. Then we do model diagnosis, note that the model with minimum AIC is not neccessary an appropriate model for the series. 
