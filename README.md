# Time-Series-Project
## This is the project of STA 4003 Time Series from the Chinese University of Hong Kong, Shenzhen.
### 1. Introduction
In this project, a dataset which contains weekly log
returns of SP500 index and 10 stocks for n1 + n2 = 260 weeks is provided, n1 is for trainning and n2 is for testing.
In this project, I need to finish two main tasks. First is forecasting and the second is to construct a portfolio with maximum sharpe ratio.

### 2. Exploratory analysis
In this part, we first plot the return as well as its ACF/PACF. Since we need a stationary time series for the ARMA and the following hybrid model, adf test is used to determine the order of differencing.
### 3. ARMA model
In this part, we first use the AIC standard to choose the order of p,q. Then we do model diagnosis, note that the model with minimum AIC is not neccessary an appropriate model for the series. We need to do diagnosis and check for the independence and normality of residuals. Based on the pattern of the residuals, we also fit a GARCH model.
### 4. Hybrid model
The problem with the ARMA and GARCH model is that they assume a linear relationship among variables and thus can only capture very little trend movement, its prediction is very close to the mean of the return. Therefore, to better predict the trend movement, we consider adding the Support Vector Machine to capture the nonlinear component in the trend movement.
In this part, I propose two hybrid models with details in the report. The hybrid model actually cannot improve much accurancy.
### 5. Portfolio construction
Based on the forecast we get from the hybrid model, we use the Markowitz’s Modern Portfolio Theory to maximize the Sharpe Ratio. I have also applied a exponential decay function to the sample covariance so that weights assigned to observations are declining as they go further back in time. 

The picture in numbers are forecasting plots.
