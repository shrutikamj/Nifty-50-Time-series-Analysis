#STEP 1 - Loading the required packages -
library(ggplot2)
library(tseries)
library(TSstudio)
library(TTR)  
library(rugarch)
library(foreach)
library(TSPred)
library(forecast)

#STEP 2 - Retrieving, viewing and identifying the data 
nifty_returns <- read.csv("D:\\Reference books FYBA - SY_M.Sc\\Assignments & Projects\\Sem 2\\Econometrics\\Group assignment -NIFTY 50-Time series Analysis\\Nifty Returns.csv")
View(nifty_returns)
str(nifty_returns)

#creating object vector to analyze it further -
returns <- nifty_returns$Returns

#STEP 3 - Converting to Time Series and plotting - 
NIFTY_ret <- ts(data = returns, dates, start = c(2018,1),
              end = c(2020,31), frequency = 365)
dates <- as.Date(c("2018-01-01 - 2020-12-30"))
View(NIFTY_ret)

#STEP 4 -PLOTTING - 
ts.plot(NIFTY_ret)
#no specific trend. But we can see too much volatility. 

#plotting histogram - 
hist(NIFTY_ret, x =)
#normal distribution 

#STEP 5 - Diagnostic Tests - Stationary, ACF, PACF -
#ADF -  H0- Non stationary, H1 - Stationary.
#Checking for stationary/unit root test -
adf.test(NIFTY_ret, alternative = c("stationary", "explosive"), k = 1)
#p<0.05 hence we can reject the H0. The data is stationary.

#ACF and PACF -
acf(NIFTY_ret)
pacf(NIFTY_ret)

#Auto Arima -
fit_arima <- auto.arima(NIFTY_ret)
fit_arima
fcst <- forecast(fit_arima, h=5)
autoplot(fcst) + autolayer(NIFTY_ret)


#STEP 6 - Model estimation- 1-ARMA(p,q) ARIMA(p,d,q)
arima(NIFTY_ret, c(1,0,1)) #least AIC. #p=1 I=0 Q=1
arima(NIFTY_ret, c(1,0,0))
arima(NIFTY_ret, c(2,0,0))
arima(NIFTY_ret, c(2,0,1))


#Least AIC 
arimafinal <- arima(NIFTY_ret, c(1,0,1))
par(mar = c(3,3,3,3))
tsdiag(arimafinal)
#AIC, SBC -
AIC(arimafinal)
BIC(arimafinal)

#coeefficients -
arimafinal 
#Plotting the residuals - 
residual <- (arimafinal$residuals)^2
plot(residual)

#Out of sample forecast -
predicted <- predict(arimafinal, n.ahead = 10)
predicted

#2 GARCH - 
ugarchspec()
garchspec <- ugarchspec(mean.model = list(armaOrder = c(1,0,1)))

modelgarch <-ugarchfit(spec = garchspec, data = NIFTY_ret) 
modelgarch
plot(modelgarch)
10
11
12
4
5
6
7
8
9
0
ugresidual <- (modelgarch@fit$residuals)^2
plot(ugresidual, type ="l", title(main = "Residuals" , xlab = "Days(T)", ylab = "ui_hat^2"))


#forecast -
forecastgarch <- ugarchforecast(fitORspec = modelgarch, out.sample = 10)
forecastgarch
#conditional volatilty using Sigma - 
forecastgarch1 <- forecastgarch@forecast$sigmaFor
plot(forecastgarch1)
#increase in conditional volatility i.e. the variance is assumed to increase for next 10 days.


actual <-ts.plot(NIFTY_ret)
pred <- predict(arimafinal, n.ahead = 30)
ts.plot(NIFTY_ret,pred$pred)


f <- fitted.values(fit_arima)
fitted_values <- nifty_returns$Fitted
actual_values <- nifty_returns$Returns
time <- nifty_returns$Date

#SUPERIMPOSED -

comb_ts <- cbind(actual_values, fitted_values)
plot.ts(comb_ts)

plot(actual_values)
lines(fitted_values, col ="red")


#PLOTTING ARIMA PREDICTED - 
plotarimapred(NIFTY_ret, fit_arima, xlim = c(2018,2021), xreg = fcst)




  


