n    = nrow(data)  
y = data 
library(itsmr)
data = airpass
y = data
y = ts(airpass, frequency = 12)
###############################
## out of sample forecasting 
###############################

nf = 20 # number of forecasting step

# Forecasting 
ARMA = matrix(0,nf,1)
for (i in 1:nf){
  y2 = y[1:(n-nf-1+i)] # window size = 
  fit = arima(y2, order = c(2,0,2), seasonal=list(order=c(0,1,2), period=12),include.mean  = TRUE, xreg = NULL, method = "ML")
  fo <- (forecast(fit,h = 1)$mean)
  ARMA[i] = as.numeric(fo)
}
ARMA
sqrt(mean( (data[(n-20+1):n] - ARMA)^2)) 
library(forecast)
forecast(fit, 30)

m=20; n = length(data);
N = n-m;
testindex = (N+1):n;
p=3; q=1; P=1 ; Q=1 
for(i in 1:m){
  trainindex = 1:(N+i-1);
  newex <- X[N+i]
  fit = arima(data_sqrt[trainindex], order=c(1,0,1), seasonal=list(order=c(1,0,1),period=12),include.mean=T);
  Xhat =  forecast(fit, h=1)$mean; #one step이니까 h=1
  err[i] = (data_sqrt[N+i] - Xhat)^2;
}
mean(err)
Xhat


m=20; n = length(data);
N = n-m;
testindex = (N+1):n;
p=3; q=1; P=1 ; Q=1 
data <- ts(data, frequency = 12)
for(i in 1:m){
  trainindex = 1:(N+i-1);
  newex <- X[N+i]
  data_tr <- ts(data[trainindex],frequency = 12) 
  fit = auto.arima(data_tr, xreg = cbind(X[trainindex]),approximation = T);
  Xhat =  forecast(fit, h=1, xreg = cbind(newex))$mean; #one step이니까 h=1
  err[i] = (data[N+i] - Xhat)^2;
}
mean(err)
Xhat
