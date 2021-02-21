armax 
x <- 1:n
const <- rep(1,n) 
time <- 1:(n) 
data_sqrt <- sqrt(data) 
# x <- 1:(n-13)
out_lm <- lm(data_sqrt~1+x)
resi <- out_lm$re
ts.plot(resi)
test(resi)

# 차분하고 arma error 인데 별로임 
x <- 1:(n-1)
out.lm_d1 <- lm(data_sqrt_d1~1+x)
resi <- out.lm_d1$residuals
ts.plot(resi)
test(resi)
const <- rep(1,n-1) # 차분에 맞춰 줄여앟ㄹ떄도 
time <- 1:(n-1) 
X = cbind(const,time)
fit_reg <- arima(data_sqrt_d1, order = c(1,0,1),seasonal = list( order = c(1,0,1), period = 12), 
                 xreg = X, include.mean = F , method = "ML")
test(fit.reg$residuals)


library(forecast)
h = 30
newx = (length(time)+1): (length(time)+h)
plot(predictions <- forecast(fit_reg, h = 30, xreg = cbind(rep(1,h),newx)))
lines(out.lm_d1$fitted.values,col = "red")
# 여기까지 차분하고 arma error 

#######

const <- rep(1,n) # 차분에 맞춰 줄여앟ㄹ떄도 
time <- 1:(n) 
X = cbind(const,time)
data_sqrt_d1 <- diff(data_sqrt,lag =1 )
data_sqrt_d12 <- diff(data_sqrt, lag =12 )
data_sqrt_d12_1 <- diff(diff(data,lag =12),lag = 1)
test(data_sqrt_d12_1)

X = cbind(const,time)
fit_reg <- arima(data_sqrt, order = c(1,0,1),seasonal = list( order = c(1,0,1), period = 12), 
                 xreg = X, include.mean = F , method = "ML")
test(fit_reg$residuals)

X =cbind(time,const)
library(forecast)
h = 30
newx = (length(time)+1): (length(time)+h)
plot(predictions <- forecast(fit_reg, h = 30, xreg = cbind(rep(1,h),newx)))
lines(x,out_lm$fitted.values,col = "red")

predictions 

##########################

const <- rep(1,n) # 차분에 맞춰 줄여앟ㄹ떄도 
time <- 1:(n) 
X = cbind(time)
nf = 20 # number of forecasting step
y = data_sqrt
# Forecasting 
ARMA = matrix(0,nf,1)
for (i in 1:nf){
  y2 = y[1:(n-nf-1+i)] # window size = 
  x2 = X[1:(n-nf-1+i),]
  newx = as.vector( X[(n-nf+i),])
  fit = arima(y2, order = c(1,0,1), seasonal=list(order=c(1,0,1), period=12),include.mean  = T, xreg = x2, method = "ML")
  fo <- (forecast(fit,h = 1, xreg = newx)$mean)
  ARMA[i] = as.numeric(fo)
}
ARMA
(mean( (data_sqrt[(n-20+1):n] - ARMA)^2)) 
ts.plot(data_sqrt[(n-20+1):n])
ts.plot(ARMA)

ts.plot(data[(n-20+1):n])
ts.plot(ARMA^2)

#########################
out.lm<- lm(data_sqrt~x)
library(forecast)
forecast(fit, 30)

mean( (data[(n-20+1):n ]-ARMA^2)^2 )
#############################################
m=30; n = length(data);
N = n-m;
testindex = (N+1):n;
p=3; q=1; P=1 ; Q=1 
err = numeric(m);
for(i in 1:m){
  trainindex = 1:(N+i-1);
  fit = arima(myst[trainindex], order=c(p,0,q),seasonal = list(c(P,0,Q),peroid = 12), include.mean=F );
  Xhat = forecast(fit, h=1)$mean;
  err[i] = (myst[N+i] - Xhat)^2;
}
mean(err)
##################################################
m=20; n = length(data);
N = n-m;
testindex = (N+1):n;
p=3; q=1; P=1 ; Q=1 
err = numeric(m);
X <- cbind(time)
for(i in 1:m){
  trainindex = 1:(N+i-1);
  newex <- X[N+i]
  fit = arima(data_sqrt[trainindex], order=c(1,0,1), seasonal=list(order=c(1,0,1),period=12),include.mean=T, xreg = cbind(X[trainindex]));
  Xhat =  forecast(fit, h=1, xreg = cbind(newex))$mean; #one step이니까 h=1
  err[i] = (data_sqrt[N+i] - Xhat)^2;
}
mean(err)
Xhat


## auto 로 짠 fit 

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
####          
X <- cbind(time,const)
X <- cbind(time)
X <- as.array(X,c(2,n))
data_sqrt <- ts(data, frequency = 12)
fit_auto_x <- auto.arima(data_sqrt, xreg =  (X),approximation = F)
plot(forecast(fit_auto_x,10,xreg = 369:379))
test(fit_auto_x$residuals)

#위는 auto.arima 돌린거고 밑은 그걸로 직접넣은거 

fit__x <- arima(data_sqrt,order= c(3,0,1),seasonal = list(order= c (2,0,1), peroid = 12), xreg = X , method = "CSS",
                    include.mean = T)
test(fit__x$residuals)

############################################################
m=30; n = length(data);
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


for(i in 1:m){
  trainindex = 1:(N+i-1);
  fit = arima(data[trainindex], order=c(p,1,q), seasonal=list(order=c(P,1,Q),period=12));
  Xhat =  forecast(fit, h=1)$mean; #one step이니까 h=1
  err[i] = (data[N+i] - Xhat)^2;
}




diffinv(predictions$mean,l2,1, xi) # 역차분











data_sqrt_d12_1 <- ts(data_sqrt_d12_1, frequency = 12)

fit <- tslm(data_sqrt_d12_1 ~ fourier(data_sqrt_d12_1, K = c(6)))

# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(6), h = 20)))


data_sqrt_d1 <- ts(data_sqrt_d12_1,frequency = 12)
 fit <- auto.arima(data_sqrt_d1, xreg = fourier(data_sqrt_d1, K = c(1,1)))
 fit %>%
  forecast(xreg = fourier(data_sqrt_d1, K = 3, h = 24)) %>%
  autoplot()

 
 library(ggplot2)
 deaths.model  <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths,K = 5, seasonal= T))
 deaths.fcast <- forecast(deaths.model, xreg=fourier(USAccDeaths, K=5, h=36))
 autoplot(deaths.fcast) + xlab("Year")
 
 # Using Fourier series for a "msts" object
 taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
 taylor.fcast <- forecast(taylor.lm,
                          data.frame(fourier(taylor, K = c(3, 3), h = 270)))
 autoplot(taylor.fcast)
 
 
 
 
 
 ## auto arima 잘안됨 
 X =cbind(time,const)
 data_sqrt_ts <- ts(data_sqrt,frequency = 12)
 fit_reg_auto <- auto.arima(data_sqrt_ts, xreg = X, approximation = F)
 test(fit_reg_auto$residuals)
 ################333
 