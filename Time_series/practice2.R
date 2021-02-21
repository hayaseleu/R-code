setwd("C:/Users/WJ/Downloads/2018-2/시계열/기말 준비")
data <- scan("practice2.csv")
library(itsmr)
data_ts <- ts(data, start = c(1987,01), frequency = 12)
ts.plot(data_ts)
ts.plot(data)
test(data)
n = length(data)

# (a) Time plot, correlograms (ACF) 
#and discuss key features of the data.

# 증가하는 trend를 가지고 있다. 
# 작은 분산을 일정해 보이지만 trend의 증가가 점점 
# 더 빠르게 증가 하는 것으로 보인다. 
# 계절성은 plot만으로는 확인하기 힘들어 보인다.
# 2013의 빠른 감소가 눈에 띈다. 


#acf에서는 강한 trend때문에 느리게 감소하는 acf를 볼 수있다.
# pacf는 ar(1)에서 강한 pacf가 보이고 12,24의 주기도 미약하게 보인다. 
# qqplot을 봐도 정규성을 따른다고 보기 힘들어 보인다. 
acf(data, lag.max = 80)

## data를 처음 살펴볼때 

par(mfrow = c(4,4)) 
for ( i in 1:16) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" Y_{t-", as.character(i), "}") ,  ylab = "Y_t")
}

par(mfrow = c(4,4)) 
for ( i in 32:47) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" Y_{t-", as.character(i), "}") ,  ylab = "Y_t")
}

par(mfrow = c(4,4)) 
for ( i in 61:75) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" Y_{t-", as.character(i), "}") ,  ylab = "Y_t")
}

# plot으로 그려도 강한 acf를 확인 할 수 있었다. 

## (b) Is it stationary? Include your evidence.

# satationary하지않다.  1. 평균이 일정해야하지만 계산을 했을때 

test(data)  
#diff signs S, Rank p에서 귀무가설을 기각함을 확인할수있다.
#diff signs S는 yi > yi-1 인즉 y의 차분이 양수인 횟수, 즉 y가 증가하는 횟수를 S라고 하고 S가 크면 증가하는 TREND가 있다고 해석한다. 
#RANK test는 yj > yi 이고 j>i 인 pair의 숫자라고 한다. 그러므로 p가 크면 증가하는 trend가 있다고 한다. 
#이는 data에 trend가 존재한다는 의미로 해석 할 수 있다.
#증가하는 trend가 때문에 local level 즉 평균이 다르다. 
# 

# 2. Var이 일정한가? 
# plot상으로는 급격한 급락이 있는 300중반을 제외하고는 일정해 보인다. 



s <- 12 # 주기 
n <- length(data)
season_index <- matrix(NA,s, ceiling(n/s))
for ( i in 1:s) {
  s_d <- seq(from = i , to = n ,  by = s)
  if (i <= (n %% s)) {
    season_index[i,1:ceiling(n/s)] <- s_d
  } else season_index[i,1:floor(n/s)] <- s_d
}


### function
seasonalize <- function(x,s) {
  sea <- season_index[s,]
  season_data <- x[sea]
  season_data <- season_data[!is.na(season_data)] 
  return(season_data)
}

par(mfrow = c(3,4))
for (i in 1:s) {
  ts.plot(seasonalize(data,i),ylab = paste("season" ,as.character(i)))
}

for (i in 1:s) {
  acf(seasonalize(data,i))
}
# 계절로 봤을때도 증가하는 trend가 보임을 확인했다.
# 분산은 plot상에서 증가하지않는 것으로 보인다. 



for ( i in 16) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}
points(data[(n-34):(n-14)],data[(n-20):n], col = "red")  # 후기의 데이터를 빨간점으로 찍음 
points(data[(n-78):(n-48)],data[(n-64):(n-34)], col = "Blue1") # 후기보다 조금 앞의 데이터를 파란색# 으로 찍음 
points(data[1:30],data[15:44], col = "green")  # 초기의 데이터를 녹색으로 찍음 


# 한점에서도 증가하는 trend가 보임을 확인했다. 

# Fidn the best model to describe the data and statistical reasonings for model selection
# including model diagnostics/checking.

# model을 총 4가지로 찾아보겠다
# 1. plot을 가지고 판단. 2. information_creteria에 기반한 모델 설정
# 3. alasso를 사용한 backward 방법 4. out of sample forecasting에서 최고의 성능을 발휘한 모델 

#plot
#plot을 봤을때 계절별로 증가하는 트렌드가 보이므로 lag =12에 대해서 차분하겠다. 
# lag 12에 대해 차분해도 1-B^12 = (1-B)(1+B+B^2+...) 이므로 
# 1차 POLYNOMIAL TREND를 제거하기에 계절 주기 s에 대해 먼저 차분하겠따 .

data_d12 <- diff(data, lag = 12)
# 단 차분할때에는 data의 숫자가 그만큼 줄어드는 것이 단점이다. 그러므로 
# 없어진 data를 대신하여 평균으로 대체하면 된다. 
ts.plot(data_d12)
test(data_d12)
## lag =12는 별로 stationary 해보이지않아서 lag 1로 다시 차분 

data_d1 <- diff(data, lag =1)
ts.plot(data_d1)
test(data_d1)
 data_d1_sup <- c(rep(mean(data_d1),(length(data)-length(data_d1))),data_d1)
# 이렇게 보니 분산이 커지는 것이 보인다. log화를 시키고 다시 차분해보자
x = 1:length(data)
fit = boxcox(data~1, plotit=TRUE);
lambda = fit$x[which.max(fit$y)]
lambda
data <- data^1.555556
data_d1 <- diff(data, lag =1)
ts.plot(data_d1)
test(data_d1)

# 분산을 안전화 시켰으며 trend역시 보이지 않는다. test결과도 ljung_box, mecleod_li만 기각했따. 
#acf,pacf 상에서 pacf는 12에서 사라지고 
# acf는 12,24로 계속 짧아지는 것으로 보아 sarima는 (1,0,0) 이 적절해보인다. 
# arima에서는 pacf 1,2,311 , acf10,1이 감마()와 알파()가 유의하게 0이 아님을 볼수있다. 그러믈 arima(1,1,1) 을적합해보겠다. 

model_plot <- arima(data,order=c(11,0,11),seasonal = list(order=c(1,1,0),period = s),include.mean = F)
tsdiag(model_plot)
ts.plot(model_plot$residuals)
test(model_plot$residuals)
plot(data)
library(forecast)
auto.arima(data_ts, approximation = F)
test(resid(auto.arima(data_ts)))
auto.arima(data_ts,approximation = F,max.D = 0)
test(resid(auto.arima(data_ts,approximation = F,max.D = 0)))


                                                   #1  2 3 4 5  6 7 8 9 10 1112 13 14 15  16 17 18 19 20 21 22 23 24
model_alsso <- arima(data,order=c(24,1,0),fixed = c(NA,0,0,0,0,NA,0,0,NA,0,0,NA,0 ,NA,NA, NA,NA,NA, 0,0,0   , NA,NA,NA))
test(model_alsso$residuals)                     
plot(forecast(model_alsso,n.ahead = 20))                     

# auto.arima d,나 D는 설정해서 직접 하도록 하자 
??auto.arima  # help 꼭보고 ic로 aicc, aic, bic 설정 , parallel true로 계산 더 바르게

model_alsso <- arima(data,order=c(24,1,0),fixed = c(NA,0,0,0,0,0,0,0,0,0,0,NA,0 ,NA,0, 0,0,NA, 0,0,0   , 0,0,NA))
test(model_alsso$residuals)                     
plot(forecast(model_alsso,n.ahead = 20))   


## 

t = 1:(n-1);
m1 = round(n/12);
m2 = m1*2;
costerm1 = cos(m1*2*pi/n*t);
sinterm1 = sin(m1*2*pi/n*t);
costerm2 = cos(m2*2*pi/n*t);
sinterm2 = sin(m2*2*pi/n*t);
out.lm1 = lm(data_d1 ~ 1 + costerm1 + sinterm1)
summary(out.lm1)
out.lm2 = lm(data_d1 ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)
summary(out.lm2)
x = as.vector(time(data_d1))
plot.ts(data_d1_sup);
title("US accidental deaths")
lines(x, out.lm1$fitted, col="blue")
lines(x,out.lm2$fitted, col="red")
legend(1975, 10800, lty=c(1,1), col=c("blue","red"), c("k=1", "k=2"))

data_deseason <- data_d1 - out.lm2$fitted.values
ts.plot(data_deseason)




