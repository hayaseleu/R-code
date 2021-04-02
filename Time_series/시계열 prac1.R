

source("TS-library.R")
# practice 1 

prac1 <- scan("practice1.csv")
ts.plot(prac1)
acf(prac1, lag = round(length(prac1)/4))


#어느정도 상승하는 trend와 계절성이 들어나고 있다.
#분산은 일정해보인다. 
#평균이 증가함으로 stationary하지않다. 
#그러므로 계절성과 trend를 제거하면 stationary한 오차 Yt를 만들수있을꺼같다. 
#코릴리오그램에서도 lag 12에서의 강한 상관성이 들어나는 것으로 보인다. 

#b 

test(prac1)

# 귀무가설 data는 iid를 모든 검정이 기각한다. 즉 yt는 iid라고 할 수 없다. iid,wn이면 stationary인데 
#iid가 아니므로 stationary인지 아닌지는 확인할수없다.
#test의 plot을 보면 residual이 강한 계절성과 상승하는 trendㄹ 보이고 있음을 알 수 있다.
#acf를 봐도 12, 24,36 즉 12의 배수끼리 강한 acf를 보이고있다.
#trend를 가진 non iid,wn에대한 power가 강한 turning ponts diff signs, rank T도 귀무가설을 기각하여 
#data가 trend가 존재하는 non iid임을 나타낸다. 


qqnorm(data)
qqline(data) 

#qqplot을 보면 정규성을 가진다고 판단하기 힘듬을 알수있다. 
#표준화해서 표준편차 1 이상의 데이터값들이 qqline을 크게 이탈하고 있다. 

#c 

x <- seq(1,length(prac1), by = 1)
poly.fit1 <- lm(prac1 ~ x)

ts.plot(data-poly.fit1$fitted.values)
summary(poly.fit1)

plot(poly.fit1)
# plot과 잔차 plot을 보면 trend는 더이상 보이지않음을 볼 수 있다. 
# 단 계절성의 존재로 108,120,132 순으로 잔차에서 큰 이탈을 보여준다. 
# 다른 plot에서도 표준화된 잔차에서도 12 , 120 ,132 등이 크게 이탈함을 볼수있다. 
#qqplot에서도 108,120,132가 qqline을 크게 이탈함을 볼수있다. 
#1차로 trend 제거 
ts.plot(poly.fit1$residuals)
# harmonic으로 seasonal 제거 
d = 12
t = 1:length(prac1)
f1 = round(length(prac1)/d)
f2 = 2*f1
f3 = 3*f1
f4 = 4*f1
f5 = 5*f1
f6 = 6*f1
f7 = 7*f1
f8 = 8*f1
f9 = 9*f1
f10 = 10*f1
f11 = 11*f1

cos1 = cos(f1*2*pi*t/length(prac1))
sin1 = sin(f1*2*pi*t/length(prac1))

cos2 = cos(f2*2*pi*t/length(prac1))
sin2 = sin(f2*2*pi*t/length(prac1))

cos3 = cos(f3*2*pi*t/length(prac1))
sin3 = sin(f3*2*pi*t/length(prac1))

cos4 = cos(f4*2*pi*t/length(prac1))
sin4 = sin(f4*2*pi*t/length(prac1))


cos5 = cos(f5*2*pi*t/length(prac1))
sin5 = sin(f5*2*pi*t/length(prac1))


cos6 = cos(f6*2*pi*t/length(prac1))
sin6 = sin(f6*2*pi*t/length(prac1))


cos7 = cos(f7*2*pi*t/length(prac1))
sin7 = sin(f7*2*pi*t/length(prac1))


cos8 = cos(f8*2*pi*t/length(prac1))
sin8 = sin(f8*2*pi*t/length(prac1))


cos9 = cos(f9*2*pi*t/length(prac1))
sin9 = sin(f9*2*pi*t/length(prac1))

cos10 = cos(f10*2*pi*t/length(prac1))
sin10 = sin(f10*2*pi*t/length(prac1))


cos11 = cos(f11*2*pi*t/length(prac1))
sin11 = sin(f11*2*pi*t/length(prac1))


fit.har1 = lm(prac1- poly.fit1$fitted.values ~ cos1+sin1)
summary(fit.har1)
plot.ts(poly.fit1$residuals)
lines(fit.har1$fitted.values)


fit.har2 = lm(poly.fit1$residuals~ cos1+sin1+cos2+sin2)
summary(fit.har2)
plot.ts(poly.fit1$residuals)
lines(fit.har2$fitted.values)


fit.har3 = lm(poly.fit1$residuals~ cos1+sin1+cos2+sin2+sin3+cos3)
summary(fit.har3)
plot.ts(poly.fit1$residuals)
lines(fit.har3$fitted.values, col = "red")
plot.ts((poly.fit1$residuals)-fit.har3$fitted.values)

fit.har4 = lm(poly.fit1$residuals~ cos1+sin1+cos2+sin2+sin3+cos3+sin4+cos4)
summary(fit.har4)
plot.ts(poly.fit1$residuals)
lines(fit.har4$fitted.values, col = "red")
plot.ts((poly.fit1$residuals)-fit.har4$fitted.values)

fit.har12 = lm(poly.fit1$residuals~ cos1+sin1+cos2+sin2+sin3+cos3+sin4+sin5+sin6+sin7+sin8+sin9+sin10+cos3+cos4+cos5+cos6+cos7+cos8+cos9+cos10+cos11+sin11)
summary(fit.har12)
plot.ts(poly.fit1$residuals)
lines(fit.har12$fitted.values, col = "red")


plot.ts(fit.har12$residuals)


#regression 으로 seasonailty를 잡기가 힘들다.
#여전히 seasonality가 존재하고 조금 감소하는 trend가 있는거 같다. 



##


a <- cumsum(rep(12,floor(length(data)/12)))
data[a]
f <- rep(0,length(data))
f[a] = 1
b <- cumsum( rep(14, floor(length(data)/14)))
g <- rep(0,length(data))
g[b] = 1
fit.har13 = lm(poly.fit1$residuals~ cos1+sin1+cos2+sin2+sin3+cos3+sin4+sin5+sin6+sin7+sin8+sin9+sin10+cos3+cos4+cos5+cos6+cos7+cos8+cos9+cos10+cos11+sin11+f+g)
summary(fit.har13)
plot.ts(poly.fit1$residuals)
lines(fit.har13$fitted.values, col = "red")


plot.ts(fit.har13$residuals)


#(d) Remove any trend/seasonality by using smoothing method.

#직접 잡아보기 

fit.ma = smooth.ma(prac1,32)
plot.ts(prac1)
lines(x,fit.ma, col = "red")
plot.ts(fit.ma)
plot.ts(prac1 - fit.ma)




# cv 활용 
h.ma = optimize(f=ma.cv, interval=c(5, floor(length(prac1)/2)), Y=prac1, l=1, tol = .Machine$double.eps^0.25)
out.cvma = smooth.ma(prac1,h.ma$minimum)
plot.ts(prac1)
lines(x,out.cvma, col = "red")
plot.ts(prac1 - out.cvma)

# ma filter취한 잔차에 seasonality 구하기 
season.avg = season(prac1 - out.cvma,d = 12)
plot.ts(prac1-out.cvma)
lines(x,season.avg, col = "red")



# 다시 원 데이터에 seasonality 빼고 잔차에서 trend

plot.ts(prac1-season.avg)
h.ma = optimize(f=ma.cv, interval=c(5, floor(length(prac1-season.avg)/2)), Y=(prac1-season.avg), l=1, tol = .Machine$double.eps^0.25)
out.cvma = smooth.ma(prac1-season.avg,h.ma$minimum)
plot.ts(prac1)
lines(x,out.cvma, col = "red")

# 이제 얻은 잔차 
plot.ts(prac1-season.avg -out.cvma)
mean(prac1-season.avg-out.cvma)
# 77 쯤에 거대한 outliner가 존재한다. 
test(prac1-season.avg -out.cvma)

qqnorm(prac1-season.avg -out.cvma)
qqline(prac1-season.avg -out.cvma, col = "red")

#acf로는 wn처럼 보이고 test에서 iid 가정을 기각하지 않는다. 


# Remove any trend/seasonality by using differencing method
out.diff.test <- diff(prac1)
plot.ts(out.diff.test)
out.diff <- diff(prac1, lag = 12)

plot.ts(out.diff)
test(out.diff)

# 충분히 seasonality가 제거 되었다.

out.diff2 <- diff(out.diff)
plot.ts(out.diff2)
test(out.diff2)
qqnorm(out.diff2)
qqline(out.diff2, col = "red")

out.diff3 <- diff(out.diff,differences = 2)
plot.ts(out.diff3)
test(out.diff3)

plot(out.diff3[1:(n-1)],out.diff3[2:n]) 
# 더이상 차분해도 더 나은 결과를 보인다고 할 수 없다. 
# 차분을 할수록 data의 양이 줄어 들어서 피하는게 좋다.

#q1 차분을 하면 acf랑 test 결과가 달라지는데
#wn, iid가 아니면 더 예측할때 도움이 되니깐 좋은게 아닌지? 

#f

test(prac1-season.avg -out.cvma)
test(out.diff)


qqnorm(prac1-season.avg -out.cvma)
qqline(prac1-season.avg -out.cvma, col = "red")
qqnorm(out.diff)
qqline(out.diff, col = "red")

n = length(prac1-season.avg -out.cvma)
final = prac1-season.avg -out.cvma
# 밑이 lag 3의 plot 


plot(final[1:(n-3)], final[4:n], xlab="Y_{t-2}", ylab="Y_t")
title("Plot of residuals - Lag3")

plot(final[1:(n-4)], final[5:n], xlab="Y_{t-2}", ylab="Y_t")
title("Plot of residuals - Lag4")



#둘다 거대한 이상치가 존재한다.
#차분의 경우에는 12만큼의 데이터의 숫자가 줄어 들었다. 
#둘다 test는 iid 가설을 기각하지 못한다. 
#qqplot은 차분이 더 잘 만족하는것 같다. 
#acf는 둘다 적은 수의 간격만이 점선을 넘어 비슷하다.
#그러므로 데이터의 숫자가 더 많은 smooth 방법의 결과를 선택한다. 

###
prac1.ts <- ts(prac1, frequency = 4)
plot(decompose(prac1.ts))

###

out <- classical(prac1, d = 12 ,order =1) 

par(mfrow=c(2,2))
plot.ts(prac1);
title("step1")
lines(x, out$m1, col="red");
plot.ts(prac1 - out$m1);  #처음 구한 trend 
title("step2")
lines(x, out$st, col="red");
plot.ts(prac1 - out$st);  #처음구한 trend를 빼고 구한 seasonality 
title("step3")
lines(x, out$m, col="red");  #데이터에 seasonailty를 빼고 다시구한 trend 
plot.ts(prac1);
lines(x, out$fit, col="red")  #최종 trend+seasonailty    
title("Final")

# classical을 이용한 최종 잔차 
plot.ts(prac1-out$fit)  # 둘은 같음 
plot.ts(out$resi)




