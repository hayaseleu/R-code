setwd("C:/Users/WJ/Downloads/2018-2/시계열")

source("TS-library.R")

# ch0

bp = c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)
weight = c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
age = c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)


par(mfrow=c(1,2))
plot(weight, bp);
title("BP vs Weight")
abline(lm(bp~weight), col="red")
plot(age, bp);
title("BP vs Age")
abline(lm(bp~age), col="red")


fit = lm(bp ~ weight + age)
summary(fit)



resi = residuals(fit)
par(mfrow=c(2,2))
plot(resi);
title("residual plot")
plot(weight, resi);
title("residual vs weight")
plot(age, resi);
title("residual vs age")
qqnorm(resi);
qqline(resi)

# huron 

source("TS-library.R")

data = scan("huron.txt")

plot.ts(data)
title("Lake Huron Water level")

n = length(data)
x = seq(from = 1 , to = n , by = 1)


out.lm = lm(data ~ x)
summary(out.lm)
plot.ts(data)
lines(out.lm$fitted.values, col = "red")

x_2 = x^2  

out.lm2 = lm(data ~ x_2+ x)
summary(out.lm2)

lines(out.lm2$fitted.values, col = "blue")
x_exp <- exp(x)

out.lm3 = lm(data ~ x_exp)
summary(out.lm3)

lines(out.lm3$fitted.values, col = "green")

x_log <- log(x)
out.lm4 = lm(data ~x_log)
summary(out.lm4)
lines(out.lm4$fitted.values, col = "purple")
# 
#  for (i in  1:4 ) {
# par(mfrow = c(2,2))
# a = c(out.lm,out.lm2,out.lm3,out.lm4)
# par(mfrow=c(2,2))
# plot(a[i])
#  }
# 
# 
plot(out.lm)
plot(out.lm2)
plot(out.lm3)
plot(out.lm4)


par(mfrow=c(2,2))
acf2(out.lm$residuals)
title("sample ACF of residuals");
plot(out.lm$residuals[1:(n-1)], out.lm$residuals[2:n], xlab="Y_{t-1}", ylab="Y_t")
title("Plot of residuals - Lag1")
plot(out.lm$residuals[1:(n-2)], out.lm$residuals[3:n], xlab="Y_{t-2}", ylab="Y_t")
title("Plot of residuals - Lag2")
plot(out.lm$residuals[1:(n-3)], out.lm$residuals[4:n], xlab="Y_{t-3}", ylab="Y_t")
title("Plot of residuals - Lag3")

ts.plot(out.lm$residuals)
test(out.lm$residuals)
acf2AR(out.lm$residuals)



ts.plot(out.lm2$residuals)
test(out.lm2$residuals)
plot(out.lm2)

# for 문 1:n 까지 acf plot 그리기 
par(mfrow = c(4,4)) 
for ( i in 1:16) {
  plot(out.lm$residuals[1:(n-i)], out.lm$residuals[(i+1):n], xlab =  paste(" Y_{t-", as.character(i), "}") ,  ylab = "Y_t")
}


# ma.cv 분석

data
str(data)
Y <- as.vector(data)
Y
n = length(Y)
cv <- 0
ind = 1:n
eps <- 1e-16
str(eps)
h = 32
#for  i =1  , h= 12
i =1 
del = seq(max(i - 1 ,1), min( i + 1 ,n), by = 1)
del
id = ind[-del]
id
Z = Y[-del]
Z  # Y 의 3번째 항부터 시작 
tmp <- (id - i )/ h
s0 <- (abs(tmp) <= 1 )
s1 <- Z*s0
m <- sum(s1)/max(eps,sum(s0))
cv <- cv + (Y[i] - m )^2 

# for  i = 2, h= 12
i=2 
del = seq(max(i - 1 ,1), min( i + 1 ,n), by = 1)
del
id = ind[-del]
id
Z = Y[-del]
Z  # Y 의 3번째 항부터 시작 
tmp <- (id - i )/ h
s0 <- (abs(tmp) <= 1 )
s1 <- Z*s0
m <- sum(s1)/max(eps,sum(s0))
cv <- cv + (Y[i] - m )^2 

ma.cv(1,data,1)




# 

h.ma = optimize(f=ma.cv, interval=c(5, length(data)/2), Y=data, l=1, tol = .Machine$double.eps^0.25)
out.ma = smooth.ma(data, h.ma$minimum)

out.ma = smooth.ma(data, 32)
par(mfrow=c(1,3))
plot.ts(data);
lines(out.ma, col="red")
title("Detrend- MA")
plot.ts(data-out.ma);
title("Residuals - MA")
acf2(data-out.ma)
title("SACF of Residuals - MA")
test((data-out.ma))
save.image("huron.Rdata")


# ch1 accidental 

rm(list=ls(all=TRUE))
source("TS-library.R")
data = scan("deaths.txt");
data = ts(data, start=1973, end=1979, freq=12)
n = length(data)
par(mfrow=c(1,2))
plot.ts(data);
title("US accidental deaths")
acf2(data, 35);
title("SACF")


# f1 = round(n/d) # 
t = 1:n;
f1 = 6;
f2 = 12;
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
costerm1 = cos(f1*2*pi/n*t);
sinterm1 = sin(f1*2*pi/n*t);
costerm2 = cos(f2*2*pi/n*t);
sinterm2 = sin(f2*2*pi/n*t);
sinterm3 = sin(f3*2*pi/n*t)
costerm3 = cos(f3*2*pi/n*t)


out.lm1 = lm(data ~ 1 + costerm1 + sinterm1)
out.lm2 = lm(data ~ 1+ costerm1 + costerm2 + sinterm1 + sinterm2)
out.lm3 = lm(data ~ costerm1 + costerm2 + costerm3 + sinterm1 + sinterm2 + sinterm3)

summary(out.lm1)
summary(out.lm2)
summary(out.lm3)
x = as.vector(time(data))
plot.ts(data);
title("US accidental deaths")
lines(x, out.lm1$fitted, col="blue")
lines(x,out.lm2$fitted, col="red")
lines(x,out.lm3$fitted.values, col = "green")  # 그린은 조금더 첫번째 제일 큰 변동에 가까워 나머지 변동보다 크다 . 
ts.plot(out.lm3$residuals)
legend(1975, 10800, lty=c(1,1), col=c("blue","red"), c("k=1", "k=2"))




par(mfrow=c(2,2));
plot.ts(data);
title("US accidental deaths")
lines(x,out.lm2$fitted, col="red")
plot(out.lm2$fitted, out.lm2$residuals);
title("Residuals vs Fitted")
acf2(out.lm2$residuals)
title("SACF-residuals")
qqnorm(out.lm2$residuals);
qqline(out.lm2$residuals)

library(itsmr)
season.avg = season(data, d=12)
plot.ts(data);
title("US accidental deaths")
lines(x, season.avg + mean(data), col="red")


diff12 = diff(data, lag=12);
par(mfrow=c(1,2))
plot.ts(data);
title("US accidental deaths")
plot(x[13:73], diff12, type="l", col="red")  # 차분 했기 때문에 길이가 짧음 (12개 줄어들어서 73-12 = 61개의 데이터 사용 = length(x[13:73]))
title("Seasonal differencing")


out = classical(data, d=12, order=1);
par(mfrow=c(2,2))
plot.ts(data);
title("step1")
lines(x, out$m1, col="red");
plot.ts(data-out$m1);
title("step2")
lines(x, out$st, col="red");
plot.ts(data-out$st);
title("step3")
lines(x, out$m, col="red");
plot.ts(data);
lines(x, out$fit, col="red")
title("Final")


# In-class activity: Do HW1 15, part (a)-(e).
data <- read.table('imports.txt')
  ts.plot(data)
data <- ts(data)
x <- seq(1,length(data), by = 1)
n = length(data)
# f 
hopt = optimize(f=ma.cv, interval=c(5, floor(n/2)), tol=.Machine$double.eps^0.25, Y=data, l=1)
hopt$minimum;

ts.plot(data)
hw15 <- smooth.ma(data,5)
lines(x,hw15, col = "red")
ts.plot(data-hw15)
test(data-hw15)
acf(data, 125)

# g (g) Apply classical decomposition and see the result. # 계젏성이 없으면 d = 1 로 두기 

out.class <- classical(data,1,1)
par(mfrow = c(2,2))
ts.plot(out.class$st)
ts.plot(out.class$m)
ts.plot(out.class$fit)
ts.plot(data- out.class$fit)

