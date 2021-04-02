# test of randomness 
source("TS-library.R")

data = scan("huron.txt")

plot.ts(data);
title("Lake Huron Water level")

n = length(data)
x = seq(from = 1 , to = n , by = 1)


out.lm = lm(data ~ x)
summary(out.lm)
plot.ts(data)
lines(out.lm$fitted.values, col = "red")

library(itsmr)
load("huron.Rdata")
test(out.lm$residuals)

# without using itsmr we can perform Box-pierce
# and Ljung-Box
Box.test(out.lm$residuals, 20)
Box.test(out.lm$residuals, 20, type="Ljung")


# Practice: Do the test of randomness for residuals obtained after detrending by
# MA(33) 

test(data - smooth.ma(data,33))

# iid하지 않음을 알 수 있다. 



# If you further interest in cheking Gaussianity of the residuals, perform the following.

qqnorm(out.lm$residuals)
qqline(out.lm$residuals, col="red")


shapiro.test(out.lm$residuals)


library(nortest)
ad.test(out.lm$residuals)



cvm.test(out.lm$residuals)



lillie.test(out.lm$residuals)



##
## Lilliefors (Kolmogorov-Smirnov) normality test
##
## data: out.lm$residuals
## D = 0.039819, p-value = 0.9645
# For Jacque-bera test
library(tseries)


jarque.bera.test(out.lm$residuals)



#Practice: Do the test of randomness/normality for residuals obtained after apply
# classical decomposition on Accidental data.

source("TS-library.R")
data = scan("deaths.txt");
data = ts(data, start=1973, end=1979, freq=12)
n = length(data)
out = classical(data, d=12, order=1)
test(out$resi)
