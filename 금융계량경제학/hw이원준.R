
library(pander)
library(fBasics)
library(dplyr)
library(lmtest)
options(scipen = 50)



setwd("C:/Users/WJ/Downloads/2018-2/c")
data <- read.table("RussianData.txt", header = TRUE )
da_ts <- ts(data, start = c( 2003, 1) , end = c(2013 , 12), frequency = 12 )


## (1) Descirbe your data $Y_{t}$ 

pander(length(data$date)) # sample size
(data[1,1]) # starting point of sample period
(data[length(data$date),1]) # end point of sample period  
mean <- data$y %>% mean(); var <- data$y %>% var(); skewness <- data$y %>% skewness(); 
kurtosis <- data$y %>% kurtosis()
pander(data.frame(mean, var, skewness, kurtosis)) #mean, var, skewness, kurtosis 

### kurosis is smaller than 3, so it does not have heavy tail than normal dist. 
### skewness is negative, so it is skewed to the left


## (2) Estimate ARMA(2,2) model : $y_{t}$ = c + $\alpha_{1}$$y_{t-1}$ + $\alpha_{2}$$y_{t-2}$ + $\upsilon_{t}$

fit_arma22 <- arima(data$y, order = c( 2 , 0 , 2 ))
pander(fit_arma22)
pander(1-pnorm((abs(fit_arma22$coef)/sqrt(diag(fit_arma22$var.coef))))^2)  # t-test of coef 
Box.test(fit_arma22$residuals, lag = 12, type = "Ljung") %>% pander()



### each coefficient estimate is significant. 
### and residuals are not correlated, they are White noise.

## (3) Estimate following arma(12,12) model and report the estimates and standard erros of the model 
# $y_{t}$ = c + $\alpha_{1}$$y_{t-1}$ + $\alpha_{2}$$y_{t-2}$ + $\alpha_{3}$$y_{t-3}$ + $\alpha_{4}$$y_{t-12}$ + $\upsilon_{t}$
  
  

fit_arma1212 <- arima(data$y, order = c( 12, 0 , 12), 
                      fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
pander(fit_arma1212)
pander(1-pnorm((abs(fit_arma1212$coef[c(1,2,3,12,24,25)])/sqrt(diag(fit_arma1212$var.coef))))^2) # t-test of coef  
Box.test(fit_arma1212$residuals, lag = 12, type = "Ljung" ) %>% pander()

### each coefficient estimate (except fixed) is significant. 
### and residuals are not correlated, they are white noise.


## (4) Compare information criteria of two models in (2) and (3)

data.frame(AIC(fit_arma1212),AIC(fit_arma22),BIC(fit_arma1212), BIC(fit_arma22)) %>% pander()
AIC(fit_arma1212) < AIC(fit_arma22) &  BIC(fit_arma1212) < BIC(fit_arma22)

### so in terms of information criteria, arma(12,12) model is better than arma(2,2) 

## (5) Conduct out of sample one-step-ahead forecast using rolling window and report the MSE, MAE

nf = 24
act_y = data$y[-cumsum(rep(1,108,by =1))]
out_arma22 = matrix(0,nf,1) ; out_arma1212 = matrix(0,nf,1) 
for (i in 1:nf) {
  y2 = data$y[ i : (i+107)]
  fit = arima(y2, order = c(2,0,2))
  coeff = matrix(as.numeric(fit$coef))
  a1 = coeff[1] ;a2 = coeff[2] ;b1 = coeff[3] ;b2 = coeff[4] ; a0 = coeff[5]
  out_arma22[i]  =  predict(fit)$pred
}
se1 = (act_y-out_arma22)^2
mae1 = abs(act_y-out_arma22)
for (i in 1:nf) {
  y2 = data$y[ i : (i+107)]
  fit = arima(y2, order = c(12,0,12),  fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
  coeff = matrix(as.numeric(fit$coef))
  a1 = coeff[1] ;a2 = coeff[2] ; a3 = coeff[3] ; a4 = coeff[12] ; b1 = coeff[24] ; a0 = coeff[25]
  out_arma1212[i]  =  predict(fit)$pred
}
se2 = (act_y-out_arma1212)^2
mae2 = abs(act_y-out_arma1212)
mse = data.frame(mean(se1),mean(se2))
mae = data.frame(mean(mae1),mean(mae2))
pander(mse)
pander(mae)


## 6 which one is better? Conduct DM test between the two model

dt = se1 - se2 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);

(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=1-pnorm(DMW) 
p_value  #mse

dt = mae1 - mae2 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=1-pnorm(DMW) 
p_value # mae 

### so we reject Null hypothesis : E(dt) = 0 
### and accept alternative hypothesis : E(dt) > 0 
### so arma(12,12) is better than arma(2,2)

## (7) consider dummy model. Conduct the estimation and specification checking as in (2). Compare information criteria 

fit_armadummy <- arima(data$y, order = c(12,0,12), xreg = data$dummy , 
                       fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA) ) 
fit_armadummy %>% pander()
fit_armadummy %>% coeftest() 
Box.test(fit_armadummy$residuals, lag =12 , type = "Ljung-Box") %>% pander()
data.frame(AIC(fit_arma1212),AIC(fit_armadummy),BIC(fit_arma1212),BIC(fit_armadummy)) %>% pander()
AIC(fit_armadummy) < AIC(fit_arma1212) ; BIC(fit_armadummy) < BIC(fit_arma1212)

### at many time dummy values are zero, so it is not suprise not to reject T-test 
### residuals are not correlated, they are white noise.
### in terms of AIC dummy model is better, but in terms of BIC arma(12,12) is better. 

## (8) conduct the out-of-sample one-step-ahed forecasting. Report MSE, MAE. Coduct the DM test 

out_armadummy = matrix(0,nf,1) 
for (i in 1:nf) {
  y2 = data$y[ i : (i+107)]
  x2 = data$dummy[ i : (i+107)]
  fit = arima(y2, order = c(12,0,12) , xreg = x2  ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA))
  coeff = matrix(as.numeric(fit$coef))
  a1 = coeff[1] ;a2 = coeff[2] ; a3 = coeff[3] ; a4 = coeff[12] ; b1 = coeff[24] ; a0 = coeff[25] ; a5 = coeff[26]
  out_armadummy[i]  =  predict(fit, newxreg = data$dummy[108+i] )$pred
}
se3 =  (act_y-out_armadummy)^2
mae3 = abs(act_y-out_armadummy)
mse = data.frame(mean(se2),mean(se3))
mae = data.frame(mean(mae2),mean(mae3))
pander(mse)
pander(mae)

dt = se3 - se2 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);

(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=1-pnorm(DMW) 
p_value  #mse

dt = mae3 - mae2 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=(1-pnorm(DMW))  
p_value # mae 

### arma(12,12) has smallr MSE but , bigger MAE. 
### in DMW test, H0: E(dt) = 0  is not rejected
### so We can not say that there is a difference in the performance of the model.


### (9) estimate your ARMAX model. Conduct the estimation and specification 

fit_b1 <- arima(data$y, order = c(12,0,12) , xreg = data$b1gr ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA)) 
fit_b2 <- arima(data$y, order = c(12,0,12) , xreg = data$b2gr ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA)) 
fit_b3 <- arima(data$y, order = c(12,0,12) , xreg = data$b3gr ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA)) 
fit_b12 <- arima(data$y, order = c(12,0,12) , xreg = cbind(data$b1gr,data$b2gr) ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA))
fit_b23 <- arima(data$y, order = c(12,0,12) , xreg = cbind(data$b2gr,data$b3gr) ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA))
fit_b13 <- arima(data$y, order = c(12,0,12) , xreg = cbind(data$b1gr,data$b3gr) ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA))
fit_b123 <- arima(data$y, order = c(12,0,12) , xreg = cbind(data$b1gr,data$b2gr,data$b3gr) ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA))
pander(fit_b1)
pander(fit_b2)
pander(fit_b3)
pander(fit_b12)
pander(fit_b13)
pander(fit_b23)
pander(fit_b123)

data.frame(fit_b1$aic,fit_b2$aic,fit_b3$aic,fit_b12$aic,fit_b13$aic,fit_b23$aic,fit_b123$aic) %>% pander()
data.frame(BIC(fit_b1),BIC(fit_b2),BIC(fit_b3),BIC(fit_b12),BIC(fit_b13),BIC(fit_b23),BIC(fit_b123)) %>% pander()
Box.test(fit_b3$residuals, lag =12 ,type = "Ljung") %>% pander

### I chose model with b3gr. because it has lest BIC and second smallest AIC
### we have 132 sample. when we have large sample, BIC is better to choose true model  than AIC. 
### residuals are not correlated, they are white noise.


BIC(fit_b3) < BIC(fit_arma1212) & AIC(fit_b3) < AIC(fit_arma1212)
BIC(fit_b3) < BIC(fit_armadummy) &AIC(fit_b3) < AIC(fit_armadummy)
pander(fit_b3)
pander(1-pnorm((abs(fit_b3$coef[c(1,2,3,12,24,25,26)])/sqrt(diag(fit_b3$var.coef))))^2) # t-test 

### coef of composite leading indicator of business cycle is 10.89
### so other conditions are same and if b3gr become increase 1 than y increase 10.89 
### in terms of information criteria, model with b3gr is the best. 

### (10) conduct the out of sample one-step ahead forecast. Report MSE, MAE

out_b3 = matrix(0,nf,1) 
for (i in 1:nf) {
  y2 = data$y[ i : (i+107)]
  x2 = data$b3gr[ i : (i+107)]
  fit = arima(y2, order = c(12,0,12) , xreg = x2  ,fixed = c(NA,NA,NA,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA))
  coeff = matrix(as.numeric(fit$coef))
  a1 = coeff[1] ;a2 = coeff[2] ; a3 = coeff[3] ; a4 = coeff[12] ; b1 = coeff[24] ; a0 = coeff[25] ; a5 = coeff[26]
  out_b3[i]  =  predict(fit, newxreg = data$b3gr[108+i] )$pred
}
se4 =  (act_y-out_b3)^2 
se4 %>% mean() %>% pander() 
mae4 = abs(act_y-out_b3)
mae4 %>% mean() %>% pander()



### 11 compare forecast errors with those for the previous models 

data.frame(mean(se2),mean(se4),mean(mae2),mean(mae4)) %>% pander()
mean(se4) < mean(se2) & mean(mae4) < mean(mae2)

dt = se2 - se4 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=(1-pnorm(DMW))  
p_value # mse

dt = mae2 - mae4 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=(1-pnorm(DMW))  
p_value # mae 

### in terms of mse,mae model with b3gr is better than arma(12,12)
### but in DMW test, H0: E(dt) = 0  is not rejected
### so We can not say that there is a difference in the performance of the model.



data.frame(mean(se2),mean(se4),mean(mae2),mean(mae4)) %>% pander()
mean(se4) < mean(se2) & mean(mae4) < mean(mae2)

dt = se3 - se4 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=(1-pnorm(DMW))  
p_value # mse

dt = mae3 - mae4 ; e=dt-mean(dt); s0=t(e)%*%e/nf;
q=round(nf^(1/3)); #bandwidth
s2=0;
L=1;
while (L<=q){
  s1=0;
  t=L+1;
  while (t<=nf){
    sloop=(1-L/(q+1))*e[t]*e[t-L];
    s1=s1+sloop;
    t=t+1;
  } 
  s2=s2+s1/nf;
  L=L+1;
}

varcov=s0+2*s2;
se=sqrt(varcov);
(DMW = sqrt(nf)*mean(dt)/se) # DMW test statistic
p_value=(1-pnorm(DMW))  
p_value # mae 


### in terms of mse,mae model with b3gr is better than armax with dummy 
### but in DMW test, H0: E(dt) = 0  is not rejected
### so We can not say that there is a difference in the performance of the model.
### so We can not say that there aredifferences in the performance of three models.