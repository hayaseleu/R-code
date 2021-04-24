setwd("C:/Users/WJ/Downloads/2018-2/c")
library(pander)
library(tidyverse)
library(fBasics)
library(fGarch)
library(rugarch)
options(scipen = 50)

data <- read.csv("S_Pdata.csv")
attach(data)

#### 1.When you specify the GARCH(1,1) model 

## (1) describe  your data(sample size, sample period, mean, variance, skewness)

pander(length(data$date)) # sample size
(data[1,1]) # starting point of sample period
(data[length(data$date),1]) # end point of sample period  
mean <- data$SPX_r %>% mean(); var <- data$SPX_r %>% var(); skewness <- data$SPX_r %>% skewness(); 
kurtosis <- data$SPX_r %>% kurtosis()
pander(data.frame(mean, var, skewness, kurtosis)) #mean, var, skewness, kurtosis 
ts.plot(SPX_r)


### kurosis is bigger than 3, so it has very heavy tail than normal dist. so i will assume std dist at some model.
### skewness is negative, so it's left tail is longer. but it is little. so i will not asuume skewed std dist. 

## (2) Test for ARCH effect and wee whether ARCH effect extist
fit1=garchFit(~garch(1,1),include.mean=F, data=SPX_r,trace=F,cond.dist="QMLE") 
summary(fit1)

#### in LM Arch Test, it has 0.9621 p-values. at Significance Level 0.05,  it reject H0 : alpha_1 = 0. 
#### so We can say there are ARCH Effect

## (3) Estimate the GARCH(1,1) model described above. Report the estimates and se. Explain the degree of volatility persistence

fit1@fit$coef
fit1@fit$se.coef


#### i assume model with qmle & mu 
#### i estimate by QMLE. so model's Std. Errors are robust.  
#### alpha + beta is called degree of volatility persistence.  alpha + beta =  0.9937258. it is very near in ONE
#### so it have high defree of volatility persistence.

## (4) Estimate the GJR-GARCH(1,1). Report the estimates and se. Does the result show the leverage effects?


gjr.spec = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0),include.mean=T), distribution.model = 'std')
fit2 = ugarchfit(gjr.spec,SPX_r)
fit2

fit2@fit$coef
fit2@fit$robust.se.coef


#### i assume model with t-dist & mu.
#### it has very small mu, but it can not reject at Significance Level 0.05. so i choose model with mu
#### gamma's estimate is 0.136192. and at t-test with s.e, it reject H0: gamma = 0. 
#### so yes. result show the leverage effets. we can say there are leverage effects that could not be explained in GARCH model. 


## (5) Estimate the GARCH-X model with vix

x=vix^2/252
sp=matrix(SPX_r)
sp1= sp[-1,]
sp1=matrix(sp1)
lagx=tslag(x,k=1,trim=TRUE)
lagx=matrix(lagx)
x1=lagx/10000
head(x1)
garchx.vix=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=x1),
                      mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model="std")
fit3 = ugarchfit(garchx.vix,sp1,solver="nloptr")
fit3
fit3@fit$coef
fit3@fit$robust.se.coef

#### i assume model with t-dist and without mu 
#### vxreg1's p-value is lower than 0.05. then reject H0 : gamma = 0
#### so gamma is significant at Significance Level 0.05.

## (6) Estimate the GARCH-X model with kernel

lagrk=matrix(tslag(SPX_rk,k=1,trim=T))
garchx.rk=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=lagrk),
                     mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model="std")
fit4 = ugarchfit(spec=garchx.rk,sp1, solver = "nloptr")
fit4
fit4@fit$coef
fit4@fit$robust.se.coef


#### i assume model with t-dist and without mu 
#### vxreg1's p-value is lower than 0.05. then reject H0 : gamma = 0
#### so gamma is significant at Significance Level 0.05.


## (7) compare log-likelihood values of all the models estimated. in terms of within-sample fitting, which model is the best.

model1 <- c(fit1@fit$ics,fit1@fit$llh)
model2 <- c(infocriteria(fit2),-fit2@fit$LLH)
model3  <- c(infocriteria(fit3),-fit3@fit$LLH)
model4  <- c(infocriteria(fit4),-fit4@fit$LLH)
loglikelihood  <- data.frame(model1, model2, model3 ,model4)  
colnames(loglikelihood) <- c('model 1','model 2' ,'model 3', 'model 4')
loglikelihood


#### model 2 has best AIC, BIC. so I choose model2 with GJR-GARCH(1,1)

## conduct out-of-sample one-step ahead forecast using rolling window procedure with the window size 1008. Consider only four models such as GARCH(1,1),GJR-GARCH(1,1), the model in (5), (6)


nf = 52                                  # number of forecast: 52
## GARCH(1,1) MODEL 
VF_garch = matrix(0,nf,1)                  # vector for GARCH(1,1) forecasts
for (i in 1:nf) {
  sp2 = sp[(2200+i):(2200+1007+i)]         # window size:1008
  fore_fit1=garchFit(~garch(1,1),include.mean=F,data=sp2,trace=F,cond.dist="QMLE") 
  # estimation for i'th window
  ut_2=(sp2)^2               # (y)^2
  h_t=fit1@h.t                             # conditional variance estimates
  omega=coef(fore_fit1)[1]
  alpha=coef(fore_fit1)[2]
  beta=coef(fore_fit1)[3]
  
  # calculate one-step ahead forecast
  VF_garch[i]=omega+alpha*ut_2[1008]+beta*h_t[1008] 
}
# VF_garch

## GJR_GARCH(1,1) MODEL


VF_GJRgarch = matrix(0,nf,1)              # vector for GJR-GARCH(1,1) forecasts
for (i in 1:nf) {
  sp2 = sp[(2200+i):(2200+1007+i)]        # window size:1008
  # estimation for i'th window
  fore_fit2 = ugarchfit(gjr.spec,sp2,solver="hybrid")  
  
  ut = sp2-coef(fore_fit2)[1]                 # y-mu
  ut_2= ut^2                             # (y-mu)^2
  I<-ifelse(ut[1008]<0,1,0)              # 1 if u_t<0, 0 otherwise
  
  h_t=matrix(sigma(fore_fit2)^2)      # h_t=matrix(fitted(fit2)) is fitted value of mean               # conditional variance estimates
  omega=as.numeric(coef(fore_fit2)[2])
  alpha=as.numeric(coef(fore_fit2)[3])
  beta=as.numeric(coef(fore_fit2)[4])
  gamma=as.numeric(coef(fore_fit2)[5])
  
  # calculate one-step ahead forecast
  VF_GJRgarch[i]=omega+(alpha+gamma*I)*ut_2[1008]+beta*h_t[1008] 
}
# VF_GJRgarch
### GARCH-X at (5)

VF_garchX_VIX = matrix(0,nf,1)
for (i in 1:nf) {
  sp2 = sp[(2200+i):(2200+1007+i)] 
  x2 = matrix(x1[(2200+i):(2200+1007+i)])
  garchx.vix=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=x2),mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model="std")
  # estimation for i'th window
  fore_fit3 = ugarchfit(spec=garchx.vix,sp2,solver="hybrid")
  ut_2=(sp2)^2            # (y)^2
  h_t=matrix(sigma(fore_fit3)^2)           # conditional variance estimates
  x_t=as.numeric(x2[1008])
  omega=as.numeric(coef(fore_fit3)[1])
  alpha=as.numeric(coef(fore_fit3)[2])
  beta=as.numeric(coef(fore_fit3)[3])
  xtreg=as.numeric(coef(fore_fit3)[4])
  
  # calculate one-step ahead forecast
  VF_garchX_VIX[i]=omega+alpha*ut_2[1008]+beta*h_t[1008]+xtreg*x_t 
}

#VF_garchX_VIX
#### GARCH-X at (6)
VF_garchX_RK = matrix(0,nf,1)              # vector for GARCH-X forecasts
for (i in 1:nf) {
  sp2 = sp[(2200+i):(2200+1007+i)]      # window size:1008
  lagrk2=matrix(lagrk[(2200+i):(2200+1007+i)])   
  garchx.rk=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=lagrk2),
                       mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model="std")
  
  # estimation for i'th window
  fore_fit4 = ugarchfit(spec=garchx.rk,sp2,solver="hybrid")
  ut_2=(sp2)^2            # (y)^2
  h_t=matrix(sigma(fore_fit4)^2)           # conditional variance estimates
  rk_t=as.numeric(lagrk2[1008])
  
  
  omega=as.numeric(coef(fore_fit4)[1])
  alpha=as.numeric(coef(fore_fit4)[2])
  beta=as.numeric(coef(fore_fit4)[3])
  xtreg=as.numeric(coef(fore_fit4)[4])
  
  # calculate one-step ahead forecast
  VF_garchX_RK[i]=omega+alpha*ut_2[1008]+beta*h_t[1008]+xtreg*rk_t 
}
# VF_garchX_RK


## (8) describe the number of frecasts and the forecast period.

nf ### the number of frecasts
date[c(3209,3260)] ###the forecast period

## (9) Report the Qlike and MSE of each model using the realized kernel as volatility proxy. in terms of out-of-sample-forecast, which model is the best?

qlike <- function(a,b) {
  c = a/b-log(a/b)-1
  return(c)
}
mse <- function(a,b) {
  c = mean((a-b)^2)
  return(c)
}
proxy <- SPX_rk[3209:3260]
d1_q <- qlike(proxy,VF_garch)
d2_q <- qlike(proxy,VF_GJRgarch)
d3_q <- qlike(proxy,VF_garchX_VIX)
d4_q <- qlike(proxy,VF_garchX_RK)
d1_mse <- mse(proxy,VF_garch)
d2_mse <- mse(proxy,VF_GJRgarch)
d3_mse <- mse(proxy,VF_garchX_VIX)
d4_mse <- mse(proxy,VF_garchX_RK)
qd <- data.frame(c(mean(d1_q),d1_mse),c(mean(d2_q),d2_mse),c(mean(d3_q),d3_mse),c(mean(d4_q),d4_mse))
colnames(qd) <- c('VF_garch','VF_GJRgarch','VF_garchX_VIX','VF_garchX_RK')
rownames(qd) <- c('qlike','mse')
qd

#### VF_GJRgarch is best in terms of qlike. VF_garch is best in terms of mse 

## (10) Conduct the Diebold-Mariano-West test using the realized kernel as volatility prox. Does the best model outperforms the rest models significantly?


dmwtest <- function(dt,nf) {
  e=dt-mean(dt);
  s0=t(e)%*%e/nf;
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
  
  DMW=sqrt(nf)*mean(dt)/se;  # DMW test statistic
  p_value=1-pnorm(DMW) 
  return(c(DMW,p_value))
}
d12 = d1_q - d2_q 
d32 = d3_q - d2_q 
d42 = d4_q - d2_q
dmwtest(d12,52) # DMW test statistic, p-value
dmwtest(d32,52) # DMW test statistic, p-value
dmwtest(d42,52) # DMW test statistic, p-value


#### so H0: E(dt) = 0 are not rejected with Significance Level 0.05. 
#### so We can not say that there are differences in the performance of three models.

