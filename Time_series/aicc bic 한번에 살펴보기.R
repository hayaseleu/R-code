qqnorm(resi)
qqline(resi)


AICC = BIC = AIC = P = Q = NULL;
pmax=24; qmax=12;

n <- length(data)
data_d1 <- diff(data, lag =1 )
data_d1_sup <- c(rep(mean(data_d1),(length(data)-length(data_d1))),data_d1)


for(p in 0:qmax){
  for(q in 0:qmax){
    fit = arima(data_d1_sup, order=c(p, 0, q), include.mean=F,method = c("ML"));  # 차분을 미리한걸 넣자! # mean 포함? 
    m = p+q+2 ;  # mean이 없으면 p+q+1 
    AIC = c(AIC, -2*fit$loglik + 2*m);
    AICC = c(AICC, -2*fit$loglik + 2*m*n/(n-m-1));
    BIC = c(BIC, -2*fit$loglik + m*log(n));
    P = c(P, p);
    Q = c(Q, q);
  }}

id1 = which.min(AICC);
id2 = which.min(BIC);
c(P[id1], Q[id1])

c(P[id2], Q[id2])

plot(AICC);
plot(BIC);


a <- arima(data_d1_sup, order = c(12,0,12), include.mean = F, method = c("ML"))
test(a$residuals)




# 이렇게 하고 계수를 줄여나가도 괜찮을듯? 
