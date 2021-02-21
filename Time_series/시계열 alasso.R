library(itsmr)
wine
library(parcor)

#log를 먼저취해야 음수가 안나옴 
data = log(wine)
data = diff(data, lag = 12)



ar.adaplasso = function(y, p, nf){
  if(missing(nf)){ nf = 10 };
  if(missing(p)){ phat = ar(y, aic = TRUE, order.max=10)$order }
  # Check y is a vector
  y = as.vector(y);
  n = length(y);
  mu.s = mean(y);
  id = 1:n;
  X = NULL;
  for(j in 1:p){
    id1 = id-j;
    id2 = id1[id1 <= 0];
    id3 = id1[id1 > 0];
    X = cbind(X, c(rep(mu.s, length(id2)), y[id3]));
  }
  pp = adalasso(X, y, k=nf, intercept=FALSE);           # intercept 포함 불포함! 
  return(pp)
}


n=250;
#phi = c(.5, .3, .1);
phi = c(.7, .3, 0, -.2);

nrep=50;
order=5;
A = B = matrix(0, nrep, order);
for(r in 1:nrep){
  data = arima.sim(n = n, list(ar = phi), sd = 1)
  y = data/sd(data);
  fit = ar.adaplasso(y, p=order)
  fit = ar.adaplasso(y, p=order)
  A[r,] = fit$coefficients.adalasso
  B[r,] = fit$coefficients.lasso
  print(r)
}

par(mfrow=c(1,2))
boxplot(A, main="aLasso");
boxplot(B, main="Lasso");  ## aLasso가 조금 더 0으로 확실히 수렴시키는듯 

ar12 <- arima(data, c(12,0,0))
(ar12)


nrep=50;
order=24;
A = B = matrix(0, nrep, order);

st_data <- data_d1
st_data <- data_d1_sup/sd(data_d1_sup)
for (r in 1:nrep) {

  fit = ar.adaplasso(st_data, p=order)
  fit = ar.adaplasso(st_data, p=order)
  A[r,] = fit$coefficients.adalasso
  B[r,] = fit$coefficients.lasso
  print(r)
}
par(mfrow=c(1,2))
boxplot(A, main="aLasso");
boxplot(B, main="Lasso") ## aLasso가 조금 더 0으로 확실히 수렴시키는듯 

## 이렇게 수렴시켜서 남긴 모델로만 다시 패널티없는 일반 회귀로 하면 더 추정이 잘됨. 즉 다시 재추정해~

## adalasso 명령어 검색해 보기 
