help(ts) # matrix 를 시계열 자료 형태로 변환 
# CRAN : THE Comprehensive R Archive Network 



dd = matrix(c(1342,1442,1252,1343,1425,1362,1456,1272,1243,1359,1412,1253,1201,1478,1322,1406,1254,1289,1497,1208))
dd.ts = ts (dd, start = c(2006,1), frequency =4 )

diffinv(x,l,d, xi) # 역차분

ts.unoion # 시계열 합집합 , 결측은 NA

ts.intersect # 시계열 교집합 없는부분은 삭제 

window #시계열 부분 추출 및 수정

window(data,c(행,열),c(행,열)) #지정한 곳부터 지정한곳 까지 추출

tsp()  # 시작시간, 종료시간, 주 보는법 

aggregate # 시계열 통합

ts.plot #시계열 plot 

cycle() # 주기보기 


(zz = ts(matrix(1:24,8,3) , s = c(2001,1), f = 4 , n = c( 'a','b','c')) )  #matrix(값,열,행)
class(zz)

(z1 = zz[,"a"] )  # n = 'a' 만 추출 

(z3 = zz[,"a"]^2 )




lag(dd.ts, k = 2) # y(t-n) , yt의 n차 lag  ( 숫자들의 위치가  앞으로 땡겨짐)


diff(dd.ts) # lag = 1, 1차 차분   # 숫자들의 위치가 lag 만큼 뒤로 밀림 

diff(dd.ts , 4)  # lag = 4로 둔 차분 y(t) - y(t-4) 

diff(dd.ts,1,2) # lag =1 , 2차 차분 

diff(diff(dd.ts)) #2차 차분 


(d1 = diff(dd.ts))


(d2 = diffinv(d1))  #역차분 

for ( i in 1:length(dd.ts)) d3 = d2 + 1142
d3


(zz1 = ts(matrix(1:24,8,3) , s = c(2001,1), f = 4 , n = c( 'a','b','c')) )  #matrix(값,열,행)

window(zz1 , s = c(2001,3) , delta = 1 ) # 2001년 3분기부터 3분기만 1년 마다  뽑기 

window(zz1, c(2001,3) , c(2002,2)) 


window(zz1, c(2001,1) , c(2001,2)) <- c(11,12)  # 수정 가능 2001년 1분기는  다 11, 2분기는 다 12가 됨 
  

aggregate(zz1)  # f = 4를 f =1로 변화하면서 분기별이 연도로 변환

aggregate(zz1, nf = 2, FUN = mean) # 상/하반기 평균자료로 변환 


acf(x , lag.max = , type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE , ci =0.95 ,..)


pacf() #partial 


ccf # cross correlation, covariance


library(forecast)
library(stats)

taperedpacf  # bootstrpa sample에 의한 pacf 추정 



filter(x , filter ,method = c('convolution', 'recursive'), side = 2 , circular =FALSE , iniit)
#filter  rep(1/3 ,3 )
# w1 = c(0.4,0.3,0.2,0.1) filter = w1 라고 하면 가중 평균 

# data , MA 상수 , convolution = MA , recursive = AR , sidle = 1(한쪽에 NA) , 2 (양쪽에 NA) , 
#circular = ma에서만 적용, 마지막 추정치의 처리, init = AR에서만 초기값의 설정 


ma(x , order , centre = TRUE)

#order = ma할 갯수 , centre = 중심 

smooth(x, kind = '3RS3R', twiceit =FALSE , endrule = 'Tukey' , do.ends = FALSE)
#KIND 종류 , twiceit =분석결과의 중복성 여부 , endrule = 첫과 끝부분 처리방법 , do.ends = 끝부분 처리여부'

decompose()

library(forecast)

tsdisplay(data, main = )  #  acf, pacf, 나타냄 main = 제목 

wapply(x, y, fun=mean, method="range", width, n=50, drop.na=TRUE, pts, ...) # local legion 에서 계산 '


acf(x , lag.max = , type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE , ci =0.95 ,..)


pacf() #partial 


ccf # cross correlation, covariance


library(forecast)
library(stats)

taperedpacf  # bootstrpa sample에 의한 pacf 추정 



filter(x , filter ,method = c('convolution', 'recursive'), side = 2 , circular =FALSE , iniit)
#filter  rep(1/3 ,3 )
# w1 = c(0.4,0.3,0.2,0.1) filter = w1 라고 하면 가중 평균 

# data , MA 상수 , convolution = MA , recursive = AR , sidle = 1(한쪽에 NA) , 2 (양쪽에 NA) , 
#circular = ma에서만 적용, 마지막 추정치의 처리, init = AR에서만 초기값의 설정 


ma(x , order , centre = TRUE)

#order = ma할 갯수 , centre = 중심 

smooth(x, kind = '3RS3R', twiceit =FALSE , endrule = 'Tukey' , do.ends = FALSE)
#KIND 종류 , twiceit =분석결과의 중복성 여부 , endrule = 첫과 끝부분 처리방법 , do.ends = 끝부분 처리여부'

