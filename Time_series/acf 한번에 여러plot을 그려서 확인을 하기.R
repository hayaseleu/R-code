
## data의 여러 acf를 plot으로 살펴볼때 
s <- 12 # 주기 
n <- length(data)
par(mfrow = c(4,4)) 

# lag1 부터 lag 16 까지 찍어보기 
par(mfrow = c(4,4))
for ( i in 1:16) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}


for ( i in 16) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}
points(data[(n-34):(n-14)],data[(n-20):n], col = "red")  # 후기의 데이터를 빨간점으로 찍음 
points(data[(n-78):(n-48)],data[(n-64):(n-34)], col = "Blue1") # 후기보다 조금 앞의 데이터를 파란색# 으로 찍음 
points(data[1:30],data[15:44], col = "green")  # 초기의 데이터를 녹색으로 찍음 


# 하나의 acf에서 시간대에 따라 다른 점 찍어보기 

for ( i in 14) {
  plot(final[1:(n-i)], final[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}
points(final[(n-34):(n-14)],final[(n-20):n], col = "red")
points(final[(n-78):(n-48)],final[(n-64):(n-34)], col = "Blue1")
points(final[1:30],final[15:44], col = "green")






par(mfrow = c(2,2)) 
for ( i in 13:16) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}

for ( i in 14) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}
points(data[(n-34):(n-14)],data[(n-20):n], col = "red")
points(data[(n-78):(n-48)],data[(n-64):(n-34)], col = "Blue1")
points(data[1:30],data[15:44], col = "green")

for ( i in 15) {
  plot(data[1:(n-i)], data[(i+1):n], xlab =  paste(" X_{t-", as.character(i), "}") ,  ylab = "X_t")
}
points(data[(n-35):(n-15)],data[(n-20):n], col = "red")
points(data[(n-79):(n-49)],data[(n-64):(n-34)], col = "Blue1")
points(data[1:30],data[16:45], col = "green")


#########################

# 주기별로 plot 그려보기 
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




# 첫 season
sea1 <- season_index[1,]
season1_data <- data[sea1]
season1_data <- season1_data[!is.na(season1_data)] 


sea2 <- season_index[2,]
season2_data <- data[sea2]
season2_data <- season2_data[!is.na(season2_data)] 
