library(fpp2)

# Create plots of the a10 data
autoplot(data_ts)
ggseasonplot(data_ts)
ggseasonplot(data_ts, polar = TRUE)


ggsubseriesplot(data_ts)
gglagplot(data_ts)

checkresiduals()