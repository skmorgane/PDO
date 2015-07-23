library(forecast)
library(plyr)
library(dplyr)
library(reshape2)

data = read.csv("monthly_NDVI.csv")
data$mdy = as.Date(paste(data$Date, "15",sep="-"), format="%Y-%m-%d")
NDVI = subset(data, as.Date(data$mdy) > '1992-02-15')

ts_NDVI = ts(NDVI$NDVI, start=c(1992,3), end=c(2014,12), frequency = 12)

plot(ts_NDVI)

seasonplot(ts_NDVI, year.labels = TRUE, year.labels.left = TRUE, col=1:33)
monthplot(ts_NDVI)

acf(ts_NDVI)
naive=naive(ts_NDVI)
seasonal_naive=snaive(ts_NDVI)

plot(ts_NDVI, xlim=c(1992,2017))
lines(naive$mean, col=2)
lines(seasonal_naive$mean, col=3)

at = seq(from=1990, to=2015, by=1)

plot(ts_NDVI, col='gray')
lines(seasadj(fit), col='red', ylab='Seasonally adjusted NDVI')

plot(ts_NDVI, col='gray', xlim=c(1992,2017), xaxt='n')
axis(side=1, at= at, las=2)

fit = stl(ts_NDVI, t.window=12, s.window=6)        #season-time loessm
plot(ts_NDVI, col='gray')
lines(fit$time.series[,2], col="red") #trend-cycle component of NDVI timeseries
y_hat=mean(ts_NDVI)
abline(h=y_hat)
plot(fit)

monthplot(fit$time.series[,"seasonal"]) #shows how NDVI changes at monthly scale

plot(fit$time.series[,"seasonal"],xlim=c(1992,2015), xaxt='n')
axis(side=1, at= at, las=2)
abline(v=at)

NDVI$month = as.numeric(strftime(as.Date(NDVI$mdy, format="%Y-%m-%d"), "%m"))
NDVI$year = as.numeric(strftime(as.Date(NDVI$mdy, format="%Y-%m-%d"), "%Y"))
NDVI$seasons = ifelse(NDVI$month < 6, "Spring",
                      ifelse(NDVI$month < 11, "Summer",
                             "Winter"))
season_max = ddply(NDVI, c("year","seasons"), summarise,max_NDVI = max(NDVI, na.rm=TRUE))

season_max = filter(season_max, seasons == "Spring" | seasons == "Summer")

spr_summ = melt(season_max, id.vars = c("year", "seasons"), measure.vars = "max_NDVI")
spr_summ = dcast(spr_summ, formula = year ~ seasons)
spr_summ$diff = (spr_summ$Spring-spr_summ$Summer)
spr_summ$scaleddiff = (spr_summ$Spring-spr_summ$Summer)/sum(spr_summ$Spring, spr_summ$Summer)
plot(spr_summ$year, spr_summ$diff)
plot(spr_summ$Summer, spr_summ$diff)
results = lm(spr_summ$diff ~ spr_summ$year)
summary(results)
