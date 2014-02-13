library(plyr)
library(TTR)

add_wateryr = function(data){
#take .csv weather file from NOAA, determines what water year and precip  
#season each month belongs to. This info is added to the original file
# and returned.
  
  wateryr = c()
  waterseason = c()
  for (i in 1:nrow(data)){
    if (data[i,4] < 4){
      water_year = data[i,3]
      season = 0
    } else if (data[i,4] > 9){
      water_year = data[i,3] + 1
      season = 0
    } else {
      water_year = data[i,3] + .5
      season = 1
    }
    wateryr = rbind(wateryr, water_year)
    waterseason = rbind(waterseason, season)
    }
  wateryr = as.vector(wateryr)
  waterseason = as.vector(waterseason)
  merged_data1 = cbind(data, wateryr)
  merged_data2 = cbind(merged_data1, waterseason)
  return(merged_data2)
}

summarize_into_seasons = function(wateryears, precip){
  #takes a dataframe and generates a matrix output of
  #summed ppt for each season in each year.
  min_yr = min(wateryears)
  max_yr = max(wateryears)
  unique_yrs = seq(min_yr,max_yr, by=0.5)
  data = cbind(wateryears, precip)
  seasonal_ppt = c()
  for (date in unique_yrs){
    year_data = subset(data, data[,1] == date)
    total_mm = sum(year_data[,2])/10
    datum = c(date, total_mm)
    seasonal_ppt = rbind(seasonal_ppt, datum)
  }
  return(seasonal_ppt)
}

smooth_timeseries = function(data, window, sample_freq){
  #gernates a smoothed data set, from a matrix, given a window size
  #and the sampling frequency (samples/year) of the time series
  min_yr = min(data[,1])
  max_yr = max(data[,1])
  seasonal.ts = ts(data[,2], start = c(min_yr), end = c(max_yr), frequency = sample_freq)
  sansimon_win <- SMA(seasonal.ts,n=window)
  length_sansimonwin = length(sansimon_win)
  smoothed = sansimon_win[window:length_sansimonwin]
  start_date = 1930 + window/sample_freq
  smoothed.ts = ts(smoothed, start = start_date, end = max_yr, frequency = sample_freq)
  return(smoothed.ts)
}

###Main 
#San Simon data processing and graphs
www = "SanSimon_ppt.csv"
sansimon.dat = read.csv(www, header=T)
sansimon.dat = as.data.frame(sansimon.dat, header=T)
new_sansimon = add_wateryr(sansimon.dat)
season_ppt = summarize_into_seasons(new_sansimon$wateryr, new_sansimon$TPCP_10thmm)
winter_ppt = subset(season_ppt, season_ppt[,1] == floor(season_ppt[,1])) 
window_size = 3
sampling_freq = 1
smoothed_winter = smooth_timeseries(winter_ppt,window_size, sampling_freq)
year = seq(min(winter_ppt[,1]+window_size*sampling_freq), max(winter_ppt[,1], by=1))
plot(year, smoothed_winter, type='l', lwd=1.5, ylab="", xlab="")
title(main="Winter Precipitation - San Simon, AZ", xlab="Year", ylab="Precipitation (mm)", cex.lab = 1.25)
abline(h=mean(winter_ppt[,2]), lty='dashed', col='black')
abline(v=1977, col='red')
abline(v=1946, col='blue')
abline(v=1998, col='blue')

#Portal 4 SW
www = "Portal_4SW_merge.csv"
portal4SW.dat = read.csv(www, header=T)
portal4SW.dat = as.data.frame(portal4SW.dat, header=T)
new_portal = add_wateryr(portal4SW.dat)
season_portal = summarize_into_seasons(new_portal$wateryr, new_portal$AvgOfTPCP)
winter_portal = subset(season_portal, season_portal[,1] == floor(season_portal[,1])) 
window_size = 3
sampling_freq = 1
smoothed_winter_portal = smooth_timeseries(winter_portal, window_size, sampling_freq)
year = seq(min(winter_portal[,1]+window_size*sampling_freq), max(winter_portal[,1], by=sampling_freq))
plot(year, smoothed_winter_portal, type='l', lwd=1.5, main = "Winter Precipitation - Portal, AZ",
        ylab = 'Precipitation (mm)')
abline(h=mean(winter_portal[,2]), lty='dashed', col='black')
mtext(window_size, "year moving average")
abline(v=1977, col='red')
abline(v=1946, col='blue')
abline(v=1998, col='blue')

