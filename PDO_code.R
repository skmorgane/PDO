library(tseries)
library(reshape)
library(TTR)

#imports PDO datafile and reshapes it for conversion to a time series object
www = "PDO_clean.csv" 
pdo.dat = read.csv(www, header=T)
pdo.df = as.data.frame(pdo.dat)
pdo.reshape = melt(pdo.dat, id=c("YEAR"))
sort.pdoreshape = pdo.reshape[order(pdo.reshape$YEAR, pdo.reshape$variable),]
PDO_vector = as.vector(sort.pdoreshape$value)
pdo.ts = ts(PDO_vector, start=c(1900,1), end=c(2013,12), frequency=12)

plot_data_window = function(data, origin, terminal, xlabel, ylabel, title_plot){
  #takes time series data and specifics on time window, plot and axis labels and makes a graph
  data_window = window(data, start = origin, end = terminal)
  window_mean = mean(data_window)
  plot(data_window, lwd=1.5, col='cornflowerblue', ylab = ylabel, xlab = xlabel,
       cex.lab = 1.6, main=title_plot,cex.main =1.25)
  hline_mean = abline(h=window_mean, lty='dashed')  
}

coldphase = plot_data_window(pdo.ts, c(1947,1), c(1976,12), "Year", 'Important Environment Condition', 
                             "Monthly Environment Data (1947-1976)")
post_1977 = plot_data_window(pdo.ts, c(1977, 1), c(2013, 12), "Year", 'Important Environment Condition', 
                             "Monthly Environment Data (1977-2013)")
both_phases = plot_data_window(pdo.ts, c(1947,1), c(2013, 12), "Year", 'Important Environment Condition', 
                               "Monthly Environment Data (1947-2013)")


pdo_win = SMA(pdo.ts,n=60) #smoothes time series by taking average over n months
all_data = plot_data_window(pdo.ts, c(1900,1), c(2013, 12), "Year", 'Important Environment Condition', 
                            "Monthly Environment Data (1900-2013)")
lines(pdo_win,lwd=3, col='black')
abline(v=1977, col='red', lwd=2)
abline(v=1946, col='blue', lwd=2)
abline(v=1925, col='red', lwd =2)

