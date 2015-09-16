library(forecast)
library(reshape)
library(pracma)


create_ts = function(date_column, data_column, starting_date){
  # takes date and data columns from a data dataframe
  # reformats and outputs a timeseries object for the
  # subset of data specified using the date input.
  # starting_date is the date the subset should begin at
  # starting_date should be formatted YYYY-MM-DD

  df = data.frame(date_column, data_column)
  subset_df = subset(df, as.Date(df$date_column) > date)
  mindate = min(subset_df$date_column)
  maxdate = max(subset_df$date_column)
  min_year = as.numeric(strftime(mindate, "%Y"))
  min_month = as.numeric(strftime(mindate, "%m"))
  max_year = as.numeric(strftime(maxdate, '%Y'))
  max_month = as.numeric(strftime(maxdate, '%m'))
  ts(subset_df$data_column, start=c(min_year, min_month), end=c(max_year, max_month),
     frequency = 12)
}

#import and process NDVI data into timeseries object

dataN = read.csv("monthly_NDVI.csv")
dataN$mdy = as.Date(paste(dataN$Date, "15",sep="-"), format="%Y-%m-%d")
tNDVI = create_ts(dataN$mdy, dataN$NDVI, '1992-02-15')

# import and process rodent energy into timeseries object
#     currently creates a long and short version. Long version
#     is entire timeseries. Short version matches current NDVI
#     timespan.

dataE = read.csv("month_energy.csv")
tE_short = create_ts(as.Date(dataE$fulldate, format="%m/%d/%Y"), 
                     dataE$Energy,'1992-02-15')
tE_all = create_ts(as.Date(dataE$fulldate, format="%m/%d/%Y"), 
                   dataE$Energy,'1977-6-15')

# import and process precipitation data
#     currently creates three versions: entire precip data (all),
#     portal time span (1977), and NDVI time span (1992)

datappt = read.csv("SanSimon_ppt_1946.csv")
datappt$mdy = as.Date(paste(datappt$YEAR, datappt$MONTH, "15",sep="-"),
                      format="%Y-%m-%d")
datappt$pptmm = datappt$ppt/10
ts_ppt_1992 = create_ts(datappt$mdy, datappt$pptmm, '1992-02-15')
ts_ppt_1977 = create_ts(datappt$mdy, datappt$pptmm, '1977-06-15')
ts_ppt_all = create_ts(datappt$mdy, datappt$pptmm, '1945-12-15')

####  Seasonal and trend fits for NDVI, Rodents, and PPt 1992-2014


x_1992_2015 = seq(from=1992, to=2015, by=1)     # used for x-axis

# Precipitation

fit_p1992 = stl(ts_ppt_1992, t.window=61, s.window=7) #Seasonal-Trend Decomp w/ Loess
plot(ts_ppt_1992, col='gray', lwd=2,                  # Plots raw time-series
     main = "Precipitation Trend 1992-2014",
     xlab = "Year",
     ylab = "Precipitation (mm)")
lines(fit_p1992$time.series[,2], col="blue", lwd=3) # adds trend-cycle component
                                                    # of NDVI timeseries decomposition
y_hat=mean(fit_p1992$time.series[,2])               
abline(h=y_hat)                                     # adds mean from trend component

plot(fit_p1992$time.series[,2], col="blue", lwd=3)  # plots just trend-cycle component of STL

plot(fit_p1992$time.series[,"seasonal"],            # plots seasonal signal from STL
     xlim=c(1992,2015), 
     xaxt='n',col='blue',lwd=1.5,
     main = "Seasonal Signal of Precipitation",
     xlab = "YEAR",
     ylab = "Seasonal Component")
axis(side=1, at= x_1992_2015, las=2)
abline(v=x_1992_2015, lty = 3)

post98 = subset(datappt, as.Date(datappt$mdy, format="%m/%d/%Y") 
                > '1992-2-15')

# NDVI

fit_n92 = stl(tNDVI, t.window=61,s.window=7)    #season-time loess
                                #using default t.window 
                                #7-month seasonal window
plot(tNDVI, col='gray', lwd=2,
     main = "NDVI Trend 1992-2014",
     xlab = "Year",
     ylab = "NDVI Index")
lines(fit_n92$time.series[,2], col="green", lwd='3') #trend-cycle component of NDVI timeseries
y_hat=mean(fit_n92$time.series[,2])
abline(h=y_hat)


plot(fit_n92$time.series[,"seasonal"],xlim=c(1992,2015), xaxt='n', col='darkgreen',
     main = "Seasonal Signal of NDVI",
     xlab = "YEAR",
     ylab = "Seasonal Component")
axis(side=1, at= x_1992_2015, las=2)
abline(v=x_1992_2015, lty = 3)

# Rodents
fit_e92 = stl(tE_short,t.window=61, s.window=7)
plot(tE_short, col='gray', lwd=2,
     main = "Rodent Energy Trend 1992-2014",
     xlab = "Year",
     ylab = "Rodent Energy (Watts) ",
     cex.lab = .1)
lines(fit_e92$time.series[,2], col="orange", lwd=3) #trend-cycle component of NDVI timeseries
y_hat=mean(fit_e92$time.series[,2])
abline(h=y_hat)

plot(fit_e92$time.series[,"seasonal"],xlim=c(1992,2015), xaxt='n', col='orange', lwd=3,
     main = "Seasonal Signal of Rodent Energy",
     xlab = "YEAR",
     ylab = "Seasonal Component")
axis(side=1, at= x_1992_2015, las=2)
abline(v=x_1992_2015, lty = 3)

# Correlations
# trend correlations
ccf_ndvi_ppt_t = ccf(fit_p1992$time.series[,"trend"], fit_n92$time.series[,"trend"], lag.max=60, plot=TRUE, main="PPT & NDVI - Trend")
ccf_ndvi_E_t= ccf(fit_n92$time.series[,"trend"], fit_e92$time.series[,"trend"], lag.max=60, plot=TRUE, main="NDVI & Rodents- Trend")
ccf_ppt_E_t = ccf(fit_p1992$time.series[,"trend"], fit_e92$time.series[,"trend"], lag.max=60, plot=TRUE, main="PPT & Rodents - Trend")

#Seasonal Correlations
ccf_ndvi_ppt_s = ccf(fit_p1992$time.series[,"seasonal"], fit_n92$time.series[,"seasonal"], lag.max=12, plot=TRUE, main="PPT & NDVI")
ccf_ndvi_E_s= ccf(fit_n92$time.series[,"seasonal"], fit_e92$time.series[,"seasonal"], lag.max=12, plot=TRUE, main="NDVI & Rodents")
ccf_ppt_E_s = ccf(fit_p1992$time.series[,"seasonal"], fit_e92$time.series[,"seasonal"], lag.max=12, plot=TRUE, main="PPT & Rodents")

# remainder correlations
ccf_ndvi_ppt_r = ccf(fit_p1992$time.series[,"remainder"], fit_n92$time.series[,"remainder"], lag.max=60, plot=TRUE, main="PPT & NDVI")
ccf_ndvi_E_r= ccf(fit_n92$time.series[,"remainder"], fit_e92$time.series[,"remainder"], lag.max=60, plot=TRUE, main="NDVI & Rodents")
ccf_ppt_E_r = ccf(fit_p1992$time.series[,"remainder"], fit_e92$time.series[,"remainder"], lag.max=60, plot=TRUE, main="PPT & rodents")

### Portal-length Seasonal and trend fits for Rodents and PPT 1977-2014

at_all = seq(from=1977, to=2015, by=1)

# ppt
fit_ppt77 = stl(ts_ppt_1977, s.window=7)
plot(ts_ppt_1977, col='gray', lwd=1.5,
     main = "Precipitation Trend 1977-2014",
     xlab = "Year",
     ylab = "Precipitation (mm)")
lines(fit_ppt77$time.series[,2], col="red")
y_hat=mean(fit_ppt77$time.series[,2])
abline(h=y_hat)

post98 = subset(datappt, as.Date(datappt$mdy, format="%m/%d/%Y") 
               > '1997-12-15')
pre98 = subset(datappt, as.Date(datappt$mdy, format="%m/%d/%Y") 
                < '1998-01-15' & as.Date(datappt$mdy, format="%m/%d/%Y") 
               > '1977-06-15')
tspre98 = ts(pre98$pptmm, start=c(1977,7), end=c(1997,12), frequency=12)
mean_77 = mean(pre98$pptmm)
mean_98 = mean(post98$pptmm)

plot(fit_ppt77$time.series[,2], col="red", 
     main = "Precipitation Trend 1977-2014",
     xlab = "Year",
     ylab = "Precipitation (mm)")
y_hat=mean(fit_ppt77$time.series[,2])
abline(h=y_hat)
segments(1977,mean_77,1997, mean_77, col= 'blue', lwd=3)
segments(1998,mean_98,2015, mean_98, col= 'blue', lwd=3)

fit = stl(tspre98, s.window=7)
plot(fit$time.series[,"seasonal"],xlim=c(1977,1997), xaxt='n',
     main = "Seasonal Signal of Precipitation 1977-1997",
     xlab = "YEAR",
     ylab = "Seasonal Component")
at_pre = seq(from=1977,to=1997, by=1)
axis(side=1, at= at_pre, las=2)
abline(v=at_pre, lty = 3)

plot(fit_ppt77)

# rodent
fit_eall = stl(tE_all,s.window=7)
plot(tE_all, col='gray',
     main = "Rodent Energy 1977-2014",
     xlab = "Year",
     ylab = "Precipitation (mm)")
lines(fit_eall$time.series[,2], col="red") #trend-cycle component of NDVI timeseries
y_hat=mean(fit_eall$time.series[,2])
abline(h=y_hat)

plot(fit_eall)

post98E = subset(dataE, as.Date(dataE$fulldate, format="%m/%d/%Y") 
                > '1997-12-15')
pre98E = subset(dataE, as.Date(dataE$fulldate, format="%m/%d/%Y") 
               < '1998-01-15')
tspre98E = ts(pre98E$Energy, start=c(1977,7), end=c(1997,12), frequency=12)
meanE_77 = mean(pre98E$Energy)
meanE_98 = mean(post98E$Energy)

plot(fit_eall$time.series[,2], col="red",
     main = "Rodent Energy 1977-2014",
     xlab = "Year",
     ylab = "Energy (Watts)")
y_hat=mean(tE_all)
abline(h=y_hat)
segments(1977,mean_77,1997, mean_77, col= 'blue', lwd=3)
segments(1998,mean_98,2015, mean_98, col= 'blue', lwd=3)

plot(fit_eall$time.series[,"seasonal"],xlim=c(1977,2015), xaxt='n',
     main = "Seasonal Signal of Rodent Energy 1977-1997",
     xlab = "YEAR",
     ylab = "Seasonal Component")
axis(side=1, at= at_all, las=2)
abline(v=at_all, lty = 3)

fit_pre98E = stl(tspre98E, s.window=7)
plot(fit_pre98E$time.series[,"seasonal"],xlim=c(1977,1997), xaxt='n',
     main = "Seasonal Signal of Rodent Energy 1977-1997",
     xlab = "YEAR",
     ylab = "Seasonal Component")
at_pre = seq(from=1977,to=1997, by=1)
axis(side=1, at= at_pre, las=2)
abline(v=at_pre, lty = 3)


### PDO
www = "./Data/PDO_clean.csv" 
pdo.dat = read.csv(www, header=T)
pdo.df = as.data.frame(pdo.dat)
pdo.reshape = melt(pdo.dat, id=c("YEAR"))
sort.pdoreshape = pdo.reshape[order(pdo.reshape$YEAR, pdo.reshape$variable),]
PDO_vector = as.vector(sort.pdoreshape$value)
pdo.ts = ts(PDO_vector, start=c(1900,1), end=c(2014,12), frequency=12)

plot(pdo.ts)
fit_pdo = stl(pdo.ts,t.window = 60, s.window=7)
plot(pdo.ts, col='gray')
lines(fit_pdo$time.series[,2], col="red") #trend-cycle component of NDVI timeseries
y_hat=mean(pdo.ts)
abline(h=y_hat)

portal_PDO = subset(pdo.df, pdo.df$YEAR > 1976) 
portalpdo.reshape = melt(portal_PDO, id=c("YEAR"))
sort.portalpdoreshape = portalpdo.reshape[order(portalpdo.reshape$YEAR, 
                                          portalpdo.reshape$variable),]
portalPDO_vector = as.vector(sort.portalpdoreshape$value)
portalpdo_ts = ts(portalPDO_vector, start=c(1977,1), end=c(2014,12), frequency=12)
portal_pdo = stl(portalpdo_ts, s.window=7)
plot(portal_pdo$time.series[,"seasonal"])


fit_ppt_all = stl(ts_ppt_all, t.window = 120, s.window=7)
plot(ts_ppt_all, col='gray', ,ylab="Preciptiation (mm)")
lines(fit_ppt_all$time.series[,2], col="red", lwd=3) 
y_hat=mean(ts_ppt_all)
abline(h=y_hat)
