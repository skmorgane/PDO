library(WaveletComp)
library(forecast)

##################### FUNCTIONS 

create_subset = function(date_column, data_column, date, data_header)
  {
  # take .csv file and process to format suitable for use in 
  # WaveletComp. Date column must be named 'date' & not be first
  # column in dataframe
  
  df = data.frame(data_column,date_column)
  subset = subset(df, as.Date(df$date_column) > date)
  subset = subset[order(subset$date_column),]
  colnames(subset) = c(data_header, "date")
  return(subset)
  
}

extract_scale_transform = function(wavelet_object, periods)
{
  headers = character()
  scale_matrix = c()
  for (period in periods)
  {
    closest_period = which.min(abs(wavelet_object$Period - period))
    period_wavelet = Re(wavelet_object$Wave[closest_period,])
    headers = append(headers, paste("period",as.character(period), sep=""))
    scale_matrix = cbind(scale_matrix, period_wavelet)
    colnames(scale_matrix) = headers
  }
  output = data.frame(scale_matrix, wavelet_object$series['date'])
}

##################### MAIN CODE

min_date = '1992-02-15'
#import and process NDVI
dataN = read.csv("monthly_NDVI.csv")
dataN$mdy = as.Date(paste(dataN$Date, "15",sep="-"), 
                    format="%Y-%m-%d")
NDVI = create_subset(dataN$mdy, dataN$NDVI, min_date, 'NDVI')

# import and process rodent energy
dataE = read.csv("month_energy.csv")
energy = create_subset(as.Date(dataE$fulldate, format="%m/%d/%Y"), 
                       dataE$Energy, min_date, "energy")

# import and process precipitation data
datappt = read.csv("SanSimon_ppt_1946.csv")
datappt$date = as.Date(paste(datappt$YEAR, datappt$MONTH, "15",sep="-"),
                      format="%Y-%m-%d")
datappt$pptmm = datappt$ppt/10
ppt = create_subset(as.Date(datappt$date, format="%Y-%m-%d"), 
                    datappt$pptmm, min_date, 'ppt')


# create wavelet transform data for NDVI, precipitation, and Energy for subsetted data

ppt.w = analyze.wavelet(ppt, "ppt",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(ppt.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Precipitation (1992-2014)")

NDVI.w = analyze.wavelet(NDVI, "NDVI",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(NDVI.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "NDVI (1992-2014)")

energy.w = analyze.wavelet(energy, "energy",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(energy.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Rodent Energy (1992-2014)")

# extract specific scales from wavelet transforms

periods = c(6,12,24,48,60)
ppt_scaledata = extract_scale_transform(ppt.w, periods)
NDVI_scaledata = extract_scale_transform(NDVI.w, periods)
energy_scaledata = extract_scale_transform(energy.w, periods)

# spectral density plots

wt.avg(energy.w)
wt.avg(ppt.w)
wt.avg(NDVI.w)

# cross-wavelet correlations (no lags to my knowledge)

ppt_ndvi = data.frame(ppt = ppt$ppt, NDVI=NDVI$NDVI, date = ppt$date)
cross_wavelet = analyze.coherency(ppt_ndvi, my.pair=c("ppt","NDVI"),
                                  loess.span = 0, dt = 1/12,
                                  lowerPeriod = 1/12, make.pval = T,
                                  n.sim=100)
wc.image(cross_wavelet)
wc.avg(cross_wavelet)
wc.image(cross_wavelet, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"))

pre_121515_NDVI = subset(NDVI_test, as.Date(NDVI_test$date, 
                                            format="%Y-%m-%d") < "2014-12-15")
ndvi_energy = data.frame(NDVI=pre_121515_NDVI$NDVI, rodent=energy$energy, date = pre_121515_NDVI$date)
cross_wavelet2 = analyze.coherency(ndvi_energy, my.pair=c("NDVI","rodent"),
                                  loess.span = 0, dt = 1/12,
                                  lowerPeriod = 1/12, make.pval = T,
                                  n.sim=100)
wc.image(cross_wavelet2)
wc.avg(cross_wavelet2)
wc.image(cross_wavelet2, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"))
