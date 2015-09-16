library(WaveletComp)

create_subset = function(date_column, data_column, date){
  
  df = data.frame(data_column,date_column)
  subset = subset(df, as.Date(df$date_column) > date)
  subset[order(subset$date_column),]
}

#import and process NDVI
dataN = read.csv("monthly_NDVI.csv")
dataN$mdy = as.Date(paste(dataN$Date, "15",sep="-"), format="%Y-%m-%d")
NDVI = create_subset(dataN$mdy, dataN$NDVI, '1992-02-15')
colnames(NDVI) = c("NDVI", "date")

# import and process rodent energy
dataE = read.csv("month_energy.csv")
energy = create_subset(as.Date(dataE$fulldate, format="%m/%d/%Y"), 
                       dataE$Energy,'1992-02-15')
colnames(energy) = c("energy", "date")

# import and process precipitation data
datappt = read.csv("SanSimon_ppt_1946.csv")
datappt$date = as.Date(paste(datappt$YEAR, datappt$MONTH, "15",sep="-"),
                      format="%Y-%m-%d")
datappt$pptmm = datappt$ppt/10
ppt = create_subset(as.Date(datappt$date, format="%Y-%m-%d"), 
                    datappt$pptmm,'1992-02-15')
colnames(ppt) = c("ppt", "date")

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

#entire rodent series and preciptiation series
energyall = data.frame(dataE$Energy, as.Date(dataE$fulldate, format="%m/%d/%Y"))
colnames(energyall) = c("Energy", "date")

ppt_portal = create_subset(as.Date(datappt$date, format="%Y-%m-%d"), 
                                 datappt$pptmm,'1977-07-15')
colnames(ppt_portal) = c("ppt", "date")

energyall.w = analyze.wavelet(energyall, "Energy",  dt=1, lowerPeriod = 2, upperPeriod = 300, make.pval = T, n.sim = 100)
wt.image(energyall.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Rodent Energy (1977-2014)")

pptportal.w = analyze.wavelet(ppt_portal, "ppt",  dt=1, lowerPeriod = 2, upperPeriod = 300, make.pval = T, n.sim = 100)
wt.image(pptportal.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Precipitation (1977-2014)")
