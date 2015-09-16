library(WaveletComp)

create_subset = function(date_column, data_column, date){
  
  df = data.frame(date_column, data_column)
  subset = subset(df, as.Date(df$date_column) > date)
  subset[order(subset$date_column),]
}

#import and process NDVI
dataN = read.csv("monthly_NDVI.csv")
dataN$mdy = as.Date(paste(dataN$Date, "15",sep="-"), format="%Y-%m-%d")
NDVI = create_subset(dataN$mdy, dataN$NDVI, '1992-02-15')

# import and process rodent energy
dataE = read.csv("month_energy.csv")
energy = create_subset(as.Date(dataE$fulldate, format="%m/%d/%Y"), 
                       dataE$Energy,'1992-02-15')

# import and process precipitation data
datappt = read.csv("SanSimon_ppt_1946.csv")
datappt$date = as.Date(paste(datappt$YEAR, datappt$MONTH, "15",sep="-"),
                      format="%Y-%m-%d")
datappt$pptmm = datappt$ppt/10


my.w = analyze.wavelet(datappt, "date", dt=1, lowerPeriod = 2, upperPeriod = 500, make.pval = T, n.sim = 100)
wt.image(my.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d" ,legend.params = list(lab = "wavelet power levels",
                                                                        mar = 4.7))
