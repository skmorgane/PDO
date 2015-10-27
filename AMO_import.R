##### Update NOAA AMO Datafile

##### This file downloads the NOAA Atlantic 
##### Multidecadal Oscillation Index (unsmoothed)
##### and exports into a csv file for further analysis

library(reshape2)

URL = "http://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data"
out <- read.fwf(URL, widths=c(5,-2,8,8,9,9,9,9,9,9,9,9,9,9), skip=1, header=FALSE,
                col.names=c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul", 
                            "Aug","Sep","Oct","Nov","Dec"))
out = head(out,-4)

AMO.df = as.data.frame(out, stringsAsFactors=FALSE)
cols = c(1:13)
AMO.df[,cols] = apply(AMO.df[,cols], 2, function(x) as.numeric(as.character(x)))
AMO.reshape = melt(AMO.df, id=c("Year"), variable.name = "Month", 
                   value.name = "AMO")
AMO.reshape$date = as.Date(paste(AMO.reshape$Year,"-",AMO.reshape$Month, "-", "15", sep=""), format="%Y-%b-%d")

write.csv(AMO.reshape, file="AMO_allyears.csv")
