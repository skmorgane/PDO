##### Update ENSO MEI Datafile

##### This file downloads Multivariate ENSO Index (MEI) 
##### from official site at NOAA.
##### Exported into a csv file for further analysis

library(reshape2)

URL =  "http://www.esrl.noaa.gov/psd/data/correlation/mei.data"
out <- read.fwf(URL, widths=c(4,-3,6,8,8,8,8,8,8,8,8,8,8,8), skip=1, header=FALSE,
                col.names=c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul", 
                            "Aug","Sep","Oct","Nov","Dec"))
out = head(out,-3)

MEI.df = as.data.frame(out, stringsAsFactors=FALSE)
cols = c(1:13)
MEI.df[,cols] = apply(MEI.df[,cols], 2, function(x) as.numeric(as.character(x)))
MEI.reshape = melt(MEI.df, id=c("Year"), variable.name = "Month", 
                   value.name = "MEI")
MEI.reshape$date = as.Date(paste(MEI.reshape$Year,"-",MEI.reshape$Month, "-", "15", sep=""), format="%Y-%b-%d")

write.csv(MEI.reshape, file="MEI_allyears.csv")
