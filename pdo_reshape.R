##### Update PDO Datafile

##### This file downloads PDO index from official site
##### Run by Nate Mantua And cleans it up so it can be
##### Exported into a csv file for further analysis

library(reshape2)

URL =  "http://jisao.washington.edu/pdo/PDO.latest"
out <- read.fwf(URL, widths=c(4,-4,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5,-2,5), 
                skip=32, header=FALSE, col.names=c("Year","Jan","Feb","Mar","Apr","May","Jun",
                                                   "Jul", "Aug","Sep","Oct","Nov","Dec"))
out = head(out,-19)

pdo.df = as.data.frame(out, stringsAsFactors=FALSE)
cols = c(1:13)
pdo.df[,cols] = apply(pdo.df[,cols], 2, function(x) as.numeric(as.character(x)))
pdo.reshape = melt(pdo.df, id=c("Year"), variable.name = "Month", 
                   value.name = "PDO")
pdo.reshape$date = as.Date(paste(pdo.reshape$Year,"-",pdo.reshape$Month, "-", "15", sep=""), format="%Y-%b-%d")

write.csv(pdo.reshape, file="PDO_allyears.csv")
