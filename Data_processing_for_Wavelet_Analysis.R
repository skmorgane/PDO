### Goal of this code is to format the monthly timeseries of NDVI, 
### precipitation, and Rodent Energy Use for use in wavelet 
### analyses using WaveletComp. This entails subsetting the 
### data so that all timeseries are the same length and years
### and creating dataframes for each timeseries containing the
### monthly values and column of dates with the specific column
### name "date"

######## FUNCTIONS

make_df = function(date_column, data_column, data_header)
{
  # Create and format a dataframe given two columns of data
  #
  # Args: 
  #    date_column: dataframe column containing date values
  #    data_column: dataframe column containing data values
  #    data_header: string to be used as column name for data column
  #
  # Returns:
  #    Dataframe ordered by date with appropriate column names
  #    for use in WaveletComp package.
  
  df = data.frame(data_column,date_column)
  df = df[order(df$date_column),]
  colnames(df) = c(data_header, "date")
  return(df)
}

strip_date = function(date){
  # Splits character date into year, month, day matrix
  #
  # Args: 
  #    date: character vector containing a date formatted 
  #    as YYYY-MM-DD
  #
  # Returns:
  #    Matrix where year, month, day are separate entries
  
  stripped = strsplit(date, "-")
  date_pieces <- matrix(unlist(stripped), ncol=3, byrow=TRUE)
  return(date_pieces)
}

make_filename = function(min_date, max_date, data_name){
  # Make filename which includes date range of data
  #
  # Args:
  #    min_date: character vector containing minimum date of data
  #    max_date: character vector containing maximum date of data
  #    data_name: character vector containing type of data
  #
  # Return:
  #    Character vector containing the data type, min and max dates, 
  #    and file type (.csv)
  
  min = strip_date(min_date)
  max = strip_date(max_date)
  filename = paste(data_name,"_",min[,2],min[,1],"_", max[,2],max[,1],".csv", sep="")
  return(filename)
}

##################### MAIN CODE

mindate = '1992-02-15'
maxdate = '2014-12-15'

# Import and process NDVI data
dataN = read.csv("monthly_NDVI.csv")
dataN$mdy = as.Date(paste(dataN$Date, "15",sep="-"), 
                    format="%Y-%m-%d")
clean_N = subset(dataN, dataN$mdy < maxdate)
clean_N = subset(clean_N, clean_N$mdy > mindate)
NDVI = make_df(clean_N$mdy, clean_N$NDVI,'NDVI')

# Import and process rodent energy
dataE = read.csv("month_energy.csv", stringsAsFactors = FALSE)
dataE$fulldate = as.Date(dataE$fulldate, format="%m/%d/%Y")
clean_E = subset(dataE, dataE$fulldate < maxdate)
clean_E = subset(clean_E, clean_E$fulldate > mindate)
energy = make_df(as.Date(clean_E$fulldate, format="%m/%d/%Y"), 
                       clean_E$Energy, "energy")

# Import and process precipitation data
datappt = read.csv("SanSimon_ppt_1946.csv")
datappt$date = as.Date(paste(datappt$YEAR, datappt$MONTH, "15",sep="-"),
                       format="%Y-%m-%d")
datappt$pptmm = datappt$ppt/10
clean_ppt = subset(datappt, datappt$date < maxdate)
clean_ppt = subset(clean_ppt, clean_ppt$date > mindate)
ppt = make_df(as.Date(clean_ppt$date, format="%m/%d/%Y"), 
                 clean_ppt$pptmm, "ppt")

# Import and process PDO data
dataPDO = read.csv("PDO_allyears.csv", stringsAsFactors = FALSE)
clean_PDO = subset(dataPDO, dataPDO$date < maxdate)
clean_PDO = subset(clean_PDO, clean_PDO$date > mindate)
PDO = make_df(as.Date(clean_PDO$date, format="%Y-%m-%d"), 
              clean_PDO$PDO, "PDO")

# Import and process MEI data
dataMEI = read.csv("MEI_allyears.csv", stringsAsFactors = FALSE)
clean_MEI = subset(dataMEI, dataMEI$date < maxdate)
clean_MEI = subset(clean_MEI, clean_MEI$date > mindate)
MEI = make_df(as.Date(clean_MEI$date, format="%Y-%m-%d"), 
              clean_MEI$MEI, "PDO")

# Write files for use in Wavelet Analysis
write.csv(NDVI, make_filename(mindate, maxdate, "NDVI"))
write.csv(energy, make_filename(mindate, maxdate, "Rodent"))
write.csv(ppt, make_filename(mindate, maxdate, "PPT"))
write.csv(PDO, make_filename(mindate, maxdate, "PDO"))
write.csv(MEI, make_filename(mindate, maxdate, "MEI"))

