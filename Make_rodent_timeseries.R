library(dplyr)

data = read.csv("monthly_E_controls.csv")
trapping_info = read.csv("Trapping_Table.csv")

add_dates = function(data_table, date_table){
  # raw rodent data file does not have date info, only period code
  # this function takes the first day of trapping for each
  # period and adds that to the raw data file.
  
  date_table$date = as.Date(paste(date_table$dy,
                                  date_table$mo, 
                                  date_table$yr, 
                                  sep="-"),
                            format="%d-%m-%Y")
  
  period_groups = group_by(date_table, period)
  census_dates = summarize(period_groups, start_date = min(date))
  merge(data_table, census_dates, by.x = "period", by.y = "period")
}

make_period_totalenergy_date_data = function(data){
  # because species-level energy isn't necessary
  # this function strips the dataframe to only two
  # columns: Period Date and Total rodent energy
  
  data$total = rowSums(data[,2:21])
  energy = data.frame(data$start_date, data$total)
  colnames(energy) = c("Date", "Energy")
  return(energy)
}

data_w_dates = add_dates(data,trapping_info)
period_energy = make_period_totalenergy_date_data(data_w_dates)


