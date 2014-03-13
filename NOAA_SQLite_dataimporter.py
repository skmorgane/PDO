"""To use this to import NOAA data, you lust open the original csv file and 
split the Date column by hand into Year, Month, Flag"""
import pandas
import pandas.io.sql
import sqlite3 as dbapi

con=dbapi.connect('SWweatherstations.sqlite')

data = pandas.read_csv("AllSanSimon.csv")
con=dbapi.connect('SWweatherstations.sqlite')
pandas.io.sql.write_frame(data, 'SanSimon', con, flavor='sqlite', if_exists='replace')

data = pandas.io.sql.read_sql("SELECT YEAR, MONTH, AVG(TPCP/100.0) FROM SanSimon WHERE TPCP >= 0 GROUP BY YEAR, MONTH", con)
rows = len(data)
Site = ['SanSimon'] * rows
data.insert(0, 'SITE', Site)
data.rename(columns={'AVG(TPCP/100.0)':'AVG_TPCP_CM'}, inplace=True)

pandas.io.sql.write_frame(data, 'SanSimon_summary', con, flavor='sqlite', if_exists='replace')