"""To use this to import NOAA data, you lust open the original csv file and 
split the Date column by hand into YEAR, MONTH, FLAG"""
import pandas
import pandas.io.sql
import sqlite3 as dbapi
import glob
import os

con=dbapi.connect('SWweatherstations.sqlite')
cur = con.cursor()
cur.execute("DROP TABLE IF EXISTS Site_ppt_avgs")
filenames = glob.glob('./Data/*_climate_allstations.csv')
for filename in filenames:
        site = os.path.basename(filename).split('_')[0]
        data = pandas.read_csv(filename)
        pandas.io.sql.write_frame(data, site, con, flavor='sqlite', if_exists='replace')
        sql = "select YEAR, MONTH, AVG(TPCP/100.0) FROM '%(site)s' WHERE TPCP >= 0 GROUP BY YEAR, MONTH"
        data_ppt = pandas.io.sql.read_sql(sql % {'site':site}, con)
        rows = len(data)
        Site_label = ['SanSimon'] * rows
        data_ppt.insert(0, 'SITE', site)
        data_ppt.rename(columns={'AVG(TPCP/100.0)':'AVG_TPCP_CM'}, inplace=True)
        pandas.io.sql.write_frame(data_ppt, 'Site_ppt_avgs', con, flavor='sqlite', if_exists='append')
