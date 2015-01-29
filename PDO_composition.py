import pandas as pd
import numpy as np
import sqlalchemy
import json

###### FUNCTIONS
def extract_data(query):
    
    """downloads data from server using SQL query and outputs pandas
    dataframe"""
    
    credentials = json.load(open("db_credentials.json", "r"))
    engine = sqlalchemy.create_engine('mysql+pymysql://morgan:{}@{}:{}/{}'.format(credentials['password'], credentials['host'], credentials['port'], credentials['database']))
    data = pd.read_sql_query(query, engine)
    return data

def calculate_individual_energy(data):
    
    """"inserts species average mass for missing values & calculates 
    individual energy use"""
    
    data['wgt'] = data[['species','wgt']].groupby("species").transform(lambda x: x.fillna(x.mean()))
    data['energy'] = 5.69 * data['wgt'] ** 0.75
    return data

def count_sampled_plots(rodent_data, trapping_data):
    plot_list = list(rodent_data['plot'].unique())
    trapping_data = trapping_data[trapping_data['plot'].isin(plot_list)]
    period_plot_count = trapping_data[['period', 'sampled']].groupby(['period']).sum()
    period_plot_count.reset_index(inplace=True)
    return period_plot_count
    
######  MAIN CODE

Trapping_Table = pd.read_csv('Trapping_Table.csv')

# Extracting rodent records for known species from control plots for the entire 
# time series

query_rats = """SELECT Rodents.mo, Rodents.dy, Rodents.yr, Rodents.period, 
                Rodents.plot, Rodents.species, Rodents.wgt 
                FROM Rodents JOIN SPECIES
                ON Rodents.species=SPECIES.`New Code`
                WHERE Rodents.plot IN ("2","4","8","11","12","14","17","22")
                AND Rodents.period > 0
                AND (Rodents.note1 Is Null 
                OR Rodents.note1 IN ("1", "2", "3", "6", "7", "10", 
                "11", "12", "13", "14"))
                AND (SPECIES.Rodent = 1) AND (SPECIES.Unknown = 0)
                """
raw_data = extract_data(query_rats)
raw_data = calculate_individual_energy(raw_data)
plots_per_period = count_sampled_plots(raw_data, Trapping_Table)

# calculates mean energy use per species per treatment for each trapping 
# session
control_sums = raw_data[['period', 'plot', 'species', 'energy'
                         ]].groupby(['period', 'species']).sum()
control_sums.reset_index(inplace=True)
control_sums = pd.merge(control_sums, plots_per_period, how='left', 
                        on=['period'])
control_sums['average'] = control_sums['energy']/control_sums['sampled']

# determines first Julian Date of trapping for each period, processes and merges
# with data for export

JulianDate_for_period = Trapping_Table[['JulianDate', 
                                        'period']].groupby(['period']).min()
JulianDate_for_period['JulianDate'] = JulianDate_for_period['JulianDate'].astype(int)
JulianDate_for_period.reset_index(inplace=True)
control_sums = pd.merge(control_sums, JulianDate_for_period, how='left', 
                        on=['period'])

# formatting and output for analysis
control_data = control_sums.drop(['plot', 'energy', 'sampled'], axis = 1)

                                              
#treatment_data_export.to_csv("Portal_Rodents_PriceProject.csv")
