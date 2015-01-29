import pandas as pd
import numpy as np
import sqlalchemy
import json
import portal

###### FUNCTIONS
def extract_data(query):
    
    """downloads data from server using SQL query and outputs pandas
    dataframe"""
    
    credentials = json.load(open("db_credentials.json", "r"))
    engine = sqlalchemy.create_engine('mysql+pymysql://morgan:{}@{}:{}/{}'.format(credentials['password'], credentials['host'], credentials['port'], credentials['database']))
    data=pd.read_sql_query(query_rats, engine)
    return data

def calculate_individual_energy(data):
    
    """"inserts species average mass for missing values & calculates 
    individual energy use"""
    
    data['wgt'] = data[['species','wgt']].groupby("species").transform(lambda x: x.fillna(x.mean()))
    data['energy'] = 5.69 * data['wgt'] ** 0.75
    return data

######  MAIN CODE

# Extracting control plot data for the entire time series

query_rats = """SELECT Rodents.mo, Rodents.dy, Rodents.yr, Rodents.period, 
                Rodents.plot, Rodents.note1, Rodents.species, Rodents.wgt 
                FROM Rodents JOIN SPECIES
                ON Rodents.species=SPECIES.`New Code`
                WHERE Rodents.plot IN ("2","4","8","11","12","14","17","22")
                AND Rodents.period > 0
                AND (SPECIES.Rodent = 1)
                AND (Rodents.note1 Is Null 
                OR Rodents.note1 IN ("1", "2", "3", "6", "7", "10", 
               "11", "12", "13", "14"))
                """
raw_data = extract_data(query_rats)
raw_data = calculate_individual_energy(raw_data)

Trapping_Table = portal.generate_trappingtable(raw_data)

# imports trapping table, adds plot type info to table, calculates 
# number of plots censused per period

#Trapping_Table = pd.read_csv('Trapping_Table.csv')
#Trapping_Table = pd.merge(Trapping_Table, plot_info, how='left', 
                          #on='plot')
#Trapping_Table['Type'] = Trapping_Table['Type Code'].map({'CO': 0, 
                                                          #'LK': 1, 
                                                          #'RE':2, 
                                                          #'SK':1})
#period_plot_count = Trapping_Table[['period', 'Type', 'sampled']].groupby(['period', 'Type']).sum()
#period_plot_count.reset_index(inplace=True)

## calculates mean energy use per species per treatment for each trapping 
## session
#treatment_sums = raw_data[['period', 'plot', 'Type', 'species', 'energy'
                           #]].groupby(['period', 'Type', 'species']).sum()
#treatment_sums.reset_index(inplace=True)
#treatment_sums = pd.merge(treatment_sums, period_plot_count, how='left',
                          #on=['period', 'Type'])
#treatment_sums['average'] = treatment_sums['energy']/treatment_sums['sampled']

## determines first Julian Date of trapping for each period, processes and merges
## with data for export

#JulianDate_for_period = Trapping_Table[['JulianDate', 
                                        #'period']].groupby(['period']).min()
#JulianDate_for_period['JulianDate'] = JulianDate_for_period['JulianDate'].astype(int)
#JulianDate_for_period.reset_index(inplace=True)
#treatment_sums = pd.merge(treatment_sums, JulianDate_for_period, how='left',
                          #on=['period'])

## formatting and output for analysis
#treatment_data_export = treatment_sums.drop(['plot', 'energy', 'sampled'], 
                                            #axis = 1)
#treatment_data_export = treatment_data_export[(treatment_data_export['species'] != 'DX')
                                              #& (treatment_data_export['species'] != 'PX')
                                              #& (treatment_data_export['species'] != 'RX')
                                              #& (treatment_data_export['species'] != 'OX')
                                              #& (treatment_data_export['species'] != 'UR')
                                              #& (treatment_data_export['species'] != 'SX')]
                                              
#treatment_data_export.to_csv("Portal_Rodents_PriceProject.csv")
