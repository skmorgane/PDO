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
    
    """ counts number of plots actually sampled in a trapping period"""
    
    plot_list = list(rodent_data['plot'].unique())
    trapping_data = trapping_data[trapping_data['plot'].isin(plot_list)]
    period_plot_count = trapping_data[['period', 'sampled']].groupby(['period']).sum()
    period_plot_count.reset_index(inplace=True)
    return period_plot_count

def calc_treatment_mean(rodent_data, period_plot_count):
    
    """calculates mean energy use per species per treatment for each trapping session"""
    
    totals = rodent_data[['period', 'plot', 'species', 'energy'
                          ]].groupby(['period', 'species']).sum()
    totals.reset_index(inplace=True)
    totals = pd.merge(totals, period_plot_count, how='left', on=['period'])
    totals['mean_energy'] = totals['energy']/totals['sampled']
    totals = totals.drop(['plot', 'energy', 'sampled'], axis = 1)
    return totals

def add_julian_date(totals, trapping_data):
    
    """selects from the trapping data the first Julian Date for a period and 
    adds to processed rodent data dataframe"""
    
    JulianDate_for_period = trapping_data[['JulianDate', 
                                            'period']].groupby(['period']).min()
    JulianDate_for_period['JulianDate'] = JulianDate_for_period['JulianDate'].astype(int)
    JulianDate_for_period.reset_index(inplace=True)
    totals = pd.merge(totals, JulianDate_for_period, how='left', on=['period'])
    return totals

def calc_relative_energy(totals):
    
    """ calculates for each species its the fraction of the total energy use in
    that trapping period"""
    
    total_E = totals[['period', 'mean_energy']].groupby(['period']).sum()
    total_E.reset_index(inplace=True)
    total_E.columns = ['period', 'total_energy']
    totals = pd.merge(totals, total_E, how='left', on=['period'])
    totals['rel_energy'] = totals['mean_energy']/totals['total_energy']
    return totals

def create_array(totals):
    """ from the main data file, extracts just period, species, rel_energy and
    uses that data to generate an array that can be used for analysis"""
    
    # currently the data is still in a dataframe with NaNs instead of 0's. Next
    # steps: replace NaN w/ zero and export into an actual array.
    
    cols = [col for col in totals.columns if col in ['period', 'species',
                                                     'rel_energy']]
    totals = totals[cols]
    array_format = pd.pivot_table(totals, values='rel_energy', index=['period'],
                                  columns=['species'])
    return array_format

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
species_energy_means = calc_treatment_mean(raw_data, plots_per_period)
species_energy_means = add_julian_date(species_energy_means, Trapping_Table)
species_energy_means = calc_relative_energy(species_energy_means)
data_array = create_array(species_energy_means)

                                           
#data_array.to_csv("monthly_relE_controls.csv")
