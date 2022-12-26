import pandas as pd
import os
import argparse
from datetime import date, timedelta
import requests
from bs4 import BeautifulSoup as BS
import math
import numpy as np
import re

parser=argparse.ArgumentParser()
parser.add_argument('--StormName',type=str,required=True)
parser.add_argument('--EventID',type=str,required=True)
args=parser.parse_args()

spath = 'D:/03_JapanAER/IN_SEASON_ANALYSIS/1. CM_data/'
ipath = 'D:/03_JapanAER/IN_SEASON_ANALYSIS/2. QC/'
fpath = 'D:/03_JapanAER/IN_SEASON_ANALYSIS/3. Reformat_data/'
fname = os.listdir(spath)[0].split('.')[0]
infile = pd.read_excel(spath + fname + '.xlsx')
infile.to_csv(ipath + fname +'.csv',index=False)

wind_grid = pd.read_csv('C:\Program Files\Impact Forecasting\IF Auto Event Response Service\TK_PROC_Scripts\hazard\data\grid\Japan_Grid_with_topo&dirough.csv')
r = wind_grid['MEAN']
x = wind_grid['Lat']
y = wind_grid['Lon']
roh = 1.15                # air density [kg/m3]
d = 100                  # Arbitrary small distance in meters used for derivation

infile.rename(columns={infile.columns[0]: "ObsID"}, inplace = True)
infile.rename(columns={infile.columns[3]: "Gust_mps_raw"}, inplace = True)
infile.rename(columns={infile.columns[4]: "Latitude"}, inplace = True)
infile.rename(columns={infile.columns[5]: "Longitude"}, inplace = True)
infile.rename(columns={infile.columns[7]: "Hanem_m"}, inplace = True)
gust_10m = np.round(infile['Gust_mps_raw']*(10/infile['Hanem_m'])**0.27,1)      # power law to get winds to 10m height
infile['Gust10_mps'] = gust_10m
infile['EventID']=args.EventID
infile['Name']=args.StormName
infile['Year']=int(str(args.EventID)[:4])

# holder = [0] * len(infile)
# for i in range(len(infile)):
    # lat = infile['Latitude'][i]
    # lon = infile['Longitude'][i]
    # idx = (abs(x-lat)+abs(y-lon)).argmin()    
    # z0 = r[idx]
    # holder[i]=z0

# c = np.log10(infile['Hanem_m']/np.array(holder))                              # use log law and account for roughness (not used)
# b = np.log10(10/np.array(holder))
# gust_log10 = np.round(infile['Gust_mps_raw']*b/c,1)
# infile['Gust10_mps']= gust_log10

outfile = infile.filter(['EventID','Name','Year','ObsID','Latitude','Longitude','Hanem_m','Gust_mps_raw','Gust10_mps'],axis=1)
outfile.to_csv(fpath + 'eventdata_AMEDAS.csv',index=False)
