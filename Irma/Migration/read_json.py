# -*- coding: utf-8 -*-
"""
Created on Sun Jun 23 21:25:04 2019

@author: Cynthia Khan
"""

import json
import pandas as pd
from pandas.io.json import json_normalize
import numpy as np


path = 'C:\\Users\\Cynthia Khan\\Downloads\\mm1_tweets\\mm1_tweets.json'

with open(path) as json_data:
   data = json.load(json_data)

df = json_normalize(data)

df1 = df[['user.id', 'geo.coordinates']]

df1 = df1[df1['geo.coordinates'].notnull()]

df1.columns = ['user', 'geo']

df2 = df1.geo.apply(pd.Series).merge(df1, right_index = True, left_index = True).drop(['geo'], axis = 1)

df2.columns = ['lat', 'long', 'user']

df3 = df2.groupby('user')[['user', 'lat', 'long']].max()

#df4 = df3[df3.lat > 31.0035]

#df5 = df3[df3.lat < 24.3959]

#df6 = df3[df3.long > -79.8198]

#df7 = df3[df3.long < -87.6256]

#frames = [df4, df5, df6, df7]

#df8 = pd.concat(frames)

df3.to_csv(r'C:\\Users\\Cynthia Khan\\Downloads\\mm1_tweets\\mm1_tweets.csv')

"""
 "FL":
  {
    "name": "Florida",
    "min_lat": 24.3959,
    "max_lat": 31.0035,
    "min_lng": -87.6256,
    "max_lng": -79.8198
  },
  """