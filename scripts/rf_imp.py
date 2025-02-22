# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
import sys
#import os


file_df = sys.argv[1]
file_imuno = sys.argv[2]
file_saida = sys.argv[3]

#file_df = "data_py_df.csv"
#file_imuno = "data_py_immun.csv"
#file_saida = "saida.csv"

n_jobs = 15
seed = 12345

#fold = os.path.dirname(os.path.realpath(__file__))
#file_df = os.path.join(fold, file_df)  
#file_imuno = os.path.join(fold, file_imuno)
#file_saida = os.path.join(fold, file_saida) 

    
def rf_res(df,immun):
    sex = np.array(pd.get_dummies(df["gender"]).iloc[:,0])
    age = np.array(df["age"])
    y = np.array(df["disease"])
    df = df[immun].copy()
    df["sex"] = sex
    df["age"] = age
    importance = []
    oob=[0,0,0,0]
    par = [3,4,5,8]
    rf = RandomForestClassifier(max_depth=3,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed)
    rf.fit(df,y)
    oob[0] = rf.oob_score_
    importance.append(rf.feature_importances_)
    rf = RandomForestClassifier(max_depth=4,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+1)
    rf.fit(df,y)
    oob[1] = rf.oob_score_
    importance.append(rf.feature_importances_)
    rf = RandomForestClassifier(max_depth=5,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+2)
    rf.fit(df,y)
    oob[2] = rf.oob_score_
    importance.append(rf.feature_importances_)
    rf = RandomForestClassifier(max_depth=8,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+3)
    rf.fit(df,y)
    oob[3] = rf.oob_score_
    importance.append(rf.feature_importances_)
    
    index_max = oob.index(max(oob))

    data = pd.DataFrame({"vars":df.columns,"imp":importance[index_max]})
    data[data['vars'].isin(immun)]
    data = data.sort_values(by='imp', ascending=False)
    data["par"] = par[index_max]
    return(data)

df = pd.read_csv(file_df,sep=";")
imuno = pd.read_csv(file_imuno).iloc[:, 0].tolist()
data = rf_res(df,imuno)
data.to_csv(file_saida,index=False)
