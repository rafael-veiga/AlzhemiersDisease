# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import auc,roc_curve
import sys


file_df = sys.argv[1]
file_imuno = sys.argv[2]
file_res = sys.argv[3]
file_saida = sys.argv[4]

#file_df = "data_py_df.csv"
#file_imuno = "data_py_immun.csv"
#file_res = "RF_auc2.csv"
#file_saida = "RF_imp.csv"

seed = 12345
n_jobs = 15
#fold = os.path.dirname(os.path.realpath(__file__))
#file_df = os.path.join(fold, file_df)  
#file_imuno = os.path.join(fold, file_imuno)
#file_saida = os.path.join(fold, file_saida) 

def rf_imp1(df,immun,par):
    sex = np.array(pd.get_dummies(df["gender"]).iloc[:,0])
    age = np.array(df["age"])
    y = np.array(df["disease"])
    df = df[immun].copy()
    df["sex"] = sex
    df["age"] = age
   
    rf = RandomForestClassifier(max_depth=par,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed)
    rf.fit(df,y)
    aux = pd.DataFrame({"vari":df.columns,"imp":rf.feature_importances_})
    aux = aux.loc[aux["vari"]!="sex"]
    aux = aux.loc[aux["vari"]!="age"]
    aux = aux.sort_values(by='imp', ascending=False)
    return(aux)
    
df = pd.read_csv(file_df,sep=";")
imuno = pd.read_csv(file_imuno).iloc[:, 0].tolist()
res =  pd.read_csv(file_res,sep=";")
tab = res["par"].value_counts().sort_values(ascending=False)
data = rf_imp1(df,imuno,int(tab.index[0]))
data.to_csv(file_saida,index=False,sep=";")

