# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold
import sys


#file_df = sys.argv[1]
#file_imuno = sys.argv[2]
#file_saida = sys.argv[3]

file_df = "data_py_df.csv"
file_imuno = "data_py_immun.csv"
file_saida = "RF_auc1.csv"

seed = 12345
n_jobs = 15
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
    
    skf = StratifiedKFold(n_splits=10,shuffle=True,random_state=seed)
    rf1 = RandomForestClassifier(max_depth=3,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed)
    rf2 = RandomForestClassifier(max_depth=4,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+1)
    rf3 = RandomForestClassifier(max_depth=5,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+2)
    rf4 = RandomForestClassifier(max_depth=8,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+3)
    data = pd.DataFrame()
    x=df
    for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
        rf1.fit(x[train_index], y[train_index])
        rf2.fit(x[train_index], y[train_index])
        rf3.fit(x[train_index], y[train_index])
        rf4.fit(x[train_index], y[train_index])
        par = 0
        yhat = 0
        if rf4.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf1.oob_score_):
            yhat = rf4.predict_proba(x[test_index])[:,1]
            par=8
        if rf3.oob_score_ >= max(rf2.oob_score_,rf1.oob_score_,rf4.oob_score_):
            yhat = rf3.predict_proba(x[test_index])[:,1]
            par=5
        if rf2.oob_score_ >= max(rf1.oob_score_,rf3.oob_score_,rf4.oob_score_):
            yhat = rf2.predict_proba(x[test_index])[:,1]
            par=4
        if rf1.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf4.oob_score_):
            yhat = rf1.predict_proba(x[test_index])[:,1]
            par=3
        data_aux = pd.DataFrame({"y_real":y[test_index],"y_hat":yhat})
        data_aux["size_tree"] = par[len(par)-1]
        data_aux["cv"] = cv
        data = pd.concat([data,data_aux ], ignore_index=True)
    return(data)
    
df = pd.read_csv(file_df,sep=";")
imuno = pd.read_csv(file_imuno).iloc[:, 0].tolist()
data = rf_res(df,imuno)
data.to_csv(file_saida,index=False,sep=";")
