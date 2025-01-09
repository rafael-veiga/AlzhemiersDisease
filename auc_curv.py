# -*- coding: utf-8 -*-
"""
Created on Thu Apr 25 12:12:11 2024

@author: rafae
"""

import pyreadr as py
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_curve, auc,roc_auc_score
from joblib import Parallel, delayed
import os
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)
n_jobs = 15
seed = 12345

def compute_auc(x, y, LR, skf):
    aucs = np.empty(skf.n_splits)
    for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
        LR.fit(x[train_index], y[train_index])
        yhat = LR.predict(x[test_index])
        aucs[cv] = roc_auc_score(y[test_index], yhat)
    return np.mean(aucs)


def calculate_auc_vs_n():
    df = py.read_r("./pos_data/fil_data_ord.rds")[None]
    df["sex"]=pd.get_dummies(df["gender"]).iloc[:,0]
    del df["gender"]
    res = df.columns
    auc_list = np.empty(200)
    auc_list[:] = np.nan
    
    for n in range(1, 201):
        print(n)
        col = list(res[0:n])
        col.insert(0, "sex")
        col.insert(0, "age")
        col.insert(0, "disease")
        aux = df[col]
        aux = aux.dropna()
        y = np.array(aux["disease"],order="C")
        del aux["disease"]
        x = np.array(aux.copy(), order="C")
        skf = StratifiedKFold(n_splits=5,random_state=seed,shuffle=True)
        LR = LogisticRegression(C=0.1,penalty="l2",solver="liblinear",max_iter=1000)
        aucs = Parallel(n_jobs=n_jobs)(delayed(compute_auc)(x, y, LR, skf) for i in range(100))
        
        aucs = np.array(aucs)
        auc_list[n-1] = np.mean(aucs)  
    data = pd.DataFrame({"n_var":range(1, 201),"auc":auc_list})
    data.to_csv("./result/auc_n.csv",index=False)




calculate_auc_vs_n()











    
            
        
    
