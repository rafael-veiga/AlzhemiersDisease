# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score
from joblib import Parallel, delayed
import sys


file_df = sys.argv[1]
file_par = sys.argv[2]
file_res = sys.argv[3]
file_saida = sys.argv[4]

#file_df = "data_py_df.csv"
#file_par = "RF_auc2.csv"
#file_res = "RF_imp.csv"
#file_saida = "rf_n_auc.csv"

seed = 12345
n_jobs = 15


def cal(data,seed,par):
    x_train,y_train,x_test,y_test = data
    RF = RandomForestClassifier(max_depth=par,n_estimators=5001,oob_score=False,random_state=seed)
    RF.fit(x_train,y_train)
    yhat = RF.predict_proba(x_test)[:,1]
    auc = roc_auc_score(y_test, yhat)
    return(float(auc))

def calculate_auc_vs_n(df,immun,par,seed):
    sex = np.array(pd.get_dummies(df["gender"]).iloc[:,0])
    age = np.array(df["age"])
    y = np.array(df["disease"])
    x = df.copy()
    x["sex"] = sex
    x["age"] = age
    
    a = []
    n_l = []
    cv_l = []
    skf = StratifiedKFold(n_splits=10,random_state=seed,shuffle=True)
    for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
        for n in range(200):
            col = ["sex","age"] + immun[0:(n+1)]
            a.append((x.loc[train_index,col].copy(),y[train_index],x.loc[test_index,col].copy(),y[test_index]))
            n_l.append(n)
            cv_l.append(cv)
    
    aucs = Parallel(n_jobs=n_jobs)(delayed(cal)(a[i],seed+i,par) for i in range(len(a)))
    df = pd.DataFrame({"n":n_l,"cv":cv_l,"auc":aucs})
    auc_list = np.empty(200)
    for i in range(200):
        auc_list[i] = np.mean(df.loc[df["n"]==i,"auc"])
    return(pd.DataFrame({"n":range(1,201),"auc":auc_list}))
    



    
df = pd.read_csv(file_df,sep=";")
res =  pd.read_csv(file_par,sep=";")
tab = res["par"].value_counts().sort_values(ascending=False)
par = int(tab.index[0])
res =  pd.read_csv(file_res,sep=";")
res = res.sort_values(by='imp', ascending=False)
res = list(res["vari"])

data = calculate_auc_vs_n(df,res,par,seed)
data.to_csv(file_saida,index=False,sep=";")

