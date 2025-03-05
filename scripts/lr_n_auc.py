# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score
from joblib import Parallel, delayed
import sys

file_df = sys.argv[1]
file_res = sys.argv[2]
file_saida = sys.argv[3]

#file_df = "data_py_df.csv"
#file_res = "res_lr.csv"
#file_saida = "lr_n_auc.csv"

seed = 12345
n_jobs = 15

def cal(data):
    x_train,y_train,x_test,y_test = data
    LR = LogisticRegression(C=0.1,penalty="l2",solver="liblinear",max_iter=1000)
    LR.fit(x_train,y_train)
    yhat = LR.predict(x_test)
    auc = roc_auc_score(y_test, yhat)
    return(float(auc))

def calculate_auc_vs_n(df,immun):
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
    
    aucs = Parallel(n_jobs=n_jobs)(delayed(cal)(data) for data in a)
    df = pd.DataFrame({"n":n_l,"cv":cv_l,"auc":aucs})
    auc_list = np.empty(200)
    for i in range(200):
        auc_list[i] = np.mean(df.loc[df["n"]==i,"auc"])
    return(pd.DataFrame({"n":range(1,201),"auc":auc_list}))
    



    
df = pd.read_csv(file_df,sep=";")
res =  pd.read_csv(file_res,sep=",")
res = list(res["vars"])
data = calculate_auc_vs_n(df,res)
data.to_csv(file_saida,index=False,sep=";")