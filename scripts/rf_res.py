# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

#import pandas as pd
#import numpy as np
#from sklearn.preprocessing import StandardScaler
#from sklearn.ensemble import RandomForestClassifier
#from sklearn.model_selection import StratifiedKFold
#from sklearn.linear_model import LogisticRegression
#from sklearn.metrics import roc_curve, auc,roc_auc_score
#from joblib import Parallel, delayed
#import os
import sys

#ags: data_df.csv data_immuno.csv saida.csv

#file_df = sys.argv[1]
#file_imuno = sys.argv[2]
#file_saida = sys.argv[3]

file_df = "data_df.csv"
file_imuno = "data_immuno.csv"
file_saida = "saida.csv"

n_jobs = 15
seed = 12345

var_size=4


def compute_auc(x, y, LR, skf):
    aucs = np.empty(skf.n_splits)
    for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
        LR.fit(x[train_index], y[train_index])
        yhat = LR.predict(x[test_index])
        aucs[cv] = roc_auc_score(y[test_index], yhat)
    return np.mean(aucs)



    
def rf_res(df,imuno):
    df["sex"]=pd.get_dummies(df["gender"]).iloc[:,0]
    del df["Sample"]
    del df["gender"]
    del df["ST1: batch"]
    del df["ST2: batch"]
    del df["ST3: batch"]
    catego =(0.0,1.0)
    skf = StratifiedKFold(n_splits=10,shuffle=True,random_state=seed)
    rf1 = RandomForestClassifier(max_depth=3,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed)
    rf2 = RandomForestClassifier(max_depth=4,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+1)
    rf3 = RandomForestClassifier(max_depth=5,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+2)
    rf4 = RandomForestClassifier(max_depth=8,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed+3)
    sk = StandardScaler()
    data = pd.DataFrame()
    for d1 in range(len(catego)):
        for d2 in range(len(catego)):
            if d1 != d2:
                aux = df.copy()
                aux["out"] = np.nan
                aux.loc[aux["disease"]==catego[d2],"out"] = 0
                aux.loc[aux["disease"]==catego[d1],"out"] = 1
                del aux["disease"]
                aux.dropna(how='any',inplace=True)
                y = np.array(aux["out"])
                x = aux
                del x["out"]
                x = sk.fit_transform(x)
                for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
                    print("d1 "+str(d1)+" d2 "+str(d2)+" cv"+str(cv))
                    rf1.fit(x[train_index], y[train_index])
                    rf2.fit(x[train_index], y[train_index])
                    rf3.fit(x[train_index], y[train_index])
                    rf4.fit(x[train_index], y[train_index])
                    par = []
                    yhat = 0
                    if rf4.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf1.oob_score_):
                        yhat = rf4.predict_proba(x[test_index])[:,1]
                        par.append(8)
                    if rf3.oob_score_ >= max(rf2.oob_score_,rf1.oob_score_,rf4.oob_score_):
                        yhat = rf3.predict_proba(x[test_index])[:,1]
                        par.append(5)
                    if rf2.oob_score_ >= max(rf1.oob_score_,rf3.oob_score_,rf4.oob_score_):
                        yhat = rf2.predict_proba(x[test_index])[:,1]
                        par.append(4)
                    if rf1.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf4.oob_score_):
                        yhat = rf1.predict_proba(x[test_index])[:,1]
                        par.append(3)
                    data_aux = pd.DataFrame({"y_real":y[test_index],"y_hat":yhat})
                    data_aux["size_tree"] = par[0]
                    data_aux["cv"] = cv
                    data_aux["d1"] = catego[d1]
                    data_aux["d2"] = catego[d2]
                    data = pd.concat([data,data_aux ], ignore_index=True)
    data.to_csv("./result/RF_auc1.csv",index=False)
  


df = pd.read_csv(file_df)
imuno = pd.read_csv(file_imuno)
#rf_res()
#rf_auc()
#rf_imp1()


# df = py.read_r("./pos_data/data_nor.rds")[None]
# df["sex"]=pd.get_dummies(df["sex"]).iloc[:,0]
# catego = list(df["disease"].cat.categories)
# catego.remove("Healthy")
# df = df.loc[df.disease!="Healthy"]
# del df["id"]
# del df["batch"]          
# y = df["disease"]
# x = df
# del x["disease"]
# col = x.columns
# rf = RandomForestClassifier(max_depth=8,n_jobs=n_jobs,n_estimators=5001,oob_score=True,random_state=seed)
# rf.fit(x,y)
# data = pd.DataFrame({"vari":col,"imp":rf.feature_importances_})
# data.loc[data["vari"]!="sex"]
# data.loc[data["vari"]!="age"]
# data.sort_values("imp",ascending=False,inplace=True)
# data.to_csv("./result/RF_imp2.csv",index=False)
# 
# 
# 

















