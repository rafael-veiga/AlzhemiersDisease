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
file_saida = sys.argv[3]
file_saida2 = sys.argv[4]

#file_df = "data_py_df.csv"
#file_imuno = "data_py_immun.csv"
#file_saida = "RF_auc1.csv"
#file_saida2 = "RF_auc2.csv"

seed = 12345
n_jobs = 15
#fold = os.path.dirname(os.path.realpath(__file__))
#file_df = os.path.join(fold, file_df)  
#file_imuno = os.path.join(fold, file_imuno)
#file_saida = os.path.join(fold, file_saida) 

class AUC_CV:    
    def __init__(self,fpr_tam=100):
        self.fpr_mean = np.linspace(0, 1, fpr_tam)
        self.size = 0
        self.fprs=[]
        self.tprs=[]

    def add_points(self,fpr,tpr):
        self.size = self.size+1
        self.fprs.append(fpr)
        self.tprs.append(tpr)
        
    def get_cur(self,cv):
        return (self.fprs[cv],self.tprs[cv])
    
    def get_mean(self):
        tpr_aux = np.ones((self.size,len(self.fpr_mean)))
        for cv in range(self.size):
            aux = np.interp(self.fpr_mean, self.fprs[cv], self.tprs[cv])
            aux[0] = 0.0
            tpr_aux[cv,:] = aux
        tpr_mean = np.ones(len(self.fpr_mean))
        tpr_mean[-1] = 1.0
        tpr_l = np.ones(len(self.fpr_mean))
        tpr_h = np.ones(len(self.fpr_mean))
        for i in range(len(self.fpr_mean)):
            tpr_mean[i] = np.mean(tpr_aux[:,i])
            se = (1.96*np.std(tpr_aux[:,i]))/np.sqrt(self.size)
            tpr_l[i] = tpr_mean[i]-se
            tpr_h[i] = tpr_mean[i]+se
        data = pd.DataFrame({"fpr":self.fpr_mean,"tpr":tpr_mean,"tpr_l":tpr_l,"tpr_h":tpr_h})
        data.loc[data.tpr_l<0,"tpr_l"] = 0
        data.loc[data.tpr_h>1,"tpr_h"] = 1
        data["auc"] = auc(self.fpr_mean,tpr_mean)
        data["auc_l"] = auc(self.fpr_mean,tpr_l)
        data["auc_h"] = auc(self.fpr_mean,tpr_h)
        return data


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
        print(cv)
        rf1.fit(x.iloc[train_index], y[train_index])
        rf2.fit(x.iloc[train_index], y[train_index])
        rf3.fit(x.iloc[train_index], y[train_index])
        rf4.fit(x.iloc[train_index], y[train_index])
        par = 0
        yhat = 0
        if rf4.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf1.oob_score_):
            yhat = rf4.predict_proba(x.iloc[test_index])[:,1]
            par=8
        if rf3.oob_score_ >= max(rf2.oob_score_,rf1.oob_score_,rf4.oob_score_):
            yhat = rf3.predict_proba(x.iloc[test_index])[:,1]
            par=5
        if rf2.oob_score_ >= max(rf1.oob_score_,rf3.oob_score_,rf4.oob_score_):
            yhat = rf2.predict_proba(x.iloc[test_index])[:,1]
            par=4
        if rf1.oob_score_ >= max(rf2.oob_score_,rf3.oob_score_,rf4.oob_score_):
            yhat = rf1.predict_proba(x.iloc[test_index])[:,1]
            par=3
        data_aux = pd.DataFrame({"y_real":y[test_index],"y_hat":yhat})
        data_aux["size_tree"] = par
        data_aux["cv"] = cv
        data = pd.concat([data,data_aux ], ignore_index=True)
    return(data)

def rf_auc(df):
    par = []
    auc_cv = AUC_CV()
    for cv in range(10):
        aux = df.loc[df["cv"]==cv]
        fpr, tpr, thresholds = roc_curve(aux["y_real"],aux["y_hat"],pos_label=1)
        auc_cv.add_points(fpr,tpr)
        par.append(aux["size_tree"].iloc[0])
    par = pd.Series(par)
    tb = par.value_counts().sort_values(ascending=False)
    data_aux = auc_cv.get_mean()
    data_aux["par"]= par
    return(data_aux)
    
df = pd.read_csv(file_df,sep=";")
imuno = pd.read_csv(file_imuno).iloc[:, 0].tolist()
data = rf_res(df,imuno)
data.to_csv(file_saida,index=False,sep=";")
data = rf_auc(data)
data.to_csv(file_saida2,index=False,sep=";")
