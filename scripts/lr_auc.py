# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 02:17:09 2024

@author: rafae
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import StratifiedKFold
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_curve, auc
import sys
import os


file_df = sys.argv[1]
file_imuno = sys.argv[2]
file_res_lr = sys.argv[3]
file_saida = sys.argv[4]

#file_df = "data_py_df.csv"
#file_imuno = "data_py_immun.csv"
#file_res_lr = "res_lr.csv"
#file_saida = "saida.csv"

seed = 12345

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
    
def lr_auc(df,immun,file_res_lr,var_size=10):
    sex = np.array(pd.get_dummies(df["gender"]).iloc[:,0])
    age = np.array(df["age"])
    y = np.array(df["disease"])
    df = df[immun].copy()
    df["sex"] = sex
    df["age"] = age
    
    skf = StratifiedKFold(n_splits=10,random_state=seed,shuffle=True)
    LR = LogisticRegression(C=0.1,penalty="l2",solver="liblinear",max_iter=1000)
    data = pd.DataFrame()
    aux=df.copy()
    res = pd.read_csv(file_res_lr)    
    res = res["vars"][0:var_size]
    res = ["sex","age"]+list(res)
    aux = aux[res]
    aux = aux.dropna()
    x = aux
    y = np.array(y,order="C")
    x = np.array(x,order="C")
    auc_cv = AUC_CV()
    for cv, (train_index, test_index) in enumerate(skf.split(x, y)):
        LR.fit(x[train_index], y[train_index])
        yhat = LR.predict(x[test_index])
        fpr, tpr, thresholds = roc_curve(y[test_index],yhat,pos_label=1)
        auc_cv.add_points(fpr, tpr)
    data_aux = auc_cv.get_mean()
    data = pd.concat([data, data_aux],ignore_index=True)
    return(data)
    
df = pd.read_csv(file_df,sep=";")
imuno = pd.read_csv(file_imuno).iloc[:, 0].tolist()
data = lr_auc(df,imuno,file_res_lr)
data.to_csv(file_saida,index=False,sep=";")
