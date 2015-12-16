# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from sklearn.svm import LinearSVC

__author__ = 'Memray'

import pandas as pd
import numpy as np
import urllib
import time
from sklearn import datasets
import pandas as pd
from pandas import Series
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import RFECV, SelectKBest, chi2, SelectFromModel


def read_data(data_path):
    # URL for the Pima Indians Diabetes dataset (UCI Machine Learning Repository)
    # url = "http://goo.gl/j0Rvxq"
    # download the file
    # raw_data = urllib.urlopen(url)
    # data_path = 'H:\Dropbox\PhD@Pittsburgh\\2.Course\Courses@Pitt\INFSCI2160_DataMining\\final_project\springleaf\FuJun\\temp_train.csv'
    # load the CSV file as a numpy matrix

    print('Reading CSV...')
    data = pd.read_csv(data_path)
    # dataset = np.loadtxt(raw_data, delimiter=",")

    print('')
    print(data.shape)
    # separate the data from the target attributes
    data.info()

    return data

class RandomForestClassifierWithCoef(RandomForestClassifier):
    def fit(self, *args, **kwargs):
        super(RandomForestClassifierWithCoef, self).fit(*args, **kwargs)
        self.coef_ = self.feature_importances_

path = 'H:\\Dropbox\\DM\\DataOnFeatureSelect\\'
filename = 'train_65.csv'
new_filename = 'train_65_reduced.csv'

time_reading_start = time.time()
data = read_data(path+filename)
time_reading_end = time.time()
print('Reading time is: {0}'.format(time_reading_end - time_reading_start))

# filling missing values: fillna
data = data.fillna(0)
# print(column_list)
# print(len(column_list))

# get X and y seperately
column_list = data.columns[:-1]
X = pd.DataFrame(data.loc[1:,column_list])
print('Size of X:{0}'.format(X.shape))

y=(pd.Series(data.target, name='target'))[1:].astype(int)

print('Size of y:{0}'.format(y.shape))

########### RandomForest failed as the poor performance ##########
# print('Start to run RandomForest...')
# rf = RandomForestClassifierWithCoef(n_estimators=1000, max_features=30, min_samples_leaf=5, n_jobs=-1)
# print('Start to run Feature Selection...')
# rfecv = RFECV(estimator=rf, step=1, cv=2, scoring='roc_auc', verbose=2)
# selector=rfecv.fit(x, y)

########### Chi-squared failed as the requirement of positive X value. Also f_classif is not feasible as input matrix must be dense  ##########
# X_new = SelectKBest(chi2, k=100).fit_transform(X, y)

########### Classification of text documents using sparse features: Comparison of different algorithms for document classification including L1-based feature selection.
time_fs_start = time.time()

lsvc = LinearSVC(C=0.01, penalty="l1", dual=False).fit(X, y)
model = SelectFromModel(lsvc, prefit=True)
X_new = model.transform(X)

time_fs_end = time.time()
########### Try some ##########
reduced_column = map(lambda (i,x): x, filter(lambda (i,x):model.get_support()[i], enumerate(column_list) ) )
X_new_df = pd.DataFrame(X_new, columns=reduced_column)
data_new = X_new_df.join(y)
data_new.to_csv(path+new_filename)