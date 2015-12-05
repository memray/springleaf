__author__ = 'Memray'

import pandas as pd
import numpy as np
import urllib

from sklearn import datasets
import pandas as pd
from pandas import Series
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import RFECV


def read_data():
    # URL for the Pima Indians Diabetes dataset (UCI Machine Learning Repository)
    # url = "http://goo.gl/j0Rvxq"
    # download the file
    # raw_data = urllib.urlopen(url)
    data_path = 'H:\Dropbox\PhD@Pittsburgh\\2.Course\Courses@Pitt\INFSCI2160_DataMining\\final_project\springleaf\FuJun\\temp_train.csv'
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

data = read_data()
x = pd.DataFrame(data)
y=(pd.Series(data.target, name='target')).astype(int)

print(y)

print('Start to run RandomForest...')
rf = RandomForestClassifierWithCoef(n_estimators=1000, max_features=30, min_samples_leaf=5, n_jobs=-1)
print('Start to run Feature Selection...')
rfecv = RFECV(estimator=rf, step=1, cv=2, scoring='roc_auc', verbose=2)
selector=rfecv.fit(x, y)
