#!/usr/bin/env python
import numpy as np
from collections import defaultdict
from matplotlib import pyplot
from sklearn import cross_validation
import os
from sklearn.lda import LDA
from sklearn import svm, grid_search, datasets, cross_validation

def read_features(file_name):
    fo = open(file_name,'r')
    lines = fo.readlines()
    features = []
    classes = []
    escp = False
    for line in lines:
        parts = line.strip().split()
        if len(parts) == 8:
            if parts[-1].strip() == '1' or parts[-1].strip() == '2':
                feat = []
                for i in range(0,len(parts)-1):
                    feat.append(float(parts[i]))
                classes.append(str(parts[-1]))
                features.append(np.array(feat))
                    
    fo.close()

    return (features,np.array(classes))
    

data_set_filepath = 'seeds_dataset.txt'
(features,classes) = read_features(data_set_filepath)
print 'Feature Array:' + str(len(features)) + ' Classes:' + str(len(classes))
parameters = {'kernel':['linear'], 'C':[1.4], 'gamma': [0]}
clf = svm.SVC()
clf = grid_search.GridSearchCV(clf, parameters, refit=True)
clf1 = clf.fit(features, classes)
scores = cross_validation.cross_val_score(clf, features, classes, cv=10, scoring='accuracy')
print scores.mean()*100
