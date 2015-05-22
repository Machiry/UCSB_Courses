#!/usr/bin/env python
import numpy as np
from collections import defaultdict
from matplotlib import pyplot
from sklearn import cross_validation
import os
from sklearn.lda import LDA
from sklearn import svm, grid_search, datasets, cross_validation

#This function computes Dichotomy value for the provided parameter
def compute_dichotomy_params(target_classifier,features):
    min_val = min(features)[0]
    max_val = max(features)[0]
    min_class = target_classifier.predict([min_val])
    max_class = target_classifier.predict([max_val])
    mid_val = None
    while min_val <= max_val:
        mid_val = (min_val + max_val) / 2
        mid_class = target_classifier.predict([min_val])
        if mid_class == min_class:
            min_val = mid_val + 1
        else:
            max_val = mid_val - 1
        
    return mid_val

#reads vector of features from file
def read_feature_vector(file_name):
    fo = open(file_name,'r')
    lines = fo.readlines()
    features = []
    classes = []
    escp = False
    for line in lines:
        parts = line.strip().split()
        if parts[-1].strip() == '1' or parts[-1].strip() == '2':
            classes.append(int(parts[-1].strip()))
            features.append([float(parts[0]),float(parts[1])])
    fo.close()

    return (features,classes)

def read_features(file_name):
    fo = open(file_name,'r')
    lines = fo.readlines()
    features = []
    classes = []
    escp = False
    for line in lines:
        parts = line.strip().split(',')
        if len(parts) == 6:
            if parts[-1].strip() == 'Low' or parts[-1].strip() == 'High':
                feat = []
                for i in range(0,len(parts)-1):
                    feat.append(float(parts[i]))
                classes.append(str(parts[-1][0]))
                features.append(np.array(feat))
                    
    fo.close()

    return (features,np.array(classes))
    

data_set_filepath = 'user_know.csv'
(features,classes) = read_features(data_set_filepath)
print 'Feature Array:' + str(len(features)) + ' Classes:' + str(len(classes))
parameters = {'kernel':['linear'], 'C':[1], 'gamma': [0]}
clf = svm.SVC()
clf = grid_search.GridSearchCV(clf, parameters, refit=True)
clf.fit(features, classes)
scores = cross_validation.cross_val_score(clf, features, classes, cv=10, scoring='accuracy')
print scores
