#!/usr/bin/env python
import numpy as np
from collections import defaultdict
from matplotlib import pyplot
from sklearn import cross_validation
from sklearn.lda import LDA

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


data_set_filepath = 'seeds_dataset.txt'
(features,classes) = read_feature_vector(data_set_filepath)

classifier = LDA()
classifier.fit(features, classes)


for n in range(3,11):
    score = cross_validation.cross_val_score(classifier, features, classes, cv=n)#, '''scoring='f1_weighted')
    print str(n) + '\t' + str(score.mean())
#print score
