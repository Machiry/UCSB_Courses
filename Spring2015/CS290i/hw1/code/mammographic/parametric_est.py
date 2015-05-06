#!/usr/bin/env python
import numpy as np
from collections import defaultdict
import scipy.stats as stats
from sklearn.naive_bayes import GaussianNB
import matplotlib.pyplot as plt
from sklearn import cross_validation

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
def read_feature_vector(file_name,target_feat):
    features = []
    classes = []
    f = open(file_name,'r')
    for curr_line in f.readlines():
        parts = curr_line.split(',')
        if not (parts[target_feat] == '?'):
            features.append([float(parts[target_feat])])
            classes.append(int(parts[-1]))

    return (features,classes)


data_set_filepath = 'mammographic_masses.data'
target_feature = 1
(features,classes) = read_feature_vector(data_set_filepath,target_feature)

classifier = GaussianNB()
#print str(features)
#print str(classes)
classifier.fit(features, classes)
print "Mean:" + str(classifier.theta_)
print "Standard deviation:" + str(classifier.sigma_)
print 'Feature Dichotomy value:' + str(compute_dichotomy_params(classifier,features))
print "Cross Validation Results:"
print "N\tMean Accuracy"
print "-------------"
for n in range(3,11):
    score = cross_validation.cross_val_score(classifier, features, classes, cv=n)#,scoring='accuracy')#, '''scoring='f1_weighted')
    print str(n) + '\t' + str(score.mean())


