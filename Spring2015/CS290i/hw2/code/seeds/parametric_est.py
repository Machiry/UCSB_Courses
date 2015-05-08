#!/usr/bin/env python
import numpy as np
from collections import defaultdict
from matplotlib import pyplot
from sklearn import cross_validation
import os
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

def read_features(file_name):
    fo = open(file_name,'r')
    lines = fo.readlines()
    x1 = []
    y1 = []
    x2 = []
    y2 = []
    escp = False
    for line in lines:
        parts = line.strip().split()
        if parts[-1].strip() == '1':
            x1.append(float(parts[0]))
            y1.append(float(parts[1]))
        elif parts[-1].strip() == '2':
            x2.append(float(parts[0]))
            y2.append(float(parts[1]))
    fo.close()

    return (x1,y1,x2,y2)
    

data_set_filepath = 'seeds_dataset.txt'
(features,classes) = read_feature_vector(data_set_filepath)
(x1,y1,x2,y2) = read_features(data_set_filepath)
classifier = LDA()
classifier.fit(features, classes)

weights = classifier.coef_[0]
dec_s = weights[0] / weights[1]
x = np.linspace(10, 22)
y = dec_s * x - (classifier.intercept_[0] / weights[1])
pyplot.xlabel('Surface Area of Wheat Kernel')
pyplot.ylabel('Perimeter of Wheat Kernel')
pyplot.scatter(x2,y2,marker='+')
pyplot.scatter(x1,y1,marker='*')
pyplot.plot(x, y, 'k--', label='weighted')
pyplot.savefig('Features_with_decision_boundary')
pyplot.close()

print "\nFeatures with Decision boundary is shown in figure: " + os.path.dirname(os.path.realpath(__file__)) + '/Features_with_decision_boundary.png\n'

print "Decision Line Equation: y = " + str(dec_s) + ' * x ' + str((classifier.intercept_[0] / weights[1])) + '\n'

print "n\tMean Accuracy\tMean Error"
for n in range(3,11):
    score = cross_validation.cross_val_score(classifier, features, classes, cv=n)#, '''scoring='f1_weighted')
    print str(n) + '\t' + str(score.mean()*100) + '\t' +  str((1 - score.mean()) * 100)
#print score
