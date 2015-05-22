#!/usr/bin/env python
import numpy as np
from collections import defaultdict
from matplotlib import pyplot
from sklearn import cross_validation
from sklearn.utils import shuffle
import pylab as pl
import os
from sklearn.lda import LDA
from sklearn import svm, grid_search, datasets, cross_validation
from sklearn.metrics import roc_curve, auc

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
                classes.append(str(int(parts[-1])-1))
                features.append(np.array(feat))
                    
    fo.close()

    return (features,np.array(classes))
    

data_set_filepath = 'seeds_dataset.txt'
(features,classes) = read_features(data_set_filepath)
parameters = {'kernel':['linear'], 'C':[1.4], 'gamma': [0]}
clf = svm.SVC()
clf = grid_search.GridSearchCV(clf, parameters, refit=True)
clf1 = clf.fit(features, classes)
print "\n\tMean Accuracy\tMean Error"
for n in range(3,12):
    score = cross_validation.cross_val_score(clf, features, classes, cv=n, scoring='accuracy')
    print str(n) + '\t' + str(score.mean()*100) + '\t' +  str((1 - score.mean()) * 100)

random_state = np.random.RandomState(0)
features, classes = shuffle(features, classes, random_state=random_state)

half = int(len(features) / 2)
X_train, X_test = features[:half], features[half:]
y_train, y_test = classes[:half], classes[half:]

classifier = svm.SVC(kernel='linear', probability=True)
probas_ = classifier.fit(X_train, y_train).predict_proba(X_test)

# Convert into binary values
y_test1 = []
for c in y_test:
    y_test1.append(int(c))
fpr, tpr, thresholds = roc_curve(y_test1, probas_[:, 1])
roc_auc = auc(fpr, tpr)
print "\nArea under the ROC curve for SVM: %f" % roc_auc

# Plot ROC curve
pl.clf()
pl.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc)
pl.plot([0, 1], [0, 1], 'k--')
pl.xlim([0.0, 1.0])
pl.ylim([0.0, 1.0])
pl.xlabel('False Positive Rate')
pl.ylabel('True Positive Rate')
pl.title('ROC using SVM')
pl.legend(loc="lower right")
pl.savefig('ROC_SVM')
pl.close()
print "\nROC Curve saved at:ROC_SVM.png"
