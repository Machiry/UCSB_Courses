import random
import numpy
import numpy as np
from sklearn import cross_validation
from sklearn.base import BaseEstimator
from matplotlib import pyplot
from sklearn.metrics import roc_curve, auc
from sklearn.utils import shuffle
import pylab as pl

class GradientDescent(BaseEstimator):

    def fit(self,X,Y,learning_rate=0.0005, reg_param=0.01):
        x = numpy.array(X)
        m, n = x.shape

        # normalize data
        self.xMean = numpy.mean(x, axis=0)
        self.xStd = numpy.std(x, axis=0)
        x = (x - self.xMean) / self.xStd

        # add const column to X
        const = numpy.array([1] * m).reshape(m, 1)
        self.X = numpy.append(const, x, axis=1)

        self.Y = numpy.array(Y)
        self.learning_rate = learning_rate
        self.reg_param = reg_param
        self.theta = numpy.random.uniform(0,0,size=n+1)
        self.cost_vals = []
        #print "reg_parambda=", self.reg_param      
        self._gradientDescend(10000)  
        return self
    
    def _sigmoid(self, x):
        z = 1.0 / (1.0 + numpy.exp((-1) * x))
        return z

    # caluclate cost
    def _costFunc(self):
        m, n = self.X.shape
        h_theta = self._sigmoid(numpy.dot(self.X, self.theta))

        cost1 = (-1) * self.Y * numpy.log(h_theta)
        cost2 = (1.0 - self.Y) * numpy.log(1.0 - h_theta)

        cost = (
            sum(cost1 - cost2) + 0.5 * self.reg_param * sum(self.theta[1:] ** 2)) / m
        return cost

    # gradient descend
    def _gradientDescend(self, iters):
        """
        gradient descend:
        X: feature matrix
        Y: response
        theta: predict parameter
        learning_rate: learning rate
        reg_param: reg_parambda, penality on theta
       """
        m, n = self.X.shape

        for i in xrange(0, iters):
            theta_temp = self.theta
            #compute_hypothesis
            h_theta = self._sigmoid(numpy.dot(self.X, self.theta))
            #compute error
            diff = h_theta - self.Y
            #update theta
            self.theta[0] = theta_temp[0] - self.learning_rate * \
                (1.0 / m) * sum(diff * self.X[:, 0])
            for j in xrange(1, n):
                val = theta_temp[
                    j] - self.learning_rate * (1.0 / m) * (sum(diff * self.X[:, j]) + self.reg_param * m * theta_temp[j])
                self.theta[j] = val
                
            cost = self._costFunc()
            self.cost_vals.append(cost)


    def predict(self, X):
        X = numpy.array(X)
        m, n = X.shape
        x = numpy.array(X)
        x = (x - self.xMean) / self.xStd
        # add const column
        const = numpy.array([1] * m).reshape(m, 1)
        X = numpy.append(const, x, axis=1)

        pred = self._sigmoid(numpy.dot(X, self.theta))
        #predict 1 if sigmoid > 0.5
        numpy.putmask(pred, pred >= 0.5, 1.0)
        #predict 0 if sigmoid < 0.5
        numpy.putmask(pred, pred < 0.5, 0.0)

        return pred
        
    def predict_proba(self, X_test):
        ret = []        
        for i in range(0,len(X_test)):
            X_curr = X_test[i]
            X = []
            X.append(X_curr)
            X = numpy.array(X)
            m, n = X.shape
            x = numpy.array(X)
            x = (x - self.xMean) / self.xStd
            # add const column
            const = numpy.array([1] * m).reshape(m, 1)
            X = numpy.append(const, x, axis=1)
            pred = self._sigmoid(numpy.dot(X, self.theta))
            ret.append([1-pred,pred])
        return numpy.array(ret)

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
                classes.append(int(parts[-1])-1)
                features.append(np.array(feat))
                    
    fo.close()

    return (features,np.array(classes))

data_set_filepath = 'seeds_dataset.txt'
(features,classes) = read_features(data_set_filepath)
lr = GradientDescent()
lr.fit(features,classes)
print 'Weight Vector:' + str(list(lr.theta)[1:])
pyplot.xlabel('Iteration Count')
pyplot.ylabel('Cost OR Error')
x_vals = []
for i in range(0,len(lr.cost_vals)):
    x_vals.append(i)
pyplot.plot(x_vals,lr.cost_vals)
pyplot.title('Cost Function For each Iteration')
pyplot.savefig('Cost_Function_Change')
pyplot.close()
print '\nCost Function Trend is saved in Figure:Cost_Function_Change.png\n'

print "\n\tMean Accuracy\tMean Error"
for n in range(3,12):
    score = cross_validation.cross_val_score(lr, features, classes, cv=n, scoring='accuracy')
    print str(n) + '\t' + str(score.mean()*100) + '\t' +  str((1 - score.mean()) * 100)
    
random_state = np.random.RandomState(0)
features, classes = shuffle(features, classes, random_state=random_state)

half = int(len(features) / 2)
X_train, X_test = features[:half], features[half:]
y_train, y_test = classes[:half], classes[half:]

classifier = GradientDescent()
probas_ = classifier.fit(X_train, y_train).predict_proba(X_test)

# Convert into binary values
y_test1 = []
for c in y_test:
    y_test1.append(int(c))
fpr, tpr, thresholds = roc_curve(y_test1, probas_[:, 1])
roc_auc = auc(fpr, tpr)
print "\nArea under the ROC curve for Gradient Descent: %f" % roc_auc

# Plot ROC curve
pl.clf()
pl.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc)
pl.plot([0, 1], [0, 1], 'k--')
pl.xlim([0.0, 1.0])
pl.ylim([0.0, 1.0])
pl.xlabel('False Positive Rate')
pl.ylabel('True Positive Rate')
pl.title('ROC using GradientDescent')
pl.legend(loc="lower right")
pl.savefig('ROC_GD')
pl.close()
print "\nROC Curve saved at:ROC_GD.png"
