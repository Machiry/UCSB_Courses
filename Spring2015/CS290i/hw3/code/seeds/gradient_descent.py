import random
import numpy
import numpy as np
from sklearn import cross_validation
from sklearn.base import BaseEstimator

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
        self.theta = numpy.random.uniform(0,1,size=n+1)

        #print "reg_parambda=", self.reg_param      
        self._gradientDescend(10000)  
    
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
            # update theta[0]
            #compute_hypothesis
            h_theta = self._sigmoid(numpy.dot(self.X, self.theta))
            #compute cost
            diff = h_theta - self.Y
            #update theta
            self.theta[0] = theta_temp[0] - self.learning_rate * \
                (1.0 / m) * sum(diff * self.X[:, 0])
            for j in xrange(1, n):
                val = theta_temp[
                    j] - self.learning_rate * (1.0 / m) * (sum(diff * self.X[:, j]) + self.reg_param * m * theta_temp[j])
                self.theta[j] = val


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
scores = cross_validation.cross_val_score(lr, features, classes, cv=10, scoring='accuracy')
print lr.theta
print scores.mean()*100
