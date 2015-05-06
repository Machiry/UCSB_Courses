import numpy as np
import scipy.stats as stats
import collections
import matplotlib.pyplot as pl

def read_features(file_name):
    f = open(file_name,'r')
    feat_dict_0 = collections.defaultdict(list)
    feat_dict_1 = collections.defaultdict(list)
    for curr_line in f.readlines():
        curr_line = curr_line.strip()
        parts = curr_line.split(',')
        to_insert_dict = None
        if parts[5] == '0':
            to_insert_dict = feat_dict_0
        else:
            to_insert_dict = feat_dict_1
        for feat in range(5):
            if not (parts[feat] == '?'):
                to_insert_dict[feat].append(float(parts[feat]))

    #sort the feature values
    for feat in range(5):
        feat_dict_0[feat].sort()
        feat_dict_1[feat].sort()

    return (feat_dict_0,feat_dict_1)

def plot_features(feat_dict_0,feat_dict_1,target_folder):
    for feat in range(5):
        l1 = feat_dict_0[feat]
        l2 = feat_dict_1[feat]
        str(np.mean(l1))
        fit1 = stats.norm.pdf(l1, np.mean(l1), np.std(l1)) 
        pl.plot(l1,fit1)
        pl.ylabel('Distribution')
        pl.xlabel('Feature ' + str(feat+1))
        fit2 = stats.norm.pdf(l2, np.mean(l2), np.std(l2))
        pl.plot(l2,fit2)
        pl.savefig(target_folder + '/' + "FeatureDist"+str(feat+1))
        #close this so as to avoid, overlapping.
        pl.close()
        

data_set_filepath = 'mammographic_masses.data'
target_folder = 'data_vis'
(feat_dict_0,feat_dict_1) = read_features(data_set_filepath)
plot_features(feat_dict_0,feat_dict_1,target_folder)
               
