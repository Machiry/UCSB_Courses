import numpy as np
from numpy.linalg import inv
import math
def find_matches(li,target):
    matches = []
    i = 0
    for i in range(0,len(li)):
        if li[i] == target:
            matches.append(i)
    return matches

def get_mean(li,matches):
    means = [0.0]*len(li[0])
    for m in matches:
        for i in range(0,len(means)):
            means[i] += li[m][i]
    for i in range(0,len(means)):
        means[i] = means[i]/len(matches)
    return means

def get_covarience_repre(li,matches):
    cov_matrix = []
    for i in range(0,len(li[0])):
        curr_f = []
        for m in matches:
            curr_f.append(li[m][i])
        cov_matrix.append(curr_f)
    return cov_matrix

def LDA(input_data,classes):
    '''no of samples'''
    n = len(input_data)
    '''no of features'''
    m = len(input_data[0])
    '''remove duplicates'''
    unique_classes = list(set(classes))
    k = len(unique_classes)
    ngroup = [0]*k
    group_mean = ([0]*m)*k
    pool_cov = ([0]*m)*m
    w = [0]*k
    #w = []
    prior_prob = [0]*k

    for i in range(0,k):
        curr_class = unique_classes[i]
        curr_class_matches = find_matches(classes,curr_class)
        ngroup[i] = len(curr_class_matches)
        group_mean[i] = get_mean(input_data,curr_class_matches)
        cov_repre = get_covarience_repre(input_data,curr_class_matches)
        cov_mat = np.cov(cov_repre)
        multiplier = (ngroup[i]-1) / (n-k)
        curr_pool_cov = np.multiply(cov_mat,multiplier)
        pool_cov = np.add(curr_pool_cov,pool_cov)
        prior_prob[i] = ngroup[i] / n

    inv_pool = inv(pool_cov)
    for i in range(0,k):
        temp = np.multiply(group_mean[i],inv_pool)
        first_term = np.multiply(np.multiply(-0.5,temp),np.array(group_mean[i]).transpose())
        sec_term = math.log(prior_prob[i])
        w_2 = np.add(first_term,sec_term)
        w.append(temp)
        w[i] = []
        w[i].append(temp)
        w[i].append(w_2)
    return w

        





    
