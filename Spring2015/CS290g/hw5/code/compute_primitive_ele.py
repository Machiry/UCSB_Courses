import sys
import math

def find_inv(p, target):
    for i in range(2,target):
        if p*i%target == 1:
            return i
    return None

def add_point(x1,y1,x2,y2,target):
    x_n = x2 - x1
    #print y2-y1
    '''if x_n < 0:
        x_n = x_n + target
    print x_n'''
    x_inv = find_inv(x_n,target)
    #print x_inv
    lam = (y2-y1)*x_inv
    #print lam
    lam = lam % target
    if lam < 0:
        lam = lam + target
    #print lam
    x3 = lam*lam - x1 - x2
    x3 = x3 % target
    y3 = lam*(x1-x3) - y1
    y3 = y3 % target
    if x3 < 0:
        x3 = target+x3
    if y3 < 0:
        y3 = target+y3
    return (x3,y3)
    
    

def squ_point(x1,y1,a,target):
    fir = (3*x1*x1+a)
    sec = find_inv(2*y1,target)
    lam = (fir*sec)%target
    x3 = lam*lam - 2*x1
    x3 = x3%target
    if x3 < 0:
        x3 = target+x3
    y3 = lam*(x1-x3) - y1
    y3 = y3%target
    if y3 < 0:
        y3 = target+y3
    return (x3,y3)


# P
target = 29
#print add_point(3,10,9,7,target)
#print squ_point(7,6,-3,target)
#print add_point(14,26,7,6,target)
#print squ_point(28,21,-3,target)
#print add_point(2,8,7,6,target)
#print squ_point(19,22,-3,target)
#print add_point(13,4,7,6,target)

print squ_point(8,12,-3,target)
print add_point(22,1,7,23,target)
#print squ_point(19,22,-3,target)
#print squ_point(19,22,-3,target)
    
