import random
import sys

'''
    Extended Euclid algorithm.
'''
def eea(n,r):
    g0 = r
    g1 = n
    u0 = 1
    u1 = 0
    v0 = 0
    v1 = 1
    while not (g1 == 0):
        q = g0 / g1
        n_g0 = g1
        n_g1 = g0 - g1*q
        n_u0 = u1
        n_u1 = u0 - u1*q
        n_v0 = v1
        n_v1 = v0 - v1*q
        u0 = n_u0
        u1 = n_u1
        v0 = n_v0
        v1 = n_v1
        g0 = n_g0
        g1 = n_g1
        
    return (g0,u0,v0)
        
    
m = 50
p = 97
q = 103
N = 9991
phiN = 9792
e = 2015
d = 8927

d1 = d%(p-1)
d2 = d%(q-1)

(modulus,q1,p1) = eea(p,q)

print str((modulus,q1,p1))

assert(modulus == 1)

m1 = (m**d1)%p
m2 = (m**d2)%q
s= m1 + p*(((m2-m1)*p1)%q)
print 'Answer:' + str(s)
sf = 83 + p*(((m2-83)*p1)%q)
print p*(((m2-83)*p1)%q)
print 'sf:' + str(sf)
   
    
