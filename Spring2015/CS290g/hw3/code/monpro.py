import sys
a = int(sys.argv[1])
b = int(sys.argv[2])
r = 32
n1 = 11
n = 29
t = a*b
print '\\\\'
print '$MonPro(' + str(a) + ',' + str(b) + ')$\\\\'
print 't = ' + str(a) + ' * ' + str(b) + ' = ' + str(t) + '\\\\'
m = (t*n1) % r
print 'm = ' + str(t) + ' * ' + str(n1) + ' mod ' + str(r) + ' = ' + str(m) + '\\\\'
u = (t + m*n) 
assert(u%r == 0)
u = u/r
if u >= n:
    print 'u = ' + '((' + str(t) + ' + ' + str(m) + ' * ' + str(n) + ') / ' + str(r) +') - ' + str(n) + ' = ' + str(u-n) +'\\\\'
    print str(u - n)
else:
    print 'u = ' + '(' + str(t) + ' + ' + str(m) + ' * ' + str(n) + ') / ' + str(r) +' = ' +str(u) +'\\\\'
    print str(u)
