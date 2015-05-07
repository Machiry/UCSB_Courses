from matplotlib import pyplot
import pylab
from sklearn.decomposition import PCA
fo = open('/home/machiry/Desktop/balance-scale.data','r')
lines = fo.readlines()
x = []
cl = []
escp = False
for line in lines:
    parts = line.strip().split(',')
    if parts[0] == 'L' or parts[0] == 'R':
        x.append([int(parts[1]),int(parts[2]),int(parts[3]),int(parts[4])])
        cl.append(parts[0])

pca = PCA(n_components=2)
new_x = pca.fit_transform(x)
i = 0
x1 = []
y1 = []
x2 = []
y2 = []
for elm in new_x:
    if cl[i] == 'L':
        x1.append(elm[0])
        y1.append(elm[1])
    else:
        x2.append(elm[0])
        y2.append(elm[1])
    i = i + 1
    #print str(elm)
#print str(new_x)

#print str(x1) + str(x2)
pyplot.scatter(x2,y2)
pyplot.show()
#pyplot.clf()
'''pyplot.scatter(x2,y2)
pyplot.show()
pyplot.clf()'''

