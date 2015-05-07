from matplotlib import pyplot
import pylab
import matplotlib.projections as proj 
from mpl_toolkits.mplot3d import axes3d, Axes3D
fo = open('/home/machiry/Desktop/Skin_NonSkin.txt','r')
lines = fo.readlines()
x1 = []
y1 = []
z1 = []
x2 = []
y2 = []
z2 = []
escp = False
for line in lines:
    parts = line.strip().split()
    if parts[-1].strip() == '1':
        x1.append(int(parts[0]))
        y1.append(int(parts[1]))
        z1.append(int(parts[2]))
    else:
        x2.append(int(parts[0]))
        y2.append(int(parts[1]))
        z2.append(int(parts[2]))
#x = [1,2,3,4,5]
#y = [5,4,3,2,1]
proj.projection_registry.register(Axes3D) 
fig = pyplot.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x1,y1,z1,c='r',marker='o')
pyplot.show()
#pyplot.clf()
'''pyplot.scatter(x2,y2)
pyplot.show()
pyplot.clf()'''

