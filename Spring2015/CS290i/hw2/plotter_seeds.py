from matplotlib import pyplot
import pylab
fo = open('/home/machiry/Desktop/seeds_dataset.txt','r')
lines = fo.readlines()
x1 = []
y1 = []
x2 = []
y2 = []
escp = False
for line in lines:
    parts = line.strip().split()
    if parts[-1].strip() == '1':
        x1.append(float(parts[0]))
        y1.append(float(parts[1]))
    elif parts[-1].strip() == '2':
        x2.append(float(parts[0]))
        y2.append(float(parts[1]))
#x = [1,2,3,4,5]
#y = [5,4,3,2,1]
pyplot.scatter(x2,y2,marker='+')
pyplot.scatter(x1,y1,marker='*')
pyplot.show()
#pyplot.clf()
'''pyplot.scatter(x2,y2)
pyplot.show()
pyplot.clf()'''

