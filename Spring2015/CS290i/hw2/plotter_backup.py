from matplotlib import pyplot
import pylab
fo = open('/home/machiry/Desktop/balance-scale.data','r')
lines = fo.readlines()
x1 = []
y1 = []
x2 = []
y2 = []
escp = False
for line in lines:
    parts = line.strip().split(',')
    if parts[0].strip() == 'L':
        x1.append(int(parts[1])*int(parts[2]))
        y1.append(int(parts[3])*int(parts[4]))
    elif parts[0].strip() == 'R' :
        x2.append(int(parts[1])*int(parts[2]))
        y2.append(int(parts[3])*int(parts[4]))
#x = [1,2,3,4,5]
#y = [5,4,3,2,1]
pyplot.scatter(x1,y1)
pyplot.show()
#pyplot.clf()
'''pyplot.scatter(x2,y2)
pyplot.show()
pyplot.clf()'''

