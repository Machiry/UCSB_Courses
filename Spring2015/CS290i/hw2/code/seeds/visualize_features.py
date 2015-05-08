from matplotlib import pyplot
import pylab

def read_features(file_name):
    fo = open(file_name,'r')
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
    fo.close()

    return (x1,y1,x2,y2)

def plot_features(x1,y1,x2,y2,target_folder):
    pyplot.xlabel('Surface Area of Wheat Kernel')
    pyplot.ylabel('Perimeter of Wheat Kernel')
    pyplot.scatter(x2,y2,marker='+')
    pyplot.scatter(x1,y1,marker='*')
    pyplot.title('Wheat Kernels Geometric Features Visualization\n(\'+\' and \'*\' represents features for classes Kama and Rosa Respectively)')
    pyplot.savefig(target_folder + '/Features_Visualization')
    pyplot.close()

dataset_file = 'seeds_dataset.txt'
target_folder = 'data_vis'
(x1,y1,x2,y2) = read_features(dataset_file)
plot_features(x1,y1,x2,y2,target_folder)

