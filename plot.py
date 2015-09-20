from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import csv

csvreader = csv.reader(open("Ez.csv") )

E = []

for row in csvreader:
    E.append(row[2])

for i in range(len(E) ):
    E[i] = float(E[i]) 

x = np.arange( np.sqrt( len(E) ))
y = np.arange( np.sqrt( len(E) ))

xx, yy = np.meshgrid(x,y)

zz = np.zeros(xx.shape)

for i in range(xx.shape[0]):
    for j in range(xx.shape[0]):
        zz[i, j] = E[i* int(np.sqrt( len(E) )) + j]
    
plt.pcolor(xx,yy,zz)
plt.colorbar()

plt.show()


