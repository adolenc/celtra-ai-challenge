import numpy as np
import matplotlib.pyplot as plt
import csv
from collections import defaultdict


# d2 = defaultdict(list)
# with open("test.csv", "r" ) as f:
#     rd = csv.reader(f, delimiter=',')
#     for line in rd:
#         d2[line[0]].append([float(line[1]), float(line[2])])
# x, y = plot_trace(d2['s1489407481x17867fcbaba115x68748903'])
# ax.plot(y, x, 'b')

def plot_trace(deg):
    x = [50, 60]
    y = np.array([deg, deg]) * np.pi / 180
    return x, y

ax = plt.subplot(111, projection='polar')
for d in range(0, 90):
    x, y = plot_trace(d*4)
    ax.plot(y, x, 'r')

for d in range(0, 20):
    x, y = plot_trace(d*4)
    ax.plot(y, x, 'b')

for d in range(86, 90):
    x, y = plot_trace(d*4)
    ax.plot(y, x, 'b')

x = [10, 40]
y = np.array([30, 30]) * np.pi / 180
ax.plot(y, x, 'black')

ax.set_rmax(60)
ax.set_theta_zero_location('N')
ax.set_theta_direction(-1)
ax.set_rlabel_position(2)
ax.set_rticks([])
ax.grid(False)

plt.show()
