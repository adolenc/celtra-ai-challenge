import numpy as np
import matplotlib.pyplot as plt
import csv
from collections import defaultdict


d = defaultdict(list)
with open("../data/learnData.csv", "r" ) as f:
    rd = csv.reader(f, delimiter=',')
    for line in rd:
        d[line[0]].append([float(line[1]), float(line[2])])

# d2 = defaultdict(list)
# with open("test.csv", "r" ) as f:
#     rd = csv.reader(f, delimiter=',')
#     for line in rd:
#         d2[line[0]].append([float(line[1]), float(line[2])])
# x, y = plot_trace(d2['s1489407481x17867fcbaba115x68748903'])
# ax.plot(y, x, 'b')

def plot_trace(trace):
    x = [t[0] for t in trace]
    y = np.array([t[1] for t in trace]) * np.pi / 180
    return x, y

ids = d.keys()
for i, id in enumerate(ids):
    ax = plt.subplot(111, projection='polar')
    x, y = plot_trace(d[id])
    ax.plot(y, x, 'r')

    ax.set_rmax(60)
    ax.set_rlabel_position(2)
    ax.set_rticks([tick for tick in range(0, 60, 10)] + [60])
    ax.grid(True)

    plt.show()
