import matplotlib.pyplot as plt
import numpy as np
import csv
from scipy.stats import norm
from collections import defaultdict, Counter


d = defaultdict(list)
with open("../data/learnData.csv", "r" ) as f:
    rd = csv.reader(f, delimiter=',')
    for line in rd:
        d[line[0]].append((float(line[1]), float(line[2])))

ids = list(d.keys())

def ang_minus2(y1, y2):
    a1 = (y2 - y1) % 360
    a2 = (y1 - y2) % 360
    if a1 < a2: return a1
    return -a2


import math
deltas = []
times = []
for id in ids:
    for (time1,theta1), (time2,theta2) in zip(d[id], d[id][1:]):
        if time2 - time1 > 0.08:
            # deltas.append(ang_minus2(theta2,theta1))
            deltas.append((ang_minus2(theta2,theta1)/((time2-time1)*10)))
            times.append(time2-time1)

data = np.array(deltas)
# data = data[data != 0]
print(data)
mu, std = norm.fit(data)
print(mu, std)

plt.hist(data, bins=360)

plt.axis([-20, 20, 0, 18000])
plt.xlabel('Relativni kot')
plt.ylabel('Število časovnih korakov')
plt.show()
