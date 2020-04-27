#!/usr/bin/env python3
# Run HMC with a particular choice of potential
import numpy as np
from matplotlib.animation import FuncAnimation
import matplotlib.pyplot as plt
import numpy.linalg
import argparse
import itertools

np.random.seed(0)

# envelope
def gauss(x): return np.exp(-x*x)
def envelope(x): return 3.5 * gauss(x * 0.3)
def f(x): return gauss(x*0.5) * (2.0 + -0.4 * np.sin(5*x) + 0.5 * np.sin(7 * x - 0.2)  + 0.3 * np.cos(11 * x + 0.4))

xs = np.arange(-10, 10, 20 / 1000)
print(xs)
fs = [f(x) for x in xs]
es = [envelope(x) for x in xs]

COLORTRUTH = "#5C6BC0"; COLORSAMPLES = "#D81B60"
plt.rcParams.update({'font.size': 10, 'font.family':'monospace'})
fig, ax = plt.subplots()
ax.plot(xs, fs, label='complicated fn', linewidth=3, color=COLORSAMPLES)
ax.plot(xs, es, label='simple envelope', linewidth=3, color=COLORTRUTH)
legend = plt.legend(frameon=False)
ax.set_title("importance sampling")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_visible(False)
ax.spines['left'].set_visible(False)
plt.tight_layout()
plt.savefig("complicated-fn-simple-envelope.png")
plt.show()
