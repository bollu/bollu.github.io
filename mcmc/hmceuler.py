#!/usr/bin/env python3
# Run HMC with a particular choice of potential
import numpy as np
from matplotlib.animation import FuncAnimation
import matplotlib.pyplot as plt
import numpy.linalg
import argparse
import itertools

np.random.seed(0)

def leapfrog(dhdq, dhdp, q0, p0, dt):
    p0 += -dhdq(q0, p0) * 0.5 * dt  # kick: half step momentum
    q0 += dhdp(q0, p0) * dt # drift: full step position
    p0 += -dhdq(q0, p0) * 0.5 * dt # kick: half step momentum
    return (q0, p0)

def euler(dhdp, dhdq, q, p, dt):
   pnew = p + -dhdq(q, p) * dt
   qnew = q + dhdp(q, p) * dt
   return (qnew, pnew)


def hmc(q0, U, dU, nsteps, dt):
    M = 5.0
    def h(q, p): return U(q) + 0.5 / M  * p*p
    def nextsample(q, p):
        for _ in range(nsteps):
            def hdp(q, p): return 1.0 / M * p
            def hdq(q, p): return dU(q)
            (q, p) = euler(hdq, hdp, q, p, dt)
        return (q, p)

    yield q0; q = q0
    while True:
        p = np.random.normal(0, M)
        (qnext, pnext) = nextsample(q, p)
        r = np.random.uniform(); 
        pnext = -p # reverse momentum so our process is reversible
        if np.log(r) < h(q, p) - h(qnext, pnext): q = qnext
        yield q

def take_every_nth(n, gen):
    for (i, x) in enumerate(gen):
        if i % n == 0: yield x

# HMC
def exp(x): return np.exp(-x*x)
def expgrad(x): return np.exp(-x*x) * -2*x
def expprop(x): return np.random.normal(loc=x, scale=1e-1)
def logexp(x): return -x*x
def logexpgrad(x): return -2*x

def neglogexp(x): return -1 * logexp(x)
def neglogexpgrad(x): return -1 * logexpgrad(x)

def negexp(x): return -1 * exp(x)
def negexpgrad(x): return -1 * expgrad(x)

COLORTRUTH = "#5C6BC0"; COLORSAMPLES = "#D81B60"
NSAMPLES = 10000; NSTEPS = 3; DT = 1e-3; DECORRELATE_STEPS = 10
xs = list(take_every_nth(DECORRELATE_STEPS, itertools.islice(hmc(0.1, neglogexp, neglogexpgrad, NSTEPS, DT), NSAMPLES*DECORRELATE_STEPS)))
ys = [exp(x) for x in xs]
fxs = np.arange(np.min(xs)-1e-1, np.max(xs)+1e-1, (np.max(xs)+1e-1 - (np.min(xs) - 1e-1)) / 100.0);
fys = [exp(x) for x in fxs];
fyscum = np.cumsum(fys); fyscum = fyscum / np.max(fyscum)
plt.rcParams.update({'font.size': 20, 'font.family':'monospace'})
fig, ax = plt.subplots(2, 1)
ax[0].plot(fxs, fys, label='prob',
        linewidth=5, color=COLORTRUTH, markersize=4.0, alpha=0.4)
ax[0].plot(xs, ys, 'x', label='prob',
        linewidth=5, color=COLORSAMPLES, markersize=4.0)

legend = plt.legend(frameon=False)
ax[0].set_title("HMC: #samples=%s | steps inside hmc: %s " % (len(xs), NSTEPS))
ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)
ax[0].spines['bottom'].set_visible(False)
ax[0].spines['left'].set_visible(False)

ax[1].hist(xs, bins=200, cumulative=True, density=True, label='prob', linewidth=5, color=COLORSAMPLES)
ax[1].plot(fxs, fyscum, linewidth=5, color=COLORTRUTH, alpha=0.5)

ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)
ax[1].spines['bottom'].set_visible(False)
ax[1].spines['left'].set_visible(False)

fig_size = plt.gcf().get_size_inches() #Get current size
plt.gcf().set_size_inches(2.0 * fig_size) 
plt.savefig("mcmc-hmc-WRONG-euler-1d-exp.png")
plt.show()
