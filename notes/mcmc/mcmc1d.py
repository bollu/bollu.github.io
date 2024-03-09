#!/usr/bin/env python3
# Run HMC with a particular choice of potential
import numpy as np
from matplotlib.animation import FuncAnimation
import matplotlib.pyplot as plt
import numpy.linalg
import argparse
import itertools

np.random.seed(0)



## dq/dt = dH/dp|_{p0, q0}
## dp/dt = -dH/dq|_{p0, q0}
def leapfrog(dhdp, dhdq, q0, p0, dt):
    # kick: half step momentum
    p0 += -dhdq(q0, p0) * 0.5 * dt

    # drift: full step position
    q0 += dhdp(q0, p0) * dt

    # kick: half step momentum
    p0 += -dhdq(q0, p0) * 0.5 * dt
    return (q0, p0)

def euler(dhdp, dhdq, q, p, dt):
   pnew = p + -dhdq(q, p) * dt
   qnew = q + dhdp(q, p) * dt
   return (qnew, pnew)

def mhsimple(x0, prob, prop):
    yield x0; x = x0;
    while True:
        xnext = prop(x); p = prob(x); pnext = prob(xnext)
        r = np.random.uniform() + 1e-5;
        if r < pnext/p: x = xnext
        yield xnext

def mh_uncorr(x0, prob, prop, iters_per_sample):
    yield x0; x = x0;
    while True:
        for i in range(iters_per_sample):
            xnext = prop(x); p = prob(x); pnext = prob(xnext)
            r = np.random.uniform()
            if r < min(1, pnext/p): x = xnext
        yield xnext

def mh_opt(x0, logprob, prop, iters_per_sample):
    yield x0; x = x0;
    while True:
        for i in range(iters_per_sample):
            xnext = prop(x);  lpnext = logprob(xnext); lp = logprob(x);
            r = np.random.uniform()
            if np.log(r) < min(0, lpnext - lp): x = xnext
        yield xnext

# MH
def exp(x): return np.exp(-x*x)
def logexp(x): return -x*x
def expgrad(x): return -2*x
def expprop(x): return np.random.normal(loc=x, scale=1e-1)

### mhsimple ###
NSAMPLES = 1000
COLORTRUTH = "#5C6BC0"; COLORSAMPLES = "#D81B60"
xs = list(itertools.islice(mhsimple(0, exp, expprop), NSAMPLES))
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
ax[0].set_title("MH  naive simple #samples=%s"% NSAMPLES)
ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)
ax[0].spines['bottom'].set_visible(False)
ax[0].spines['left'].set_visible(False)

ax[1].set_title("cumulative sum")
ax[1].hist(xs, bins=200, cumulative=True, density=True, label='prob', linewidth=5, color=COLORSAMPLES)
ax[1].plot(fxs, fyscum, linewidth=5, color=COLORTRUTH, alpha=0.5)
ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)
ax[1].spines['bottom'].set_visible(False)
ax[1].spines['left'].set_visible(False)
plt.tight_layout()
fig_size = plt.gcf().get_size_inches() #Get current size
plt.gcf().set_size_inches(2.0 * fig_size) 
plt.savefig("mcmc-mh-simple-1d-exp.png")
plt.show()

## MH UNCORR ##

NSAMPLES = 1000; ITERS_PER_SAMPLE = 20
COLORTRUTH = "#5C6BC0"; COLORSAMPLES = "#D81B60"
xs = list(itertools.islice(mh_uncorr(0, exp, expprop, ITERS_PER_SAMPLE), NSAMPLES))
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
ax[0].set_title("MH-decor #samples=%s | #iters/sample: %s"% (NSAMPLES, ITERS_PER_SAMPLE))
ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)
ax[0].spines['bottom'].set_visible(False)
ax[0].spines['left'].set_visible(False)

ax[1].set_title("cumulative sum")
ax[1].hist(xs, bins=200, cumulative=True, density=True, label='prob', linewidth=5, color=COLORSAMPLES)
ax[1].plot(fxs, fyscum, linewidth=5, color=COLORTRUTH, alpha=0.5)
ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)
ax[1].spines['bottom'].set_visible(False)
ax[1].spines['left'].set_visible(False)
# plt.tight_layout()
fig_size = plt.gcf().get_size_inches() #Get current size
plt.gcf().set_size_inches(2.0 * fig_size) 
plt.savefig("mcmc-mh-uncorr-1d-exp.png")
plt.show()

## MH STARTX 600 ##

NSAMPLES = 1000; ITERS_PER_SAMPLE = 20
COLORTRUTH = "#5C6BC0"; COLORSAMPLES = "#D81B60"
xs = list(itertools.islice(mh_opt(600, logexp, expprop, ITERS_PER_SAMPLE), NSAMPLES))
ys = [exp(x) for x in xs] 
fxs = np.arange(np.min(xs)-1e-1, np.max(xs)+1e-1, (np.max(xs)+1e-1 - (np.min(xs) - 1e-1)) / 1000.0);
fys = [exp(x) for x in fxs];
fyscum = np.cumsum(fys); fyscum = fyscum / np.max(fyscum)
plt.rcParams.update({'font.size': 20, 'font.family':'monospace'})
fig, ax = plt.subplots(3, 1)
ax[0].plot(fxs, fys, label='prob',
        linewidth=5, color=COLORTRUTH, markersize=4.0, alpha=0.4)
ax[0].plot(xs, ys, 'x', label='prob',
        linewidth=5, color=COLORSAMPLES, markersize=4.0)

legend = plt.legend(frameon=False)
ax[0].set_title("MH #samples=%s | #iters/sample: %s"% (NSAMPLES, ITERS_PER_SAMPLE))
ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)
ax[0].spines['bottom'].set_visible(False)
ax[0].spines['left'].set_visible(False)

ax[1].set_title("cumulative sum")
ax[1].hist(xs, bins=400, cumulative=True, density=True, label='prob', linewidth=5, color=COLORSAMPLES)
ax[1].plot(fxs, fyscum, linewidth=5, color=COLORTRUTH, alpha=0.5)
ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)
ax[1].spines['bottom'].set_visible(False)
ax[1].spines['left'].set_visible(False)

RCUTOFF = 2
xszoom = [x for x in xs if x < RCUTOFF]
l = np.min(xs)-1e-1; r = min(RCUTOFF, np.max(xszoom)+1e-1)
fxszoom = np.arange(l, r, (r - l) / 1000.0);
fyszoom = [exp(x) for x in fxszoom];
fyszoomcum = np.cumsum(fyszoom); fyszoomcum = fyszoomcum / np.max(fyszoomcum)

ax[2].set_title("cumulative (zoomed: %4.2f < x < %4.2f)=%s " % (l, r, len(xszoom)))
ax[2].hist(xszoom, bins=400, cumulative=True, density=True, label='prob', linewidth=5, color=COLORSAMPLES)
ax[2].plot(fxszoom, fyszoomcum, linewidth=5, color=COLORTRUTH, alpha=0.5)

ax[2].spines['top'].set_visible(False)
ax[2].spines['right'].set_visible(False)
ax[2].spines['bottom'].set_visible(False)
ax[2].spines['left'].set_visible(False)

plt.tight_layout()
fig_size = plt.gcf().get_size_inches() #Get current size
plt.gcf().set_size_inches(2.0 * fig_size) 
plt.tight_layout()
plt.savefig("mcmc-mh-uncorr-1d-exp-startx-600.png")
plt.show()
