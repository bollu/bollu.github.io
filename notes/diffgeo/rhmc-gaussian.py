#!/usr/bin/env python3
# Run HMC with a particular choice of potential
import numpy as np
from matplotlib.animation import FuncAnimation
import matplotlib.pyplot as plt
import numpy.linalg
import numpy.random

# dq/dt = dH/dp
# dp/dt = -dH/dq (a = -del V)
def leapfroge(dhdp, dhdq, q, p, dt):
    p += -dhdq(q, p) * 0.5 * dt 
    q += dhdp(q, p) * dt
    p += -dhdq(q, p) * 0.5 * dt 
    return (q, p)

def planet(integrator, n, dt):
    STRENGTH = 0.5

    # minimise potential V(q): q, K(p, q) p^2
    q = np.array([0.0, 1.0])
    p = np.array([-1.0, 0.0])
    
    # H = STRENGTH * |q| (potential) + p^2/2 (kinetic)
    def H(qcur, pcur): return STRENGTH * np.linalg.norm(q) + np.dot(p, p) / 2
    def dhdp(qcur, pcur): return p
    def dhdq(qcur, pcur): return STRENGTH * 2 * q / np.linalg.norm(q)
    
    qs = []
    for i in range(n):
        print("q: %10s | p: %10s | H: %6.4f" % (q, p, H(q, p)))
        (q, p) = integrator(dhdp, dhdq, q, p, dt)
        qs.append(q.copy())
    return np.asarray(qs)


STARTQ = np.asarray([0.0, 20.0])

GAUSSIAN_CENTERS = np.asarray([[0.0, 0.0]])
GAUSSIAN_WEIGHTS = np.asarray([1.0])

# H = -log P(qcur) + p^2/2 (kinetic)
def prob(qcur): 
    s = 0
    for i in range(GAUSSIAN_CENTERS.shape[0]):
        c = GAUSSIAN_CENTERS[i]
        w = GAUSSIAN_WEIGHTS[i]
        s += w * np.exp(-1 * np.dot(qcur - c, qcur - c))
    return s

def dprob(qcur): 
    s = 0
    # d/dx(exp(- (x - μ)^2)) = (-2 (x - μ)) * exp(- (x - μ)^2)
    for i in range(GAUSSIAN_CENTERS.shape[0]):
        c = GAUSSIAN_CENTERS[i]
        w = GAUSSIAN_WEIGHTS[i]
        s += -2 * (qcur - c) * w * np.exp(-1 * np.dot(qcur - c, qcur - c))
    return s

# search for maxima of gaussian distribution
def gaussian_hmc(startq, integrator, n):
    def H(qcur, pcur): return -np.log(prob(qcur)) + np.dot(pcur, pcur) / 2
    def dhdp(qcur, pcur): return pcur
    def dhdq(qcur, pcur): return -1.0 / prob(qcur) * dprob(qcur)

    # starting positions
    qcur = startq.copy()
    qs = []
    naccept = 0

    for i in range(n):
        qs.append(qcur.copy())

        # redraw momentum
        pcur = np.array([np.random.normal(), np.random.normal()])
        curH = H(qcur, pcur)

        # make proposal from physiccs
        qprop = qcur.copy()
        pprop = pcur.copy()

        N_PHYSICS_STEPS = 3
        # 0.1 * x = 8
        DT = 0.2
        for _ in range(N_PHYSICS_STEPS):
            (qprop, pprop) = integrator(dhdp, dhdq, qprop, pprop, DT)

        # Negate momentum at end of trajectory to make the proposal symmetric
        # WHY? I don't understand this!
        pprop = -pprop

        # get propsal energy
        propH = H(qprop, pprop)

        # minimise energy: new < old
        # e(cur - prop) > 1
        # cur - prop > 0
        # cur > prop (energy reduces!)
        if np.random.uniform() < np.exp(curH - propH):
        # print("NOTE: using MH acceptance criteria for HMC")
        # if np.log(np.random.uniform()) < np.log(prob(qprop)) -  np.log(prob(qcur)):
             qcur = qprop.copy()
             pcur = pprop.copy()
             naccept += 1

    return (naccept, np.asarray(qs))



def gaussian_mh(startq, n):
    qcur = startq.copy()
    qs = []
    naccept = 0

    for i in range(n):
        qprop = qcur + np.array([np.random.normal(), np.random.normal()])
        # we want prob to increase
        if np.log(np.random.uniform()) < np.log(prob(qprop)) - np.log(prob(qcur)):
            qcur = qprop
            naccept += 1
        qs.append(qcur.copy())

    return (naccept, np.asarray(qs))

NITERS = int(10)
hmc_naccept, hmc_qs = gaussian_hmc(STARTQ, leapfroge, NITERS)

print ("Ideal acceptance ratio: ~20%")

print("HMC acceptance ratio: %4.2f%%" % (hmc_naccept / NITERS * 100))

plt.rcParams.update({'font.size': 12, 'font.family':'monospace'})
fig, ax = plt.subplots()

X, Y = np.meshgrid(np.linspace(-3, 10.0, 100), np.linspace(-3, 10.0, 100))
Z = np.empty(X.shape)
for i in range(X.shape[0]):
    for j in range(X.shape[1]):
        Z[i][j] = np.log(prob(np.asarray([X[i][j], Y[i][j]], dtype=np.float)))

plt.contourf(X, Y, Z, cmap='gray')

ax.scatter(hmc_qs[:, 0], hmc_qs[:, 1], label='HMC', c='#E91E6355')

mh_naccept, mh_qs = gaussian_mh(STARTQ, NITERS)

print("MH acceptance ratio: %4.2f%%" % (mh_naccept / NITERS * 100))

ax.scatter(mh_qs[:, 0], mh_qs[:, 1], label='MH', c='#26C6DA55')


box = ax.get_position(); ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])


legend = ax.legend(frameon=False, loc='center left', bbox_to_anchor=(1, 0.5))
legend.legendHandles[0].set_color('#E91E63') # HMC
legend.legendHandles[1].set_color('#26C6DA') # MH

ax.set_title("HMC v/s MH")

ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_visible(False)
ax.spines['left'].set_visible(False)


plt.savefig("hmc.png")
plt.show()
