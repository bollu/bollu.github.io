# Run HMC with a particular choice of potential
import numpy as np
from matplotlib.animation import FuncAnimation
import matplotlib.pyplot as plt
import numpy.linalg

# dq/dt = dH/dp
# dp/dt = -dH/dq (a = -del V)
def leapfrog(dhdp, dhdq, q, p, dt):
    p += -dhdq(q, p) * 0.5 * dt 

    # full step position
    # q += dt * p 
    q += dhdp(q, p) * dt
    
    # half step position
    p += -dhdq(q, p) * 0.5 * dt 
    return (q, p)

def euler(dhdp, dhdq, q, p, dt):
    pnew = p + -dhdq(q, p) * dt
    qnew = q + dhdp(q, p) * dt
    return (qnew, pnew)

def straightline(integrator, dt):
    q = np.array([10.0])
    p = np.array([0.0])
    
    # H = |r| (potential) + p^2/2 (kinetic)
    def H(qcur, pcur): return np.dot(p, p) * 0.5 + np.linalg.norm(q)
    def dhdp(qcur, pcur): return pcur
    def dhdq(qcur, pcur): return 2.0 * q / np.linalg.norm(q)
    
    qs = []
    for i in range(10):
        print("q: %10s | p: %10s | H: %6.4f" % (q, p, H(q, p)))
        qs.append(q)
        (q, p) = integrator(dhdp, dhdq, q, p, dt)
    return np.array(qs)


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


NITERS = 15
TIMESTEP = 1

print("planet simulation with leapfrog")
planet_leapfrog = planet(leapfrog, NITERS, TIMESTEP)

plt.rcParams.update({'font.size': 12, 'font.family':'monospace'})
fig, ax = plt.subplots()
print(planet_leapfrog)
ax.plot(planet_leapfrog[:, 0], planet_leapfrog[:, 1], label='leapfrog',
        linewidth=3, color='#00ACC1')



print("planet simulation with euler")
planet_euler = planet(euler, NITERS, TIMESTEP)
ax.plot(planet_euler[:, 0], planet_euler[:, 1], label='euler',
        linewidth=3, color='#D81B60')

legend = plt.legend(frameon=False)

ax.set_title("leapfrog v/s euler: NITERS=%s dt=%s" % (NITERS, TIMESTEP))

ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_visible(False)
ax.spines['left'].set_visible(False)


plt.savefig("leapfrog-vs-euler.png")
plt.show()
