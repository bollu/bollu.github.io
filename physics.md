# Solid State

- [Lecture here](https://podcasts.ox.ac.uk/01-introduction-condensed-matter-einstein-model-vibrations-solids)

## Lecture 1: 

There is a law (`C/N = 3Kb`) for solids. This was known as 
[law of dulong-petit](https://en.wikipedia.org/wiki/Dulong%E2%80%93Petit_law).
called as

#### Boltzmann model

Boltzmann: used equipartition to show that `C / N = 3 Kb` in solids.
in solids, `Cp = Cv` so we just call it `C`. The model is that particles
are trapped in potential wells. Each particle has degrees of freedom is `px, py, pz, x, y, z`.
(`x y z` show up here unlike in gases because we need to pay energy to move them
around). Hence, total `C/N` is `1/2 * deg. of freedom` = `1/2 * 6 = 3`.

#### Einstein model
In the model, we again have a potential well with harmonic
oscillator frequency $\omega$. Then, we have to treat this potential
well using QM. The eigenstates are $E_n = \frac{h}{2 \pi} \omega(n + 1/2)$
recall that this was in 1D, while we live in 3D so we need to fix that a
little.

Our partition function is $Z = \sum_{n \geq 0} e^{-\beta E_n}$ where $\beta = \frac{1}{k_b T}$.

Average energy by differentiating partition function is 
$\langle E \rangle \equiv \frac{1}{Z} \frac{\partial Z}{\partial \beta} = \dots$

We can get the heat capacity as $\frac{\partial E}{\partial T} = \dots$.

