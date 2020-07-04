
## Gibbard- Satterthwaite theorem: 

- **unanimous** if it always selects an alternative that is top-ranked by everyone.
- **strategyproof** if it is a dominant strategy for an individual with any
  permissible ranking to truthfully report her preferences
Then the only unanimous, strategyproof social choice function is a dictatorship

## Revelation principle:

if a social choice function can be implemented by an arbitrary mechanism (i.e.
if that mechanism has an equilibrium outcome that corresponds to the outcome of
the social choice function), then the same function can be implemented by an
incentive-compatible-direct-mechanism.


N: set of agents
A: set of alternatives A = {a, b, c, ... m}
social choice mechanism: reports from agents -> (alternative a* in A, maybe payments)

- **Onto mechanism** --- for any alternative a, there exists a preference
profile for which A is selected.

- **DSIC**: no agent can gain by reporting false preferences.

- `u[i, a](z): Payment -> Utility`.
  utility of agent (i in N) if alternative (a in A) is selected and
  she needs to pay (z in R).


- `u[i, a]^{-1}(w): Utility -> Payment`
   when the  utility of alternative a is w, agent u needs to pay u[i, a]^{-1}(w)


- ui = [ui_a, ui_b, ..., ui_m] determines an agent's type, and is their
  private information.

- Denote u = (u1, u2, ...,un) as a type profile.
  u_-i = (u1, u2, ... u_{i-1}, u_{i+1}, ..., un)
  as type profile for agents except for agent i.


Denote utility of alternative a to agent i at 0 payment as v[i, a] = u[i, a](0).
This is called as a value of alternative a to agent i.

Let a-overbar[i] = arg max_{a in A} v[i, a]; a-underbar[i] = arg min_{a in A} v[i, a];
Most and least preferred alternatives at 0 payment.

A utility profile u is **quasi linear** u[i, a](z) = v[i, a] - z for all i, a, z.
In this case, the values v[i, a] completely determine the type profile.

Let the quasi linear domain U_QL be set of all quasi linear types of a 
single agent where v[i, a] can take any value in R; UU_QL = prod_i U_QL be
the set of all quasi linear type profiles.



We consider non quasi linear utilities such that:
- u[i, a](z) is continuously and strictly decreasing. [Agents prefer to make smaller payments]
- lim_{z \to +\infty} u[i, a](z) < min_{a' in A} v[i, a'] 
     [Agent prefers *worst alternative* at 0 payment v/s any other alternative at very large payment.]

Denote a domain that obeys these two conditions as U0

A social choice mechanism (x, t) on type domain U subset U0 is comprised
of a choice rule x: U -> A and a payment rule t = (t[1], t[2], ... t[n]): U -> R^n.

If the reported type profile is uhat in U, the choice made is x(uhat),
and the utility of agent i is u[i, x(uhat)](t[i](uhat))

A mechanism is **DSIC** iff for any agent i in N, any type u[i] in U[i] of agent i,
any reported profile from other agents uhat[-i], agent i cannot gain by
mis-reporting u[i] as uhat[i]:


u[i, x((u[i], uhat[-i]))](t[i](u[i], uhat[-i])) >= 
u[i, x((uhat[i], uhat[-i]))](t[i](uhat[i], uhat[-i]))


A mechanism is **individually rational (IR)** if and only if, by
truthfully participating in the mechanism,
regardless of the reports made by the other agents,
no agent can be worse off than having their worst alternative at
zero payment selected and not making any payment.


u[i, x(u[i], uhat[-i])](t[i](u[i], uhat[-i)) >= min{a in A} v[i, a] = u[i, a, uhat[-i])](0)


We want:
1. DSIC
2. Deterministic
3. Onto
4. IR
5. No subsidy [mechanism makes no positive transfers to agent].

We say that a mechanism is **agent-independent** if an agent’s payment is independent of her report,
conditioned on a particular alternative being selected --- 

fixing the type profile of the rest of the agents u[-i], for all u[i], u'[i],
  x(u[i], u[-i]) = x(u'[i], u[-i]) => t[i](u[i], u[-i]) = t[i](u'[i], u[-i]).

Given an agent independent mechanism and any u[[−i] ∈ U[−i],
if there exists u[i] ∈ U[i] such that  x(u[i], u[−i]) = a, let the
agent independent prince be the payment i pays when a is selected: 

t[i, a](u[-i]) =defn= t[i](u[i], u[-i])

which depends only on u[-i].

Agent independece and Agent maximising are necessary and sufficient for DSIC.


# Parallel Domains

Given any type of an agent u[i] in U0, for each alternative a, we define the willingness to pay
p[i, a] as the payment for a at which the agent is indifferent between getting
a at p[i, a], v/s getting the least preferred alternative a-underbar[i] at
zero payment:

```
p[i, a] = u[i, a]^{-1}(v[i, a-underbar[i]])
```

p[i, a] is the max.agent a can be charged if alternative a was selected, without
violating IR.

p[i, a-underbar[i]] = 0

always holds.

**Parallel Domain** A domain is a parallel domain if for all u[i] in U[i],

u[i, a](z + (p[i, a] - p[i, b])) = u[i, b](z) | FA a, b in A st v[i, a] >= v[i, b]

We call u[i] as a parallel type if the above is satisfied.


For any utility level w, we have:

u[i, a]^{-1}[w] - u[i, b]^{-1}[w] = p[i, a] - p[i, b] = u[i, a]^{-1}(v[i, b])

In other words, the differences in payments on a and b to achieve w is a constant
that does not depend on w.

Intuitively, the trade-off with money depends on how happy the agent is with the alternative, not how
much she is paying. As her utility w changed, the amount of pay-difference
she has is constant.

## Robert's theorem
P1. Dominant-strategy incentive compatible
P2. Deterministic
P3. Onto
P4. Individually rational
P5. No subsidy

With 3 or more alternatives and unrestricted parallel domain U, for every
social choice mechanism that satisfies P1-P5, there exists non-negative
weights k1...kn (not all 0) and real constnts c1, ... cm such that
for all u in U,

x(u) \in arg max_{a \in A} \sum_{i=1}^n k_i p_{i, a} + C_a


# Dictatorship on expanding the domain

With at least 3 alternatives, and a utility domain $U = prod U_i$ such that:
- $U_{||} \subset U_i$ for all $i$
- $U_{||} \subsetneq U_i$ f
