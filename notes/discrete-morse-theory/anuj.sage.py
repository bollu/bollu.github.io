#!/usr/bin/env python3
from typing import *
import sympy

class Vec:
	def __init__(self, n, x, y):
		self.n = n
		self.x = x 
		self.y = y

	def __gt__(self, other):
		return self.x > other.x or (self.x == other.x and self.y > other.y)

	def name(self) -> str:
		XNAMES = "ABCD"
		YNAMES = "PQRS"
		return XNAMES[self.x] + YNAMES[self.y]

	def __str__(self) -> str:
		return f"({self.x}, {self.y})"
	__repr__ = __str__

	def __eq__(self, other) -> bool:
		return self.x == self.x and self.y == other.y

	def __hash__(self) -> int:
		return hash((self.x, self.y))

def grid_points(n : int):
	return [Vec(n, i, j) for i in range(n) for j in range(n)]


def directions(n) -> List[Vec]:
	return [Vec(n, 1, 0), Vec(n, 0, 1), Vec(n, -1, 0), Vec(n, 0, -1)]

def add_vec_dir(v : Vec, d : Vec) -> Vec:
	return Vec(v.n, \
		(v.n + d.x + v.x) % v.n, \
		(v.n + d.y + v.y) % v.n)

def adj_points(v : Vec):
	out = []
	for d in directions(v.n):
		out.append(add_vec_dir(v, d))
	return out


def xor(x : bool, y : bool):
	return (x and (not y)) or ((not x) and y)
class Parityvec:
	vec : List[bool]
	def __init__(self, vec):
		assert isinstance(vec, List)
		assert len(vec) == 4
		vec = list(map(bool, vec))
		self.vec = vec

	def __hash__(self):
		return hash((self.vec[0], self.vec[1], self.vec[2], self.vec[3]))
	def __eq__(self, other):
		return self.vec == other.vec
	def __str__(self):
		return "<" + "".join(["1" if p else "0" for p in self.vec]) + ">"
	def __getitem__(self, i):
		assert i >= 0
		return self.vec.__getitem__(i)

	def parity(self):
		out = False
		for b in self.vec:
			out = xor(out, b)
		return out

	__repr__ = __str__

def parityvecs():
	for parityvec in range(16):
		ps = [None for _ in range(4)]
		for ix in range(4):
			# write in endian where fastest changing value is rightmost.
			ps[4 - 1 - ix] = bool(parityvec & (1 << ix))
		yield Parityvec(ps)


def system_to_matrix_vector_gf2(equations, variables):	
	M = Matrix(GF(2), len(equations), len(variables), lambda i, j: equations[i].lhs().coefficient(variables[j]))
	v = Matrix(GF(2), [eqn.rhs() for eqn in equations]).T
	return (M, v)

def solve_system(M, v):
	w = M \ v
	return (w, M * w == v)

def make_variable2solution_dict(vs, sol):
	assert len(sol.dimensions()) == 2
	assert len(vs) == sol.dimensions()[0]
	assert sol.dimensions()[1] == 1
	return { vs[i] : sol[i][0] for i in range(len(vs))}


class Torus:
	n : int

	def __init__(self, n : int) -> "Torus":
		self.n = n
		self.XNAMES = "ABCD"
		self.YNAMES = "PQRS"
		self.edge2parity2var = { v : { w : { p : None for p in [False, True] } for w in grid_points(n) } for v in grid_points(n) }
		self.v2dir2parity2term = { v : { d : { p : None for p in [False, True] } for d in directions(n) }  for v in grid_points(n) }
		self.equations = []
		self.variables = []

		for v in grid_points(n):
			right = Vec(t.n, 1, 0)
			up = Vec(t.n, 0, 1)
			dirs = [right, up]
			for d in dirs:
				w = add_vec_dir(v, d)
				eF = var(f"{v.name()}_{w.name()}_F")
				eT = var(f"{v.name()}_{w.name()}_T")
				self.variables.append(eF)
				self.variables.append(eT)
				self.edge2parity2var[v][w][False] = eF
				self.edge2parity2var[v][w][True] = eT

				self.edge2parity2var[w][v][False] = eF
				self.edge2parity2var[w][v][True] = eT
				print(f"### {v}:{v.name()} ---{d}----> {w}:{w.name()}")
				print(f"{v} ---[{False}]---> {w}: {eF}")
				print(f"{v} ---[{True}]---> {w}: {eT}")
				print(f"{w} ---[{False}]---> {v}: {eF}")
				print(f"{w} ---[{True}]---> {v}: {eT}")

		self.variables = list(set(self.variables))
		print(self.variables)

		# ^      ^ 
		# |      | 
		# x -e-> x -e-> 
		# ^      ^
		# |      |
		# x -e-> x -e->

		# ^    ^    ^ 
		# |    |    |
		# x -> x -> x ->
		# ^    ^    ^
		# |    |    |
		# x -> x -> x ->
		# ^    ^    ^
		# |    |    | 
		# x -> x -> x ->
		if self.n == 1:
			# the vertex has two edges, one that goes right and one that goes up.
			assert(len(self.variables) == 2)
		else:			
			# each vertex adds 2 edges, and each edge has parity 2
			assert(len(self.variables) == 2 * 2 * self.n * self.n)
		self.vertex2var = \
			[[[self.edge2parity2var[v][add_vec_dir(v, d)][p] for p in [0, 1]] \
				for d in directions(n)] \
				for v in grid_points(n)]
		
		for v in grid_points(n):			
			for parityvec in parityvecs():
				print(parityvec, directions(n))
				for (p, d) in zip(parityvec, directions(n)):
					w = add_vec_dir(v, d)
					term = self.edge2parity2var[v][w][p]
					self.v2dir2parity2term[v][d][p] = term
					print(f"{v} ---[{d}:{p}]---> {w}: {term}")


		for v in grid_points(n):
			for parityvec in parityvecs():
				self.equations.append(self.v2parity2eqn(v, parityvec))
	def v2parity2eqn(self, v, parityvec):
		e = 0
		for (p, d) in zip(parityvec, directions(self.n)):
			e += self.v2dir2parity2term[v][d][p]
		return e
	
	def original_system(self):
		equations = []
		for v in grid_points(t.n):
			for parityvec in parityvecs():
				equations.append(self.v2parity2eqn(v, parityvec) == parityvec.parity())
		return equations

	def perturbed_system(self, perturbv : Vec):
		equations = []
		for v in grid_points(t.n):
			for parityvec in parityvecs():
				rhs = parityvec.parity()
				if v == perturbv:
					rhs = xor(rhs, True)
				equations.append(self.v2parity2eqn(v, parityvec) == rhs)
		return equations

	def __str__(self):
		out = ""
		for v in grid_points(self.n):
			print(f"#Vertex {v} {v.name()}:")
			for parityvec in parityvecs():
				print(f"{parityvec}:{self.v2parity2eqn(v, parityvec)}")
		return out

t = Torus(1)
print(t)


print("### Creating equations by adding all equations with edge variables of parity F ")
# Now get equations involving only variables of the form x0
sum_eqn = 0
neqns = 0 # this will be the number of grid points, so we need the grid size to be odd.
for v in grid_points(t.n):
	allzeros = Parityvec([False, False, False, False])
	sum_eqn += t.v2parity2eqn(v, allzeros)
	neqns += 1

# Note that when the grid is (2k+1)x(2k+1), the system is insoluble, since
# in the usual graph G, we ask for each equal to be equal to 0,
# but in the perturbed system, we ask for each equation to be equal to 1,
# and we have an odd number of equations.
print(f"{sum_eqn} = 0 | {neqns}")

print("### Original system and its solution: ")
original_system = t.original_system()
print("\n".join(map(str, original_system)))
(M, v) = system_to_matrix_vector_gf2(original_system, t.variables)
print(M.str())
print("solving system...")
(w, is_exact) = solve_system(M, v)
print(f"solution to original_system (exact = {is_exact}):")
print(w.T)
print(make_variable2solution_dict(t.variables, w))

print("### Perturbed system and its solution: ")
perturbed_system = t.perturbed_system(grid_points(t.n)[0])
print("\n".join(map(str, perturbed_system)))
(M2, v2) = system_to_matrix_vector_gf2(perturbed_system, t.variables)
print(M2.str())
try:
	print("solving pertuebed system...")
	(w2, is_exact2) = solve_system(M2, v2)
	print(f"solution to perturbed system (exact = {is_exact2}):")
	print(make_variable2solution_dict(t.variables, w2))
except ValueError as err:
	print(f"ERROR: unable to solve perturbed system: {err}")

"""
For the 2x2 torus, in the perturbed system, we see the eqn:
### Perturbed system and its solution:
2*AP_AQ_F + 2*AP_BP_F == 1

This is clearly insoluble mod 2.

What happens for larger system?

"""
# print(t.equations_with_variables(t.variables[0]))
# print(t.solve())
# # Define the variables
# x, y, z = symbols('x y z')

# # Define the equations
# eq1 = x + y + z
# eq2 = x * y + y * z
# eq3 = x + z

# # Solve the equations over Z/2Z
# solutions = solve((eq1, eq2, eq3), (x, y, z), domain={0, 1})
# print("Solutions over Z/2Z:", solutions)