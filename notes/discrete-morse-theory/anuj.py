#!/usr/bin/env python3
from typing import *
from sympy import *
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
	for i in range(n):
		for j in range(n):
			yield Vec(n, i, j)

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

	__repr__ = __str__

def parityvecs():
	for parityvec in range(16):
		ps = [None for _ in range(4)]
		for ix in range(4):
			# write in endian where fastest changing value is rightmost.
			ps[4 - 1 - ix] = bool(parityvec & (1 << ix))
		yield Parityvec(ps)

class Torus:
	n : int

	def __init__(self, n : int) -> "Torus":
		self.n = n
		self.XNAMES = "ABCD"
		self.YNAMES = "PQRS"
		self.edge2parity2var = { v : { w : { p : None for p in [False, True] } for w in grid_points(n) } for v in grid_points(n) }
		self.v2dir2parity2term = { v : { d : { p : None for p in [False, True] } for d in directions(n) }  for v in grid_points(n) }
		self.equations = []

		for v in grid_points(n):
			for d in directions(v):
				w = add_vec_dir(v, d)
				if v > w: continue
				# --=={@@}==--
				eF = sympy.Symbol(f"{v.name()}-{w.name()}-F")
				eT = sympy.Symbol(f"{v.name()}-{w.name()}-T")
				self.edge2parity2var[v][w][False] = eF
				self.edge2parity2var[v][w][True] = eT
				self.edge2parity2var[w][v][False] = eF
				self.edge2parity2var[w][v][True] = eT
				print(f"### {v}:{v.name()} ---{d}----> {w}:{w.name()}")
				print(f"{v} ---[{False}]---> {w}: {eF}")
				print(f"{v} ---[{True}]---> {w}: {eT}")
				print(f"{w} ---[{False}]---> {v}: {eF}")
				print(f"{w} ---[{True}]---> {v}: {eT}")


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
	
	@property
	def variables(self):
		out = []
		for row in range(self.n):
			for col in range(self.n):
				for direction in range(4):
					for parity in range(2):
						out.append(self.vertex2var[row][col][direction][parity])
		return out
	def solve(self):
		return solve(self.equations, self.variables, domain={0, 1})

	def __str__(self):
		out = ""
		for v in grid_points(self.n):
			print(f"#Vertex {v} {v.name()}:")
			for parityvec in parityvecs():
				print(f"{parityvec}:{self.v2parity2eqn(v, parityvec)}")
		return out

t = Torus(3)
print(t)

# Now get equations involving only variables of the form x0
large_eqn = 0
neqns = 0 # this will be the number of grid points, so we need the grid size to be odd.
for v in grid_points(t.n):
	allzeros = Parityvec([False, False, False, False])
	large_eqn += t.v2parity2eqn(v, allzeros)
	neqns += 1

# Note that when the grid is (2k+1)x(2k+1), the system is insoluble, since
# in the usual graph G, we ask for each equal to be equal to 0,
# but in the perturbed system, we ask for each equation to be equal to 1,
# and we have an odd number of equations.
print(f"{large_eqn} = 0 | {neqns}") 

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