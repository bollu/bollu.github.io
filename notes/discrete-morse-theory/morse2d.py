#!/usr/bin/env python3
from typing import *

class ChElem:
    name : str
    _boundary : "Chain"
    
    def __init__(self, name, boundary):
        self.name = name
        self._boundary = boundary
        assert isinstance(name, str)
        assert isinstance(self._boundary, Chain)

    @property
    def boundary(self):
        return self._boundary
    def __add__(self, other):
        return tochain(self) + tochain(other)

    def __mul__(self, other):
        return tochain(self) * other

    def __sub__(self, other):
        return tochain(self) - tochain(other)

    def __str__(self):
        return self.name
    __repr__ = __str__

def tochain(x):
    if isinstance(x, Chain): 
        return x
    elif isinstance(x, ChElem):
        return Chain(x)

class Chain:
    val2coeff: Dict[ChElem, int]

    def __init__(self, val=None):
        self.val2coeff = {}
        if not val:
            return
        self.append(val)

    def __str__(self) -> str:
        outs = ""
        for val, coeff in self.val2coeff.items():
            if coeff == 1:
                if not outs:
                    outs += f"{val}"
                else:
                    outs += f"+{val}"
            elif coeff == -1:
                outs += f"-{val}"
            elif coeff == 0:
                continue
            else:
                if coeff > 0:
                    outs += f"+{coeff}{val}"
                else:
                    outs += f"{coeff}{val}"
        if not outs:
            return "[]"
        return f"[{outs}]"

    __repr__ = __str__

    def __mul__(self, other: int) -> "Chain":
        out = Chain()
        for val, coeff in self.val2coeff.items():
            out.append(val, other * coeff)
        return out

    def __mod__(self, other: int) -> "Chain":
        out = Chain()
        for val, coeff in self.val2coeff.items():
            out.append(val, coeff % other)
        return out

    def __add__(self, other: "Chain") -> "Chain":
        other = tochain(other)
        out = Chain()
        for val, coeff in self.val2coeff.items():
            out.append(val, coeff)

        for val, coeff in other.val2coeff.items():
            out.append(val, coeff)
        return out

    def __iadd__(self, other: "Chain"):
        other = tochain(other)
        for val, coeff in other.val2coeff.items():
            self.append(val, coeff)
        return self

    def __isub__(self, other: "Chain"):
        other = tochain(other)
        for val, coeff in other.val2coeff.items():
            self.append(val, -coeff)
        return self

    def __sub__(self, other: "Chain") -> "Chain":
        other = tochain(other)
        out = Chain()
        for val, coeff in self.val2coeff.items():
            out.append(val, coeff)
        for val, coeff in other.val2coeff.items():
            out.append(val, -coeff)
        return out

    def append(self, val, coeff: int = 1):
        assert isinstance(val, ChElem)
        if val not in self.val2coeff:
            self.val2coeff[val] = coeff
        else:
            self.val2coeff[val] += coeff

    @property
    def boundary(self):
        out = Chain()
        for val, coeff in self.val2coeff.items():
            if isinstance(val, Chain):
                out += val
            else:
                out = out
        return out

    def __iter__(self):
        return self.val2coeff.__iter__()

    def items(self):
        return self.val2coeff.items()



a = ChElem("a", Chain())
b = ChElem("b", Chain())
c = ChElem("c", Chain())

ab = ChElem("ab", b - a)
bc = ChElem("bc", c - b)
ca = ChElem("ca", a - c)

abc = ChElem("abc", ab + bc + ca)
print("a,b,c:", a, b, c)
print("a+b+c:", a + b + c)
# print((ab + bc + ca) % 2)

# print(ab.boundary)  # boundary of vertex is 0
print("ab:", ab)
print("ab.boundary: ", ab.boundary)

print("abc:", abc)
print("abc.boundary: ", abc.boundary)
print("abc.boundary.boundary: ", abc.boundary.boundary)

class Complex2:
    vertices: Set[Chain]
    edges: Set[Chain]
    faces: Set[Chain]

    def __init__(
        self,
        faces: Set[Chain] = set(),
        edges: Set[Chain] = set(),
        vertices: Set[Chain] = set(),
    ):
        self.faces = set(faces)
        self.edges = set(edges)
        self.vertices = set(vertices)

        for face in self.faces:
            for edge in face.boundary:
                self.edges.add(edge)

        for edge in self.edges:
            for vertex in edge.boundary:
                self.vertices.add(vertex)

        for face in self.faces:
            assert isinstance(face, ChElem)
        for edge in self.edges:
            assert isinstance(edge, ChElem)
        for vertex in self.vertices:
            assert isinstance(vertex, ChElem)

        
    def __str__(self):
        out = ""
        out += f"faces: {str(self.faces)}\n"
        out += f"edges: {set(self.edges)}\n"
        out += f"vertices: {str(self.vertices)}"
        return out

print(Complex2(faces=[abc]))

