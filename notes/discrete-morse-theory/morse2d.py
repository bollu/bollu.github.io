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


def test_triangle():
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
    return abc
test_triangle()

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

    def function(self) -> "Function":
        return Function(self)

    def get_elem_faces(self, elem):
        assert elem in self
        return elem.boundary

    def get_elem_cofaces(self, elem):
        assert elem in self
        if elem in self.faces:
            return set()
        if elem in self.edges:
            return set([face for face in self.faces if elem in face.boundary])
        if elem in self.vertices:
            return set([edge for edge in self.edges if elem in edge.boundary])
        raise RuntimeError("unreachable!")

    def dimension_function(self) -> "Function":
        f = self.function()
        for face in self.faces:
            f[face] = 3
        for edge in self.edges:
            f[edge] = 2
        for vertex in self.vertices:
            f[vertex] = 1

        return f
    def __str__(self):
        out = ""
        out += f"faces: {str(self.faces)}\n"
        out += f"edges: {set(self.edges)}\n"
        out += f"vertices: {str(self.vertices)}"
        return out
    def contains(self, item):
        return item in self.faces or \
            item in self.edges or \
            item in self.vertices

    def __iter__(self):
        for vertex in self.vertices:
            yield vertex
        for edge in self.edges:
            yield edge 
        for face in self.faces:
            yield face



class Function:
    simplex : Complex2
    val : Dict[ChElem, int] # map each chain element to a value.
    
    def __str__(self):
        out = []
        for elem in self.simplex:
            out.append(f"{elem}:{self[elem]}")
        return " ".join(out)

    def __init__(self, simplex):
        self.simplex = simplex
        self.val = dict()
        assert isinstance(simplex, Complex2)


    def set(self, elem, val):
        assert elem in self.simplex
        self.val[elem] = val
    
    def __getitem__(self, elem):
        assert elem in self.simplex
        if elem in self.val:
            return self.val[elem]
        else:
            return 0

    def __setitem__(self, elem, val):
        self.set(elem, val)

    def get_violations(self, elem):
        """
        returns lower dimensional faces where f(face) > f(elem),
        and higher dimensional cofaces where f(coface) < f(elem)
        """
        lower_violations = set()
        higher_violations = set()
        for lower in self.simplex.get_elem_faces(elem):
            if self[lower] > self[elem]:
                lower_violations.add(lower)
        for higher in self.simplex.get_elem_cofaces(elem):
            if self[higher] < self[elem]:
                higher_violations.add(higher)

        return (lower_violations, higher_violations)

    def is_critical_at(self, elem):
        """A function is critical at a simplex if there is at most 1 lower violation
        and at most 1 higher violation.
        
        The intuition is that a critical point, there is one direction in which the function
        increases and one where it decreases, like a saddle.
        """ 
        (lower_violations, higher_violations) = self.get_violations(elem)
        return len(lower_violations) <= 1 and len(higher_violations) <= 1

    def critical_set(self):
        """Set where the morse function is critical"""
        out = set()
        for elem in self.simplex:
            if is_critical_at(elem):
                out.add(elem)
        return out
    def is_morse(self):
        for elem in self.simplex:
            if not self.is_critical_at(elem):
                return False
        return True

def test_complex_and_dimension():
    abc = test_triangle()
    abc_complex = Complex2(faces=[abc])
    f = abc_complex.dimension_function()
    print(f)
    for elem in abc_complex:
        print(f"violations for {elem}:{f.get_violations(elem)}")
    print("dimension function is morse?: ", f.is_morse())
test_complex_and_dimension()
