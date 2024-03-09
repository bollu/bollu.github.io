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
            if self[lower] >= self[elem]:
                lower_violations.add(lower)
        for higher in self.simplex.get_elem_cofaces(elem):
            if self[higher] <= self[elem]:
                higher_violations.add(higher)

        return (lower_violations, higher_violations)
    
    def is_morse_at(self, elem):
        """A function is morse at a simplex if there is at most 1 lower violation
        and at most 1 higher violation.
        
        The intuition is that a morse point, there is one direction in which the function
        increases and one where it decreases, like a saddle.
        """ 
        (lower_violations, higher_violations) = self.get_violations(elem)
        return len(lower_violations) <= 1 and len(higher_violations) <= 1

    def is_critical_at(self, elem):
        """A function is critical at a simplex if there are no lower and higher violations.
        Thus, it behaves exactly like the dimension function at a critical point.

        See that this makes sense. If we think of the function dim : Tri -> Z,
        which maps points in a triangle to its dimension, the function only changes value abruptly
        at the crticial points, and is constant elsewhere.
        It is at these boundaries that the value of the dimension function changes
        (ie, the derivative is undefined, since the value "jumps").
        Formally, a point is critical if either f'(x) = 0 or f'(x) is undefined.
        """ 
        (lower_violations, higher_violations) = self.get_violations(elem)
        return len(lower_violations) == 0 and len(higher_violations) == 0

    def critical_set(self):
        """Set where the morse function is critical"""
        out = set()
        for elem in self.simplex:
            if self.is_critical_at(elem):
                out.add(elem)
        return out

    def pairings(self):
        """
        return a set of pairs of simplices (lower, higher), such that:
        1. lower in higher.boundary
        2. f[lower] >= f[higher]
        """
        out = set()
        
        simplices = self.simplex.edges
        simplices.update(self.simplex.faces)
        for higher in simplices:
            for lower in higher.boundary:
                if self[lower] >= self[higher]:
                    out.add((lower, higher))
        return out 

    def is_morse(self):
        for elem in self.simplex:
            if not self.is_morse_at(elem):
                return False
        return True

def test_complex_and_dimension():
    print("### Triangle and its dimension function is morse")
    abc = test_triangle()
    abc_complex = Complex2(faces=[abc])
    f = abc_complex.dimension_function()
    print(f)
    for elem in abc_complex:
        print(f"violations for {elem}:{f.get_violations(elem)}")
    print("is morse?: ", f.is_morse())
    print("critical set: ", f.critical_set())
    print("pairings for f: ", f.pairings())
test_complex_and_dimension()

def test_nontrivial_morse():
    """
        2
        a
       / \
      1    3
     /  4   \
    c---1----b
    0        2
    """
    print("### Nontrivial morse function: 'Discrete Morse Theory and its Applications' by Rachel Elana Zax")
    a = ChElem("a", Chain())
    b = ChElem("b", Chain())
    c = ChElem("c", Chain())
    ab = ChElem("ab", b - a)
    bc = ChElem("bc", c - b)
    ca = ChElem("ca", c - a)
    abc = ChElem("abc", ab + bc + ca)
    simplex = Complex2(faces=[abc])
    f = Function(simplex)
    f[a] = 2
    f[b] = 2
    f[c] = 0
    f[ab] = 3
    f[bc] = 1
    f[ca] = 1
    f[abc] = 4
    print(f)
    for elem in simplex:
        print(f"violations for {elem}:{f.get_violations(elem)}")
    print("morse?: ", f.is_morse())
    print("critical set: ", f.critical_set())
    print("pairings: ", f.pairings())
test_nontrivial_morse()

"""
### The Pairing function and its properties

Theorem: for any x in X (the simplicial complex),
let f be a morse function on X
the set { (a, b) in f.pairings() : a = x or b = x} has cardinality at most 1.
This means that in the full set of pairings, a given simplex x can occur *at most once*.

Proof:
-----

First, recall that (lower, higher) in pairing iff
1. lower in higher.boundary
2. f[lower] >= f[higher]

- Fix a higher face.
- Since f is morse, there can be at most one lower face such that (lower, higher) in pairing.
- Otherwise, the function f would fail to be morse, since the higher point would have more than one violation.
- so we know that there is at most one pair of the form (lower, higher).
- By the exact same argument, there can be at most one pair (higher, highest).
- We have that f[lower] >= f[higher] and f[higher] >= f[highest].
- Let lower be a k dimensional simplex of the form [v1 ... vk] (reoder vertices as necessary WLOG)
- Then higher is a (k+1) dimension simplex (since it contains lower) of the form [v1 .. vk vk+1],
  and highest is a (k+2) dimension simplex of the form [v1 ... vk vk+1 vk+2].
- consider the (k+1) dimension simplex higher' := [v1...vk vk+2].
- See that `lower in higher'` and `higher' in highest`.
- from the above, we know that (lower, higher') is not in the pairing, and neither is (higher', highest).
- This means that the value of the function behaves like the dimension function, and so we have the inequalities:
- `f[lower] < f[higher']` and `f[higher'] < f[highest]`.
- Combining the inequalities, we get the chain `f[higher] <= f[lower] < f[higher'] < f[highest] <= f[higher]`
- This is absurd, and thus such a thing cannot happen.

Visual proof:
- consider the lattice of subsets, since that's what a simplicial complex is.
- draw according to hasse diagram, but also add edges *according to f*. So when f is dimension, the edges are exactly the inlcusions
  in the hasse diagram.
- When f is morse, for a given set S, we claim that at most one edge related to it can be flipped.
- First of all, because it is morse, at most one incoming edge and at most one outgoing edge can be flipped.
- Now, we suppose that both an outgoing and an incoming edge are flipped. Picking a concrete example, I draw the hasse diagram below
  with the correct orientations based on dimension:

```
  abc
 //  \\ 
 v    v
ac    ab 
  \\// \\ 
   vv   v
   a    b
```

- Now let's suppose that I make a morse function by perturning ab, so that f[ab] >= f[ab] >= f[abc].
- So I change two edges, one with a -> ab and one with ab -> abc. Now the hasse diagram looks as follows:

```
  abc
 ^^
 //  \\ 
      v
ac    ab 
 ^^
  \\// \\ 
    v   v
   a    b
```

- But as one can see, this immediately gives a cycle with a -> ac -> abc -> ab -> a
- Moreover, the edges that "climb down" actually respect dimension, so we have that f[abc] > f[ab] > f[a], which makes
  the cycle wrong.
- Why do things not break when I have a single edge?
- Suppose I flip the edge a -> ac:

```
  abc
 
 //  \\ 
v     v
ac    ab 
 ^^
  \\// \\ 
    v   v
   a    b
```

- See that I have not created a cycle, since it is not possible to have inclusions "laterally" in the hasse diagram.
- We really need to climb two steps to be able to climb back down from another path.
- Said differently, there is a *unique* edge x <-> y of length 1, but there are multiple paths x <-> z of length 2
  which lets us find a cycle.
- Fascinating proof. But I have no idea why this condition captures morseness!
- If I had to guess, the idea is that we can have f[lower] >= f[higher]. Let's think about f[lower] = f[higher] for a minute.
- This is a critical point since the rate of change of f has stalled at this point. So it's a "critical point" in the usual sense.
- Since we are thinking about equality, we showed that we cannot have f[higher] = f[highest]. This means that the
  second derivative continues to exist, so it imposes some kind of convexity constraint!
- Since we now know that P does not have cycles, we can easily come up with a morse function by toposorting the DAG and setting
  the value of the function to be the visit time. 


### Simplicial Collapse


"""
