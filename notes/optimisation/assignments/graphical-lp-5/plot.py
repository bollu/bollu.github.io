#!/usr/bin/env python
from sympy import *
from sympy.matrices import *

GT = "GT"
LT = "LT"





def mk_standard_form_constraints(A, signs, b):
    """
    input: EQNS
    output: (A, b) such that Ax <= b
    """

    assert(NVARS > 0)

    assert(len(signs) == len(A))
    assert(len(b) == len(A))

    for i in range(len(A)):
        # flip the sign | ax >= b | -ax <= -b
        if signs[i] == GT:
            A[i] = [c * -1 for c in A[i]]
            b[i] *= -1

    A = Matrix(A)
    b = Matrix(b)

    return (A, b)


eqns = [[4, 2, GT, 10], [-3, 2, LT, 3], [1, 1, LT, 3], [1, 0, GT, 0], [1, 0, GT, 0]]
if __name__ == "__main__":
    NVARS = len(eqns[0]) - 2
    A = [c[0:-2] for c in eqns]
    signs = [c[-2] for c in eqns]
    b = [c[-1] for c in eqns]

    A, b = mk_standard_form_constraints(A, signs, b)
    print(A)
    print(b)

    vars = Matrix([Symbol("x" + str(i)) for i in range(A.shape[1])])
    print(vars)
    print(A * vars)
