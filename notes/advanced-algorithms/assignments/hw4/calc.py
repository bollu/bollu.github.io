#!/usr/bin/env python3

def numchildren(k, i):
    """Return number of children at level i, for tree with num leaves n = 2^2^k"""
    if (0 <= i <= k - 1):
        return 2 ** (2 ** (k - i - 1))

    else:
        return 2
def numnodes(k, l):
    """Return total number of nodes at level l"""
    assert(0 <= l <= k - 1)
    return 2 ** ((2 ** (k - 1)) * ((2 ** l - 1)/(2 ** (l - 1))))
