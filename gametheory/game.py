import numpy as np

def minmax(A):
  return np.max(np.min(A, axis=1))


def maxmin(A):
  return np.min(np.max(A, axis=0))
