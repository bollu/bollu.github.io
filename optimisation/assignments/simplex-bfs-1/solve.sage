#!/usr/bin/env python

# use the flow() function, query it for the LP. Or hack sage so you can
# query it for the LP. Not too hard.
# http://doc.sagemath.org/html/en/reference/graphs/sage/graphs/generic_graph.html#sage.graphs.generic_graph.GenericGraph.flow
import json
import pprint
p = MixedIntegerLinearProgram(maximization=True)
VAR = p.new_variable(real=True)

# vertices
x1 = p["x1"]
x2 = p["x2"]


# maximise flow out of source
p.set_objective(-1 * x1 + -1 * x2)
p.add_constraint(x1 + 10 * x2 <= 5)
p.add_constraint(4 * x1 + x2 <= 4)
p.add_constraint(x1 >= 0)
p.add_constraint(x2 >= 0)
print(p.solve())
opt1, opt2 = tuple(p.get_values([x1, x2]))
print(opt1, opt2)
