#!/usr/bin/env python

# use the flow() function, query it for the LP. Or hack sage so you can
# query it for the LP. Not too hard.
# http://doc.sagemath.org/html/en/reference/graphs/sage/graphs/generic_graph.html#sage.graphs.generic_graph.GenericGraph.flow
import json
import pprint
p = MixedIntegerLinearProgram(maximization=True)
VAR = p.new_variable(real=True)

# vertices
vs = ["a", "b", "c", "d", "e", "f", "g", "h", "j", "k"]

# capacities
cs = { v: {w: None for w in vs} for v in vs} 
# horizontal line 1
cs["a"]["b"] = 1
cs["b"]["c"] = 4
cs["c"]["g"] = 9

# horizontal line 2
cs["d"]["e"] = 1
cs["e"]["f"] = 1
cs["f"]["g"] = 8

# horizontal line 3
cs["h"]["j"] = 7
cs["j"]["k"] = 7
cs["k"]["g"] = 10

# vertical line 1
cs["a"]["d"] = 6
cs["d"]["h"] = 6

# vertical line 2
cs["b"]["e"] = 3
cs["e"]["j"] = 8


# vertical line 2
cs["c"]["f"] = 5
cs["f"]["k"] = 4

# diagonal line 1

SOURCE = "d"
SINK = "g"

PP = pprint.PrettyPrinter(indent=2)

for i in range(len(vs)):
    for j in range(i, len(vs)):
        v = vs[i]
        w = vs[j]

        if w in cs[v]:
            # add the reverse edge (w, v)
            # if (v, w) in E
            if v != SOURCE and w != SINK:
                cs[w][v] = cs[v][w]



# print("CAPACITIES:")
# PP.pprint(cs)

flows = { v: {w: None for w in vs} for v in vs}
for v in vs:
    for w in vs:
        if v == w: continue
        if cs[v][w] is None: continue

        flows[v][w] = VAR[v +"_" + w]
        print("%s -> %s: %s" % (v, w, flows[v][w]))
        # add flow constraint: flow < max flow
        p.add_constraint(flows[v][w] <= cs[v][w])

# print(json.dumps(flows))
# print("FLOWS:")
# PP.pprint(flows)


def get_flow_out_of_source(v):
    f = None
    for w in vs:
        fcur = flows[v][w]
        if fcur is None: continue
        if f is None: 
            f = fcur
        else:
            f += fcur
    return f


def get_flow_into_source(w):
    f = None
    for v in vs:
        fcur = flows[v][w]
        if fcur is None: continue
        if f is None:
            f = fcur
        else:
            f += fcur
    return f


# add continuity constraint: flow inward = flow outward
for v in vs:
    print(v)
    fin = get_flow_into_source(v)
    fout = get_flow_out_of_source(v)
    if fin is not None and fout is not None:
        c = fin == fout
        print(c)
        p.add_constraint(c)

def get_flow_vars():
    return [(v, w, flows[v][w]) for v in vs for w in vs if flows[v][w] is not None]

    # fvars = []
    # for v in vs:
    #     for w in vs:
    #         f = flows[v][w]
    #         if f is not None: fvars.append(f)
    # return fvars


# maximise flow out of source
p.set_objective(get_flow_out_of_source(SOURCE))
maxflow = p.solve()
opt = p.get_values(map(lambda v: v[2], get_flow_vars()))

print("VALS:")
print(PP.pprint(zip(get_flow_vars(), opt)))

print("problem:")
print(p.show())

print("maxflow:")
print(maxflow)
print("optimal values:")
print(opt)
