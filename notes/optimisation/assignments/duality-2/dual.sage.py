
# This file was *autogenerated* from the file dual.sage
from sage.all_cmdline import *   # import sage library

_sage_const_3 = Integer(3); _sage_const_2 = Integer(2); _sage_const_1 = Integer(1); _sage_const_0 = Integer(0); _sage_const_4 = Integer(4); _sage_const_80 = Integer(80); _sage_const_1p5 = RealNumber('1.5'); _sage_const_0p1 = RealNumber('0.1'); _sage_const_180p0 = RealNumber('180.0')
p = MixedIntegerLinearProgram(maximization=False)
v = p.new_variable(real=True, nonnegative=True)
y1, y2 = v["y1"], v["y2"]

#MIN z=-4y-y2
#subject to
#-2y1-y2 >= 1
#-3y1+y2 >= -1
#and y1,y2 >= 0;

p.set_objective(-_sage_const_4  * y1 - y2)
p.add_constraint(-_sage_const_2  * y1 >= _sage_const_1 )
p.add_constraint(-_sage_const_3  * y2 >= -_sage_const_1 )


# p.solve()
# Aopt, Bopt = tuple(p.get_values([A, B]))

poly = p.polyhedron()

plotter = poly.projection()

plot_verts = plotter.render_points_2d(color="black", size=_sage_const_80 , axes=False)
plot_vert_texts = Graphics()
for v in  poly.vertices():
    pos = (v[_sage_const_0 ] + _sage_const_0p1 , v[_sage_const_1 ] - _sage_const_0p1 )
    s = str(v.vector()) 
    color = "black"
    # if abs(v[0] - Aopt) + (v[1] - Bopt) < 0.1:
    #     s += " (OPTIMAL)"
    #     color = "red"
    plot_vert_texts += text(s,
                            pos, 
                            fontsize="small", 
                            fontweight="bold", 
                            color=color,
                             bounding_box={'boxstyle':'round', 'fc':'w'})

def slope_degrees(v1, v2):
    return  float(atan2(v2[_sage_const_1 ] - v1[_sage_const_1 ], v2[_sage_const_0 ] - v1[_sage_const_0 ]) * _sage_const_180p0  / pi)


plot_edge_texts = Graphics()
for edge in poly.faces(_sage_const_1 ):
    # vector from center to midpoint
    center = poly.center()
    (v1, v2) = edge.ambient_Vrepresentation()
    mid = (v1.vector() + v2.vector()) / _sage_const_2 

    center_to_mid = mid - center
    center_to_mid *= _sage_const_1p5 

    # unpack tuple, since we will have just one constraint.
    (ineq,) = edge.ambient_Hrepresentation()
    plot_edge_texts += text(ineq.repr_pretty(), 
                            center + center_to_mid, 
                            fontsize="small", 
                            fontweight="bold", 
                            color="green",
                             bounding_box={'boxstyle':'round', 'fc':'w'})


# print((Aopt, Bopt))
# plot_optimal = point([Aopt, Bopt], color="red", size=300)

plot_outline = plotter.render_outline_2d(color="blue", thickness=_sage_const_4 )

(Avar, Bvar) = var('A'), var('B')

# BB = poly.bounding_box()
# plot_objective_level_sets = density_plot(10 * Avar + 7.5 * Bvar, 
#                                    (Avar, BB[0][0], BB[1][0]), 
#                                    (Bvar, BB[0][1], BB[1][1]),
#                                    axes=False,
#                                    linestyles="dashdot", fill=False, linewidths=3,
#                                    labels=True, label_inline=True)
#                                    # colorbar=True, colorbar_orientation="horizontal",
# plot_objective_level_sets.axes(False)

# plot =  plot_objective_level_sets +  plot_outline + plot_verts  + plot_vert_texts + plot_edge_texts + plot_optimal 
plot = plot_outline + plot_verts  + plot_vert_texts + plot_edge_texts 
plot.axes(False)
plot.show(axes=False)
plot.save_image("dual.png")
plot.matplotlib()

