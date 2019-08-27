p = MixedIntegerLinearProgram(maximization=False)
v = p.new_variable(real=True, nonnegative=True)
A, B = v["A"], v["B"]
p.set_objective(10 *A + 7.5 *B)
p.add_constraint(4 * A + 2 * B >= 10)
p.add_constraint(-3 * A + 2 * B <= 3)
p.add_constraint(A + B <= 3)


p.solve()
Aopt, Bopt = tuple(p.get_values([A, B]))

poly = p.polyhedron()

plotter = poly.projection()

plot_verts = plotter.render_points_2d(color="black", size=80, axes=False)
plot_vert_texts = Graphics()
for v in  poly.vertices():
    pos = (v[0] + 0.1, v[1] + 0.1)
    s = str(v.vector()) 
    color = "black"
    if abs(v[0] - Aopt) + (v[1] - Bopt) < 0.1:
        s += " (OPTIMAL)"
        color = "red"
    plot_vert_texts += text(s,
                            pos, 
                            fontsize="small", 
                            fontweight="bold", 
                            color=color,
                             bounding_box={'boxstyle':'round', 'fc':'w'})

def slope_degrees(v1, v2):
    return  float(atan2(v2[1] - v1[1], v2[0] - v1[0]) * 180.0 / pi)



plot_edge_texts = Graphics()
for edge in poly.faces(1):
    # vector from center to midpoint
    center = poly.center()
    (v1, v2) = edge.ambient_Vrepresentation()
    mid = (v1.vector() + v2.vector()) / 2

    center_to_mid = mid - center
    center_to_mid *= 1.3

    # unpack tuple, since we will have just one constraint.
    (ineq,) = edge.ambient_Hrepresentation()
    plot_edge_texts += text(ineq.repr_pretty(), 
                            center + center_to_mid, 
                            fontsize="small", 
                            fontweight="bold", 
                            color="green",
                             bounding_box={'boxstyle':'round', 'fc':'w'})


print((Aopt, Bopt))
plot_optimal = point([Aopt, Bopt], color="red", size=300)

plot_outline = plotter.render_outline_2d(color="blue", thickness=4)

(Avar, Bvar) = var('A'), var('B')
BB = poly.bounding_box()
plot_objective_level_sets = density_plot(10 * Avar + 7.5 * Bvar, 
                                   (Avar, BB[0][0], BB[1][0]), 
                                   (Bvar, BB[0][1], BB[1][1]),
                                   axes=False,
                                   linestyles="dashdot", fill=False, linewidths=3,
                                   labels=True, label_inline=True)
                                   # colorbar=True, colorbar_orientation="horizontal",
plot_objective_level_sets.axes(False)

plot =  plot_objective_level_sets +  plot_outline + plot_verts  + plot_vert_texts + plot_edge_texts + plot_optimal 
plot.axes(False)
plot.show(axes=False)
plot.save_image("image.png")
plot.matplotlib()

