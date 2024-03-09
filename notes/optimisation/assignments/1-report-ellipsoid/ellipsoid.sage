
def ellipsoid(poly, minvolume):

    # define 2D ellipse with radii (500, 500)
    # The initial radii is picked based on a notion of descriptive complexity
    # of the polyhedra.
    (ax, ay) = (500, 500)

    i = 0
    while True:
        # pick a random point on the unit disk (0, 0). This distribution is not
        # truly uniform, but it works in a pinch
        randr = random()
        randtheta = randrange(-100, 100) / 100.0 * 2.0 * pi

        # transform point to point in ellipse
        randx = randr * cos(randtheta) * ax
        randy = randr * sin(randtheta) * ay
        randp = (randx, randy)

        print("* %d " % (i, ))
        i += 1

        print("trying point: (%4.2f, %4.2f)" % (randx, randy))

        # now we have a random point in ellipse, check if the point is
        # in the polytope. Return the point if it does
        if poly.contains(randp):
            print("polyhedra contains point: (%4.2f, %4.2f)" % (randx, randy))
            return randp
        else:
            print("polyhedra does not contain point. Shrinking.")
            print("  old radii: (%4.2f, %4.2f)" % (ax, ay))
            # we do not have the point in the ellipse. Shorten ellipse
            # based on this info.
            ax = min(ax, abs(randx))
            ay = min(ay, abs(randy))
            print("  new radii: (%4.2f, %4.2f)" % (ax, ay))

            # compute overapproximation of volume (bounding box)
            # if this is less than the minimum volume, return. Otherwise,
            # continue
            if ax * ay < minvolume:
                print("V bounding box(%4.2f) < min V (%4.2f)" % 
                    (ax * ay, minvolume))
                return None


# Run ellipsoid  on feasible polytope
print("Running ellipsoid on feasible polytope")
p = polytopes.regular_polygon(5) * 10
ellipsoid(p, 0.1)


# Run ellipsoid on infeasible (empty) polytope
print(" ")
print("Running ellipsoid on empty polytope")
p = Polyhedron()
ellipsoid(p, 0.1)

