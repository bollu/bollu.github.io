Order
- Rasterisation
- Rastersiation 2
- Visibility
- Lighting
- Ray tracing

# Rasterisation 1

- Clipping: find part of line inside window. First clip then
  rasterize for performance.

- Triangles are scan-converted / rasterised.

## Scan conversion / rasterisation

- first rendered into the frame buffer. frame buffer is a pixellated version of
the original mathematical description.

- Point has been converted to `(u, v)`. Round to `(i, j)`, fill in frame buffer at `(i, j)`

## Scan conversion of line
### Incremental algorithm

```cpp
void drawLine(vec2 pstart, vec2 pend, Color color) {
    vec2 delta = pend - pstart;
    float slope = delta.y / delta.x;

    vec2 pcur = pstart;
    while (pcur.x < pend.x) {
        FBO[pcur.x, round(pcur.y)] = color;
        // increment point
        pcur.x += 1;
        pcur.y += slope;
    }
    FBO[pend.x, pend.y] = color;
}
```

### Incremental algorithm, keeping slope integral

```cpp
void drawLine(vec2 pstart, vec2 pend, Color color) {
    vec2 delta = pend - pstart;

    // NOTICE: this is an integer
    int cur_dy = 0

    vec2 pcur = pstart;

    while(pcur.x < pend.x) {
        FBO[pcur.x, pcur.y] = color;
        pcur.x += 1;

        cur_dy += delta.y;
        if (cur_dy >= delta.x) {
            cur_dy -= delta.x;
            pcur.y++;
        }
    }

}
```

Note, what to do when `dy > dx`? apparently, we step through `y`, while
using `x` as the slope.

### Mid point line algorithm / Bresenham's algorithm
Let the equation of the line be `ax + by + c = 0`. Let `b` be even without
loss of generality. If `b` is odd, multiple the entire equation by `2`.

Let `L = ax + by + c`

Assume that the slope is `0 < -a/b < 1`. Note that if a point `p` is above the line `L`, the `L(p) < 0`, and vice versa.

Let the start point be `pstart`, end point be `pend`.

##### Initialize
compute `L(pstart)`. Draw `L(pstart)`.

##### Step
Assume we have drawn the current point, `pcur`. Assume we also have `L(pcur)`.

To draw the next point, we need to pick between `right = pcur + (1, 0)` and `topright = pcur + (1, 1)`.

To make the decision, consider the midpoint of `right` and `topright`: `M = (x + 1, y + 1/2)`. if `M` is above the line, then pick `topright`. Otherwise, pick `right`.

To determine whether `M` is above or below the line, we need to evaluate `L(M)`.
Notice that `L(M) = L(x + 1, y + 1/2) = L(pcur) + a + b/2`. Under our assumptions, we know `L(x, y)` and `b` is even, hence computation of `L(M)` is fully integral.

Now, if `L(M) > 0` then the midpoint is *below* the line. That is, the line is *above* the midpoint. So, we pick `topright.`

If `L(M) < 0`, then the midpoint is *above* the line. That is, the line is *below* the midpoint. So, we pick `right`.


Now, we need to know `L(nextpoint)`.
Notice that `L(topright)` = `L(pcur + (1, 1))` = `L(pcur) + a + b`.
Similarly, `L(right) = L(pcur + (1, 0)) = L(pcur) + a`. All of these computations are integral.

##### Alternate way of thinking about computing `L(M)`

Note that we only need a float division for `L(M) = L(pcur + (1, 1/2)) = L(pcur) + a + b/2`. However, notice that we only care about the *sign* of `L(M)`. Hence, we can compute `2L(M)` with no problem at all!. So, we can choose to use `2L(M) = 2a + b`, which gets rid of the pesky division by 2 without having to mess with the line equation. This will change our increments of `L(M)` into `2a` and `2(a + b)`.


So, in general, rather than storing `L(p)`, we will store `2L(p)`, since we only care aobut the sign of `L(p`). Since `L(p)` is linear, `2L(p) + 2L(q) = 2L(p + q)`, which works out.

##### pseudocode (write this up later)



# Rasterisation 2
## drawing patterened lines
use a bitmask to determine if we wish to draw a line or not

## Shared points and edges in a polygon
We may have common points in edges in a polygon. We will wind up drawing them
twice. So, we treat all lines as half-open: `[begin, end)`,

## Clipping
We need to perform clipping to reduce the amount of time we spend drawing.

##### Scissoring ( TODO: fill this up)
Doing clipping and scan-conversion at the same time.

##### Point clipping

Let the point be at `(x, y)`. Let the clip rect be `(xm, ym), (xM, yM)`. Check for point in AABB (`xm <= x <= xM)`, ditto for `y`.

We can use point clipping to clip anything, to check this condition during scan conversion. However, this is expensive! So, we can come up with better strategies to clip lines.

##### Line clipping

- [Cohen sutherland](https://www.geeksforgeeks.org/line-clipping-set-1-cohen-sutherland-algorithm/)

##### Clipping polygons

- Sutherland Hodgman algorithm.

## Draw polygons
- Scan line algorithm.
- [scan line corner cases with proper explanation](http://www.cse.iitm.ac.in/~vplab/courses/CG/PDF/SCANLINE.pdf)
- Better source with algorithmic details: [link here](https://hackernoon.com/computer-graphics-scan-line-polygon-fill-algorithm-3cb47283df6)


### Scan line algorithm 

##### Edge bucket
```cpp
struct EdgeBucket {
    int ymax;
    int ymin;
    int curx;
    int slopesign;
    int deltaX; // absolute difference between edge's vertex points
    int deltaY; // absolute difference between edge's vertex points
    int sum; 
}
```
##### Edge table
contains all edges that make up the polygon.

- Vertices ordered left to right.
- Edges maintained in *increasing `yMin` order*
- edges are removed from ET once the `ActiveList` finishes processing them.
- algorithm is done filling in polygon when all edges are removed.

##### Active List
- Edges pushed into `ActiveList` when the edge's yMin = y of current scan line
- Edges will always be put into the `ActiveList` in pairs.
- Edges are maintained in increasing `x` order.
- resorted after every pass.


##### Algorithm:

1. Create ET
    1. Process the vertices list in pairs, start with [numOfVertices-1] and [0].
    2. For each vertex pair, create an edge bucket
2. Sort ET by yMin
3. Process the ET
    1. Start on the scan line equal to theyMin of the first edge in the ET
    2. While the ET contains edges
        1. Check if any edges in the AL need to be removes (when yMax == current scan line)
            1. If an edge is removed from the AL, remove the associated the Edge Bucket from the Edge Table.
        2. If any edges have a yMin == current scan line, add them to the AL
        3. Sort the edges in AL by X
        4. Fill in the scan line between pairs of edges in AL
        5. Increment current scan line
        6. Increment all the X's in the AL edges based on their slope
            1. If the edge's slope is vertical, the bucket's x member is NOT incremented.

# Visibility


# Lighting

Blinn phong equations:

- diffuse: K_d * (light, normal)
- specular: `cos(theta)^k = (eye . normal)^k, k = specular coefficient`
- ambient: 

# Raytracing