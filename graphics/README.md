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


## 



# Lighting

Blinn phong equations:

- diffuse: K_d * (light, normal)
- specular: `cos(theta)^k = (eye . normal)^k, k = specular coefficient`
- ambient: 