## The unreasonable effectiveness of functional programming
### Declarative animations

I'd like to show off the usual approach to building animations in javascript,
and what we can learn by building a tiny version of such a library.

#### An animation
<div id="animation-1"></div>

Here, we build a simple animation. The code that __describes__ the animation is:

```js
const anim_circle = anim_const("cx", 100)
    .seq(anim_const("cr", 0))
    .seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*dur=*/50))
    .seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*dur=*/100)
        .par(anim_interpolated(ease_cubic, "cr", 40, 100)))
    .seq(anim_delay(10))
    .seq(anim_interpolated(ease_linear, "cx", 100, 100))
    .seq(anim_interpolated(ease_cubic, "cr", 0, 100));

```

The entire animation is built out of one primitive and three combinators:
1. `anim_interpolated` to change to a given value in some given duration.
2. `seq` to run animations in sequence.
3. `par` to run animations in parallel.
4. `anim_delay` to add a delay.

I haven't shown how this `anim_circle` object is linked to the circle above. 
Don't worry, it's fairly straightforward. I also haven't described what is
_functional_ about all of this. We'll get to that too.

#### What is `anim_circle`?

`anim_circle` is a _function_, which can be invoked as `val = anim_circle(t)`, where
${0 \leq t \leq \texttt{duration}}$. It returns a dictionary `val`, where
`val.cx` and `val.cr` have values as the animation demands. I've plotted
the values of `val.cx` and `val.cr` as well pass `t` from `0` to `duration`.
This `duration` is calculated for us by the library, and can be accessed as
`anim_circle.duration`.

- PLOT

To play with the `anim_circle`, feel free to open your console on the page
and try:

- `anim_circle(0)`
- `anim_circle(anim_circle.duration)`
- `anim_circle(anim_circle.duration/2.0)`

#### Purity ⇒ Declarative

#### Purity ⇒ Time Travel

<div id="animation-2"></div>
Play with the slider to move through the animation!  </br>
<input type="range" id="animation-2-scrubber" min=0 max=1000 value=0 style="width:80%">


#### Purity ⇒ Composition: stagger

<div id="animation-3"></div>
Play with the slider to move through the animation!  </br>
<input type="range" id="animation-3-scrubber" min=0 max=1000 value=0 style="width:80%">


#### The algebra

We can show that these functions have the algebraic structure of a semiring.
- Our multplication is sequential composition, since it's not commutative.
- Our addition is parallel composition, since it is commutative.
- The 0 is `delay(INFINITY)`, because `0 * x = 0` (sequentially composing something
  to happen after an infinite 

<!--script src="ANIMGENERATED.js"></script-->
<script src="anim.js"></script>
<script src="./interior-point.js"></script>

<script type="text/javascript">
document.body.onload = function() { init_interior_point(); }

</script>
