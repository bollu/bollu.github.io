## The unreasonable effectiveness of Declarative programming
### Declarative animations

I'd like to show off the usual approach to building animations in javascript,
and what we can learn by building a tiny version of such a library.

#### A complex animation
<div id="animation-1"></div>


The blue circle's animation is quite complex. It consists of multiple
stages. (1) The circle grows in size. (2) It continues to grow in size
at a faster rate, as it shoots off to the right. (3) It pauses. (4) It
moves to the middle. (5) It pauses again. (6) It shrinks to nothing.

All of this is captured by a single object `anim_circle` which _declares_ what the
animation is doing:

```js
// cx = location | cr = radius
let anim_circle = anim_const("cx", 100)
    .seq(anim_const("cr", 0))
    // (1) grow in size.
    .seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*time=*/3))
    // (2) go to right while growing.
    .seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*time=*/1)
        .par(anim_interpolated(ease_cubic, "cr", 70, 1)))
    // (3) pause.
    .seq(anim_delay(/*time=*/3))
    // (4) come back to the left.
    .seq(anim_interpolated(ease_cubic, "cx", 100, 1))
    // (5) pause again.
    .seq(anim_delay(/*time=*/2))
    // (6) shrink to nothing.
    .seq(anim_interpolated(ease_cubic, "cr", 0, 1));
```

The entire animation is built out of one primitive and three combinators:
1. `anim_interpolated(ease, name, val, time)` to change to a named value with name `name`
   to value `val` in duration `time`. 
2. `anim1.seq(anim2)` to run `anim2` once `anim1` has completed.
3. `anim1.par(anim2)` to run `anim2` in parallel with `anim1`.
4. `anim_delay(time)` to do nothing for time `time`.

#### What is `anim_circle`, really?

`anim_circle` is a _function_, which can be invoked as `val = anim_circle(t)`.
It returns an object `val`.  `val.cx` and `val.cr` have values as the animation dictates.
**That's it**. It **does not modify the DOM**. It **does not edit the** `circle` tag.
Given a time `t0`, it computes `cx` and `cr` at time `t0`. Keep it simple, stupid!


<div id='plot'></div>


Here is a plot of the values of `val.cx` and `val.cr` for different values of `t`.
This plotting code calls `anim_circle` at different times to plot the
results. The function `anim_circle` _is_ these plots,
since it doesn't compute anything else.

> Fancy ways of saying that `anim_circle` doesn't change anything else is to say that it 
> is _side-effect-free_, or _refrentially transparent_.

#### Playing with `anim_circle` in the browser


- <a  onclick="foo()">
  Click on this link, paste into the browser's console, and hit enter.
  </a> **This instantly updates the circle's animation and the plots**, since the code
  in the clipboard overwrites the definition of `anim_circle`! Scroll
  back to the top to see the new animation and charts.
  As our page is declarative, both the animations and the plots are driven
  by the definition of `anim_circle`.

The code that's been copied onto your clipboard is:

```js
// cx = location | cr = radius
anim_circle = anim_const("cx", 100)
    .seq(anim_const("cr", 0))
    // (1) grow in size.
    .seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*time=*/3))
    // (2) go to right while growing.
    .seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*time=*/1)
        .par(anim_interpolated(ease_cubic, "cr", 70, 1)))
    // (3) shrink to nothing.
    .seq(anim_interpolated(ease_cubic, "cr", 0, 1)); plot()
```
You can explore different definitions `anim_circle`s. Feel free to
play around.  Try evaluating `anim_circle(0)`, `anim_circle(anim_circle.duration)`,
`anim_circle(anim_circle.duration/2.0)` in the console to get a feel for what
`anim_circle` returns. <a onclick="writeOldToClipboard()">
To go back to the original state of affairs, 
click on this link to copy the old code onto your clipboard </a>. Paste
the text, into your console, hit enter. Everything will be back to
original.


#### Declarative ⇒ Pure

As hinted above, since our specification of the animation was entirely declarative,
it can't really "do anything else" like manipulate the DOM. This gives us 
fantastic debugging and editing capabilities. As it's "just" a mathemtical
function:

```
anim_circle: (t:Time) -> (cx: float, cr: float)
```

we can easily swap it (by pasting the code above), poke it (by calling `anim_circle(0.5)`),
and in general deal with is as a **unit of thought**. It has no unpleasant
interactions with the rest of the world.

#### Purity ⇒ Time Travel

Due to this purity, we also get **time-travel-debugging**. The slider is hooked up
to `anim_circle`, and displays the circle as dictated by `anim_circle(t_slider)`.
`t_slider` is received from the slider.

<div id="animation-2"></div>
Drag the slider to move through the animation!  </br>
<input type="range" id="animation-2-scrubber" min=0 max=1000 value=0 style="width:80%">


#### Declarative ⇒ Composition: staggering animations

<div id="animation-3"></div>
Drag the slider to move through the animation!  </br>
<input type="range" id="animation-3-scrubber" min=0 max=1000 value=0 style="width:80%">

#### Declarative ⇒ Debuggable

As hinted above, since our specification of the animation was entirely declarative,
it can't really "do anything else" like manipulate the DOM. This gives us 
fantastic debugging and editing capabilities. As it's "just" a mathemtical
function:

```
anim_circle: (t:Time) -> (cx: float, cr: float)
```



#### The algebra

We can show that these functions have the algebraic structure of a semiring.
- Our multplication is sequential composition, since it's not commutative.
- Our addition is parallel composition, since it is commutative.
- The 0 is `delay(INFINITY)`, because `0 * x = 0` (sequentially composing something
  to happen after an infinite 

#### `anim.js` versus the world
###### `anim.js` versus `d3`
###### `anim.js` versus `anime.js`

#### The full code listing

The entire "library", which is written very defensively and sprinked with
asserts fits in exactly a 100 lines of code. It can be golfed further
at the expense of either asserts, clarity, or by adding some higher-order
functions that factor out some common work. I was loath to do any of these.
The full code listing of `anim.js` is:

```
function assert_precondition(t, out, tstart) {
    console.assert(typeof(t) === "number");
    if (out === undefined) { out = {}; }
    console.assert(typeof(out) === "object");
    if (tstart === undefined) { tstart = 0; }
    else { console.assert(typeof(tstart) === "number"); }
    console.assert(t >= tstart);
    return [out, tstart];
}

function anim_delay(duration) {
    console.assert(typeof(duration) === "number");
    let f = function(t, out, tstart) { 
        [out, tstart] = assert_precondition(t, out, tstart); return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_const(field, v) {
    let f = function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart); out[field] = v; return out;
    };
    f.duration = 0;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;


function ease_linear(vstart, tlin, vend) { return (1.0 - tlin) * vstart + tlin * vend; }

function ease_cubic(vstart, tlin, vend) {
    const cube = (1 - tlin)*(1-tlin)*(1-tlin); return cube * vstart + (1 - cube) * vend;
}
                                        
function anim_interpolated(fease, field, vend, duration) {
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        if (t < tstart + duration && duration !== 0) {
            const tlin = (t - tstart) /duration;
            console.assert(tlin >= 0);
            console.assert(tlin <= 1);
            const vstart = out[field];
            out[field] = fease(vstart, tlin, vend);
        } else { out[field] = vend; }
        return out;
    };
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;

}

function anim_sequence(anim1, anim2) {
    const duration = anim1.duration + anim2.duration;
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        anim1(t, out, tstart);
        if (t >= tstart + anim1.duration) { anim2(t, out, tstart + anim1.duration); }
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_parallel(anim1, anim2) {
    const duration = Math.max(anim1.duration, anim2.duration);
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        if (t >= tstart) { anim1(t, out, tstart); anim2(t, out, tstart); }
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_parallel_list(xs) {
    var x = xs[0]; for(var i = 1; i < xs.length; ++i) { x = x.par(xs[i]); }
    return x;
}

function anim_stagger(xs, delta) {
    console.assert(typeof(delta) == "number");
    var ys = [];
    for(var i = 0; i < xs.length; ++i) {
        ys.push(anim_delay(delta*i).seq(xs[i]));
    }
    var y = ys[0];
    for(var i = 1; i < ys.length; ++i) {
        y = y.par(ys[i]);
    }
    return y;
}
```

<!--script src="ANIMGENERATED.js"></script-->
<script src="anim.js"></script>
<script src="./interior-point.js"></script>

<script type="text/javascript">
document.body.onload = function() { init_interior_point(); }

</script>

