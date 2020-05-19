# The Unreasonable Effectiveness Of Declarative Programming
## Declarative animations

I show off [`minanim.js`](https://github.com/bollu/mathemagic/blob/master/declarative/minanim.js),
a tiny, 100LoC, yet feature-complete library for building animations _declaratively_,
and why someone would want to do things this way. Enjoy!

## A complex animation
<div id="animation-1"></div>


The blue circle's animation is quite complex. It consists of multiple
stages. (1) The circle grows in size. (2) It continues to grow in size
at a faster rate, as it shoots off to the right. (3) It pauses. (4) It
moves to the middle. (5) It pauses again. (6) It shrinks to nothing.

All of this is captured by a single object `anim_circle` (written using `minanim.js`)
which _declares_ what the animation is doing:

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
1. `anim_const(name, val)` to set a constant value `val` to name `name`.
2. `anim_interpolated(ease, name, val, time)` to change to a named value with name `name`
   to value `val` in duration `time`. 
3. `anim1.seq(anim2)` to run `anim2` once `anim1` has completed.
4. `anim1.par(anim2)` to run `anim2` in parallel with `anim1`.
5. `anim_delay(time)` to do nothing for time `time`.

## What is `anim_circle`, really?

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

## Playing with this Webpage: edit `anim_circle` in the browser!


- <a  onclick="foo()">
  **An ORDER**: Click on this link, paste what has been copied to your clipboard into the browser's console, and hit enter.
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

- **To go back to the original state of affairs**, 
  click on this link to copy the old code onto your clipboard </a>. Paste
  the text, into your console, hit enter. Everything will be back to
  original.


## Declarative ⇒ Pure

As hinted above, since our specification of the animation was entirely declarative,
it can't really "do anything else" like manipulate the DOM. This gives us 
fantastic debugging and editing capabilities. As it's "just" a mathematical
function:

```
anim_circle: (t:Time) -> (cx: float, cr: float)
```

We can easily swap it (by pasting the code above), poke it (by calling `anim_circle(0.5)`),
and in general deal with is as a **unit of thought**. It has no unpleasant
interactions with the rest of the world.

## Purity ⇒ Time Travel

Due to this purity, we also get **time-travel-debugging**. The slider is hooked up
to `anim_circle`, and displays the circle as dictated by `anim_circle(t_slider)`.
`t_slider` is received from the slider.

<div id="animation-2"></div>
Drag the slider to move through the animation!  </br>

<input type="range" id="animation-2-scrubber" min=0 max=1000 value=0 style="width:80%">


## Declarative ⇒ Composition: staggering animations

Our framework is _composable_, because we can build larger objects from smaller
objects in a natural way. As an example, a _staggered_ animation is a nice
way to make the entry of multiple objects feel less monotonous.

<div id="animation-3"></div>


### Step 1
The code to achieve this creates a list of animations called `as` which
has the animations of the ball rising up. Each element `as[i]` has
the animation of the ball rising up for the same amount of time. This is
visualized here:


```text
*as[0]===*
*as[1]===*
*as[2]===*
*as[3]===*
*as[4]===*
```

<div id="animation-31"></div>



### Step 2
Next, each element `as[i]` is modified by creating a new animation `xs[i]`.
`xs[i]` runs `as[i]` after a delay of `delta*i`.
We then compose all the `xs[i]` in parallel to create a single animation `x`.
This animation has the balls rising from the bottom in a staggered fashion.

```text
  | |xs[0] = -delay=0-*as[0]===*
  |P|xs[1] = -delay=1------*as[1]===*
x=|A|xs[2] = -delay=2-----------*as[2]===*
  |R|xs[3] = -delay=3----------------*as[3]===*
  | |xs[4] = -delay=4--------------------*as[4]===*
```


<div id="animation-32"></div>

### Step 3
Next, we similarly create an array of animations called `bs` which
has animations of the balls disappearing. These are staggered as before.
This is shown here:

```text
  | |ys[0] = -delay=0-*bs[0]===*
  |P|ys[1] = -delay=1------*bs[1]===*
y=|A|ys[2] = -delay=2-----------*bs[2]===*
  |R|ys[3] = -delay=3----------------*bs[3]===*
  | |ys[4] = -delay=4--------------------*bs[4]===*
```

<div id="animation-33"></div>

### Step 4
Finally, we compose `x` and `y`, such that `y` is staggered relative to `x`
by some delay. This allows the first few balls to start disappearing
_while new balls_ continue entering.

```text
     |  | |xs[0] = -delay=0-*as[0]===*
     |  |P|xs[1] = -delay=1------*as[1]===*
     |x=|A|xs[2] = -delay=2-----------*as[2]===*
     |  |R|xs[3] = -delay=3----------------*as[3]===*
     |  | |xs[4] = -delay=4--------------------*as[4]===*
     |                 | |ys[0] = -delay=0-*bs[0]===*
     |                 |P|ys[1] = -delay=1------*bs[1]===*
anim=|---delay-------y=|A|ys[2] = -delay=2-----------*bs[2]===*
     |                 |R|ys[3] = -delay=3----------------*bs[3]===*
     |                 | |ys[4] = -delay=4--------------------*bs[4]===*
```


<div id="animation-34"></div>
Drag the slider to move through the animation!  </br>
<input type="range" id="animation-34-scrubber" min=0 max=1000 value=0 style="width:80%">


### Reflection

Notice that the final animation network is quite complex. It's hopeless
to build it "manually". In code, we write special helpers
called `anim_stagger` that allow us to stagger animation, and then use
it, along with `.seq()` and `.par()` to build the full animation:

```js
const anim = anim_parallel_list(anim_circles_start)
    .seq(anim_stagger([anim_stagger(anim_circles_enter, STAGGER),
                       anim_stagger(anim_circles_leave, STAGGER)], 300));
```

This describes the complicated network:

```text
     |  | |xs[0] = -delay=0-*as[0]===*
     |  |P|xs[1] = -delay=1------*as[1]===*
     |x=|A|xs[2] = -delay=2-----------*as[2]===*
     |  |R|xs[3] = -delay=3----------------*as[3]===*
     |  | |xs[4] = -delay=4--------------------*as[4]===*
     |             | |bs[0] = -delay=0-*as[0]===*
     |             |P|bs[1] = -delay=1------*as[1]===*
anim=|---delay---y=|A|bs[2] = -delay=2-----------*as[2]===*
     |             |R|bs[3] = -delay=3----------------*as[3]===*
     |             | |bs[4] = -delay=4--------------------*as[4]===*
```

## Declarative ⇒ Debuggable

As hinted above, since our specification of the animation was entirely declarative,
it can't really "do anything else" like manipulate the DOM. This gives us 
fantastic debugging and editing capabilities. As it's "just" a mathematical
function:

```
anim_circle: (t:Time) -> (cx: float, cr: float)
```

so we can play with it on the console, edit it interactively, and plot it.
It's behaviour can be studied on a piece of paper, since it's entirely
decoupled from the real world.


## The power of `easing`

So far, we have been using the same `easing` parameter everywhere: 
`easing_cubic`. This parameter is a way to _warp time_. We only tell the
library what the _final value_ is supposed to be. It's our library's job
to figure out how to get from the current value to the final value. However,
there are _many ways_ to get from the initial value to the final value. We
could:
- Change the value in constant increments. This is what `easing_linear` does.
- Change the value so that it chages slowly in the beginning, and much
  faster later. This is what `easing_cubic` does.
- Change the value so that it changes quickly, _overshoots_, and then
  comes back to the final value. This is what `ease_out_back` does. 

There are many easing functions. Indeed, infinitely many, since we can write
any function we want. A quick example of the three mentioned above, with
a slide to notice the difference:

<div id="animation-showoff-easing"></div>
Drag the slider to move through the animation!  </br>
<input type="range" id="animation-showoff-easing-scrubber" min=0 max=1000 value=0 style="width:80%">


##### Code for easing:

```js
const interpolators = [ease_cubic, ease_linear, ease_out_back]
for(var i = 0; i < NINTERPOLATORS; ++i) {
    ...
    anim_circles_start.push(anim_const("cx" + i, 200));
    anim_circles_enter.push(anim_interpolated(interpolators[i], "cx" + i, 300, 200));
}
...
const anim = anim_parallel_list(anim_circles_start).seq(anim_parallel_list(anim_circles_enter));

```

## `minanim.js` versus the world

Both `d3.js` and `anime.js` are libraries that intertwine 
_computing_ with _animation_. On the other hand, our implementation describes
_only_ how values change. It's up to us to render this using
SVG/canvas/what-have-you. 

Building a layer like `anime.js` on top of this is not hard. On the other hand,
using `anime.js` purely is impossible.

## Code Walkthrough / API documentation

The entire "library", which is written very defensively and sprinkled with
asserts fits in [**exactly 100 lines of code**](https://github.com/bollu/mathemagic/blob/master/declarative/minanim.js). It can be golfed further
at the expense of either asserts, clarity, or by adding some higher-order
functions that factor out some common work. I was loath to do any of these.
So here's the full source code, explained as we go on.


- We write `assert_precondition(t, out, tstart)` to check that `t`
  and `tstart` are numbers such that `t >= tstart`, and that `out` is an object. 
  If `tstart` is uninitialized, we initialize `tstart` to `0`. If
  `out` is uninitialized, we initialize `out` to `{}`.

```js
// t, tstart: number. out: object
function assert_precondition(t, out, tstart) {
    console.assert(typeof(t) === "number");
    if (out === undefined) { out = {}; }
    console.assert(typeof(out) === "object");
    if (tstart === undefined) { tstart = 0; }
    else { console.assert(typeof(tstart) === "number"); }
    console.assert(t >= tstart);
    return [out, tstart];
}
```

- `anim_delay(duration)` creates a function `f`. On being invoked, it returns
  whatever value of `out` has been given to it. That is, it doesn't
  modify anything. It has three fields, `duration`, `par`, and `seq`.
  `duration`. `duration` is how long the animation runs for. `par, seq`
  are methods for chaining, that allows us to compose this delay animation
  in parallel and in sequence with other animations.

```js
// duration: number
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
```



- `const(field, v)` creates a function `f`. On being invoked, it sets
  `out.v = field`. It takes zero time to run such an animation, hence it's
  duration is `0`. Useful for instaneously setting the value at the
  start of an animation.

```js
// field: string. v: number
function anim_const(field, v) {
    let f = function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart); out[field] = v; return out;
    };
    f.duration = 0;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}
```


- We implement two **easing functions**, which takes a parameter 
  `tlin` such that `0 <= tlin <= 1`, and two parameters `vstart` and `vend`.
  The functions allow us to animate a change from `vstart` to `vend` smoothly.
  We are to imagine `tlin` as a time. When `tlin=0`, we are at `vstart`.
  When `tlin=1`, we will be at `tend`.
  In between, we want values between `vstart` and `vend`. To animate values,
  we often want the change from `vstart` to `vend` to happen a certain way.
  For example, we often want the change to start slowly, and then for the
  change to happen faster towards the end. A good reference for this is
  [`easings.net`](https://easings.net/). Our animation library can use
  _any_ easing function we see fit.

```js
// vstart, vend: number. tlin: number, 0 <= tlin <= 1
function ease_linear(vstart, tlin, vend) { return (1.0 - tlin) * vstart + tlin * vend; }

function ease_cubic(vstart, tlin, vend) {
    const cube = (1 - tlin)*(1-tlin)*(1-tlin); return cube * vstart + (1 - cube) * vend;
}
function ease_out_back(vstart, tlin, vend) {
    const c1 = 1.70158; const c3 = c1 + 1; const t = 1 + c3 * pow(x - 1, 3) + c1 * pow(x - 1, 2);
    return (1-t) * vstart + t*vend;
}
```

- `anim_interpolated(duration)` creates a function `f`. On being invoked, 
  it figures out if its animation is running or has already ended.
  We have a **precondition** `t >= tstart`, which is checked by
  `assert_precondition`, and is maintained by the library.
  So, we only need to care whether the animation is currently running
  or has ended. If the animation is currently running, we find the
  current value using `fease`. If the animation has ended, we set
  the value to the `end` value.


<div id="animation-interpolated"></div>

                                        
```js
// fease: easing function.
// field: string. vend: number. duration: number >= 0
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
```

- `anim_sequence(anim1, anim2)` sets up `anim2` to begin
  running once `anim1` has completed. When it is invoked, `t >= tstart`. So
  it can run `anim1` immediately. If it learns that
  `anim1` has completed, it then invokes `anim2`. The total time taken for
  this animation is its `duration`. This is the sum of durations of `anim1`
  and `anim2`.


<div id="animation-sequence"></div>


```js
// anim1, anim2: anim
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
```

- `anim_parallel(anim1, anim2)` sets up `anim1` and
  `anim2` to run in parallel. When it is invoked, `t >= tstart`. So it can
  launch `anim1, anim2` both immediately. The `duration` of this animation
  is the _maximum_ time taken by `anim1`, `anim2`.


<div id="animation-parallel"></div>

```js
// anim1, anim2: anim
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
```


- `anim_parallel_list(xs)` is a helpful to write the animations
  in `xs` in parallel. It chains together the elements of the list
  with `par` calls.

```js
// xs: list[animation]
function anim_parallel_list(xs) {
    var x = xs[0]; for(var i = 1; i < xs.length; ++i) { x = x.par(xs[i]); }
    return x;
}
```

- `anim_stagger(xs, delta)` is a combinator to stagger the animations in
  the list of animations `xs`. It
  delays the animation at `xs[i]` for a duration `delta*i`. 

```js
// xs: list[animation]. delta: duration
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

## Conclusion

We saw how to write a tiny, declarative, composable animation library that
does _one_ thing: compose functions that manipulate values over time,
and does it well.

If you like this content, check out the repo at [bollu/mathemagic](https://github.com/bollu/mathemagic)

<!--script src="ANIMGENERATED.js"></script-->
<script src="./minanim.js"></script>
<script src="./script.js"></script>

<script type="text/javascript">
document.body.onload = function() { init_animations(); }

</script>

