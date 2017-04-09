+++
Categories = ["math"]
Description = ""
Tags = []
date = "2015-07-17T20:19:14+05:30"
title = "Math Rambling - Dirac Delta derivative"
+++

<!-- Loading MathJax -->
<style>
code.has-jax {font: inherit;
              font-size: 0.9em;
              background: inherit;
              border: inherit;
              color: #
</style>

<script type="text/javascript"
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
  
<script type="text/x-mathjax-config">

MathJax.Hub.Config({
  tex2jax: {
    inlineMath: [['$','$'], ['\\(','\\)']],
    displayMath: [['$$','$$']],
    scale: 10,
    minScaleAdjust: 10,
    processEscapes: true,
    processEnvironments: true,
    skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
    TeX: { equationNumbers: { autoNumber: "AMS" },
         extensions: ["AMSmath.js", "AMSsymbols.js"] }
  }
});
</script>

<script type="text/x-mathjax-config">
  MathJax.Hub.Queue(function() {
    // Fix <code> tags after MathJax finishes running. This is a
    // hack to overcome a shortcoming of Markdown. Discussion at
    // https://github.com/mojombo/jekyll/issues/199
    var all = MathJax.Hub.getAllJax(), i;
    for(i = 0; i < all.length; i += 1) {
        all[i].SourceElement().parentNode.className += ' has-jax';
    }
});
</script>


I've been studying Quantum Mechanics from Shankar's _Principles of Quantum Mechanics_ recently,
and came across  the _derivative_ of the Dirac delta function that had me stumped.

`$ \delta'(x - x') =  \frac{d}{dx} \delta(x - x') = -\frac{d}{dx'} \delta(x - x') $` 

I understood neither what the formula represented, how the two sides are equal.

Thankfully, some Wikipedia and [Reddit (specifically /r/math and /u/danielsmw)](https://www.reddit.com/r/math/comments/3ddbd7/derivative_of_the_dirac_delta_function/ct41cv6) helped me find the answer. I'm writing this for myself,
and so that someone else might find this useful.

#### Terminology

I will call `$\frac{d}{dx} \delta(x - x')$` as the first form,
and `$-\frac{d}{dx'} \delta(x - x')$` as the second form


#### Breaking this down into two parts:
1. show what the derivative computes
2. show that both forms are equal


## 1. Computing the derivative of the Dirac Delta

Since the Dirac Delta function can only be sensibly manipulated in an integral, let's
stick the given form into an integral.


<div>
$$
\delta'(x - x') = \frac{d}{dx} \delta(x - x') \\

\int_{-\infty}^{\infty} \delta' (x - x') f(x') dx' \\
= \int_{-\infty}^{\infty} \frac{d}{dx} \delta(x - x') f(x') dx' \\
$$
Writing out the derivative explicitly by taking the limit,

$$
= \int_{-\infty}^{\infty} \lim{h \to 0} \; \frac{\delta(x - x' + h) - \delta(x - x')}{h} f(x') dx' \\
= \lim{h \to 0} \; \frac{ \int_{-\infty}^{\infty} \delta((x + h) - x') f(x') dx' - \int_{-\infty}^{\infty} \delta(x - x') f(x') dx'}{h} \\
= \lim{h \to 0} \; \frac{f(x + h) - f(x)}{h} \\
=  f'(x)
$$
</div>

#### Writing only the first and last steps,

<div>
$$
\int_{-\infty}^{\infty} \delta' (x - x') f(x') dx' = f'(x) 
$$
</div>

This shows us what the derivative of Dirac delta does. On being multiplied with a function,
it "picks" the derivative of the function at one point.

## 2. Equivalence to the second form

We derived the "meaning" of the derivative. Now, it's time to show that the second form
is equivalent to the first form.

<div>
Take the second form of the delta function as the derivative,

$$
\delta'(x - x') = - \frac{d}{dx'} \delta(x - x') \\

\int_{-\infty}^{\infty} \delta' (x - x') f(x') dx' \\
= \int_{-\infty}^{\infty} - \frac{d}{dx'} \delta(x - x') f(x') dx' \\
$$


Just like the first time, open up the derivative with the limit definition

$$
= \int_{-\infty}^{\infty} \lim{h \to 0} \; - (\frac{\delta(x - (x' + h)) - \delta(x - x')}{h}) f(x') dx' \\
= \lim{h \to 0} \; \frac{ \int_{-\infty}^{\infty} \delta((x - h) - x') f(x') dx' - \int_{-\infty}^{\infty} \delta(x - x') f(x') dx'}{h} \\
= \lim{h \to 0} \; - \frac{f(x - h) - f(x)}{h} \\
= \lim{h \to 0} \; \frac{f(x) - f(x - h)}{h} \\
=  f'(x)
$$
</div>

## Conclusion

That shows that the derivate of the Delta Function has two equivalent forms, both of which
simply "pick out" the derivative of the function it's operating on.

<div>
$$
\delta'(x - x')  = \frac{d}{dx} \delta(x - x') = -\frac{d}{dx'} \delta(x - x') 
$$
</div>

Writing it with a function to operate on (this is the version I prefer):

#### First form:
<div>
$$
\int_{-\infty}^{\infty} \delta' (x - x') f(x') dx' = \\
\int_{-\infty}^{\infty} \frac{d}{dx}\delta(x - x') f(x') dx' = \\
f'(x)
$$
</div>

#### Second form:
<div>
$$
\int_{-\infty}^{\infty} \delta' (x - x') f(x') dx' = \\
\int_{-\infty}^{\infty} -\frac{d}{dx'}\delta(x - x') f(x') dx' = \\
f'(x)
$$
</div>

## A note on notation


In a violent disregard for mathematical purity, one can choose to abuse notation
and think of the above transformation as - 

<div>
$$
\delta'(x - x') = \delta(x - x') \frac{d}{dx}
$$
</div>


We can write it that way, since one can choose to  think that the delta function transforms

<div>
$$
\int_{-\infty}^{\infty} \delta'(x - x')f(x') dx' \to \\
\int_{-\infty}^{\infty} \delta(x - x')\frac{d}{dx}f(x')dx' = \\
\int_{-\infty}^{\infty} \delta(x - x') f'(x') = \\
f'(x)
$$
</div>

The original forms and the rewritten one are equivalent, although the original is "purer" than the other. Which one to use in is
up to you :)

So, to wrap it up:

<div>
$$
\delta'(x - x')  = \frac{d}{dx} \delta(x - x') = -\frac{d}{dx'} \delta(x - x') = \delta(x - x') \frac{d}{dx} 
$$
</div>
