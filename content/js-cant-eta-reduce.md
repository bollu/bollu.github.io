+++
Categories = []
Description = ""
Tags = []
date = "2017-04-09"
title = "JavaScript cannot Eta-Reduce"
draft=true
+++

<p data-height="265" data-theme-id="light" data-slug-hash="JWgoLB" data-default-tab="js,result" data-user="bollu" data-embed-version="2" data-pen-title="JS cannot eta reduce" class="codepen">See the Pen <a href="http://codepen.io/bollu/pen/JWgoLB/">JS cannot eta reduce</a> by Siddharth (<a href="http://codepen.io/bollu">@bollu</a>) on <a href="http://codepen.io">CodePen</a>.</p>
<script async src="https://production-assets.codepen.io/assets/embed/ei.js"></script>

## The Example:

Let's create an object which has a value that we are interested in printing out.
The object `listener` has a `value` that it writes out to the DOM.

```js
var listener = {
  click: function() {
    document.getElementById("value").innerHTML = this.value;
  },
  value: 3.1415,
};
```

Now, let's attach the click method to the button, which wraps the `listener.click()`
in a function call.

```
document.getElementById("non-eta-reduced").onclick = function() {
    listener.click();
  };

```
However, this looks like something that can be eta-reduced! After all, if all
we're doing is wrapping a function call `listener.click()` with another function,
we should be able to replace this with just `listener.click`?

Let's try that:

```js
document.getElementById("eta-reduced").onclick = listener.click;
```

however, if you click the `eta-reduced` button, no value is displayed. What gives?

## JavaScript and `this`

when you attach any event listener to a DOM node,
the **`this` gets bound to the DOM node**. 

So, in the eta-reduced case, the `this.value` refers to the button's DOM node's value.
Hence, `this.value` is undefined!

I found this weird at first glance, but once you understand how `this` works in javaScript,
I suppose that it is straightforward enough. I wish there was a way to be more explicit about
where `this` comes from and goes. I'd love to hear people's thoughts and experiences when it
comes to teaching something like this to a newbie.

