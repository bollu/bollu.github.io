+++
Categories = []
Description = ""
Tags = []
date = "2017-03-02T02:01:00+05:30"
title = "js cant eta reduce"
draft=true
+++


Behold, JavaScript cannot eta reduce!

```js
MyObject = {
    foo: function() {
        console.log(this.value);
      },
    value: 0
  };

document.getElementById("woo").addEventListener("click", function() { 
    MyObject.foo()
    });
```


Upon eta reuction,

```js
MyObject = {
    foo: function() {
        console.log(this.value);
      },
    value: 0
  };

document.getElementById("woo").addEventListener("click", MyObject);
```

## JavaScript and `this`

when you call `DOMNode.addEventListener`, the **`this` gets bound to the DOM node**.

