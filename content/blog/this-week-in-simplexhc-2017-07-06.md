+++
date = "2017-07-11 20:09:46+02:00"
title = "This week in simplexhc: July 11 2017"
+++

##### Alternate codegen style:

- This makes the tail call directly exposed.
- Matcher can still be inlined.

```
typedef void (*HaskellFunc)();
int globalSideEffect;

void function_one() alwaysinline {
    globalSideEffect = 0;
    HaskellFunc continuation =  matcher(1); // "tail call" forces a call to be tail call optimised.
    tail call continuation();
}

void function_two() alwaysinline {
    globalSideEffect = 42;
    HaskellFunc continuation = matcher(2);
    tail call continuation();
}

HaskellFunc matcher(int i) {
    switch (i) {
        case 1: return function_one;
        case 2: return function_two();
    }
}
```