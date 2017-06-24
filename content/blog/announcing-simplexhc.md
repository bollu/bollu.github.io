+++
date = "2017-06-24T17:34:10+02:00"
title = "Announcing simplexhc"

+++

# Simplexhc - a STG to LLVM compiler

I'm trying to understand GHC's design choices when it comes to compilation. The way GHC currently compiles
is to first reduce Haskell to a `Core` language, which is a minimal subset of Haskell, in some sense.

Next, it compiles `Core` to `STG`, which is an "abstract machine" that is Haskell's view of
what the hardware *should* be like. `STG` stands for the "spineless, tagless, G-machine"
(Yep, a totally badass name). [The best source for it is the original paper, "implementing functional programming languages on stock hardware"](https://www.dcc.fc.up.pt/~pbv/aulas/linguagens/peytonjones92implementing.pdf)

Alas, reality is different: `x86` is quite different from `STG`, so we compile down `STG` to
`C--`, an imperative C-like language. 

Usually, `C--` gets compiled down directly to assembly. However, there is a backend which
converts this to `LLVM` as well. However, this is a terrible way to generate `LLVM` from `STG`, as:

1. Going to `C--` and then from there going to `LLVM` loses a lot of the semantic information that
one can provide to the `LLVM` optimiser.

2. `STG`'s lowering to `C--` and the decisions taken when implementing it were based in the 90's. That's not to 
say that the GHC team has done an awesome job keeping it up to date: they have! But, I wonder what a complete rewrite of this lowering would look like. Hence, I'm trying to experiment in this space and see what happens.

[The current repo of `simplexhc`](https://github.com/bollu/simplexhc) currently contains the original `STG` implementation *as an interpreter*. However, I've started work on the `LLVM` backend, so I decided to start blogging about it.


## Current work: Representing the main loop

The heart of STG lies in following continuations.
As a motivating example, let's take a dummy example and see what
this would like in C:

##### C-pseudocode

```lang=c
void matcher(int i);
int globalSideEffect;

void function_one() {
    globalSideEffect = 0;
    matcher(1);
}


void function_two() {
    globalSideEffect = 42;
    matcher(2);
    
}

/*
 * This is the "main loop" of STG. A function calls "matcher" with the
 * correct "tag" corresponding to the next function (continuation). The 
 * matcher matches on the tag and dispatches the call.
 *
 * If we do not perform tail call elimination, the call stack will eventually blow,
 * since there are no returns anywhere!
 */
void matcher(int i) {
    switch (i) {
        case 1: function_one(); break;
        case 2: function_two(); break;
    }
}

// entry point
int main() {
    matcher(1);
}
```

The problem with this approach is obvious:

##### STG paper approach:

In the `STG` paper, here is how they describe doing this:

```cpp
function_one() {
   globalSideEffect = 0;
   return &function_two;
}

function_two() {
    globalSideEffect = 42;
    return &function_one;
}

main() {
    cont = &function_one;
    while(1) {
        cont_next = (*cont)();
        cont = cont_next;
    }
}
```
The transformation is quite clever: They choose to return *function pointers*.
The main function becomes the "continuation follower". `main` calls the
current continuation, `cont`, which returns the _next_ continuation: `cont_next`.
Then, we assign `cont` to `cont_next` and repeat this process. Note that
we don't blow up our stack doing this, because we only use a constant amount of
stack space.

However, this has a problem: namely, we cannot provide a type to the functions
`function_one` and `function_two`! Their types are roughly

```haskell
type Cont = () -> Cont*
```

which you can't directly `typedef` in `C` (or `LLVM`). The workaround would
be something along the lines of:

```cpp
struct Continuation {
     typedef struct Continuation * (*ContFuncTy)();
     ContFuncTy cont;
}
```

However, this adds a struct indirection as well. I'm not sure how much of an
impact this would have on compilation, but it just seems hacky.


On the other hand, if we reconsider the original solution with a couple of
minor changes:

##### Pseudo-LLVM

```cpp
void matcher(int i);
int globalSideEffect;

void function_one() alwaysinline {
    globalSideEffect = 0;
    tail call matcher(1);
}


void function_two() alwaysinline {
    globalSideEffect = 42;
    tail call matcher(2);
}

void matcher(int i) {
    switch (i) {
        case 1: function_one(); break;
        case 2: function_two(); break;
    }
}
```

Think of the `alwaysinline` attributes as hints to the compiler (in this case, `LLVM`)
to _always_ inline these function calls.

Next, the `tail call` is an annotation telling the compiler to force these calls to
be tail calls.

In the current state (without inlining), they don't look like tail calls. However,
on inlining, we get code that looks like this:



```lang=c
void matcher(int i);
int globalSideEffect;

void function_one() alwaysinline {
    globalSideEffect = 0;
    tail call matcher(1);
}


void function_two() alwaysinline {
    globalSideEffect = 42;
    tail call matcher(2);
}

void matcher(int i) {
    switch (i) {
        case 1: {
            globalSideEffect = 0;
            tail call matcher(1)
        }; break;

        case 2:  {
            globalSideEffect = 42;
            tail call matcher(42);
        };
        break;
    }
}
```

which _can_ be tail called, since intuitively, the "tail call" occurs
at the end of the call site.


The problem with this approach is that the `matcher` would need to cover
**every possible call in our Haskell program**. While this could be
quite large, I have a feeling that agressive dead code elimination with LLVM's
decent ability to generate code for `switch` shouldn't lead to too much of a
cache hit.

The reason this is absolutely wonderful is that now, the calls are explicit. There
is no pointer-chasing that the compiler needs to perform, which should lead to
better inter-procedural analysis.

Of course, it's all conjecture at this point. I'll come back with data :)

I'd love to hear thoughts and comments about this. Please do leave a comment,
or e-mail me at `siddharth.bhat at research dot iiit dot ac dot in`.
