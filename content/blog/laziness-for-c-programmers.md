+++
title = "Nonstrictness for C programmers"
date = "2017-12-25T11:57:38+05:30"
draft = true
+++

The aim of this blog post is to explain haskell's evaluation model without
having to jump through too many hoops `:)`.

`GHC` (the Glasgow haskell compiler) internally uses multiple intermediate
representations, in order of original, to what is finally produced:

- Haskell (the source language)
- Core (a minimal set of constructs to represent the source language)
- STG (Spineless tagless G-machine, a low-level intermediate representation that accurately captures non-strict evaluation)
- C-- (A custom version of C meant for platform independence during code generation)
- [Optional] LLVM (the intermediate representation of the [LLVM optimising compiler framework](http://todo))
- Assembly


Here, I will show how to lower simple non-strict programs from a `Core` like language
down to `C` (which is arguably `C--`), while skipping `STG`, since it doesn't
really add anything to the high-level discussion at this point.

###### Side node: nonsrict versus lazy (This section can be skipped)

I will use the word `non-strict` throughout, and not `lazy`.
Roughly speaking, `lazy` is more of an implementation detail that guarantees
that a value that is once computed is cached. (operational semantics)

`non-strict` is a evaluation order detail that guarantees that values are
not evaluated until there are truly required. (denotational semantics).

`lazy` is one way to _implement_ `non-strict`.

This is pure pedantry, but I'd like to keep it straight, since there seems to be
a lot of confusion involving the two words in general.

### Showing off non-strictness:
We first need a toy example to work with to explain the fundamentals of
non-strict evaluation, so let's consider the example below. It should be
relatively straightforward, except for the `case` construct which is explained
below.

We distinguish between primitive values (integers such as `1`, `2`, `3`) and boxed values
(functions, data structures). Boxed values can be evaluated non-stricly. Primitive values
do not need evaluation: they are primitive.

```hs
-- Lines starting with a `--` are comments.
-- K is a function that takes two arguments, `x` and `y`, that are both boxed values.
-- K returns the first argument, `x`, ignoring the second argument, `y`.
K :: Boxed -> Boxed -> Boxed
K x y = x

-- one is a function that returns the value 1# (primitive 1)
one :: () -> PrimInt
one = 1

-- Loopy is a function that takes zero arguments, and tries to return a boxed value.
-- Loopy invokes itself, resulting in an infinite loop, so it does not actually return.
loopy :: () -> Boxed
loopy = loopy

-- main is our program entry point.
-- main takes no arguments, and returns nothing
main :: () -> Void
main = case K(one, loopy) of -- force K to be evaluated with a `case`
            x -> case x of  -- Call the return value of K as `x`, and force evaluation.
                    primx -> printPrimInt(primx) -- Call the vreturn value of `x` as `primx` and then print it.
```

##### A quick explanation about `case`:
`case` is a language construct that *forces* evaluation. In general, no value
is evaluated unless it is *forced* by a case.

### Analysing the example:

###### The strict interpretation:

If we were coming from a strict world, we would have assumed that the expression
`K(one, loopy)` would first try to evaluate the arguments, `one` and `loopy`.
Evaluating `one` would return the primitive value `1`, so this has no problem.

On trying to evaluate `loopy`, we would need to re-evaluate `loopy`, and so on
ad infitum, which would cause this program to never halt.

This is because, as programmers coming from a strict world, we assume that
*values are evaluated as soon as possible*.

So, the output of this program is to have the program infinite-loop for ever,
under the strict interpretation.

###### The non-strict interpretation:

In the non-strict world, we try to evaluate `K(1, loopy)` since we are asked the result
of it by the `case` expression. However, we do not try to evaluate `loopy`, since 
no one has asked what it's value is!

Now, we know that

```hs
K x y = x
```

Therefore, 

```hs
K one loopy = one
```

regardless of what value `loopy` held.

So, at the case expression:
```hs
main = case K(one, loopy) of -- force K to be evaluated with a `case`
>>>         x -> ...
```

`x = one`, we can continue with the computation.

```hs
main :: () -> Void
main = case K(one, loopy) of -- force K to be evaluated with a `case`
            x -> case x of  -- Call the return value of K as `x`, and force evaluation.
>>>                    primx -> printPrimInt(primx) -- Call the vreturn value of `x` as `primx` and then print it.
```

Here, we force `x = one` to be evaluated with `case x of`.
since `one = 1`, `primx = 1`.
Once `primx` is returned, we print it out with `printPrimInt(primx)`.
 
The output of the program under non-strict interpretation is for it to print out `1`.

### Where does the difference come from?

Clearly, there is a divide: strictness tells us that this program should
never halt. Non-strictness tells us that this program will print an output!

To formalize a notion of strictness, we need a notion of `bottom` (`_|_`).

A value is said to be `bottom` if in trying to evaluate it, we reach an
undefined state. (TODO: refine this, ask ben).

Now, if a function is *strict*, it would first evaluate its arguments and then
compute the result. So, if a strict function is given a value that is `bottom`,
the function will try to evaluate the argument, resulting in the computation
screwing up, causing the output of the whole function to be `bottom`.

Formally, a function `f` is strict iff (if and only if) `f(bottom) = bottom`.

Conversely, a *non-strict* function does not need to evaluate its arguments if it
does not use them, as in the case of `K 1 loopy`. In this case, `f(bottom)` need
not be equal to bottom.

Formally, a function `f` is non-strict iff (if and only if) `f(bottom) /= bottom`.

As Paul Halmos says, " A good stack of examples, as large as possible, is indispensable for a thorough understanding of any concept, and when I want to learn something new, I make it my first job to build one.". Let us consider some examples.

1. `id`
```hs
id x = x

id (3) = 1
id (bottom) = bottom
```

`id` is strict, since `id(bottom) = bottom`.


2. `const`
```hs
const_one x = 1

const_one(bottom) = 1
const_one(3) = 1
```

`const_one` is not strict, as `const_one(bottom) /= bottom`.

3. `K`

```hs
K x y = x

K 1 2 = 1
K 1 bottom = 1
K bottom 2 = bottom
```

Note that `K(bottom, y) = bottom`, so K is *strict in its first argument*, and
`K(x, bottom) /= bottom`, so K is *non-strict in its second argument*.

This is a neat example showing how a function can be strict and lazy in different
arguments of the function.

### Compiling laziness

Now, we need a *strategy* to compile the lazy version of our program.
Clearly, `C` cannot express laziness directly, so we need some other
mechanism to implement this. I will first code-dump, and then explain as we go along.

###### [Code dump - Click for compiler explorer link (runnable program)](http://rextester.com/TCY24926)
```c
#include <assert.h>
#include <stdio.h>
#define STACK_SIZE 50000

/* a boxed value is a function that can be executed to compute something. */
typedef void (*Boxed)();

/* a return continuation that receives a boxed value. */
typedef void (*BoxedContinuation)(Boxed);

/* A return continuation that receives an int value. */
typedef void (*IntContinuation)(int);

/* Custom stack allocated on the .data section*/
void *stack[STACK_SIZE];

/* Stack pointer */
int sp = 0;

/* Push a continuation `cont` */
void pushContinuation(void *cont) {
    assert(sp >= 0);
    assert(sp < STACK_SIZE);
    stack[sp] = cont;
    sp++;
}

/* Pop a continuation frame. */
void *popContinuation() {
    assert(sp < STACK_SIZE);
    assert(sp >= 0);
    sp--;
    void *cont = stack[sp];
    return cont;
}

/* one = 1# */
void one() {
    printf("in function: %s\n", __FUNCTION__);
    void *f = popContinuation();
    (*(IntContinuation)(f))(1);
}

/* bottom = bottom */
void bottom() {
    printf("in function: %s\n", __FUNCTION__);
    bottom();
}

/* K x y = x */
void K(Boxed x, Boxed y) {
    printf("in function: %s\n", __FUNCTION__);
    void *f = popContinuation();
    (*(BoxedContinuation)(f))(x);
}

void XForceContinuation(int i) {
    printf("in function: %s\n", __FUNCTION__);
    printf("%d", i);
}

void KContinuation(Boxed x) {
    printf("in function: %s\n", __FUNCTION__);
    pushContinuation((void *)XForceContinuation);
    x();
}

int main() {
    printf("in function: %s\n", __FUNCTION__);
    pushContinuation((void *)KContinuation);
    K(one, bottom);
    return 1;
}
```

### Another example: Encoding factorial

```hs
fact :: Int -> Int
fact x = case x of
            0 -> 1
            n -> case fact (n - 1) of
                        factnm1 -> n * factnm1
```
