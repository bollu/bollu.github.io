+++
Categories = ["haskell"]
Description = ""
Tags = ["haskell"]
date = "2015-11-27T13:08:53+05:30"
title = "Continuation Monad - Part 1"
draft = true
+++

I've been trying to understand how the `Continuation` monad in Haskell works for a while now, and I think I've finally
grokked it. I'm putting this out there in the hopes that it's going to help someone looking for the same path to enlightenment
that I was.

## Preface - Function Application
Let's consider what function application looks like in Haskell. `$` is the operator most commonly used to apply function, such that `f $ x = f x`.

The type of $ is
```haskell
$ :: (a -> b) -> a -> b
$     f          x  = y
```
So, given a function `f :: (a -> b)` and a value `x :: a`, it gives out a value of `y :: b`.
 
## Reverse Function Application

Now, if we choose to flip the order of parameters of `$`, we wind up with something interesting

```
let (|>) = flip ($)
(|>) :: a -> (a -> b) -> b
```

This is more natural to use a lot of the time, rather than `$`. For example, consider
```
map (+1) $ filter even $ take 20 [1..]
```
One needs to read the function application from the inside to the outside, or left to right which is not very "pipeline-y".

However, re-writing this using the `|>` operator, it becomes

```
[1..] |> take 20 |> filter even |> map (+1)
```

Which immediately makes the order of flow of data obvious. 


There is something interesting going on here - when we wish to add more steps into the pipeline, we need to use another `(|>)`. The only exceptional
steps are the first step that creates data, and the final step that stops applying the value on functions.

However, with a cute trick, we can see that the final step can always be "extended" by passing through an `id`. That is,
```
x |> f1 |> f2 |> ... |> f_n = x |> f1 |> f2 |> ... |> f_n |> id
```
due to the nature of `id`.

Therefore, in some sense, there are only two operations, one that lifts a raw value into a streaming process that can take more data, and one that composes streams,
which sounds suspiciously like a monad.

Let's see what type we're trying to abstract over anyway. Since we don't care about the initial value, we want to abstract over the (|>) that's already applied to a value.


So, the type that we are interested in is that of
```
x :: a
(|>) :: a -> (a -> b) -> b
(x |>) :: (a -> b) -> b
```

So, the type that we want is `(a -> b) -> b`, which makes sense. We have a pipeline that whishes to do something with the raw value it started with of type `a`, and
then will give out the return type. Let's wrap this into a typeclass and see what happens.

```
data Cont r a = Cont {
    cont :: (a -> r) -> r
}
```
the `r` stands for return in the `Cont r a` type.

I'm cheating a little here, since there are two ways to write this - `Cont r a` and `Cont a r`. I picked `Cont r a`, since `Cont a r` cannot be useful as a typeclass.

## Functor instance

Let's try and derive a `Functor` instance for this.

```
fmap :: (a -> b) -> Cont r a -> Cont r b
```
* `(a -> b)` is something that transforms an `a` to a `b`
* `Cont r a` is a "pipeline" that expects an `a` and returns an `r`
* We need to create a `Cont r b` that takes a `b` and returns an `r`

```
fmap :: (a -> b) -> Cont r a -> Cont r b
fmap f c = Cont {
    cont = \aUser -> 
}
```


## Applicative instance
## Monad instance
## So... what?
## next episode - callCC
