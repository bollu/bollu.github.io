+++
title = "Ghc Micro-Optimisations, or, why proc points suck"
date = "2017-12-28T12:33:21+05:30"
draft = true

+++

Note: I'm putting this up because college has started again, and I'm not finding enough time to polish this up.
However, I do want to come back to this and work on this. I'm publishing this semi-unformatted because I think
the finding is interesting.


# Introduction


On working on [`simplexhc`](http://github.com/bollu/simplexhc-cpp), a custom haskell-to-llvm compiler, I've noticed several strange
performance behaviours of [`GHC (Glasgow haskell compiler)`](https://wiki.haskell.org/GHC). Under certain benchmarks, GHC is slower than C by **2x to 3x**, and this GHC slowdown **grows non-linearly with problem size**. This is the story of identifying the source of the slowdown. I don't yet have a good solution, however.

There were the compilers used where gathering data:
- GHC, `-O3` pass pipeline, native backend.
- Clang, with `-O3` pass pipeline.


# A motivating problem: ackermann

Let us consider the ackermann function as a function that should have similar performance behaviour between `GHC` and `C`.

### Haskell
```hs
ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

main :: IO ()
main = print $ ackermann 3 11

```

### Canonical C
```
#include <stdio.h>

int ackerman(int m, int n) {
    if (m == 0) return n + 1;
    if (m > 0 && n  == 0) return ackerman(m - 1, 1);
    return ackerman(m - 1, ackerman(m, n - 1));
}

int main() {
    printf("%d", ackerman(3, 11));
    return 0;
}
```

On executing the two executables with `perf`, here are the numbers:
### Perf numbers - ackermann(3, 13)

###### Haskell
I compile with `ghc -O3 
```
╭─bollu@cantordust ~/work/sxhc/haskell-microbenchmarks/ackerman/hs  ‹master*› 
╰─$ sudo perf stat -d ./a.out                             130 ↵
65533

 Performance counter stats for './a.out':

      10842.395693      task-clock (msec)         #    1.000 CPUs utilized          
                19      context-switches          #    0.002 K/sec                  
                 0      cpu-migrations            #    0.000 K/sec                  
             1,137      page-faults               #    0.105 K/sec                  
   36,39,57,50,625      cycles                    #    3.357 GHz                      (49.97%)
   75,81,29,97,093      instructions              #    2.08  insn per cycle           (62.48%)
   23,15,94,07,810      branches                  # 2136.005 M/sec                    (62.48%)
         46,61,682      branch-misses             #    0.02% of all branches          (62.51%)
   10,23,55,71,823      L1-dcache-loads           #  944.032 M/sec                    (62.46%)
      91,81,89,927      L1-dcache-load-misses     #    8.97% of all L1-dcache hits    (25.02%)
       2,39,32,901      LLC-loads                 #    2.207 M/sec                    (25.00%)
            27,686      LLC-load-misses           #    0.12% of all LL-cache hits     (37.47%)

      10.841942305 seconds time elapsed
```

###### C
```
╭─bollu@cantordust ~/work/sxhc/haskell-microbenchmarks/ackerman/c  ‹master*› 
╰─$ sudo perf stat -d ./a.out 
65533
 Performance counter stats for './a.out':

       2639.699328      task-clock (msec)         #    1.000 CPUs utilized          
                 4      context-switches          #    0.002 K/sec                  
                 0      cpu-migrations            #    0.000 K/sec                  
               305      page-faults               #    0.116 K/sec                  
    8,72,38,76,574      cycles                    #    3.305 GHz                      (50.06%)
   27,14,60,77,471      instructions              #    3.11  insn per cycle           (62.61%)
    8,57,29,19,162      branches                  # 3247.688 M/sec                    (62.66%)
         78,25,907      branch-misses             #    0.09% of all branches          (62.72%)
    2,86,47,68,221      L1-dcache-loads           # 1085.263 M/sec                    (62.29%)
      65,40,28,017      L1-dcache-load-misses     #   22.83% of all L1-dcache hits    (24.86%)
         60,00,153      LLC-loads                 #    2.273 M/sec                    (24.85%)
             3,237      LLC-load-misses           #    0.05% of all LL-cache hits     (37.51%)

       2.639808518 seconds time elapsed
```

We have a **5x** slowdown between GHC and C. This is nuts, so let's try and look at problem scaling. That is,
how performance degrades as problem size changes.


### Perf scaling as problem size increases (all numbers in seconds):

<iframe width="900" height="800" frameborder="0" scrolling="no" src="//plot.ly/~bollu/74.embed"></iframe>

The scaling is non-linear, and GHC gets way worse with performance in comparison to C. This implies that 
we should probably look at the assembly and see what the hell is going on

# How does haskell compile?

I've written about how haskell compiles in detail [here](TODO: add link to blog post on compilation).

The long and short of it is that every *lazy value* becomes a *function* thaat needs to be evaluated.
Every *case / patten match* becomes a *continuation*, that needs to be entered into once the lazy value is
fully computed.

so, rougly,

```hs
case x of
    xval -> rhs
```

compiles into:

```c
pushContinuation(xval_rhs);
x();

xval_rhs(xval) {
    //rhs here.
}

x() {
    xval = /*compute*/
    cont = popContinuation();
    cont(xval);
}
```

Each such point where we need to create a continuation is called a *proc point*. These seem to be the bane of
performance, but unfortuantely, this is how we need to compile because of laziness. 

### C equivalent of haskell ackermann compilation

Here is the source code listing of the C version of the haskell code. Note that the performance closely matches that of GHC.
So, from now on, when I explain code in the blog post, I will always benchmark using the C version, since it's an easier to talk
about C performance.

<iframe height="1000px" width="100%" src="https://repl.it/@bollu/Acermann-Haskell-compliation-C-equivalent?lite=true" scrolling="no" frameborder="no" allowtransparency="true" allowfullscreen="true" sandbox="allow-forms allow-pointer-lock allow-popups allow-same-origin allow-scripts allow-modals"></iframe>

### Performance between GHC and haskell-C-equivalent:
- C code was compiled with `clang -O3`
- Haskell code was compiled what `GHC -O3`

<iframe width="900" height="800" frameborder="0" scrolling="no" src="//plot.ly/~bollu/78.embed"></iframe>

Notice that the haskell-like-C code scales much better than the GHC version, probably due to GC effects, and because of better
instruction selection with LLVM. 

There is still a **1.4x** slowdown between haskell-like-C and C.


### Explaining this performance difference
In theory, picking the right instructions should lead to better performance, but it should be at maximum, around 10% of a performance difference. I suspected that there was something wrong with the way in which we access memory. 

The obvious culprit is the stack that we manage manually. Since we perform stack manipulations ourselves without using `push`, `pop`,
`call`, and `ret`, I suspected this was screwing things up. However, it was quite possible that there was something subtler going on.

### Teasing apart the solution: Is it the stack, or is it something else?

To tell the difference, I created a program where we use our custom stack, but do no real compute. I chose to load from a volatile
variable as "compute" to hinder all optimistaions. I wrote this program in the regular C style and the continuation style and benchmarked the results. I do not choose to embed the files because the code is straightforward. The links to the files are posted below.

- [Link to C code that was run](https://github.com/bollu/haskell-microbenchmarks/blob/16bafdb6b0f664175d37256aac9f7cacd1e94596/nop-call-stack/c/test.c)
- [Link to haskell-like-C code that was run](https://github.com/bollu/haskell-microbenchmarks/blob/16bafdb6b0f664175d37256aac9f7cacd1e94596/nop-call-stack/haskell-like-c/test.c)


<iframe width="900" height="800" frameborder="0" scrolling="no" src="//plot.ly/~bollu/80.embed"></iframe>

It is interesting that this is ~10% of overhead, at worst. However, this does not explan the **1.4x** and greater slowdown that we have seen between `C` and `haskell-like-C`

Something else is going wrong. It's time to read some assembly :)
### Reading the assembly

I used Intel VTune to get instruction-level information about memory latency (as I was now suspecting that it is memory usage patterns that
was leading to the performance difference). I floundered around with this for a while, I will only post the final results I discovered, with a lot of help from [Ben Gamari](http://bgamari.github.io/).

###### High level overview between C & Haskell-like-C:
- Data is exported from VTune.
![Memory access delta 2](/ghc-micro-optimisations-stack/mem-usage-delta.png)
(I don't really know how to export data from VTune, and googling did not help, so this is the best I can do right now).

- Dark blue is `C`, light blue is `haskell-like-C`.
- Notice that `haskell-like-C` performs more data transfer (bandwidth usage) and **takes longer** performing the transfers (latency).

- Let's check instruction-level differences with `perf`.
- Code compiled with `-O2` to have cleaner code. Code still exhibits similar characteristics.
`O3` in clang is `O2` with much larger inlining and unrolling sizes.

### C ackermann code with perf:

<!--![C ackermann code VTune](/ghc-micro-optimisations-stack/c-cpu-time.png)-->

```asm
       │    0000000000400510 <ackerman>:
       │    ackerman():
 12.34 │      push   %rbx
 10.83 │      test   %edi,%edi
       │    ↓ je     32
  0.03 │      data16 nopw %cs:0x0(%rax,%rax,1)
  5.49 │10:   lea    -0x1(%rdi),%ebx
  0.09 │      test   %edi,%edi
       │    ↓ jle    20
  8.29 │      mov    $0x1,%eax
  0.09 │      test   %esi,%esi
       │    ↓ je     28
  5.52 │20:   add    $0xffffffff,%esi
  1.18 │    → callq  ackerman
 13.99 │28:   mov    %eax,%esi
  0.06 │      mov    %ebx,%edi
  1.00 │      test   %ebx,%ebx
       │    ↑ jne    10
  0.41 │    ↓ jmp    34
       │32:   mov    %esi,%eax
 12.90 │34:   add    $0x1,%eax
  0.06 │      pop    %rbx
 27.72 │    ← retq   
```

The two hottest centers are around context switches:

```
...
1.18  │    → callq  ackerman
13.99 │28:   mov    %eax,%esi
...
27.72 │    ← retq   
```
### Haskell-like C ackermann code with perf:

###### `case_ackerman_aval_bdec`:

```
       │    case_ackerman_aval_bdec():
  4.95 │      test   %esi,%esi
  5.40 │    ↓ je     9b
       │      mov    %esi,%eax
       │      nop
 27.78 │10:   lea    -0x1(%rax),%ecx
  2.00 │      test   %edx,%edx
  1.16 │    ↓ je     8a
 10.08 │      movslq g_ret_sp,%r9
       │      mov    %r9,%r8
       │      mov    %edx,%edi
  0.03 │      test   $0x1,%dl
       │    ↓ je     48
       │      lea    0x1(%r9),%r8
       │      mov    %r9,%rsi
 10.56 │      shl    $0x4,%rsi
 13.27 │      movq   $0x4005a0,0x601050(%rsi)
       │      mov    %rcx,0x601058(%rsi)
  0.34 │      lea    -0x1(%rdx),%edi
       │48:   cmp    $0x1,%edx
       │    ↓ je     80
  0.79 │      shl    $0x4,%r8
  5.53 │      lea    0x601068(%r8),%rsi
  6.50 │      nop
 11.61 │60:   movq   $0x4005a0,-0x18(%rsi)
       │      mov    %rcx,-0x10(%rsi)
       │      movq   $0x4005a0,-0x8(%rsi)
       │      mov    %rcx,(%rsi)
       │      add    $0x20,%rsi
       │      add    $0xfffffffe,%edi
       │    ↑ jne    60
       │80:   add    %edx,%r9d
       │      mov    %r9d,g_ret_sp
       │8a:   add    $0xffffffffffffffff,%rax
       │      mov    $0x1,%edx
       │      test   %ecx,%ecx
       │    ↑ jne    10
       │9b:   movslq g_ret_sp,%rax
       │      add    $0xffffffffffffffff,%rax
       │      mov    %eax,g_ret_sp
       │      shl    $0x4,%rax
       │      mov    0x601050(%rax),%rdi
       │      mov    0x601058(%rax),%rsi
       │      add    $0x1,%edx
       │    ↑ jmpq   *%rdi                                                   
```

###### `pushReturn`
```
       │    pushReturn():
 15.26 │      movslq g_ret_sp,%rax
  1.73 │      lea    0x1(%rax),%ecx
  0.15 │      mov    %ecx,g_ret_sp
  2.41 │      shl    $0x4,%rax
 13.35 │      mov    %rdi,0x601050(%rax)
 37.59 │      mov    %rsi,0x601058(%rax)
 29.51 │    ← retq
```
###### `popReturn`
```
       │    popReturn():
 40.01 │      movslq g_ret_sp,%rcx
       │      add    $0xffffffffffffffff,%rcx
       │      mov    %ecx,g_ret_sp
 12.44 │      shl    $0x4,%rcx
 16.88 │      mov    0x601050(%rcx),%rax
 29.90 │      mov    0x601058(%rcx),%rdx
  0.77 │    ← retq

```

Clearly, memory stuff is expensive: `pushReturn` and `popReturn` spend most of their time in memory fetching.


###### Analysis

We can see that it is the load and store from memory that is consuming a lot of the time. However, on taking a deeper look, it appears that there are *more* loads in the haskell case than in the C case. If we trace the register usage across the program, the real problem emerges: Clang is able to allocate registers across the functions (TODO: insert function names) such that *no data needs to be passed on the stack*. However, in the Haskell case, we clearly use the "global stack" to pass data. This is **one extra load** per function call!. This obviously leads to a *huge* overhead across runs of the program.

### How do we teach LLVM about proc point


I'm not entirely sure how to generate better code than what I already have. If we wish to use continuations, we _will_ have that indirect branch. 

### Closing thoughts


Much thanks to Ben Gamari, who helped me debug this stuff, and taught me a lot about perf in the process.

If anyone has any solutions to this thorny aproblem, please e-mail me (`siddu.druid@gmail.com`), or leave a message here.

I suspect that [SPJ's and Kavon's work on proc-points](https://icfp17.sigplan.org/event/hiw-2017-native-support-for-explicit-stacks-in-llvm) will possibly alleviate this. I plan on pinging them after putting this up so I can get some feedback.
