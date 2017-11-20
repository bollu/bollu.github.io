+++
title: "This Week In Simplexhc Nov 5 2017"
date: 2017-11-05T15:38:59+01:00
draft: true
+++


<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=star&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=fork&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


This is going to be a weekly blog post about the status of
[simplexhc](http://github.com/bollu/simplexhc), an STG to LLVM compiler.
Read more about this [in the introduction blog post](https://pixel-druid.com/blog/announcing-simplexhc/).

### Quick progress update:

- Investigating `ackermann` function as a baseline benchmark to see how to improve simplexhc to `GHC` / `clang` performance.
- Trying to replace the `malloc` with something smarter / better.
- Collected a bunch of thoughts about how to go from current LLVM generated to LLVM that's as almost as nice as straight up C code.


### Benchmarks:

I compare the implementation of the Ackermann function in `C`, `haskell` (compiled with `GHC`), and straight up `STG` fed directly to `sxhc`. 

#### Haskell
###### Source files
```hs
{- ackermann.hs -}
module Ackerman where

ackerman :: Int -> Int -> Int
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))
```

```hs
{- main.hs -}
import Ackerman
import GHC.Prim


main :: IO ()
main = print $ ackerman 3 12
```

###### Generated STG

##### C
###### C source
```cpp
#include <iostream>

int ackerman(int m, int n) {
    if (m == 0) return n + 1;
    if (n == 0) return ackerman(m - 1, 1);
    return ackerman(m - 1, ackerman(m, n - 1));
}

int main() {
    std::cout << ackerman(3, 12);
    return 0;
}
```

###### Generated ll
```ll
; ModuleID = 'main.cpp'
source_filename = "main.cpp"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.12.0"

%"class.std::__1::basic_ostream" = type { i32 (...)**, %"class.std::__1::basic_ios.base" }
%"class.std::__1::basic_ios.base" = type <{ %"class.std::__1::ios_base", %"class.std::__1::basic_ostream"*, i32 }>
%"class.std::__1::ios_base" = type { i32 (...)**, i32, i64, i64, i32, i32, i8*, i8*, void (i32, %"class.std::__1::ios_base"*, i32)**, i32*, i64, i64, i64*, i64, i64, i8**, i64, i64 }

@_ZNSt3__14coutE = external global %"class.std::__1::basic_ostream", align 8

; Function Attrs: nounwind readnone ssp uwtable
define i32 @_Z8ackermanii(i32, i32) local_unnamed_addr #0 {
  %3 = icmp eq i32 %0, 0
  br i1 %3, label %6, label %4

; <label>:4:                                      ; preds = %2
  br label %9

; <label>:5:                                      ; preds = %17
  br label %6

; <label>:6:                                      ; preds = %5, %2
  %7 = phi i32 [ %1, %2 ], [ %18, %5 ]
  %8 = add nsw i32 %7, 1
  ret i32 %8

; <label>:9:                                      ; preds = %4, %17
  %10 = phi i32 [ %18, %17 ], [ %1, %4 ]
  %11 = phi i32 [ %13, %17 ], [ %0, %4 ]
  %12 = icmp eq i32 %10, 0
  %13 = add nsw i32 %11, -1
  br i1 %12, label %17, label %14

; <label>:14:                                     ; preds = %9
  %15 = add nsw i32 %10, -1
  %16 = tail call i32 @_Z8ackermanii(i32 %11, i32 %15)
  br label %17

; <label>:17:                                     ; preds = %14, %9
  %18 = phi i32 [ %16, %14 ], [ 1, %9 ]
  %19 = icmp eq i32 %13, 0
  br i1 %19, label %5, label %9
}

; Function Attrs: norecurse ssp uwtable
define i32 @main() local_unnamed_addr #1 {
  %1 = tail call i32 @_Z8ackermanii(i32 3, i32 12)
  %2 = tail call dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEElsEi(%"class.std::__1::basic_ostream"* nonnull @_ZNSt3__14coutE, i32 %1)
  ret i32 0
}

declare dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEElsEi(%"class.std::__1::basic_ostream"*, i32) local_unnamed_addr #2

attributes #0 = { nounwind readnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { norecurse ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"PIC Level", i32 2}
!1 = !{!"Apple LLVM version 9.0.0 (clang-900.0.38)"}
```
##### `sxhc`

###### input `stg`
```hs
    case aint () of
            0 -> primAdd (bint 1);
            aval -> case bint () of
                0 -> case primSubtract (aval 1) of
                        adec -> ackerman (adec 1);;
                bval -> case primSubtract (bval 1) of
                            bdec -> case primSubtract (aval 1) of
                                        adec -> case ackerman (aval bdec) of
                                                    bnew -> ackerman (adec bnew);;;;;
};


binding main = \() -> Boxed {
    case ackerman (3 9) of
        x -> printInt (x);
};
```

###### output `ll`
```c

```
