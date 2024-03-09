; ModuleID = 'ite.c'
source_filename = "ite.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@g = common dso_local global i32 0, align 4

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo() #0 {
entry:
  %z = alloca i32, align 4
  %x = alloca i32, align 4
  %0 = load i32, i32* @g, align 4
  %add = add nsw i32 %0, 1
  store i32 %add, i32* %x, align 4
  %1 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %1, 10
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 20, i32* %z, align 4
  br label %if.end

if.else:                                          ; preds = %entry
  store i32 30, i32* %z, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %2 = load i32, i32* %z, align 4
  %add1 = add nsw i32 %2, 10
  ret i32 %add1
}

attributes #0 = { noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.0 (https://github.com/llvm-mirror/clang.git 8297e93480c636dc90fd14653c5a66406193363f) (https://github.com/llvm-mirror/llvm 9ab94cbc044f33844e1e08c881a3380f958c054c)"}
