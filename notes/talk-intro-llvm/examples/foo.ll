; ModuleID = 'foo.c'
source_filename = "foo.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind readnone uwtable
define dso_local i32 @foo(i32 %x) local_unnamed_addr #0 {
entry:
  %mul = mul nsw i32 %x, 10
  br label %for.body

for.cond.cleanup:                                 ; preds = %for.body
  ret i32 %mul1

for.body:                                         ; preds = %entry, %for.body
  %i.07 = phi i32 [ 0, %entry ], [ %add, %for.body ]
  %p.06 = phi i32 [ 2, %entry ], [ %mul1, %for.body ]
  %mul1 = mul nsw i32 %mul, %p.06
  %add = add nuw nsw i32 %i.07, 2
  %cmp = icmp ult i32 %add, 10
  br i1 %cmp, label %for.body, label %for.cond.cleanup
}

attributes #0 = { norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.0 (https://github.com/llvm-mirror/clang.git 8297e93480c636dc90fd14653c5a66406193363f) (https://github.com/llvm-mirror/llvm 9ab94cbc044f33844e1e08c881a3380f958c054c)"}
