; ModuleID = 'program.c'
source_filename = "program.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@sieve = common dso_local local_unnamed_addr global [12500000 x i8] zeroinitializer, align 16
@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

; Function Attrs: nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
entry:
  br label %for.body

for.cond.cleanup:                                 ; preds = %for.inc14
  %call = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i32 %count.1)
  ret i32 0

for.body:                                         ; preds = %for.inc14, %entry
  %i.034 = phi i64 [ 2, %entry ], [ %inc15, %for.inc14 ]
  %count.033 = phi i32 [ 0, %entry ], [ %count.1, %for.inc14 ]
  %0 = lshr i64 %i.034, 3
  %arrayidx = getelementptr inbounds [12500000 x i8], [12500000 x i8]* @sieve, i64 0, i64 %0
  %1 = load i8, i8* %arrayidx, align 1, !tbaa !2
  %conv = sext i8 %1 to i32
  %2 = trunc i64 %i.034 to i32
  %sh_prom = and i32 %2, 7
  %shl = shl i32 1, %sh_prom
  %and1 = and i32 %shl, %conv
  %tobool = icmp eq i32 %and1, 0
  br i1 %tobool, label %if.then, label %for.inc14

if.then:                                          ; preds = %for.body
  %inc = add nsw i32 %count.033, 1
  %mul = mul nsw i64 %i.034, %i.034
  %cmp331 = icmp ult i64 %mul, 100000000
  br i1 %cmp331, label %for.body6, label %for.inc14

for.body6:                                        ; preds = %if.then, %for.body6
  %j.032 = phi i64 [ %add, %for.body6 ], [ %mul, %if.then ]
  %3 = trunc i64 %j.032 to i32
  %sh_prom8 = and i32 %3, 7
  %shl9 = shl i32 1, %sh_prom8
  %4 = lshr i64 %j.032, 3
  %arrayidx11 = getelementptr inbounds [12500000 x i8], [12500000 x i8]* @sieve, i64 0, i64 %4
  %5 = load i8, i8* %arrayidx11, align 1, !tbaa !2
  %6 = trunc i32 %shl9 to i8
  %conv13 = or i8 %5, %6
  store i8 %conv13, i8* %arrayidx11, align 1, !tbaa !2
  %add = add nuw nsw i64 %j.032, %i.034
  %cmp3 = icmp ult i64 %add, 100000000
  br i1 %cmp3, label %for.body6, label %for.inc14

for.inc14:                                        ; preds = %for.body6, %if.then, %for.body
  %count.1 = phi i32 [ %count.033, %for.body ], [ %inc, %if.then ], [ %inc, %for.body6 ]
  %inc15 = add nuw nsw i64 %i.034, 1
  %exitcond = icmp eq i64 %inc15, 100000000
  br i1 %exitcond, label %for.cond.cleanup, label %for.body
}

; Function Attrs: nounwind
declare dso_local i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #1

attributes #0 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.0.0 (https://github.com/llvm-mirror/clang.git 290cc27fe21bfce479d30a3194a20045b90afb0e) (https://github.com/llvm-mirror/llvm.git a7fd2c3c2aca4613862c46c75318c154de32c3d7)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
