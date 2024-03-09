	.text
	.file	"ite.c"
	.globl	foo                     # -- Begin function foo
	.p2align	4, 0x90
	.type	foo,@function
foo:                                    # @foo
	.cfi_startproc
# %bb.0:                                # %entry
	cmpl	$9, g(%rip)
	movl	$30, %ecx
	movl	$40, %eax
	cmovll	%ecx, %eax
	retq
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo
	.cfi_endproc
                                        # -- End function
	.type	g,@object               # @g
	.comm	g,4,4

	.ident	"clang version 9.0.0 (https://github.com/llvm-mirror/clang.git 8297e93480c636dc90fd14653c5a66406193363f) (https://github.com/llvm-mirror/llvm 9ab94cbc044f33844e1e08c881a3380f958c054c)"
	.section	".note.GNU-stack","",@progbits
