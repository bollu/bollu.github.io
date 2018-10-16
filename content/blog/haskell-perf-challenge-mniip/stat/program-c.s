	.text
	.file	"program.c"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	xorl	%esi, %esi
	movl	$2, %r8d
	.p2align	4, 0x90
.LBB0_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	movq	%r8, %rcx
	shrq	$3, %rcx
	movsbl	sieve(%rcx), %ecx
	movl	%r8d, %edx
	andb	$7, %dl
	movzbl	%dl, %edx
	btl	%edx, %ecx
	jb	.LBB0_4
# %bb.2:                                # %if.then
                                        #   in Loop: Header=BB0_1 Depth=1
	addl	$1, %esi
	movq	%r8, %rdx
	imulq	%r8, %rdx
	cmpq	$99999999, %rdx         # imm = 0x5F5E0FF
	ja	.LBB0_4
	.p2align	4, 0x90
.LBB0_3:                                # %for.body6
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%edx, %ecx
	andb	$7, %cl
	movq	%rdx, %rdi
	shrq	$3, %rdi
	movb	$1, %al
	shlb	%cl, %al
	orb	%al, sieve(%rdi)
	addq	%r8, %rdx
	cmpq	$100000000, %rdx        # imm = 0x5F5E100
	jb	.LBB0_3
	.p2align	4, 0x90
.LBB0_4:                                # %for.inc14
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$1, %r8
	cmpq	$100000000, %r8         # imm = 0x5F5E100
	jne	.LBB0_1
# %bb.5:                                # %for.cond.cleanup
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.L.str, %edi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	sieve,@object           # @sieve
	.comm	sieve,12500000,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%ld\n"
	.size	.L.str, 5


	.ident	"clang version 7.0.0 (https://github.com/llvm-mirror/clang.git 290cc27fe21bfce479d30a3194a20045b90afb0e) (https://github.com/llvm-mirror/llvm.git a7fd2c3c2aca4613862c46c75318c154de32c3d7)"
	.section	".note.GNU-stack","",@progbits
