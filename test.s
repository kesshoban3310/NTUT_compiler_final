	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $1, %rax
	movq %rax, -8(%rbp)
	subq $8, %rsp
	movq $2, %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
	ret
	.data
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
true_str:
	.string "True\n"
false_str:
	.string "False\n"
