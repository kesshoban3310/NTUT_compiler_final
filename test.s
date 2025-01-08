	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	movq %rax, -8(%rbp)
	movq $2, %rax
	movq %rax, -16(%rbp)
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	popq %rbp
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
