	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $123, %rax
	movq %rax, 0(%rbp)
	movq 0(%rbp), %rax
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
