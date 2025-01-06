	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $123, %rax
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	popq %rbp
	ret
	.data
fmt:
	.string "%d\n"
