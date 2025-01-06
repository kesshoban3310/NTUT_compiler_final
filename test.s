	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $123, %rax
	pushq %rax
	movq $321, %rax
	popq %rbx
	subq %rax, %rbx
	movq %rbx, %rax
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	popq %rbp
	ret
	.data
fmt:
	.string "%d\n"
