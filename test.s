	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $2, %rax
	pushq %rax
	movq $17, %rax
	popq %rbx
	cqto
	idivq %rbx
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
