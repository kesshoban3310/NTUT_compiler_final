	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	pushq %rax
	leaq L0, %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	popq %rbp
	ret
	.data
L0:
	.string "foo"
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
