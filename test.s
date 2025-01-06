	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq L0, %rsi
	leaq fmt_str, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	popq %rbp
	ret
	.data
L0:
	.string "Hello, World!"
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
