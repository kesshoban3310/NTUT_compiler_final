	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	setl %al
	movzbq %al, %rax
	testq %rax, %rax
	jz false_str
	leaq true_str, %rdi
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
