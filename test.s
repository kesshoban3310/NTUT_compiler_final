	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq L1, %rsi
	leaq L0, %rdi
	movq $0, %rax
	call strcat
	movq %rax, %rsi
	leaq fmt_str, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	popq %rbp
	ret
	.data
L0:
	.string "foo"
L1:
	.string "bar"
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
true_str:
	.string "True\n"
false_str:
	.string "False\n"
