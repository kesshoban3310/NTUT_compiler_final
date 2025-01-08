	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	leaq L0, %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_str, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
	leave
	ret
	.data
L0:
	.string "foo"
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
true_str:
	.string "True\n"
false_str:
	.string "False\n"
none_str:
	.string "None\n"
