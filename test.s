	.text
	.globl	main
foo:
	pushq %rbp
	movq %rsp, %rbp
	leaq L0, %rax
	leave
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	call foo
	addq $0, %rsp
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
	.data
L0:
	.string "123"
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
