	.text
	.globl	main
test1:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $1, %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
	ret
test2:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $2, %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $3, %rax
	movq %rax, -8(%rbp)
	call test1
	addq $0, %rsp
	call test2
	addq $0, %rsp
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
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
