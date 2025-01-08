	.text
	.globl	main
test:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $1, %rax
	movq %rax, -8(%rbp)
	subq $8, %rsp
	movq $2, %rax
	movq %rax, -16(%rbp)
	subq $8, %rsp
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	leaq L0, %rax
	movq %rax, %rsi
	leaq fmt_str, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $1, %rax
	movq %rax, -8(%rbp)
	subq $8, %rsp
	movq $2, %rax
	movq %rax, -16(%rbp)
	call test
	addq $0, %rsp
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movq $1, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
	ret
	.data
L0:
	.string "Hello, World!"
fmt_int:
	.string "%d\n"
fmt_str:
	.string "%s\n"
true_str:
	.string "True\n"
false_str:
	.string "False\n"
