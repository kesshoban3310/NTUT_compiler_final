	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	andq %rbx, %rax
	testq %rax, %rax
	jz L0
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L1
L0:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L1:
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	orq %rbx, %rax
	testq %rax, %rax
	jz L2
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L3
L2:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L3:
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
