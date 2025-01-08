	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	setg %al
	movzbq %al, %rax
	testq %rax, %rax
	jz L2
	movq $1, %rax
	movq 0(%rax), %rax
	testq %rax, %rax
	jz L2
	movq $1, %rax
	jmp L3
L2:
	movq $0, %rax
L3:
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
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	setl %al
	movzbq %al, %rax
	testq %rax, %rax
	jnz L6
	movq $1, %rax
	movq 0(%rax), %rax
	testq %rax, %rax
	jnz L6
	movq $0, %rax
	jmp L7
L6:
	movq $1, %rax
L7:
	testq %rax, %rax
	jz L4
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L5
L4:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L5:
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
