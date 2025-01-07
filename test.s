	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
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
	movq $1, %rax
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
	movq $1, %rax
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
	movq $1, %rax
	testq %rax, %rax
	jz L6
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L7
L6:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L7:
	movq $1, %rax
	testq %rax, %rax
	jz L8
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L9
L8:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L9:
	movq $1, %rax
	testq %rax, %rax
	jz L10
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L11
L10:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L11:
	movq $0, %rax
	testq %rax, %rax
	jz L12
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L13
L12:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L13:
	movq $0, %rax
	testq %rax, %rax
	jz L14
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L15
L14:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L15:
	movq $0, %rax
	testq %rax, %rax
	jz L16
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L17
L16:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L17:
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
