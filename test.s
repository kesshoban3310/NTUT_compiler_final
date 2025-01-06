	.text
	.globl	main
main:
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $123, %rax
	call print_int
	popq %rbp
	ret
	.data
