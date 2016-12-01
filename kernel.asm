; CpS 230 Lab 8: Julio Cesar W. College-Student (jwhat331)
;---------------------------------------------------
; Warm-up lab exercise to introduce the basics of
; writing, building, and running 8086/8088 assembly
; code programs on DOSBox.
;---------------------------------------------------
bits 16

; This tells NASM to generate addresses +256 bytes
; into our memory segment (which is how DOS will load
; us into memory).
org	0x100

stack_A	TIMES 256 db 0
stack_B	TIMES 256 db 0

section	.text
start:
	; Print programmer information
	mov ah, 0x09
	mov dx, info
	int 0x21
	
	
	mov sp, stack_A + stack_size
	push task_A
	pushf
	pusha
	
	
	mov [saved_sp], sp
	mov sp, task_B + stack_size
	push task_B
	pushf
	pusha
	
	
	popa
	popf
	
task_B:
	mov ah, 0x09
	mov dx, msg_B
	int 0x21
	call yield
	jmp task_B
	
task_A:
	mov ah, 0x09
	mov dx, msg_A
	int 0x21
	call yield
	jmp task_A
	
	
	

; a function to switch between tasks A and B 
yield: 
	pushf ;push the flags 
	pusha ; push all GPRs 
	; switch stacks 
	xchg [saved_sp], sp 
	popa 
	popf 
	ret
	
section	.data
stack_size equ 256
saved_sp dw 0
msg_A db 13, 10, "task one", 13, 10, "$"
msg_B db 13, 10, "task two", 13, 10, "$"
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, "$"