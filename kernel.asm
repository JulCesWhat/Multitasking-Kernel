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

section	.text
start:
	; Print programmer information
	mov ah, 0x09
	mov dx, info
	int 0x21
	
	
	mov sp, 0x200
	push startT2
	pushf
	pusha
	
	
	mov [saved_sp], sp
	mov sp, 0x300
	push startT1
	pushf
	pusha
	
	
	popa
	popf
	
startT1:
	mov ah, 0x09
	mov dx, taskOne
	int 0x21
	call yield
	jmp startT1
	

startT2:
	mov ah, 0x09
	mov dx, taskTwo
	int 0x21
	call yield
	jmp startT2
	
	

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
saved_sp dw 0
saved_sp_2 dw 0
taskOne db 13, 10, "task one", 13, 10, "$"
taskTwo db 13, 10, "task two", 13, 10, "$"
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, "$"