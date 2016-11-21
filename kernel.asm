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

.setUP:
	mov sp, stack_array
	mov cx, 0
	mov ax, 288
	
	
	; Terminate program
	mov	ah, 0x4C			; DOS API Function number (terminate with status code)
	mov	al, 0				; Parameter (status code 0 == success)
	int	0x21				; Call DOS

	
startTO:
	mov ah, 0x09
	mov dx, taskOne
	int 0x21
	jmp .startTO
	

startTT:
	mov ah, 0x09
	mov dx, taskTwo
	int 0x21
	jmp .startTT
	
	

; a function to switch between tasks A and B 
yield: 
	pushf ;push the flags 
	pusha ; push all GPRs 
	; switch stacks 
	mov [saved_sp_1], sp 
	mov sp, [saved_sp_2] 
	popa 
	popf 
	ret
	
section	.data
saved_sp_1 db 0
saved_sp_2 db 0
taskOne db 13, 10, "task one", 13, 10, "$"
taskTwo db 13, 10, "task two", 13, 10, "$"
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, "$"
stack_array TIMES 4608 db 0