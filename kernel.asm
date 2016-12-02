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
org	0

stack_A	TIMES 256 db 0
stack_B	TIMES 256 db 0

section	.text
start:

	mov	ax, cs
	mov	ds, ax

	; Print programmer information
	mov dx, info
	call puts	
	
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
	mov dx, msg_B
	call puts
	call yield
	jmp task_B
	
task_A:
	mov dx, msg_A
	call puts
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
	
; print NUL-terminated string from DS:DX to screen using BIOS (INT 10h)
; takes NUL-terminated string pointed to by DS:DX
; clobbers nothing
; returns nothing
puts:
	push	ax
	push	cx
	push	si
	
	mov	ah, 0x0e
	mov	cx, 1		; no repetition of chars
	
	mov	si, dx
.loop:	mov	al, [si]
	inc	si
	cmp	al, 0
	jz	.end
	int	0x10
	jmp	.loop
.end:
	pop	si
	pop	cx
	pop	ax
	ret
	
	
section	.data
stack_size equ 256
saved_sp dw 0
msg_A db 13, 10, "task one", 13, 10, 0
msg_B db 13, 10, "task two", 13, 10, 0
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, 0