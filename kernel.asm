; CpS 230 Lab 10: Alice B. College-Student (acoll555)
;---------------------------------------------------
; Timer-interrupt-hook demo.  Updates a 16-bit count
; 18.2 times per second (thanks to the INT 8 timer
; interrupt); displays the current count in the
; upper-left corner of the text-mode screen while
; the "main" program waits for user input...
;---------------------------------------------------
bits 16

; COM program (CS = DS = SS), so we need an ORG 0x100
org	0

; Where to find the INT 8 handler vector within the IVT [interrupt vector table]
IVT8_OFFSET_SLOT	equ	4 * 8			; Each IVT entry is 4 bytes; this is the 8th
IVT8_SEGMENT_SLOT	equ	IVT8_OFFSET_SLOT + 2	; Segment after Offset

stack_A	TIMES 256 db 0
stack_B	TIMES 256 db 0
stack_C	TIMES 256 db 0


section	.text
start:

	; Print programmer information
	mov dx, info
	call puts
	
	mov	ax, cs
	mov	ds, ax

	; Set ES=0x0000 (segment of IVT)
	mov	ax, 0x0000
	mov	es, ax
	
	mov sp, stack_A + stack_size
	pushf
	push cs
	push task_A
	pusha
	push ds
	push es
	
	mov [saved_sp], sp
	mov sp, stack_B + stack_size
	
	
	; TODO Install interrupt hook
	; 0. disable interrupts (so we can't be...INTERRUPTED...)
	; 1. save current INT 8 handler address (segment:offset) into ivt8_offset and ivt8_segment
	; 2. set new INT 8 handler address (OUR code's segment:offset)
	; 3. reenable interrupts (GO!)
	cli
	mov ax, [es:IVT8_OFFSET_SLOT]
	mov [ivt8_offset], ax
	mov ax, [es:IVT8_SEGMENT_SLOT]
	mov [ivt8_segment], ax
	mov word [es:IVT8_OFFSET_SLOT], timer_isr
	mov [es:IVT8_SEGMENT_SLOT], cs
	sti
	
	
	; Start the "main" program that prompts for user input until an empty line is entered	
task_B:
	mov dx, msg_B
	call puts
	;call yield
	jmp task_B
	
task_A:
	mov dx, msg_A
	call puts
	;call yield
	jmp task_A
	

; INT 8 Timer ISR (interrupt service routine)
; cannot clobber anything; must CHAIN to original caller (for interrupt acknowledgment)
; DS/ES == ???? (at entry, and must retain their original values at exit)
timer_isr:
	; TODO: save any registers we clobber to the stack
	; TODO: print current counter value in upper-left corner of the screen	
	; TODO: increment current counter value	
	; TODO: restore any registers we clobbered from the stack
	pusha
	push ds
	push es	
	xchg [saved_sp], sp 
	pop es
	pop ds
	popa
	
	; Chain (i.e., jump) to the original INT 8 handler 
	jmp	far [cs:ivt8_offset]	; Use CS as the segment here, since who knows what DS is now


; print NUL-terminated string from DS:DX to the screen (at the current "cursor" location) using BIOS INT 0x10
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
ivt8_offset	dw	0
ivt8_segment	dw	0

stack_size equ 256
saved_sp dw 0
msg_A db 13, 10, "task one", 13, 10, 0
msg_B db 13, 10, "task two", 13, 10, 0
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, 0