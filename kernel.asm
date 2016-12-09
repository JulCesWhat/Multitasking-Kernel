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
countdown  equ	8000h ; approx 36 interrupts per second

CpR	equ	80	; 80 characters per row
RpS	equ	25	; 25 rows per screen
BpC	equ	2	; 2 bytes per character

; Compute starting offset to store "BJU!" in VRAM centered on row 12
MESSAGE_START	equ	(1 * CpR * BpC) + (1 * BpC)


CHARS	equ	4	; number of characters in the message "BJU!"
spacer  dw  0  ; filling up those extra lines...
placed  dw  0

section	.text
start:


	; Print programmer information
	mov dx, info
	call puts
	
	mov	ax, cs
	mov	ds, ax
	
	mov sp, stack_A + stack_size
	pushf
	push cs
	push task_A
	pusha
	push ds
	push es
	;mov [saved_sp], sp
	mov [stack_SP + 6], sp
		

	mov	ax, 0xb800
	mov	es, ax

	mov sp, stack_C + stack_size
	pushf
	push cs
	push task_C
	pusha
	push ds
	push es	
	mov [stack_SP + 4], sp

	; Set ES=0x0000 (segment of IVT)
	mov	ax, 0x0000
	mov	es, ax

	mov sp, stack_D + stack_size
	pushf
	push cs
	push task_D
	pusha
	push ds
	push es	
	mov [stack_SP + 2], sp
	
	
	mov sp, stack_B + stack_size
	mov [stack_SP], sp
	

	   cli
	   mov	al,00110110b  ; bit 7,6 = (00) timer counter 0
			      ; bit 5,4 = (11) write LSB then MSB
			      ; bit 3-1 = (011) generate square wave
			      ; bit 0 = (0) binary counter
	   out	43h,al	      ; prep PIT, counter 0, square wave&init count
	   jmp	$+2
	   mov	cx,countdown  ; default is 0x0000 (65536) (18.2 per sec)
			      ; interrupts when counter decrements to 0
	   mov	al,cl	      ; send LSB of timer count
	   out	40h,al
	   jmp	$+2
	   mov	al,ch	      ; send MSB of timer count
	   out	40h,al
	   jmp	$+2
	   sti
	
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

	mov ax, [printb]
	cmp ax, 0
	je .end
	mov dx, msg_B
	call puts
	mov ax, 0
	mov [printb], ax
	.end:
	
	jmp task_B
	
task_A:

	mov ax, [printa]
	cmp ax, 0
	je .end 
	mov dx, msg_A
	call puts
	mov ax, 0
	mov [printa], ax
	.end:
	jmp task_A
	
task_C:
	
	; Clear screen to black (copy 80*25*2 byte of ZERO to the framebuffer)
	mov	al, 0
	mov	cx, CpR*RpS*BpC
	mov	di, 0
	rep	stosb
	
	; "BJU!" in bright blue on white, center of screen, in text mode
	mov	di, MESSAGE_START
	mov	ah, 0x0A	; background = 1 (blue), foreground = 15 (bright white)
	
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'o'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '.'
	stosw
    mov	al, '/'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'y'
	stosw
    mov	al, '/'
	stosw
    mov	al, '`'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ':'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'h'
	stosw
    mov	al, '/'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers 
    
    mov	al, ' '
	stosw
    mov	al, '`'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'y'
	stosw
    mov	al, '.'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, '/'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 's'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, '/'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, '-'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, '.'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers 
    
    mov	al, ' '
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, '-'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, '.'
	stosw
    
    call put_spacers
    
    mov	al, '-'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, '+'
	stosw

    call put_spacers

    mov	al, '.'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, '/'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, ':'
	stosw
    
    call put_spacers 
    
    mov	al, ' '
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, '+'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ':'
	stosw
    mov	al, 's'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, '`'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, ':'
	stosw
    mov	al, ':'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '.'
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '/'
	stosw
    mov	al, 'y'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '/'
	stosw
    mov	al, '/'
	stosw
    mov	al, '+'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, '+'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, '.'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers 
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, '.'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '`'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, '/'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ':'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 'o'
	stosw
    mov	al, 's'
	stosw
    mov	al, 's'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '+'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'o'
	stosw
    mov	al, '`'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers 
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, '`'
	stosw
    mov	al, 'y'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, '-'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
    
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ':'
	stosw
    mov	al, 'h'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'd'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'N'
	stosw
    mov	al, 'm'
	stosw
    mov	al, 'y'
	stosw
    mov	al, '.'
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    mov	al, ' '
	stosw
    
    call put_spacers
	
	mov ax, 1
	mov [printa], ax
	mov [printb], ax
	
	jmp task_C
	
put_spacers:
    mov cx, 0
    mov [placed], cx
    mov dx, 48
    mov [spacer], dx
    jmp .loopy
    
.loopy:
    inc cx
    mov [placed], cx
    mov al, ' '
    stosw
    cmp dx, cx
    jg .loopy
    jmp .goback
    
.goback:
    ret

task_D:
	xor     si, si
	mov bx, [place_notes]						; note count

	; set up timer command register and counter register

	;set up	
	mov al, 0b6h						; set 8253 command register
	out 43h, al							; for channel 2, mode 3

	.nloop:
		mov     ax, 34dch				; low part of clock freq.
		mov     dx, 12h					; hight part of clock freq.
		push	bx
		mov		bx, [notes + si]		; get note from data segment
		div     bx
		pop		bx
		out     42h, al					; 8253 command register (low byte)
		mov     al, ah
		out     42h, al					; 8253 command regsieter (high byte)
		
		; turn on low bits in 8255 output port

		in      al, 61h					; read current value of 8255 port
		or      al, 3					; clear low bits
		out     61h, al					; send new value to port

		; loop while note is sounding

		mov cx, [noteLength + si]
		.loopLength:
		push cx
		;mov     cx, 6d60h
		mov 	cx, 20D0h
		.rpta:							 ; 1/10 sec delay
			dec cx
			cmp cx, 0
			jne .rpta
		pop cx
		dec cx
		cmp cx, 0
		jne .loopLength

		; turn off speaker, check note count, set up next note

		xor     al, 3 
		out     61h, al					; turn off speaker


		;mov     cx, 0af0h				; 1/100 sec delay
		mov cx, [silenceLength + si]
		.length:
		push cx
		;mov cx, 6d60h
		mov cx, 20D0h
		.rptb:							
			dec cx
			cmp cx, 0
			jne .rptb
		pop cx
		dec cx
		cmp cx, 0
		jne .length

				
		inc si							; increment note pointer
		inc si
		dec bx							; decrement note counter
		cmp bx, 0
		jne .nloop	
	
	jmp task_D
	

; INT 8 Timer ISR (interrupt service routine)
; cannot clobber anything; must CHAIN to original caller (for interrupt acknowledgment)
; DS/ES == ???? (at entry, and must retain their original values at exit)
timer_isr:

	pusha
	push ds
	push es
	
	
	mov bx, [num_sp]
	shl bx, 1
	mov [stack_SP + bx], sp 
	mov bx, [num_sp]
	add bx, 1

	cmp bx, 4
	jne .change_sp
	mov bx, 0
	
.change_sp:
	mov [num_sp], bx
	shl bx, 1

	mov sp, [stack_SP + bx]	
	
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

printa dw 0
printb dw 0

stack_A	TIMES 256 dw 0
stack_B	TIMES 256 dw 0
stack_C	TIMES 256 dw 0
stack_D	TIMES 256 dw 0
stack_SP	TIMES 32 dw 0

ivt8_offset	dw	0
ivt8_segment	dw	0

stack_size equ 256
num_sp dw 0
saved_sp dw 0
msg_A db 13, 10, "task one", 13, 10, 0
msg_B db 13, 10, "task two", 13, 10, 0
msg_C db 13, 10, "task three", 13, 10, 0
msg_D db 13, 10, "task four", 13, 10, 0
info db 13, 10, "CpS 230 Lab 4: Julio C W. College-Student (jwhat331)", 13, 10, 0

;Music section 
place_notes dw 59


noteLength			dw	4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 10, 14, 4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 8, 2, 4, 3, 4, 4, 6, 4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 8, 3, 6, 8, 10, 12, 12, 12, 12, 4, 4, 2, 4, 2
silenceLength	    dw	4, 4, 4, 4, 2, 4, 5, 2, 4, 2, 2, 2, 5, 2, 2, 2, 4, 4, 4, 4, 4, 4, 2, 4, 5, 2, 4, 5, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 5, 2, 4, 5, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 2
notes			    dw	440, 587, 440, 587, 440, 587, 440, 415, 440, 440, 415, 440, 392, 370, 392, 370, 349, 293, 440, 587, 440, 587, 440, 587, 440, 415, 440, 392, 392, 370, 392, 523, 466, 449, 415, 440, 587, 440, 587, 440, 587, 440, 415, 440, 523, 523, 440, 415, 349, 294, 294, 349, 440, 523, 622, 587, 415, 440, 349



