; CpS 230 Team Project: Andrew Carter and Cesar Whatley
;---------------------------------------------------
; Multi-threading kernel that contains distinct
; tasks, distinguishable yielding, and proper
; interrupt management to display and run
; multiple time-based, input-based, and sequence-
; based tasks nearly simultaneously
;---------------------------------------------------
bits 16

; COM program (CS = DS = SS), so we need an ORG 0x100
org	0

section	.text
start:


	; Print project header information
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
   mov	al,00110110b    ; bit 7,6 = (00) timer counter 0
                        ; bit 5,4 = (11) write LSB then MSB
                        ; bit 3-1 = (011) generate square wave
                        ; bit 0 = (0) binary counter
   out	43h,al	        ; prep PIT, counter 0, square wave&init count
   jmp	$+2 
   mov	cx,countdown    ; default is 0x0000 (65536) (18.2 per sec)
                        ; interrupts when counter decrements to 0
   mov	al,cl	        ; send LSB of timer count
   out	40h,al
   jmp	$+2
   mov	al,ch           ; send MSB of timer count
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
	mov dx, msg_B
	call puts
	
	mov bx, 950
	.rptaaaa:
	 mov cx, 1000
    .rptaa:						 ; 1/10 sec delay
    dec cx
    cmp cx, 0
    jne .rptaa
	dec bx
	cmp bx, 0
	jne .rptaaaa
	
	
	jmp task_B
	
	
task_A:

	.getRPNInput:
	mov ah, 0x00
	int 0x16
	
	push ax
	mov	cx, 1
	mov	ah, 0x0e
	int	0x10
	pop ax
	
	cmp al, 0x30
	jl .notNumber
	cmp al, 0x39
	jg .notNumber
	mov bl, al
	xor ax, ax
	sub bx, 0x30
	mov ax, bx
	call numCreate
	mov ax, 1
	mov [numPrev], ax

	jmp .getRPNInput
	ret 


	.notNumber:

	cmp al, 0x2B				;Add to values from the stack
	je .add

	cmp al, 0x2D				;Substract to values from the stack
	je .substract

	cmp al, 0x2A				;Multiply to values from the stack
	je .multiply

	cmp al, 0x2F				;Divide to values from the stack
	je .divide
	
	cmp al, 0x7E
	je .netate

	cmp al, 0x20				;Space, enter, carrige return
	je .doNothing
		
	cmp al, 0x0D				;Carrige return
	je .printNumber

	jmp .otherThanSpace			;jump to other from space

;--------------------------
;		ADD SECTION
;--------------------------
	.add:
	mov bx, [numPrev]
	cmp bx, 0
	je .addNow
	call getNumCreate
	call push_val

	.addNow:
	call addition
	mov ax, 0
	mov [numPrev], ax
	mov bx, [oppSuccess]
	cmp bx, 1
	je .printNumber

	jmp .getRPNInput		;if it can't add :)

;--------------------------
;		SUBSTRACTION SECTION
;--------------------------
	.substract:
	mov bx, [numPrev]
	cmp bx, 0
	je .substracNow
	call getNumCreate
	call push_val

	.substracNow:
	call substraction
	mov ax, 0
	mov [numPrev], ax
	mov bx, [oppSuccess]
	cmp bx, 1
	je .printNumber

	jmp .getRPNInput		;if it can't substract :)


;--------------------------
;		MULTIPLICATION SECTION
;--------------------------
	.multiply:
	mov bx, [numPrev]
	cmp bx, 0
	je .multiplyNow
	call getNumCreate
	call push_val

	.multiplyNow:
	call multiplication
	mov ax, 0
	mov [numPrev], ax
	mov bx, [oppSuccess]
	cmp bx, 1
	je .printNumber

	jmp .getRPNInput		;if it can't multiply


;--------------------------
;		DIVISION SECTION
;--------------------------
	.divide:
	mov bx, [numPrev]
	cmp bx, 0
	je .divideNow
	call getNumCreate
	call push_val

	.divideNow:
	call division
	mov ax, 0
	mov [numPrev], ax
	mov bx, [oppSuccess]
	cmp bx, 1
	je .printNumber

	jmp .getRPNInput		;if it can't multiply
	
	
	
;--------------------------
;		NEGATION SECTION
;--------------------------
	.netate:
	mov bx, [numPrev]
	cmp bx, 0
	je .negateNow
	call getNumCreate
	call push_val
	
	.negateNow:
	call getation
	mov ax, 0
	mov [numPrev], ax
	mov bx, [oppSuccess]
	cmp bx, 1
	je .printNumber

	jmp .getRPNInput		;if it can't multiply

;--------------------------
;		CREATE NUM OR DO NOTHING
;--------------------------
	.otherThanSpace:
	mov dx, msg_iligal_print
	call puts

	.doNothing:
	mov bx, [numPrev]
	cmp bx, 0
	je .getRPNInput

	call getNumCreate
	call push_val
	mov ax, 0
	mov [numPrev], ax
	
	jmp .getRPNInput

;--------------------------
;		PRINT NUM FROM STACK
;--------------------------
	.printNumber:
	mov bx, [numPrev]
	cmp bx, 0
	je .printNow
	call getNumCreate
	call push_val

	.printNow:
	call Print_Val_Stack
	mov ax, 0
	mov [numPrev], ax
	mov [oppSuccess], ax
	jmp .getRPNInput
	jmp task_A
	


;--------------------------
;		ADD TO NUM FROM STACK
;--------------------------
addition:
	push ax
	push bx
	push cx

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	mov bx, 1
	mov [opfirstPartSucc], bx
	mov cx, ax

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr

	add ax, cx				;;Addition success
	call push_val
	mov bx, 1
	mov [oppSuccess], bx
	;mov dx, msg_oppsucc_print
	;call puts

	jmp .endAddition

	.printErr:
	mov bx, [opfirstPartSucc]
	cmp bx, 1
	jne .printErrNow

	mov ax, cx
	call push_val

	.printErrNow:
	mov bx, 0
	mov [oppSuccess], bx
	mov dx, msg_oppfail_print
	call puts

	.endAddition:
	mov bx, 0
	mov [opfirstPartSucc], bx

	pop cx
	pop bx
	pop ax
	ret

;--------------------------
;		SUBSTRACT TWO NUM FROM STACK
;--------------------------
substraction:
	push ax
	push bx
	push cx

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	mov bx, 1
	mov [opfirstPartSucc], bx
	mov cx, ax

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr

	sub ax, cx				;;sub success
	call push_val
	mov bx, 1
	mov [oppSuccess], bx
	;mov dx, msg_oppsucc_print
	;call puts

	jmp .endsubs

	.printErr:
	mov bx, [opfirstPartSucc]
	cmp bx, 1
	jne .printErrNow

	mov ax, cx
	call push_val

	.printErrNow:
	mov bx, 0
	mov [oppSuccess], bx
	;mov dx, msg_oppfail_print
	;call puts

	.endsubs:
	mov bx, 0
	mov [opfirstPartSucc], bx

	pop cx
	pop bx
	pop ax
	ret
	
;--------------------------
;		NEGATE ONE ITEM FROM STACK
;--------------------------
getation:
	push ax
	push bx
	
	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	
	neg ax 					;;multiply success
	call push_val
	mov bx, 1
	mov [oppSuccess], bx
	;mov dx, msg_oppsucc_print
	;call puts
	
	jmp .endNeg
	
	.printErr:
	mov bx, 0
	mov [oppSuccess], bx
	;mov dx, msg_oppfail_print
	;call puts

	.endNeg:
	
	pop bx
	pop ax
	ret

;--------------------------
;		MULTIPLY TWO NUM FROM STACK
;--------------------------
multiplication:
	push ax
	push bx
	push cx

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	mov bx, 1
	mov [opfirstPartSucc], bx
	mov cx, ax

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr

	imul ax, cx				;;multiply success
	call push_val
	mov bx, 1
	mov [oppSuccess], bx
	;mov dx, msg_oppsucc_print
	;call puts

	jmp .endmulti

	.printErr:
	mov bx, [opfirstPartSucc]
	cmp bx, 1
	jne .printErrNow

	mov ax, cx
	call push_val

	.printErrNow:
	mov bx, 0
	mov [oppSuccess], bx
	;mov dx, msg_oppfail_print
	;call puts

	.endmulti:
	mov bx, 0
	mov [opfirstPartSucc], bx

	pop cx
	pop bx
	pop ax
	ret


;--------------------------
;		DIVISION TWO NUM FROM STACK
;--------------------------
division:
	push ax
	push bx
	push cx
	push dx

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	mov bx, 1
	mov [opfirstPartSucc], bx
	mov cx, ax

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr

	mov bx, cx
	cwd

	idiv bx			;;division success
	call push_val
	mov bx, 1
	mov [oppSuccess], bx
	;mov dx, msg_oppsucc_print
	;call puts


	jmp .enddivi

	.printErr:
	mov bx, [opfirstPartSucc]
	cmp bx, 1
	jne .printErrNow

	mov ax, cx
	call push_val

	.printErrNow:
	mov bx, 0
	mov [oppSuccess], bx
	;mov dx, msg_oppfail_print
	;call puts

	.enddivi:
	mov bx, 0
	mov [opfirstPartSucc], bx

	pop dx
	pop cx
	pop bx
	pop ax
	ret 

;--------------------------
;		GET CREATED NUM
;--------------------------
;This is used for the multle digit number math
;Does not return anythin, but gets its value from ax
numCreate:
	push bx
	push cx
	push ax

	mov bx, 0
	.for:
	cmp bx, [bigNum_stack_In]
	je .endfor
	push bx
	shl bx, 1
	mov cx, [bigNum_stack + bx]
	mov ax, 10
	mul cx
	mov [bigNum_stack + bx], ax
	pop bx
	add bx, 1
	jmp .for

	.endfor:
	mov cx, bx
	shl bx, 1
	pop ax
	mov [bigNum_stack + bx], ax
	add cx, 1
	mov  [bigNum_stack_In], cx

	pop cx
	pop bx 
	ret


;--------------------------
;		CREATE NUM
;--------------------------
;This is used to get the number created from the number_stack
;array. It returns the value in ax
getNumCreate:
	push bx
	push cx
	xor ax, ax

	mov bx, 0
	.for:
	cmp bx, [bigNum_stack_In]
	je .endfor
	push bx
	shl bx, 1
	add ax, [bigNum_stack + bx]
	pop bx
	add bx, 1
	jmp .for

	.endfor:
	mov bx, 0
	mov [bigNum_stack_In], bx

	pop cx
	pop bx
	ret 


;--------------------------
;		PUSH_VAL
;--------------------------
push_val:
	push bx
	push cx

	mov bx, [rpn_stack_stIn]
	mov cx, bx
	shl bx, 1
	mov [rpn_stack_st + bx], ax
	add cx, 1
	mov [rpn_stack_stIn], cx

	pop cx
	pop bx
	ret


;--------------------------
;		POP_VAL
;--------------------------
pop_val:
	push bx
	push cx

	mov cx, 0

	mov	bx, [rpn_stack_stIn]
	cmp bx, 1
	jl .error
	sub bx, 1
	mov [rpn_stack_stIn], bx
	shl bx, 1
	mov ax, [rpn_stack_st + bx]
	mov  [rpn_stack_st + bx], cx

	mov cx, 1
	mov [pop_val_succ], cx
	jmp .end_pop
.error:
	mov cx, 0
	mov [pop_val_succ], cx
.end_pop:
	pop cx
	pop bx
	ret

;--------------------------
;		PRINT VAL FROM THE STACK
;--------------------------
Print_Val_Stack:
	push cx
	push bx
	push dx

	call pop_val
	mov bx, [pop_val_succ]
	cmp bx, 0
	je .printErr
	
	mov dx, msg_end
	call puts

	mov cx, ax
	call Print_Number

	mov dx, msg_end
	call puts
	
	jmp .endPrint

	.printErr:
	mov dx, msg_error_print
	call puts

	.endPrint:
	pop dx
	pop bx
	pop cx
	ret

;--------------------------
;			print f
;--------------------------
Print_Number:
	    mov byte [buffer+9],'$'
        lea si,[buffer+9]

        MOV AX,CX           ;CX = VALUE THAT I WANT TO CONVERT
        MOV BX,10         
ASC2:
        mov dx,0            ; clear dx prior to dividing dx:ax by bx
        DIV BX              ;DIV AX/10
        ADD DX,48           ;ADD 48 TO REMAINDER TO GET ASCII CHARACTER OF NUMBER 
        dec si              ; store characters in reverse order
        mov [si],dl
        CMP AX,0            
        JZ EXTT             ;IF AX=0, END OF THE PROCEDURE
        JMP ASC2            ;ELSE REPEAT
EXTT:
        mov ah,9            ; print string
        mov dx,si
        int 21h
        RET	

	
	
	
	
	
	
	
	
	
task_C:
	
	; Clear screen to black (copy 80*25*2 byte of ZERO to the framebuffer)
	mov	al, 0
	mov	cx, CpR*RpS*BpC
	mov	di, 0
	rep	stosb
	
	mov	di, MESSAGE_START
	mov	ah, 0x0A	; background = 0 (black), foreground = 10 (lime green)
	
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
	mov bx, [place_notes]			; note count

                                    ; set up timer command register and counter register

	;set up	
	mov al, 0b6h					; set 8253 command register
	out 43h, al						; for channel 2, mode 3

.nloop:
    mov     ax, 34dch			; low part of clock freq.
    mov     dx, 12h				; hight part of clock freq.
    push	bx
    mov		bx, [notes + si]	; get note from data segment
    div     bx
    pop		bx
    out     42h, al				; 8253 command register (low byte)
    mov     al, ah
    out     42h, al				; 8253 command regsieter (high byte)
    
    ; turn on low bits in 8255 output port

    in      al, 61h				; read current value of 8255 port
    or      al, 3				; clear low bits
    out     61h, al				; send new value to port

    ; loop while note is sounding

    mov cx, [noteLength + si]
.loopLength:
    push cx
    mov 	cx, 20D0h
    .rpta:						 ; 1/10 sec delay
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
    
; Where to find the INT 8 handler vector within the IVT [interrupt vector table]
IVT8_OFFSET_SLOT	equ	4 * 8			        ; Each IVT entry is 4 bytes; this is the 8th
IVT8_SEGMENT_SLOT	equ	IVT8_OFFSET_SLOT + 2	; Segment after Offset
countdown           equ	8000h                   ; approx 36 interrupts per second

CpR	equ	80	; 80 characters per row
RpS	equ	20	; 25 rows per screen
BpC	equ	2	; 2 bytes per character

; Compute starting offset to store ASCII art in RAM
MESSAGE_START	equ	(1 * CpR * BpC) + (1 * BpC)


CHARS	equ	4	; number of characters in the message "BJU!"
spacer  dw  0   ; filling up those extra lines...
placed  dw  0

;	Second picture
MESSAGE_STARTA	equ	(23 * CpR * BpC) + (((CpR - (CHARS / 2)) / 2) * BpC)

printa dw 0
printb dw 0

stack_A	    TIMES 256 dw 0
stack_B	    TIMES 256 dw 0
stack_C	    TIMES 256 dw 0
stack_D 	TIMES 256 dw 0
stack_E 	TIMES 256 dw 0
stack_SP    TIMES 32 dw 0

ivt8_offset	    dw	0
ivt8_segment	dw	0

stack_size  equ 256
num_sp      dw 0
saved_sp    dw 0
;msg_A   db 13, 10, "task one", 13, 10, 0
msg_B   db 13, 10, "task two", 13, 10, 0
;msg_C   db 13, 10, "task three", 13, 10, 0
;msg_D   db 13, 10, "task four", 13, 10, 0
info    db 13, 10, "CpS 230 Team Project: Andrew Carter and Julio Whatley", 13, 10, 0

;Music section 
place_notes dw 59


noteLength		dw	4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 10, 14, 4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 8, 2, 4, 3, 4, 4, 6, 4, 4, 4, 4, 2, 4, 2, 2, 4, 2, 8, 3, 6, 8, 10, 12, 12, 12, 12, 4, 4, 2, 4, 2
silenceLength	dw	4, 4, 4, 4, 2, 4, 5, 2, 4, 2, 2, 2, 5, 2, 2, 2, 4, 4, 4, 4, 4, 4, 2, 4, 5, 2, 4, 5, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 5, 2, 4, 5, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 2
notes			dw	440, 587, 440, 587, 440, 587, 440, 415, 440, 440, 415, 440, 392, 370, 392, 370, 349, 293, 440, 587, 440, 587, 440, 587, 440, 415, 440, 392, 392, 370, 392, 523, 466, 449, 415, 440, 587, 440, 587, 440, 587, 440, 415, 440, 523, 523, 440, 415, 349, 294, 294, 349, 440, 523, 622, 587, 415, 440, 349


;RPN SECTION
rpn_stack_st TIMES 32 dw 0
rpn_stack_stIn dw 0

opfirstPartSucc dw 0
oppSuccess		dw 0

pop_val_succ dw 0
msg_error_print db 13, 10, "There is nothing in the stack.", 13, 10, 0
msg_end db " ", 13, 10, 0
;msg_succes_print db 13, 10, "Poping value from stack.", 13, 10, 0
msg_oppfail_print db 13, 10, "Not enough values on stack for opp. :).", 13, 10, 0
;msg_oppsucc_print db 13, 10, "Opperation success :).", 13, 10, 0

msg_iligal_print db 13, 10, "Illegal command, will be treated as a space :).", 13, 10, 0

bigNum_stack TIMES 32 dw 0
bigNum_stack_In dw 0

printNum	   dw 0
numPrev		   dw 0

buffer TIMES 10 db 0

