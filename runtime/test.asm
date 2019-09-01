
; Used instructions
%define I_ADDSUB 0
%define I_CELL_INIT 0
%define I_CELL_READ 0
%define I_STATE_ENTER 0
%define I_STATE_LEAVE 0
%define I_BUFFER_ALLOC 0
%define I_BUFFER_LOAD 0
%define I_BUFFER_STORE 0
%define I_RANDOM 0
%define I_INIT_TRIGGERS 0
%define I_TRIGGER 0
%define I_KILL 0
%define I_EXP2_BODY 0
%define I_FDONE 1
%define I_FOP 2
%define I_CONSTANT 3
%define I_PROC_CALL 0
%define I_NOTE_PROPERTY 0
%define I_STACK_LOAD 1
%define I_STACK_STORE 0
%define I_CELL_STORE 0
%define I_COMPARE 0
%define I_ROUND 0
%define I_IF 0
%define I_ENDIF 0
%define I_ELSE 0

%define COMPACT_IMPLICIT_OPCODES 1

%include "clinklang.asm"


global main

section main text align=1

main:
	int3
	push		MusicData
	call		ClinklangCompute
	pop			ecx
	ret


section musdat rdata align=1

%define b(c) _snip_id_%+c

MusicData:
	; Bytecodes
	db b(proc)
	db b(constant)+2, b(constant)+0
	db b(label)
	db b(stack_load)+0
	db b(fop)+(~0xfe), b(fdone) ; fsin
	db b(cvtpd2ps), b(output)
	db b(constant)+1, b(add)
	db b(cmp), b(loopjump)
	db b(proc), 0

	; Notes
	db 1
%rep 4
	dd 1
	db 0x80
%endrep

	; Constant pool
	dd 0.0
	dd 1.0
	dd 1000.0
