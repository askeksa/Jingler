
; Used instructions
%define I_STATE_ENTER 1
%define I_STATE_LEAVE 1
%define I_KILL 1
%define I_CELL_INIT 1
%define I_CELL_READ 1
%define I_ADDSUB 1
%define I_FPUTNEXT 1
%define I_RANDOM 1
%define I_BUFFER_STORE 1
%define I_BUFFER_LOAD 1
%define I_BUFFER_ALLOC 1
%define I_CALL_INSTRUMENT 1
%define I_EXP2_BODY 1
%define I_FDONE 1
%define I_FOP 15
%define I_PROC 1
%define I_PROC_CALL 50
%define I_CONSTANT 50
%define I_NOTE_PROPERTY 3
%define I_STACK_LOAD 25
%define I_STACK_STORE 25
%define I_CELL_STORE 25
%define I_LABEL 1
%define I_IF 1
%define I_LOOPJUMP 1
%define I_ENDIF 1
%define I_ELSE 1
%define I_ROUND 4
%define I_COMPARE 7

%define COMPACT_IMPLICIT_OPCODES 1

%include "../runtime/clinklang.asm"

extern __imp__VirtualAlloc@16
extern __imp__VirtualFree@12

global _RunClinklang

%define CODE_SPACE 0x10000

%define MEM_COMMIT 0x00001000
%define MEM_RESERVE 0x00002000
%define MEM_RELEASE 0x00008000
%define PAGE_EXECUTE_READWRITE 0x40

section cmd text align=1

_RunClinklang:
	; Parameters: Bytecode+Constants, Length
	pusha

	call	ResetState

	push	PAGE_EXECUTE_READWRITE
	push	MEM_COMMIT | MEM_RESERVE
	push	CODE_SPACE
	push	0
	call	[__imp__VirtualAlloc@16]

	push	MEM_RELEASE
	push	0
	push	eax

	mov		esi, [esp + (3+8+1)*4]
	mov		edi, eax
	mov		eax, [esp + (3+8+2)*4]
	call	RenderMusic

	call	[__imp__VirtualFree@12]

	popa
	mov		eax, MusicBuffer
	ret
