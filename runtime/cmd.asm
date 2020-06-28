
%include "../runtime/used_instructions.inc"
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
	; Parameters: Bytecode, Constants, Length
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
	call	GenerateCode

	mov		esi, [esp + (3+8+2)*4]
	call	RunStaticCode

	mov		esi, [esp + (3+8+2)*4]
	mov		eax, [esp + (3+8+3)*4]
	call	RenderSamples

	call	[__imp__VirtualFree@12]

	popa
	mov		eax, MusicBuffer
	ret
