
%include "../runtime/used_instructions.inc"
%include "../runtime/clinklang.asm"

extern __imp__VirtualAlloc@16
extern __imp__VirtualFree@12

global _CompileBytecode
global _ReleaseBytecode
global _RunStaticCode
global _RenderSamples
global _NoteOn
global _NoteOff

%define CODE_SPACE 0x10000

%define MEM_COMMIT 0x00001000
%define MEM_RESERVE 0x00002000
%define MEM_RELEASE 0x00008000
%define PAGE_EXECUTE_READWRITE 0x40

section codemem bss align=4
CodeMemory:
	resd 1

section cmd text align=1

_CompileBytecode:
	; Parameters: Bytecode
	pusha

	call	LoadGmDls

	push	PAGE_EXECUTE_READWRITE
	push	MEM_COMMIT | MEM_RESERVE
	push	CODE_SPACE
	push	0
	call	[__imp__VirtualAlloc@16]
	mov		[CodeMemory], eax

	mov		esi, [esp + (8+1)*4]
	mov		edi, eax
	call	GenerateCode

	popa
	ret

_ReleaseBytecode:
	push	MEM_RELEASE
	push	0
	push	dword [CodeMemory]
	call	[__imp__VirtualFree@12]
	ret

_RunStaticCode:
	; Parameters: Constants
	pusha

	call	ResetState

	mov		esi, [esp + (8+1)*4]
	call	RunStaticCode

	popa
	ret

_RenderSamples:
	; Parameters: Constants, Length
	pusha

	mov		esi, [esp + (8+1)*4]
	mov		eax, [esp + (8+2)*4]
	call	RenderSamples

	popa
	mov		eax, MusicBuffer
	ret

_NoteOn:
	; Parameters: Channel, Sample offset, Key, Velocity
	pusha

	mov		eax, [esp + (8+1)*4]
	mov		ebx, [esp + (8+2)*4]
	mov		ecx, [esp + (8+3)*4]
	mov		edx, [esp + (8+4)*4]
	call	NoteOn

	popa
	ret

_NoteOff:
	; Parameters: Channel, Sample offset, Key
	pusha

	mov		eax, [esp + (8+1)*4]
	mov		ebx, [esp + (8+2)*4]
	mov		ecx, [esp + (8+3)*4]
	call	NoteOff

	popa
	ret
