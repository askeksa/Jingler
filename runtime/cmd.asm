
%include "../../runtime/used_instructions.inc"
%include "../../runtime/jingler.asm"

%if __BITS__ == 32
extern __imp__VirtualAlloc@16
extern __imp__VirtualFree@12
%define L(label) _%+label
%else
extern __imp_VirtualAlloc
extern __imp_VirtualFree
%define L(label) label
%endif

global L(CompileBytecode)
global L(ReleaseBytecode)
global L(ResetState)
global L(RunProcedure)
global L(NoteOn)
global L(NoteOff)

%define CODE_SPACE 0x10000

%define MEM_COMMIT 0x00001000
%define MEM_RESERVE 0x00002000
%define MEM_RELEASE 0x00008000
%define PAGE_EXECUTE_READWRITE 0x40

section codemem bss align=8
CodeMemory:
	resq 1

section cmd text align=1

L(CompileBytecode):
	; Parameters: Bytecode
%if __BITS__ == 32
	pusha

	call	JinglerLoadGmDls

	push	PAGE_EXECUTE_READWRITE
	push	MEM_COMMIT | MEM_RESERVE
	push	CODE_SPACE
	push	0
	call	[__imp__VirtualAlloc@16]
	mov		[CodeMemory], eax

	mov		esi, [esp + (8+1)*4]
	mov		edi, eax
	call	JinglerGenerateCode

	popa
%else
	push	rbx
	push	rsi
	push	rdi
	push	rbp
	push	rcx

	call	JinglerLoadGmDls

	mov		rcx, 0
	mov		rdx, CODE_SPACE
	mov		r8, MEM_COMMIT | MEM_RESERVE
	mov		r9, PAGE_EXECUTE_READWRITE
	sub		rsp, 32
	call	[rel __imp_VirtualAlloc]
	add		rsp, 32
	mov		[rel CodeMemory], rax

	pop		rsi
	mov		rdi, rax
	call	JinglerGenerateCode

	pop		rbp
	pop		rdi
	pop		rsi
	pop		rbx
%endif
	ret

L(ReleaseBytecode):
%if __BITS__ == 32
	push	MEM_RELEASE
	push	0
	push	dword [CodeMemory]
	call	[__imp__VirtualFree@12]
%else
	mov		rcx, [rel CodeMemory]
	mov		rdx, 0
	mov		r8, MEM_RELEASE
	sub		rsp, 40
	call	[rel __imp_VirtualFree]
	add		rsp, 40
%endif
	ret

L(ResetState):
%if __BITS__ == 32
	pusha

	call	JinglerResetState

	popa
%else
	push	rsi
	push	rdi

	call	JinglerResetState

	pop		rdi
	pop		rsi
%endif
	ret

L(RunProcedure):
	; Parameters: Constants, Procedure ID
%if __BITS__ == 32
	pusha

	ldmxcsr	[MXCSR]

	xor		ebp, ebp
	mov		esi, [esp + (8+1)*4]
	mov		edx, [esp + (8+2)*4]
	runproc	edx
	mov		[esp + 7*4], ebx ; eax

	popa
%else
	push	rbx
	push	rsi
	push	rdi
	push	rbp

	ldmxcsr	[rel MXCSR]

	xor		rbp, rbp
	mov		rsi, rcx
	runproc	rdx
	mov		rax, rbx

	pop		rbp
	pop		rdi
	pop		rsi
	pop		rbx
%endif
	ret

L(NoteOn):
	; Parameters: Channel, Sample offset, Key, Velocity
%if __BITS__ == 32
	pusha

	mov		eax, [esp + (8+1)*4]
	mov		ebx, [esp + (8+2)*4]
	mov		ecx, [esp + (8+3)*4]
	mov		edx, [esp + (8+4)*4]
	call	JinglerNoteOn

	popa
%else
	push	rbx
	push	rsi
	push	rdi

	mov		eax, ecx
	mov		ebx, edx
	mov		ecx, r8d
	mov		edx, r9d
	call	JinglerNoteOn

	pop		rdi
	pop		rsi
	pop		rbx
%endif
	ret

L(NoteOff):
	; Parameters: Channel, Sample offset, Key
%if __BITS__ == 32
	pusha

	mov		eax, [esp + (8+1)*4]
	mov		ebx, [esp + (8+2)*4]
	mov		ecx, [esp + (8+3)*4]
	call	JinglerNoteOff

	popa
%else
	push	rbx
	push	rsi
	push	rdi

	mov		eax, ecx
	mov		ebx, edx
	mov		ecx, r8d
	call	JinglerNoteOff

	pop		rdi
	pop		rsi
	pop		rbx
%endif
	ret
