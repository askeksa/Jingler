; Set to 1 to generate code into statically allocated memory.
; Only works when linking with Crinkler, since this memory is otherwise not executable.
%ifndef JINGLER_CRINKLER
	%define JINGLER_CRINKLER 0
%endif

; Graphics latency compensation (in samples) for synchronization.
%define JINGLER_TIMER_OFFSET 2048


; Various helpers for writing code that assembles as both 32 and 64 bit
%if __BITS__ == 32
	; Use 64 bit register name whenever the size differs
	%define rax eax
	%define rbx ebx
	%define rcx ecx
	%define rdx edx
	%define rsi esi
	%define rdi edi
	%define rbp ebp
	%define rsp esp

	; Scale, data, space and instructions for 32-bit pointers
	%define PSIZE 4
	%define dp dd
	%define resp resd
	%define stosp stosd

	; Use label directly in addressing
	%macro rlea 1
	%endmacro
	%define rlabel(addr) addr

	%define L(label) _%+label

	extern __imp__OpenFile@12
	extern __imp__ReadFile@20
	extern __imp__CloseHandle@4
	extern __imp__VirtualAlloc@16
	extern __imp__VirtualFree@12
	extern __imp__waveOutOpen@24
	extern __imp__waveOutPrepareHeader@12
	extern __imp__waveOutWrite@12
	extern __imp__waveOutGetPosition@12
%else
	; Scale, data, space and instructions for 64-bit pointers
	%define PSIZE 8
	%define dp dq
	%define resp resq
	%define stosp stosq

	; Load label into register before using it in addressing
	%macro rlea 1
		mov		r8, %1
	%endmacro
	%define rlabel(addr) r8

	%define L(label) label

	extern __imp_OpenFile
	extern __imp_ReadFile
	extern __imp_CloseHandle
	extern __imp_VirtualAlloc
	extern __imp_VirtualFree
%endif

; Public symbols
global L(Jingler_GenerateMusic)
global L(Jingler_Ready)
global L(Jingler_StartMusic)
global L(Jingler_GetPosition)
global L(Jingler_MusicBuffer)
global L(Jingler_TicksPerSecond)
global L(Jingler_MusicLength)

L(Jingler_Ready) equ NoteAllocPtr
L(Jingler_MusicBuffer) equ MusicBuffer

section tps rdata align=4
L(Jingler_TicksPerSecond):
	dd TICKS_PER_SECOND

section muslen rdata align=4
L(Jingler_MusicLength):
	dd MUSIC_LENGTH


OF_READ equ 0x00000000
MEM_COMMIT equ 0x00001000
MEM_RESERVE equ 0x00002000
MEM_RELEASE equ 0x00008000
PAGE_EXECUTE_READWRITE equ 0x40

GMDLS_SIZE equ 0x348014
GMDLS_ROUNDED_SIZE equ (GMDLS_SIZE + 0xFFFF) & ~0xFFFF
GMDLS_OFFSETS equ 0x43E3A
GMDLS_DATA equ 0x4462C
GMDLS_COUNT equ 495

MAX_STACK equ 0x1000
STATE_SPACE equ 0x100000
BUFFER_SPACE equ 0x1000000
MUSIC_SPACE equ 0x1000000
MAX_TRACKS equ 0xFF
MAX_NOTE_COUNT equ 0x10000
CODE_SPACE equ 0x100000

LOWEST_IMPLICIT_INSTRUCTION equ 0x14

; Constants for round instruction
%define ROUND_NEAREST 0
%define ROUND_FLOOR 1
%define ROUND_CEIL 2
%define ROUND_TRUNCATE 3

; Constants for compare instruction
%define COMPARE_EQ 0
%define COMPARE_LT 1
%define COMPARE_LE 2
%define COMPARE_NE 4
%define COMPARE_GE 5
%define COMPARE_GT 6

; Constants for note_property instruction
%define NOTE_LENGTH 0
%define NOTE_KEY 1
%define NOTE_VELOCITY 2

; Size of a proc_call instruction
%define CALL_SIZE (_snip_proc_call.end - _snip_proc_call)

;; Snip macros

%if COMPACT_IMPLICIT_OPCODES
	; Chop off the most significant 1 of an opcode starting with 101
	%define IMPLICIT_CODE(opcode) ((opcode) & ((opcode >> 2) | (opcode >> 3) | 1))
%else
	%define IMPLICIT_CODE(opcode) (opcode)
%endif

section snips text align=1
	_snips:
section snipsize data align=1
	_snipsizes:
section snipdead text align=1

%define _snip_prev _snips
%define _snip_code 0
%define _snip_notfirst 0
%define _snip_prev_number 0
%define _snip_prev_id 256
%define _inout_string ""

%macro _snip_flush 2 ; kind, name
	section snipsize
	%if _snip_notfirst
		db _%1_%2-_snip_prev
		%if _snip_prev_number
			db _snip_prev_number
		%endif
	%endif
	%define _snip_notfirst 1
	%define _snip_prev _%1_%2
	_%1_size_%2:
	section snips
	%if _snip_code
		ret
	%endif
	_%1_%2:
%endmacro

%macro basesnip 1
	_snip_flush snip, %1
	%define _snip_code 0
	section snips
%endmacro

%macro snip 3 ; name, inout, number
	%if %3
		%defstr _inout_%1 %2
		%rep %3
			%strcat _inout_string _inout_%1 _inout_string
		%endrep
		basesnip %1
		%xdefine _snip_id_%1 _snip_prev_id-%3
		%xdefine _snip_number_%1 %3
		%define _snip_prev_number _snip_number_%1
		%define _snip_prev_id _snip_id_%1
	%else
		section snipdead
		_snip_%1:
	%endif
%endmacro

%macro snipcode 1-2 1
	%if %2
		_snip_flush snipcode, %1
		section snipsize
		db -1
		%define _snip_code 1
		%define _snip_prev_number 0
		section snips
	%else
		section snipdead
		_snipcode_%1:
	%endif
%endmacro

%macro snipend 0
	%xdefine _snip_prev_number _snip_prev_id - 1 + IMPLICIT_CODE(LOWEST_IMPLICIT_INSTRUCTION)
	_snip_flush end_of, snips
%endmacro


;; Runtime macros

%macro runproc 1
	; ESI/RSI = Constant pool
	mov			rdi, StateSpace
	mov			rbx, rdi
	rlea		ProcPointers
	call		[dword rlabel(ProcPointers) + %1*PSIZE]
%endmacro


section jinglgen text align=1

_Jingler_GenerateMusic:

LoadGmDlsFile:
	mov			rdi, GmDls
%if __BITS__ == 32
	push		byte OF_READ
	push		rdi
	push		GmDlsName
	call		[__imp__OpenFile@12]

	push		byte 0
	push		rdi
	push		GMDLS_SIZE
	push		rdi
	push		eax
	call		[__imp__ReadFile@20]
%else
	sub			rsp, byte 40 ; Shadow space + alignment

	lea			rcx, [rel GmDlsName]
	mov			rdx, rdi
	mov			r8, OF_READ
	call		[rel __imp_OpenFile]

	mov			rcx, rax
	mov			rdx, rdi
	mov			r8, GMDLS_SIZE
	mov			r9, rdi
	mov			qword [rsp + 32], 0
	call		[rel __imp_ReadFile]

	add			rsp, byte 40
%endif

UnpackNotes:
	mov			rsi, MusicData

	mov			rdx, 3 ; Component
.componentloop:
	lodsd
	xchg		ebp, eax ; Value factor

	mov			rdi, TrackStarts
	mov			rbx, NoteHeaders
	mov			ecx, NUM_TRACKS + NUM_PARAMETERS
.trackloop:
	mov			rax, rbx
	stosp

.noteloop:
	; Load value
	xor			eax, eax
	lodsb
	cmp			al, 0x80
	je			.next_track
	jb			.bytevalue
	not			al
	mov			ah, al
	lodsb
.bytevalue:
	imul		eax, ebp
	mov			[rbx + rdx*4], eax
	add			rbx, byte 16
	jmp			.noteloop

.next_track:
	mov			dword [rbx], 0x80000000 ; Track terminator
	add			rbx, byte 16
	loop		.trackloop

	dec			rdx
	jns			.componentloop

%if JINGLER_CRINKLER
GenerateCode:
	mov			rdi, GeneratedCode
%else
AllocateCodeSpace:
%if __BITS__ == 32
	push		PAGE_EXECUTE_READWRITE
	push		MEM_COMMIT | MEM_RESERVE
	push		CODE_SPACE
	push		0
	call		[__imp__VirtualAlloc@16]
%else
	sub			rsp, byte 40 ; Shadow space + alignment

	mov			rcx, 0
	mov			rdx, CODE_SPACE
	mov			r8, MEM_COMMIT | MEM_RESERVE
	mov			r9, PAGE_EXECUTE_READWRITE
	call		[rel __imp_VirtualAlloc]

	add			rsp, 40
%endif
	xchg		rdi, rax
GenerateCode:
%endif

	; ESI/RSI = Bytecode
	; EDI/RDI = Space for generated code

	mov			edx, OUT_S >> 4
.mainloop:
	xor			eax, eax
	lodsb
	cmp			al, 0
	je			.decode_done
	push		rsi
	rlea		InoutCodes
	or			dl, [rlabel(InoutCodes) + rax]
	mov			rbx, _snipsizes
	mov			rsi, _snips
	xor			rcx, rcx
.inout_loop:
	add			rsi, rcx
	mov			cl, [rbx]
	inc			rbx
	shr			dl, 1
	jc			.not
	rep movsb
.not:
	cmp			byte [rbx], -1
	jne			.inout_loop

.decodeloop:
	cmp			byte [rbx], -1
	jne			.decode
	inc			rbx
	add			rsi, rcx
	mov			cl, [rbx]
	inc			rbx
	mov			rbp, rsi
.decode:
	add			rsi, rcx
	mov			cl, [rbx]
	inc			rbx
	add			al, [rbx]
	inc			rbx
	jnc			.decodeloop

.emit:
	rep movsb
	pop			rsi
	call		rbp
	jmp			.mainloop
.decode_done:

RunStaticCode:
	; ESI/RSI = Constant pool

	ldmxcsr		[rel MXCSR]
	runproc		MAIN_STATIC_PROC_ID

	rlea		NoteAllocPtr
	mov			[rlabel(NoteAllocPtr)], rdi

RenderSamples:
	; ESI/RSI = Constant pool

	xor			rax, rax
.sample:
	push		rax

%if NUM_PARAMETERS > 0
	mov			rcx, NUM_PARAMETERS
.paramloop:
	rlea		TrackStarts
	mov			rdi, [rlabel(TrackStarts) + NUM_TRACKS*PSIZE + rcx*PSIZE - PSIZE]
.findkey:
	fild		dword [rdi+8]
	rlea		ParameterScaleBias
	fmul		dword [rlabel(ParameterScaleBias) + rcx*8 - 8]
	fadd		dword [rlabel(ParameterScaleBias) + rcx*8 - 4]
	mov			eax, [rdi]
	cmp			eax, [rdi+4]
	jne			.interpolate
	rlea		ParameterStates
	fstp		dword [rlabel(ParameterStates) + rcx*4 - 4]
	add			rdi, byte 16
	rlea		TrackStarts
	mov			[rlabel(TrackStarts) + NUM_TRACKS*PSIZE + rcx*PSIZE - PSIZE], rdi
	jmp			.findkey
.interpolate:
	rlea		ParameterStates
	fsub		dword [rlabel(ParameterStates) + rcx*4 - 4]
	fimul		dword [rdi+4]
	fidiv		dword [rdi]
	fadd		dword [rlabel(ParameterStates) + rcx*4 - 4]
	fstp		dword [rsi + PARAMETER_OFFSET*4 + rcx*4 - 4]
	inc			dword [rdi+4]
	loop		.paramloop
%endif

	xor			rbp, rbp
	runproc		MAIN_DYNAMIC_PROC_ID

	pop			rax
	cvtpd2ps	xmm0, [rbx]
	rlea		MusicBuffer
	movq		[rlabel(MusicBuffer) + rax*8], xmm0

	inc			eax
	cmp			eax, TOTAL_SAMPLES
	jne			.sample
	ret


;; Snips

	; In/out snips to adjust stack state between instructions, preserving flags
	basesnip	r_to_t
	lea			rbx, [rbx - 16]

	basesnip	t_to_b
	movapd		[rbx], xmm0

	basesnip	s_to_b
	movapd		xmm0, [rbx]

	basesnip	t_to_r
	lea			rbx, [rbx + 16]

	; Plain snips
	snipcode	plain

	snip		state_enter, rr, I_STATE_ENTER
	push		rdi

	snip		state_leave, rr, I_STATE_LEAVE
	pop			rdi

	snip		cell_fetch, sr, I_CELL_FETCH
	pop			rax
	movapd		xmm0, [rax]
	push		rax

	snip		cell_push, sr, I_CELL_PUSH
	push		rdi
	movapd		xmm0, [rdi]
	add			rdi, byte 16

	snip		cell_read, sr, I_CELL_READ
	movapd		xmm0, [rdi]
	add			rdi, byte 16

	snip		cell_init, rs, I_CELL_INIT
	movapd		[rdi], xmm0
	add			rdi, byte 16

	snip		cell_pop, rs, I_CELL_POP
	pop			rax
	movapd		[rax], xmm0

	snip		gmdls_sample, rt, I_GMDLS_SAMPLE
	push		rcx
	cvtsd2si	eax, [rbx]
%if __BITS__ == 32
	mov			ecx, GmDls + GMDLS_DATA
	add			ecx, [GmDls + GMDLS_OFFSETS + eax*4]
	add			ecx, [ecx] ; Last dword of wsmp chunk
%else
	mov			rcx, GmDls + GMDLS_DATA
	mov			eax, [rcx - GMDLS_DATA + GMDLS_OFFSETS + rax*4]
	add			rcx, rax
	mov			eax, [rcx]
	add			rcx, rax
%endif
	cvtsd2si	eax, xmm0
	mov			edx, eax
	cmp			dword [rcx], byte 0 ; Loop length or looped flag
	je			.direct
	sub			eax, [rcx - 4] ; Loop base
	jb			.direct
	xor			edx, edx
	div			dword [rcx] ; Loop length
	add			edx, [rcx - 4] ; Loop base
.direct:
	mov			eax, [rcx + 8] ; Length of data chunk
	shr			eax, 1 ; Sample count
	cmp			edx, eax
	jae			.lookup
	lea			rcx, [rcx + 10 + rdx*2] ; 2 bytes before sample data
.lookup:
	cvtsi2sd	xmm0, dword [rcx]
	pop			rcx

	snip		addsub, rt, I_ADDSUB
	addsubpd	xmm0, [rbx]

	snip		fputnext, rt, I_FPUTNEXT
	fld			qword [rbx]

	snip		random, rt, I_RANDOM
	cvtsd2si	eax, xmm0
	mul			dword [rsi]
	xor			eax, edx
	xor			eax, [byte rbx+4]
	mul			dword [rsi]
	xor			eax, edx
	xor			eax, [byte rbx+0]
	mul			dword [rsi]
	xor			eax, edx
	cvtsi2sd	xmm0, eax

	snip		buffer_load_with_offset, rt, I_BUFFER_LOAD_WITH_OFFSET
	cvtsd2si	eax, xmm0
	neg			eax
	add			eax, [rbx]
	jb			.no_wrap
	add			eax, [rbx + 4]
.no_wrap:
	shl			eax, 4
	add			rax, [rbx + 8]
	movapd		xmm0, [rax]

	snip		buffer_load_indexed, rt, I_BUFFER_LOAD_INDEXED
	cvtsd2si	eax, xmm0
	xorpd		xmm0, xmm0
	cmp			eax, [rbx + 4]
	jae			.outside
	shl			eax, 4
	add			rax, [rbx + 8]
	movapd		xmm0, [rax]
.outside:

	snip		buffer_load, st, I_BUFFER_LOAD
	mov			eax, [rbx]
	shl			eax, 4
	add			rax, [rbx + 8]
	movapd		xmm0, [rax]

	snip		buffer_store_and_step, rs, I_BUFFER_STORE_AND_STEP
	mov			eax, [rbx]
	shl			eax, 4
	add			rax, [rbx + 8]
	movapd		[rax], xmm0

	inc			dword [rbx]
	mov			eax, [rbx]
	cmp			eax, [rbx + 4]
	jb			.no_wrap
	and			dword [rbx], byte 0
.no_wrap:

	snip		buffer_alloc, ts, I_BUFFER_ALLOC
	rlea		BufferAllocPtr
	mov			rax, [rlabel(BufferAllocPtr)]
	mov			[rbx + 8], rax
	cvtsd2si	eax, xmm0
	mov			[rbx + 4], eax
	shl			eax, 4
	add			[rlabel(BufferAllocPtr)], rax
	and			dword [rbx], byte 0

	snip		play_instrument1, ss, I_PLAY_INSTRUMENT1
	push		rdi

.noteloop:
	; Find note headers for track
	rlea		TrackStarts
	lea			rcx, [rlabel(TrackStarts) + rbp*PSIZE] ; Note headers for track
	mov			rdi, [rcx]

	; Count down to note trigger
	dec			dword [rdi]
	jns			_snip_play_instrument2.no_more_notes + CALL_SIZE

	; Allocate note object and copy header
	movapd		xmm0, [rdi]
	rlea		NoteAllocPtr
	mov			rdi, [rlabel(NoteAllocPtr)]
	add			dword [rcx], byte 16
	movapd		[rdi], xmm0

	; Chain note object into note list
	rlea		NoteChains
	lea			rcx, [rlabel(NoteChains) + rbp*4]
	mov			eax, [rcx]
	mov			[rdi], eax
	mov			[rcx], edi
%if __BITS__ == 64
	mov			r9, NoteSpaceBase
	sub			[rcx], r9d
%endif

	; Call instrument init procedure
	add			rdi, byte 16

	snip		play_instrument2, ss, I_PLAY_INSTRUMENT2
	; Bump alloc pointer to end of allocated note object
	rlea		NoteAllocPtr
	mov			[rlabel(NoteAllocPtr)], rdi
	jmp			_snip_play_instrument1.noteloop - CALL_SIZE
.no_more_notes:

	; Process all active notes for this instrument
	rlea		NoteChains
	lea			rcx, [rlabel(NoteChains) + rbp*4]
.activeloop:
	mov			edi, [rcx]
	test		edi, edi
	je			_snip_play_instrument3.activedone + CALL_SIZE
%if __BITS__ == 64
	mov			r9, NoteSpaceBase
	add			rdi, r9
%endif
	push		rdi

	; Call instrument process procedure
	add			rdi, byte 16

	snip		play_instrument3, ss, I_PLAY_INSTRUMENT3
	pop			rcx
	dec			dword [rcx + 4]
	jmp			_snip_play_instrument2.activeloop - CALL_SIZE
.activedone:

	inc			rbp
	pop			rdi

	snip		kill, ss, I_KILL
	mov			eax, [rcx]
%if __BITS__ == 64
	mov			r9, NoteSpaceBase
	add			rax, r9
%endif
	mov			eax, [rax]
	mov			[rcx], eax

	snip		exp2_body, ss, I_EXP2_BODY
	; Assumes fop 0xFC (frndint) before.
	fld			qword [rbx]
	fsub		st0, st1
	f2xm1
	fld1
	faddp		st1, st0
	fscale
	fstp		st1

	snip		fdone, ss, I_FDONE
	fstp		qword [rbx]

	snip		fdone2, tr, I_FDONE2
	fstp		qword [rbx]

	snip		fld1, ss, I_FLD1
	fld1

	; Byte parameter snips
	snipcode	notbyte, I_FOP
	not			al
	stosb

	snip		fop, ss, I_FOP
	fld			qword [rbx]
	db			0xd9 ; Various floating point ops

	; Proc snip
	snipcode	proc, I_PROC
	movzx		ecx, dh
	inc			dh
	rlea		ProcPointers
	mov			[rlabel(ProcPointers) + rcx*PSIZE], rdi

	snip		proc, ss, I_PROC
	ret

	; Pointer snips
%if __BITS__ == 32
	snipcode	pointer, I_PROC_CALL+I_NOTE_PROPERTY+I_CONSTANT
	shl			eax, 2
	add			[rdi-4], eax
%else
	snipcode	proc_call, I_PROC_CALL
	shl			eax, 3
	add			[rdi-4], eax
%endif

	snip		proc_call, ss, I_PROC_CALL
	rlea		ProcPointers
	call		[dword rlabel(ProcPointers) + 0]
.end:
%if __BITS__ == 64
	snipcode	pointer, I_NOTE_PROPERTY+I_CONSTANT
	shl			eax, 2
	add			[rdi-4], eax
%endif

	snip		note_property, sr, I_NOTE_PROPERTY
	mov			eax, [rcx]
%if __BITS__ == 64
	mov			r9, NoteSpaceBase
	add			rax, r9
%endif
	cvtsi2sd	xmm0, [dword rax + 4]

	snip		constant, sr, I_CONSTANT
	cvtss2sd	xmm0, [dword rsi + 0]

%if __BITS__ == 32
	snipcode	byte_index, I_CONSTANT_BYTE_INDEX+I_PROC_CALL_BYTE_INDEX
%else
	snipcode	constant_byte_index, I_CONSTANT_BYTE_INDEX
%endif
	lodsb
	shl			eax, 2
	add			[rdi-4], eax

	snip		constant_byte_index, sr, I_CONSTANT_BYTE_INDEX
	cvtss2sd	xmm0, [dword rsi + 0]

%if __BITS__ == 64
	snipcode	proc_call_byte_index, I_PROC_CALL_BYTE_INDEX
	lodsb
	shl			eax, 3
	add			[rdi-4], eax
%endif

	snip		proc_call_byte_index, ss, I_PROC_CALL_BYTE_INDEX
	rlea		ProcPointers
	call		[dword rlabel(ProcPointers) + 0]

	; Offset snips
	snipcode	offset, I_STACK_LOAD+I_STACK_STORE
	shl			eax, 4
	stosd

	snip		stack_load, sr, I_STACK_LOAD
	db			0x66, 0x0f, 0x28, 0x83 ; movapd xmm0, [dword ebx + offset*16]

	snip		stack_store, rs, I_STACK_STORE
	db			0x66, 0x0f, 0x29, 0x83 ; movapd [dword ebx + offset*16], xmm0

	; Label snips
	snipcode	label, I_LABEL+I_IF
	pop			rcx
	push		rdi
	push		rcx

	snip		label, rr, I_LABEL

	snip		if, rr, I_IF
	db			0x0f, 0x82 ; jb label
	dd			0

	; Loopjump snip
	snipcode	loopjump, I_LOOPJUMP
	pop			rcx
	pop			rax
	push		rcx
	mov			[rdi-4], eax
	sub			[rdi-4], edi

	snip		loopjump, rr, I_LOOPJUMP
	db			0x0f, 0x82 ; jb label
	dd			0

	; Endif snips
	snipcode	endif, I_ENDIF+I_ELSE
	pop			rcx
	pop			rax
	push		rcx
	mov			[rax-4], edi
	sub			[rax-4], eax

	snip		endif, rr, I_ENDIF

	snip		else, rr, I_ELSE
	db			0xe9 ; jmp label
	dd			0
	; Follow by label instruction

	; Immediate snips
	snipcode	immediate, I_COMPARE+I_ROUND
	stosb

	snip		round, st, I_ROUND
	db			0x66, 0x0f, 0x3a, 0x09, 0x03 ; roundpd xmm0, [rbx], mode

	snip		compare, rt, I_COMPARE
	db			0x66, 0x0f, 0xc2, 0x03 ; cmppd xmm0, [rbx], mode

	; Implicit snip
	snipcode	implicit
%if COMPACT_IMPLICIT_OPCODES
	bsr			ecx, eax
	add			ecx, byte 2
	bts			eax, ecx
%endif
	mov			[rdi-2], al

	basesnip	implicit
	movapd		xmm0, [rbx] ; <op>pd xmm0, [rbx]

	snipend


;; In/out

IN_R	equ	0x03
IN_T	equ	0x0A
IN_B	equ	0x08
IN_S	equ	0x0C
OUT_R	equ	0xC0
OUT_T	equ	0x50
OUT_B	equ	0x70
OUT_S	equ	0x30

%define _last_inout 0

%macro inout 1
	%substr _in %1 1
	%substr _out %1 2
	%if _in == 'r'
		%xdefine _inkind IN_R
	%elif _in == 't'
		%xdefine _inkind IN_T
	%elif _in == 'b'
		%xdefine _inkind IN_B
	%elif _in == 's'
		%xdefine _inkind IN_S
	%else
		%error Invalid input kind: _in
	%endif
	%if _out == 'r'
		%xdefine _outkind OUT_R
	%elif _out == 't'
		%xdefine _outkind OUT_T
	%elif _out == 'b'
		%xdefine _outkind OUT_B
	%elif _out == 's'
		%xdefine _outkind OUT_S
	%else
		%error Invalid output kind: _out
	%endif
	%xdefine _last_inout _inkind | _outkind
	db _last_inout
%endmacro

%define _implicit_index 0

%macro iinstr 3 ; name, inout, repr
	%if COMPACT_IMPLICIT_OPCODES
		%if ((%3) & 0xfc != 0x14 && (%3) & 0xf8 != 0x28 && (%3) & 0xf0 != 0x50)
			%error Uncompactable implicit opcode: %3
		%endif
	%endif
	%xdefine _code (IMPLICIT_CODE(%3) - IMPLICIT_CODE(LOWEST_IMPLICIT_INSTRUCTION) + 1)
	%xdefine _snip_id_%1 _code
	%rep _code - _implicit_index
		db _last_inout
	%endrep
	%defstr _inout_%1 %2
	inout _inout_%1
	%xdefine _implicit_index (_code + 1)
%endmacro

section inout rdata align=1

InoutCodes:
	; Inout for implicit instructions
	iinstr	merge_lr, rt, 0x14 ; unpcklpd (r.y = m.x)
	iinstr	split_rl, sr, 0x15 ; unpckhpd (r.x = m.y)
	iinstr	expand_l, bt, 0x16 ; movhpd m -> r (r.y = m.x)
	iinstr	expand_r, bs, 0x17 ; movhpd r -> m (m.x = r.y)
	iinstr	pop, rb, 0x28 ; movapd m -> r
	iinstr	popnext, rb, 0x29 ; movapd r -> m
	iinstr	buffer_index_and_length, st, 0x2a ; cvtpi2pd
	iinstr	cmp, rr, 0x2e ; ucomisd
	iinstr	sqrt, st, 0x51
	iinstr	and, rt, 0x54
	iinstr	andn, rt, 0x55
	iinstr	or, rt, 0x56
	iinstr	xor, rt, 0x57
	iinstr	add, rt, 0x58
	iinstr	mul, rt, 0x59
	iinstr	sub, rt, 0x5c
	iinstr	min, rt, 0x5d
	iinstr	div, rt, 0x5e
	iinstr	max, rt, 0x5f

	%strlen _inout_length _inout_string
	%define _n_instructions (_inout_length / 2)
	%assign _opcode_count (_implicit_index + _n_instructions)
	%if _opcode_count > 256
		%error Too many opcodes: _opcode_count
	%endif
	%rep 256 - _opcode_count
		db _last_inout
	%endrep
	%xdefine _index 0
	%rep _n_instructions
		%substr _inout _inout_string (_index * 2 + 1), 2
		inout _inout
		%xdefine _index (_index + 1)
	%endrep

section mxcsr data align=4
MXCSR:
	dd 0x9fc0

section	notept bss align=PSIZE
NoteAllocPtr:
	resp 1

section	bufferpt data align=4
BufferAllocPtr:
	dp BufferSpace

%if NUM_PARAMETERS > 0
section paramst bss align=4
ParameterStates:
	resd NUM_PARAMETERS
%endif

section tracks bss align=PSIZE
TrackStarts:
.align16:
	resp MAX_TRACKS

section notech bss align=4
NoteChains:
.align16:
	resd MAX_TRACKS

section notehdrs bss align=16
NoteHeaders:
.align16:
	reso MAX_NOTE_COUNT

section procptrs bss align=PSIZE
ProcPointers:
.align16:
	resp 256

%if JINGLER_CRINKLER
section code bss align=1
GeneratedCode:
.align16:
	resb CODE_SPACE
%endif

section state bss align=16
NoteSpaceBase:
	reso MAX_STACK
StateSpace:
.align16:
	reso STATE_SPACE

section buffersp bss align=16
BufferSpace:
.align16:
	reso BUFFER_SPACE

section music bss align=8
MusicBuffer:
.align24:
	resq MUSIC_SPACE

section gmdls bss align=4
GmDls:
.align16:
	resb GMDLS_ROUNDED_SIZE

section gmdlsnam rdata align=1
GmDlsName:
	db "drivers/gm.dls",0


; Playback

%if __BITS__ == 32

section startsnd text align=1
L(Jingler_StartMusic):
	; Start music
	push	byte 0
	push	byte 0
	push	byte 0
	push	WaveFormat
	push	byte -1
	push	WaveOutHandle
	call	[__imp__waveOutOpen@24]

	push	byte 32					; sizeof(WAVEHDR)
	push	WaveHdr
	push	dword [WaveOutHandle]
	call	[__imp__waveOutPrepareHeader@12]

	push	byte 32					; sizeof(WAVEHDR)
	push	WaveHdr
	push	dword [WaveOutHandle]
	call	[__imp__waveOutWrite@12]
	ret

section getpos text align=1
L(Jingler_GetPosition):
	push	byte 32					; sizeof(MMTIME)
	push	WaveTime
	push	dword [WaveOutHandle]
	call	[__imp__waveOutGetPosition@12]

	fild	dword [WaveTime+4]
%if JINGLER_TIMER_OFFSET>0
	fiadd	dword [c_timeoffset]
%endif
	fidiv	dword [c_ticklength]
	ret

section	WaveForm rdata align=1
WaveFormat:
	dw	3,2
	dd	SAMPLE_RATE
	dd	SAMPLE_RATE*8
	dw	8,32,0

section WaveHdr data align=4
WaveHdr:
	dd	MusicBuffer
	dd	(TOTAL_SAMPLES*8)
	dd	0,0,0,0,0,0

section wavehand bss align=4
WaveOutHandle:
	resd 1

section WaveTime data align=4
WaveTime:
	dd	4,0,0,0,0,0,0,0

section offset rdata align=4
c_timeoffset:
	dd JINGLER_TIMER_OFFSET*8

section tempo rdata align=4
c_ticklength:
	dd SAMPLES_PER_TICK*8

%endif
