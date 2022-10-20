
extern __imp__OpenFile@12
extern __imp__ReadFile@20
extern __imp__CloseHandle@4

global UnpackNotes
global LoadGmDls
global GenerateCode
global RunStaticCode
global RenderSamples
global ResetState
global NoteOn
global NodeOff

OF_READ equ 0x00000000

GMDLS_SIZE equ 0x350000 ; Real size 0x348014
GMDLS_OFFSETS equ 0x43E3A
GMDLS_DATA equ 0x4462C
GMDLS_COUNT equ 495

MAX_STACK equ 0x100
STATE_SPACE equ 0x1000
NOTE_SPACE equ 0x100000
BUFFER_SPACE equ 0x1000000
MUSIC_SPACE equ 0x1000000
MAX_TRACKS equ 0xFF
MAX_NOTE_COUNT equ 0x10000

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
%define NOTE_TONE 1
%define NOTE_VELOCITY 2

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


section unpackn text align=1

UnpackNotes:
	; ESI = Notes
	; ECX = Number of tracks

	mov			edx, 3 ; Component
.componentloop:
	lodsd
	xchg		ebp, eax ; Value factor

	mov			edi, TrackStarts
	mov			ebx, NoteHeaders
	push		ecx
.trackloop:
	mov			[edi], ebx
	scasd

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
	mov			[ebx + edx*4], eax
	add			ebx, byte 16
	jmp			.noteloop

.next_track:
	mov			dword [ebx], 0x80000000 ; Track terminator
	add			ebx, byte 16
	loop		.trackloop
	pop			ecx

	dec			edx
	jns			.componentloop
	ret

section loadgm text align=1

LoadGmDls:
	mov			edi, GmDls

	push		byte OF_READ
	push		edi
	push		GmDlsName
	call		[__imp__OpenFile@12]
	push		eax ; for CloseFile

	push		byte 0
	push		edi
	push		GMDLS_SIZE
	push		edi
	push		eax
	call		[__imp__ReadFile@20]

	call		[__imp__CloseHandle@4]
	ret

section generate text align=1

GenerateCode:
	; ESI = Bytecode
	; EDI = Space for generated code

	mov			edx, OUT_S >> 4
.mainloop:
	xor			eax, eax
	lodsb
	cmp			al, 0
	je			.decode_done
	push		esi
	or			dl, [InoutCodes + eax]
	mov			ebx, _snipsizes
	mov			esi, _snips
	xor			ecx, ecx
.inout_loop:
	add			esi, ecx
	mov			cl, [ebx]
	inc			ebx
	shr			dl, 1
	jc			.not
	rep movsb
.not:
	cmp			[ebx], byte -1
	jne			.inout_loop

.decodeloop:
	cmp			[ebx], byte -1
	jne			.decode
	inc			ebx
	add			esi, ecx
	mov			cl, [ebx]
	inc			ebx
	mov			ebp, esi
.decode:
	add			esi, ecx
	mov			cl, [ebx]
	inc			ebx
	add			al, [ebx]
	inc			ebx
	jnc			.decodeloop

.emit:
	rep movsb
	pop			esi
	call		ebp
	jmp			.mainloop
.decode_done:
	ret

section rstatic text align=1

RunStaticCode:
	; ESI = Constant pool

	ldmxcsr		[MXCSR]
	mov			edi, StateSpace
	mov			ebx, edi
	call		[ProcPointers + 0*4]
	ret

section render text align=1

RenderSamples:
	; EAX = Number of samples to render
	; ESI = Constant pool

	ldmxcsr		[MXCSR]
	push		eax
	xor			ebp, ebp
.sample:
	xor			eax, eax
	mov			[InstrumentIndex], eax

	mov			edi, StateSpace
	mov			ebx, edi
	call		[ProcPointers + 1*4]

	cvtpd2ps	xmm0, [ebx]
	movq		[MusicBuffer + ebp*8], xmm0

	inc			ebp
	cmp			[esp], ebp
	jne			.sample
	pop			eax
	ret

section reset text align=1

ResetState:
	; Clear buffer space
	mov			edi, BufferSpace
	mov			ecx, [BufferAllocPtr]
	sub			ecx, edi
	shr			ecx, 2
	mov			[BufferAllocPtr], edi
	xor			eax, eax
	rep stosd

	; Clear note space
	mov			edi, NoteHeaders
	mov			ecx, MAX_NOTE_COUNT*4
	xor			eax, eax
	rep stosd

	; Clear note chains
	mov			edi, NoteChains
	mov			ecx, MAX_TRACKS
	xor			eax, eax
	rep stosd

	; Make space for notes
	mov			edi, TrackStarts
	mov			ebx, NoteHeaders
	mov			ecx, MAX_TRACKS
.trackloop:
	add			ebx, MAX_NOTE_COUNT/(MAX_TRACKS+1)*16
	mov			dword [ebx], 0x80000000 ; Track terminator
	mov			[edi], ebx
	scasd
	loop		.trackloop
	ret

section noteon text align=1

NoteOn:
	; EAX = Channel
	; EBX = Sample offset
	; ECX = Key
	; EDX = Velocity

	; Push note onto stack of notes to be triggered
	mov			edi, [TrackStarts + eax*4]
	sub			edi, byte 16
	mov			[TrackStarts + eax*4], edi

	jmp			.bubble
.bubbleloop:
	movapd		xmm0, [edi + 16]
	movapd		[edi], xmm0
	add			edi, byte 16
.bubble:
	cmp			[edi + 16], ebx
	jbe			.bubbleloop

	mov			[edi], ebx
	mov			dword [edi + 4], 0x7FFFFFFF
	mov			[edi + 8], ecx
	mov			[edi + 12], edx
	ret

section noteoff text align=1

NoteOff:
	; EAX = Channel
	; EBX = Sample offset
	; ECX = Key

	; First look through notes that haven't been triggered yet
	mov			edi, [TrackStarts + eax*4]
	jmp			.prenote
.prenoteloop:
	add			edi, byte 16
.prenote:
	cmp			[edi], ebx
	ja			.live
	cmp			[edi + 8], ecx
	jne			.prenoteloop

	sub			ebx, [edi]
	jmp			.write

.live:
	; Then look through live notes
	lea			edi, [NoteChains + eax*4]
.noteloop:
	mov			edi, [edi]
	test		edi, edi
	jz			.done
	cmp			[edi + 8], ecx
	jne			.noteloop
	cmp			dword [edi + 4], 0x10000000
	jl			.noteloop

.write:
	mov			[edi + 4], ebx
.done:
	ret


;; Snips

	; In/out snips
	basesnip	r_to_t
	sub			ebx, byte 16

	basesnip	t_to_b
	movapd		[ebx], xmm0

	basesnip	s_to_b
	movapd		xmm0, [ebx]

	basesnip	t_to_r
	add			ebx, byte 16

	; Plain snips
	snipcode	plain

	snip		state_enter, rr, I_STATE_ENTER
	push		edi

	snip		state_leave, rr, I_STATE_LEAVE
	pop			edi

	snip		kill, rs, I_KILL
	pextrb		[ebp-1], xmm0, 7

	snip		cell_init, rs, I_CELL_INIT
	movapd		[edi], xmm0
	add			edi, byte 16

	snip		cell_read, sr, I_CELL_READ
	movapd		xmm0, [edi]
	add			edi, byte 16

	snip		gmdls_sample, rt, I_GMDLS_SAMPLE
	cvtsd2si	eax, [ebx]
	mov			ecx, GmDls + GMDLS_DATA
	add			ecx, [GmDls + GMDLS_OFFSETS + eax*4]
	add			ecx, [ecx] ; Last dword of wsmp chunk
	cvtsd2si	eax, xmm0
	mov			edx, eax
	cmp			dword [ecx], byte 0 ; Loop length or looped flag
	je			.direct
	sub			eax, [ecx - 4] ; Loop base
	jb			.direct
	xor			edx, edx
	div			dword [ecx] ; Loop length
	add			edx, [ecx - 4] ; Loop base
.direct:
	mov			eax, [ecx + 8] ; Length of data chunk
	shr			eax, 1 ; Sample count
	cmp			edx, eax
	jae			.lookup
	lea			ecx, [ecx + 10 + edx*2] ; 2 bytes before sample data
.lookup:
	cvtsi2sd	xmm0, dword [ecx]

	snip		addsub, rt, I_ADDSUB
	addsubpd	xmm0, [ebx]

	snip		fputnext, rt, I_FPUTNEXT
	fld			qword [ebx]

	snip		random, rt, I_RANDOM
	cvtsd2si	eax, xmm0
	mov			ecx, 0xCD9E8D57
	mul			ecx
	xor			eax, edx
	xor			eax, [byte ebx+4]
	mul			ecx
	xor			eax, edx
	xor			eax, [byte ebx+0]
	mul			ecx
	xor			eax, edx
	cvtsi2sd	xmm0, eax

	snip		buffer_load_with_offset, rt, I_BUFFER_LOAD_WITH_OFFSET
	cvtsd2si	eax, xmm0
	add			eax, [ebx]
	cmp			eax, [ebx + 4]
	jb			.no_wrap
	sub			eax, [ebx + 4]
.no_wrap:
	shl			eax, 4
	add			eax, [ebx + 8]
	movapd		xmm0, [eax]

	snip		buffer_load, st, I_BUFFER_LOAD
	mov			eax, [ebx]
	shl			eax, 4
	add			eax, [ebx + 8]
	movapd		xmm0, [eax]

	snip		buffer_store_and_step, rs, I_BUFFER_STORE_AND_STEP
	mov			eax, [ebx]
	shl			eax, 4
	add			eax, [ebx + 8]
	movapd		[eax], xmm0

	dec			dword [ebx]
	jns			.no_wrap
	mov			eax, [ebx + 4]
	add			[ebx], eax
.no_wrap:

	snip		buffer_alloc, ts, I_BUFFER_ALLOC
	mov			eax, [BufferAllocPtr]
	mov			[ebx + 8], eax
	cvtsd2si	eax, xmm0
	mov			[ebx + 4], eax
	shl			eax, 4
	add			[BufferAllocPtr], eax
	and			dword [ebx], byte 0

	snip		call_instrument, ss, I_CALL_INSTRUMENT
	pusha
	mov			ebp, [InstrumentIndex]

.noteloop:
	; Find note headers for track
	lea			ecx, [TrackStarts + ebp*4] ; Note headers for track
	mov			edx, [ecx]

	; Count down to note trigger
	dec			dword [edx]
	jns			.no_more_notes

	; Allocate note object and copy header
	mov			edi, [NoteAllocPtr]
	movapd		xmm0, [edx]
	add			dword [ecx], byte 16
	movapd		[edi], xmm0

	; Chain note object into note list
	mov			eax, [NoteChains + ebp*4]
	mov			[edi], eax
	mov			[NoteChains + ebp*4], edi

	; Call instrument init procedure
	add			edi, byte 16
	call		[ProcPointers + ebp*8 + 2*4]

	; Bump alloc pointer to end of allocated note object
	mov			[NoteAllocPtr], edi
	jmp			.noteloop
.no_more_notes:

	; Process all active notes for this instrument
	lea			eax, [NoteChains + ebp*4]
.activeloop:
	mov			edi, eax
.killloop:
	mov			edi, [edi]
	mov			[eax], edi
	test		edi, edi
	je			.activedone
	cmp			[edi + 15], byte 0
	jne			.killloop
	push		edi

	; Call instrument process procedure
	add			edi, byte 16
	call		[ProcPointers + ebp*8 + 3*4]

	pop			eax
	dec			dword [eax + 4]
	jmp			.activeloop
.activedone:

	inc			dword [InstrumentIndex]
	popa

	snip		exp2_body, ss, I_EXP2_BODY
	; Assumes fop 0xFC (frndint) before.
	fld			qword [ebx]
	fsub		st0, st1
	f2xm1
	fld1
	faddp		st1, st0
	fscale
	fstp		st1

	snip		fdone, ss, I_FDONE
	fstp		qword [ebx]

	; Byte parameter snips
	snipcode	notbyte, I_FOP
	not			al
	stosb

	snip		fop, ss, I_FOP
	fld			qword [ebx]
	db			0xd9 ; Various floating point ops

	; Proc snip
	snipcode	proc, I_PROC
	lea			eax, [edi-3]
	movzx		ecx, dh
	inc			dh
	mov			[ProcPointers + ecx*4], eax

	snip		proc, ss, I_PROC
	pop			ebp
	ret
	push		ebp
	mov			ebp, edi

	; Pointer snips
	snipcode	pointer, I_CONSTANT+I_PROC_CALL+I_NOTE_PROPERTY
	shl			eax, 2
	add			[edi-4], eax

	snip		proc_call, ss, I_PROC_CALL
	call		[ProcPointers]

	snip		constant, sr, I_CONSTANT
	cvtss2sd	xmm0, [dword esi + 0]

	snip		note_property, sr, I_NOTE_PROPERTY
	cvtsi2sd	xmm0, [dword ebp - 3*4]

	; Offset snips
	snipcode	offset, I_STACK_LOAD+I_STACK_STORE+I_CELL_STORE
	shl			eax, 4
	stosd

	snip		stack_load, sr, I_STACK_LOAD
	db			0x66, 0x0f, 0x28, 0x83 ; movapd xmm0, [dword ebx + offset*16]

	snip		stack_store, rs, I_STACK_STORE
	db			0x66, 0x0f, 0x29, 0x83 ; movapd [dword ebx + offset*16], xmm0

	snip		cell_store, rs, I_CELL_STORE
	db			0x66, 0x0f, 0x29, 0x85 ; movapd [dword ebp + offset*16], xmm0

	; Label snips
	snipcode	label, I_LABEL+I_IF
	pop			ecx
	push		edi
	push		ecx

	snip		label, rr, I_LABEL

	snip		if, rr, I_IF
	db			0x0f, 0x82 ; jb label
	dd			0

	; Loopjump snip
	snipcode	loopjump, I_LOOPJUMP
	pop			ecx
	pop			eax
	push		ecx
	mov			[edi-4], eax
	sub			[edi-4], edi

	snip		loopjump, rr, I_LOOPJUMP
	db			0x0f, 0x82 ; jb label
	dd			0

	; Endif snips
	snipcode	endif, I_ENDIF+I_ELSE
	pop			ecx
	pop			eax
	push		ecx
	mov			[eax-4], edi
	sub			[eax-4], eax

	snip		endif, rr, I_ENDIF

	snip		else, rr, I_ELSE
	db			0xe9 ; jmp label
	dd			0
	; Follow by label instruction

	; Immediate snips
	snipcode	immediate, I_COMPARE+I_ROUND
	stosb

	snip		round, st, I_ROUND
	db			0x66, 0x0f, 0x3a, 0x09, 0x03 ; roundpd xmm0, [ebx], mode

	snip		compare, rt, I_COMPARE
	db			0x66, 0x0f, 0xc2, 0x03 ; cmppd xmm0, [ebx], mode

	; Implicit snip
	snipcode	implicit
%if COMPACT_IMPLICIT_OPCODES
	bsr			ecx, eax
	add			ecx, byte 2
	bts			eax, ecx
%endif
	mov			[edi-2], al

	basesnip	implicit
	movapd		xmm0, [ebx] ; <op>pd xmm0, [ebx]

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

section	notept data align=4
NoteAllocPtr:
	dd NoteSpace

section	bufferpt data align=4
BufferAllocPtr:
	dd BufferSpace

section instidx bss align=4
InstrumentIndex:
	resd 1

section tracks bss align=4
TrackStarts:
.align16:
	resd MAX_TRACKS

section notech bss align=4
NoteChains:
.align16:
	resd MAX_TRACKS

section notehdrs bss align=16
NoteHeaders:
.align16:
	reso MAX_NOTE_COUNT

section procptrs bss align=4
ProcPointers:
.align16:
	resd 256

section ckcode bss align=1
GeneratedCode:
.align16:
	resb 65536

section ckstate bss align=16
	reso MAX_STACK
StateSpace:
.align16:
	reso STATE_SPACE

section notesp bss align=16
NoteSpace:
.align16:
	reso NOTE_SPACE

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
	resb GMDLS_SIZE

section gmdlsnam rdata align=1
GmDlsName:
	db "drivers/gm.dls",0
