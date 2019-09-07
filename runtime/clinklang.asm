
global ClinklangCompute


MAX_STACK equ 256
STATE_SPACE equ 256
NOTE_SPACE equ 65536
BUFFER_SPACE equ 65536
MUSIC_SPACE equ 0x100000
MAX_TRACKS equ 255
MAX_NOTE_COUNT equ 65536

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
	%endif
%endmacro

%macro snipend 0
	%xdefine _snip_prev_number _snip_prev_id - 1 + IMPLICIT_CODE(LOWEST_IMPLICIT_INSTRUCTION)
	_snip_flush end_of, snips
%endmacro


section ckmain text align=1

ClinklangCompute:
	mov			esi, [esp + 4]
	mov			edi, GeneratedCode

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

UnpackNotes:
	lodsd
	push		eax ; Music length

	mov			edx, 3 ; Component
	xor			eax, eax
	lodsb
	xchg		ebp, eax ; Number of tracks

.componentloop:
	lodsd
	push		eax ; Value factor

	mov			edi, TrackStarts
	mov			ebx, NoteHeaders
	mov			ecx, ebp
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
	imul		eax, [esp]
	mov			[ebx + edx*4], eax
	add			ebx, byte 16
	jmp			.noteloop

.next_track:
	mov			dword [ebx], 0x80000000 ; Track terminator
	add			ebx, byte 16
	loop		.trackloop
	pop			eax

	dec			edx
	jns			.componentloop

	; ESI = Constant pool
RunGeneratedCode:
	xor			ebp, ebp

	mov			edi, StateSpace
	mov			ebx, edi
	call		[ProcPointers + 0*4]

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

	snip		addsub, rt, I_ADDSUB
	addsubpd	xmm0, [ebx]

	snip		cell_init, rs, I_CELL_INIT
	movapd		[edi], xmm0
	add			edi, byte 16

	snip		cell_read, sr, I_CELL_READ
	movapd		xmm0, [edi]
	add			edi, byte 16

	snip		state_enter, rr, I_STATE_ENTER
	push		edi

	snip		state_leave, rr, I_STATE_LEAVE
	pop			edi

	snip		buffer_alloc, ts, I_BUFFER_ALLOC
	mov			eax, [BufferAllocPtr]
	mov			[ebx + 4], eax
	cvtsd2si	eax, xmm0
	mov			[ebx], eax
	shl			eax, 4
	add			[BufferAllocPtr], eax

	snip		buffer_load, sr, I_BUFFER_LOAD
	xor			edx, edx
	cvtsd2si	eax, [ebx]
	add			ebx, byte 16
	div			dword [ebx]
	shl			edx, 4
	add			edx, [ebx + 4]
	movapd		xmm0, [edx]

	snip		buffer_store, rs, I_BUFFER_STORE
	xor			edx, edx
	cvtsd2si	eax, [ebx]
	add			ebx, byte 16
	div			dword [ebx]
	shl			edx, 4
	add			edx, [ebx + 4]
	movapd		[edx], xmm0

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

	snip		trigger, ss, I_TRIGGER
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
	mov			eax, [NoteChain]
	mov			[edi], eax
	mov			[NoteChain], edi

	; Call instrument init procedure
	add			edi, byte 16
	call		[ProcPointers + ebp*8 + 2*4]

	; Bump alloc pointer to end of allocated note object
	mov			[NoteAllocPtr], edi
	jmp			.noteloop
.no_more_notes:

	; Process all active notes for this instrument
	mov			eax, NoteChain
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
	jmp			.activeloop
.activedone:

	inc			dword [InstrumentIndex]
	popa

	snip		kill, rs, I_KILL
	pextrb		[ebp-1], xmm0, 7

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

	; Pointer snips
	snipcode	pointer, I_CONSTANT+I_PROC_CALL+I_NOTE_PROPERTY
	shl			eax, 2
	add			[edi-4], eax

	snip		constant, sr, I_CONSTANT
	cvtss2sd	xmm0, [dword esi + 0]

	snip		proc_call, ss, I_PROC_CALL
	call		[ProcPointers]

	snip		note_property, sr, I_NOTE_PROPERTY
	cvtsi2sd	xmm0, [dword ebp - 3*4]

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

	; Immediate snips
	snipcode	immediate, I_COMPARE+I_ROUND
	stosb

	snip		compare, rt, I_COMPARE
	db			0x66, 0x0f, 0xc2, 0x03 ; cmppd xmm0, [ebx], mode

	snip		round, st, I_ROUND
	db			0x66, 0x0f, 0x3a, 0x09, 0x03 ; roundpd xmm0, [ebx], mode

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
		%error "Invalid input kind" _in
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
		%error "Invalid output kind" _out
	%endif
	db _inkind | _outkind
%endmacro

%define _implicit_index 0

%macro iinstr 3 ; name, inout, repr
	%if COMPACT_IMPLICIT_OPCODES
		%if ((%3) & 0xfc != 0x14 && (%3) & 0xf8 != 0x28 && (%3) & 0xf0 != 0x50)
			%error "Uncompactable implicit opcode"
		%endif
	%endif
	%xdefine _code (IMPLICIT_CODE(%3) - IMPLICIT_CODE(LOWEST_IMPLICIT_INSTRUCTION) + 1)
	%xdefine _snip_id_%1 _code
	%rep _code - _implicit_index
		db 0
	%endrep
	%defstr _inout_%1 %2
	inout _inout_%1
	%xdefine _implicit_index (_code + 1)
%endmacro

section inout rdata align=1

InoutCodes:
	; Inout for implicit instructions
	iinstr	expand, bt, 0x14 ; unpcklpd
	iinstr	split_rl, sr, 0x15 ; unpckhpd
	iinstr	merge_lr, rt, 0x16 ; movhpd m -> r
	iinstr	split_lr, tr, 0x17 ; movhpd r -> m
	iinstr	pop, rb, 0x28 ; movapd m -> r
	iinstr	popnext, rb, 0x29 ; movapd r -> m
	iinstr	buffer_length, st, 0x2a ; cvtpi2pd
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
	%rep 256 - _n_instructions - _implicit_index
		db 0
	%endrep
	%xdefine _index 0
	%rep _n_instructions
		%substr _inout _inout_string (_index * 2 + 1), 2
		inout _inout
		%xdefine _index (_index + 1)
	%endrep

section	notept data align=4
NoteAllocPtr:
	dd NoteSpace

section	bufferpt data align=4
BufferAllocPtr:
	dd BufferSpace

section notech data align=4
NoteChain:
	dd 0

section instidx bss align=4
InstrumentIndex:
	resd 1

section tracks bss align=4
TrackStarts:
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
