
extern __imp____libm_sse2_sin
global DecodeMain


first_msvcrt_function equ __imp____libm_sse2_sin
SAMPLES_PER_TICK equ 2362

LOWEST_IMPLICIT_INSTRUCTION equ 0x14

;; Snip macros

section snips text align=1
	_snips:
section snipsize data align=1
	_snipsizes:
%define _snip_prev _snips
%define _snip_code 0
%define _snip_notfirst 0
%define _snip_prev_number 0
%define _snip_prev_id 256
%define _inout_string ""

%macro _snip_flush 2 ; kind, name
	section snips
	%if _snip_code
		ret
	%endif
	_%1_%2:
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
%endmacro

%macro basesnip 1
	_snip_flush snip, %1
	%define _snip_code 0
	section snips
%endmacro

%macro snip 3 ; name, inout, number
	%defstr _inout_%1 %2
	%rep %3
		%strcat _inout_string _inout_%1 _inout_string
	%endrep
	basesnip %1
	%xdefine _snip_id_%1 _snip_prev_id-%3
	%xdefine _snip_number_%1 %3
	%define _snip_prev_number _snip_number_%1
	%define _snip_prev_id _snip_id_%1
%endmacro

%macro snipcode 1
	_snip_flush snipcode, %1
	db -1
	%define _snip_code 1
	%define _snip_prev_number 0
	section snips
%endmacro

%macro snipend 0
	%xdefine _snip_prev_number _snip_prev_id - 1 + LOWEST_IMPLICIT_INSTRUCTION
	_snip_flush end_of, snips
%endmacro


section decoder text align=1
;; Central decode loop
; TODO: Add actual entry point
DecodeMain:
	mov			edx, OUT_S >> 4

.mainloop:
	xor			eax, eax
	lodsb
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

	cmp			[esi], byte 0
	jne			.mainloop
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

	snip		addsub, rt, 1
	addsubpd	xmm0, [ebx]

	snip		init_cell, rs, 1
	movapd		[edi], xmm0
	add			edi, byte 16

	snip		read_cell, sr, 1
	movapd		xmm0, [edi]
	add			edi, byte 16

	snip		output, rs, 1
	; Assumes xmm0 is already converted to ps
	cvtsd2si	eax, [ebx]
	movq		[MusicBuffer + eax*8], xmm0

	snip		buffer_alloc, ts, 1
	mov			eax, [BufferAllocPtr]
	mov			[ebx + 4], eax
	cvtsd2si	eax, xmm0
	mov			[ebx], eax
	add			[BufferAllocPtr], eax

	snip		buffer_load, sr, 1
	xor			edx, edx
	cvtsd2si	eax, [ebx]
	add			ebx, byte 16
	div			dword [ebx]
	shl			edx, 4
	add			edx, [ebx + 4]
	movapd		xmm0, [edx]

	snip		buffer_store, rs, 1
	xor			edx, edx
	cvtsd2si	eax, [ebx]
	add			ebx, byte 16
	div			dword [ebx]
	shl			edx, 4
	add			edx, [ebx + 4]
	movapd		[edx], xmm0

	snip		random, rt, 1
	cvtsd2si	eax, xmm0
	mov			ecx, 0xCD9E8D57
	mul			ecx
	xor			eax, edx
	xor			eax, dword [byte ebx+4]
	mul			ecx
	xor			eax, edx
	xor			eax, dword [byte ebx+0]
	mul			ecx
	xor			eax, edx
	cvtsi2sd	xmm0, eax

	snip		trigger, rs, 1
	pusha

	; Are we at a tick?
	xor			edx, edx
	cvtsd2si	eax, xmm0
	mov			ecx, SAMPLES_PER_TICK
	div			ecx
	test		edx, edx
	jne			.notrigger

	; Loop through all notes for this instrument
	mov			esi, [NoteDistancePtr]
	xchg		eax, ebp
.noteloop:
	; Load note distance
	xor			eax, eax
	cmp			[esi], byte 0
	jge			.bytedist
	lodsb
	not			al
	mov			ah, al
.bytedist:
	lodsb

	sub			ebp, eax
	jne			.notnow
	; Triggered now

	; Allocate note object and chain it into note list
	mov			eax, [NoteChainEndPtr]
	mov			edi, [NoteAllocPtr]
	mov			[NoteChainEndPtr], edi
	mov			[eax], edi
	scasd

	; Fetch length
	mov			eax, [NoteLengthPtr]
	movzx		eax, byte [eax]
	mov			ecx, SAMPLES_PER_TICK
	mul			ecx
	stosd

	; Fetch tone
	mov			eax, [NoteTonePtr]
	movzx		eax, byte [eax]
	stosd

	; Fetch velocity
	mov			eax, [NoteVelocityPtr]
	movzx		eax, byte [eax]
	stosd

	; Call instrument init procedure
	mov			eax, [InstrumentIndex]
	call		[ProcPointers + eax*8]

	; Bump alloc pointer to end of allocated object
	mov			[NoteAllocPtr], edi

.notnow:
	inc			dword [NoteLengthPtr]
	inc			dword [NoteTonePtr]
	inc			dword [NoteVelocityPtr]

	cmp			[esi], byte 0x80
	jne			.noteloop
	lodsb
	mov			[NoteDistancePtr], esi
.notrigger:

	; Process all active notes for this instrument
	mov			eax, NoteChainStart
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
	mov			eax, [InstrumentIndex]
	call		[ProcPointers + eax*8 + 4]

	pop			eax
	jmp			.activeloop
.activedone:
	mov			[NoteChainEndPtr], eax

	inc			dword [InstrumentIndex]
	popa

	snip		kill, rs, 1
	pextrb		[ebp-1], xmm0, 7

	; Proc snip
	snipcode	proc
	lea			eax, [edi-3]
	movzx		ecx, dh
	inc			dh
	mov			[ProcPointers + ecx*4], eax

	snip		proc, ss, 1
	pop			ebp
	ret
	push		ebp
	mov			ebp, edi

	; Offset snips
	snipcode	offset
	shl			eax, 4
	stosd

	snip		load_stack, sr, 1
	db			0x66, 0x0f, 0x28, 0x83 ; movapd xmm0, [dword ebx + offset*16]

	snip		store_stack, rs, 1
	db			0x66, 0x0f, 0x29, 0x83 ; movapd [dword ebx + offset*16], xmm0

	snip		store_cell, rs, 1
	db			0x66, 0x0f, 0x29, 0x85 ; movapd [dword ebp + offset*16], xmm0

	; Immediate snips
	snipcode	immediate
	stosb

	snip		compare, rt, 7
	db			0x66, 0x0f, 0xc2, 0x03 ; cmppd xmm0, [ebx], mode

	snip		round, st, 4
	db			0x66, 0x0f, 0x3a, 0x09, 0x03 ; roundpd xmm0, [ebx], mode

	; Label snip
	snipcode	label
	pop			ecx
	push		edi
	push		ecx

	snip label, rr, 1

	; Jump snip
	snipcode	jump
	pop			ecx
	pop			eax
	push		ecx
	stosd
	sub			[edi-4], edi

	snip		jump, rr, 1
	db			0x0f, 0x8c ; jl label

	; Pointer snips
	snipcode	pointer
	shl			eax, 2
	add			[edi-4], eax

	snip		constant_mono_f32, sr, 1
	cvtss2sd	xmm0, dword [ConstantPool]

	snip		proc_call, ss, 1
	call		[ProcPointers]

	snip		msvcrt_call_1_1, rr, 1
	call		[first_msvcrt_function]

	snip		msvcrt_call_2_1, rt, 1
	movapd		xmm1, [ebx]
	call		[first_msvcrt_function]

	snip		noteproperty, sr, 3
	cvtsi2sd	xmm0, [dword ebp - 3*4]

	; Implicit snip
	snipcode	implicit
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
	%xdefine _code ((%3) - LOWEST_IMPLICIT_INSTRUCTION + 1)
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
	iinstr	expand, bt, 0x14
	iinstr	split_rl, sr, 0x15
	iinstr	merge_lr, rt, 0x16
	iinstr	split_lr, tr, 0x17
	iinstr	pop, rs, 0x28
	iinstr	popnext, rb, 0x29
	iinstr	buffer_length, st, 0x2a
	iinstr	cmp, rr, 0x2e
	iinstr	sqrt, st, 0x51
	iinstr	and, rt, 0x54
	iinstr	andn, rt, 0x55
	iinstr	or, rt, 0x56
	iinstr	xor, rt, 0x57
	iinstr	add, rt, 0x58
	iinstr	mul, rt, 0x59
	iinstr	cvtpd2ps, st, 0x5a
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

section	ptrs bss align=4
NoteAllocPtr:
	resd 1
BufferAllocPtr:
	resd 1
NoteDistancePtr:
	resd 1
NoteLengthPtr:
	resd 1
NoteChainStart:
	resd 1
NoteChainEndPtr:
	resd 1
NoteTonePtr:
	resd 1
NoteVelocityPtr:
	resd 1
InstrumentIndex:
	resd 1

section procptrs bss align=4
ProcPointers:
	resd 256

section music bss align=8
MusicBuffer:
	resq 1000000

section constant rdata align=4
ConstantPool:
	times 256 dd 0
