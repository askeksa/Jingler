
%include "used_instructions.inc"
%include "clinklang.asm"

global main

extern __imp__fopen
extern __imp__fwrite
extern __imp__fclose

%define SAMPLE_RATE 44100
%xdefine TOTAL_SAMPLES MUSIC_SPACE

section main text align=1

main:
	mov		esi, Notes
	mov		ecx, 1
	call	UnpackNotes

	mov		edi, GeneratedCode
	call	GenerateCode

	call	RunStaticCode

	mov		eax, TOTAL_SAMPLES
	call	RenderSamples


	push	filemode
	push	filename
	call	[__imp__fopen]
	add		esp, byte 2*4

	push	eax
	push	eax
	push	eax
	push	byte 44
	push	byte 1
	push	WavFileHeader
	call	[__imp__fwrite]
	add		esp, byte 4*4

	push	dword [WavFileHeader+40]
	push	byte 1
	push	MusicBuffer
	call	[__imp__fwrite]
	add		esp, byte 4*4

	call	[__imp__fclose]
	add		esp, byte 1*4

	ret


section musdat rdata align=1

%define b(c) _snip_id_%+c

Notes:
	; Velocity
	dd 1
	db 0x80
	; Tone
	dd 1
	db 60, 64, 67, 72, 0x80
	; Length
	dd 5292
	db 2, 1, 9, 1, 0x80
	; Distance
	dd 5292
	db 4, 1, 1, 1, 0x80

Bytecodes:
	; Global init
	db b(proc)

	; Global update
	db b(proc)
	db b(constant)+0, b(call_instrument)

	; Instrument 0 init
	db b(proc)
	db b(constant)+0, b(cell_init)
	db b(constant)+2, b(note_property)+NOTE_TONE, b(div)
	db b(fop)+(~0xfc), b(exp2_body), b(fdone) ; exp2
	db b(constant)+3, b(mul), b(cell_init)
	db b(constant)+5, b(cell_init)
	db b(constant)+0, b(cell_init)

	; Instrument 0 update
	db b(proc)
	db b(cell_read), b(stack_load)+0, b(cell_read), b(add), b(cell_store)+0
	db b(fop)+(~0xfe), b(fdone) ; sin
	db b(cell_read), b(stack_load)+0, b(constant)+4, b(mul), b(cell_store)+2
	db b(mul), b(expand), b(add)
	db b(cell_read), b(stack_load)+0, b(constant)+1, b(add), b(cell_store)+3
	db b(note_property)+NOTE_LENGTH, b(compare)+COMPARE_EQ, b(kill)

	db b(proc), 0

ConstantPool:
	; Constant pool
	dd 0.0
	dd 1.0
	dd 12.0
	dd 0.00116485395967 ; Radians/sample of C-0 at 44100Hz
	dd 0.9999
	dd 0.25

section WavFile rdata align=4
WavFileHeader:
	db	"RIFF"
	dd	36+TOTAL_SAMPLES*8
	db	"WAVE"
	db	"fmt "
	dd	16
	dw	3,2
	dd	SAMPLE_RATE
	dd	SAMPLE_RATE*8
	dw	8,32
	db	"data"
	dd	TOTAL_SAMPLES*8

	section file rdata align=1
filename:
	db "test.wav",0
filemode:
	db "wb",0
