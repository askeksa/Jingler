; Stand-alone Windows executable for playing a Jingler song.

global _main

extern __imp__printf
extern __imp__CreateThread@24
extern __imp__GetAsyncKeyState@4
extern __imp__ExitProcess@4
extern __imp__Sleep@4

%define L(label) _%+label

extern L(Jingler_GenerateMusic)
extern L(Jingler_Ready)
extern L(Jingler_StartMusic)
extern L(Jingler_GetPosition)
extern L(Jingler_MusicBuffer)
extern L(Jingler_TicksPerSecond)
extern L(Jingler_MusicLength)


	section main text align=1

_main:
	push	message
	push	messageformat
	call	[__imp__printf]
	add		esp, byte 2*4

	push	byte 0
	push	byte 0
	push	byte 0
	push	L(Jingler_GenerateMusic)
	push	byte 0
	push	byte 0
	call	[__imp__CreateThread@24]

.ready:
	cmp	dword [L(Jingler_Ready)], byte 0
	je	.ready

	push	1000
	call	[__imp__Sleep@4]

	call	L(Jingler_StartMusic)

.playloop:
	mov		ebx, 60

	fild	dword [L(Jingler_MusicLength)]
	fdiv	dword [L(Jingler_TicksPerSecond)]
	push	eax
	fistp	dword [esp]
	pop		eax ; music length in seconds

	xor		edx, edx
	div		ebx
	push	edx
	push	eax

	call	L(Jingler_GetPosition)
	fdiv	dword [L(Jingler_TicksPerSecond)]
	push	eax
	fistp	dword [esp]
	pop		eax ; play position in seconds

	xor		edx, edx
	div		ebx
	push	edx
	push	eax

	push	timeformat
	call	[__imp__printf]
	add		esp, byte 5*4

	push	byte 100
	call	[__imp__Sleep@4]

	push	byte 27
	call	[__imp__GetAsyncKeyState@4]
	test	ax, ax
	je		.playloop

	push	byte 0
	call	[__imp__ExitProcess@4]

	section mformat rdata align=1
messageformat:
	db "%s",0

	section tformat rdata align=1
timeformat:
	db 13,"Playing %d:%02d / %d:%02d",0

	section message rdata align=1
message:
	incbin "music.txt"
	db 0
