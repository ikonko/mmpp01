;------------------------------------------------------------------------------
; Rozbali data levelu.
; I: A=<0, 19> cislo levelu
; O: -
; M: vsetky
UnpackLevel:	push	af
		add	a,a
		ld	l,a
		ld	h,0
		ld	de,Levels
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		ld	de,LevelBuffer+1023
UnpackLevelA:	ld	a,(hl)
		rlca
		rrca
		jp	c,UnpackLevelB
		and	0Fh
		add	a,3
		ld	c,a
		ld	a,(hl)
		rrca
		rrca
		rrca
		rrca
		and	7
		ld	b,a
		dec	hl
		ld	a,e
		add	a,(hl)
		dec	hl
		push	hl
		ld	l,a
		ld	a,d
		adc	a,b
		ld	h,a
		ld	b,0
		jp	UnpackLevelF

UnpackLevelB:	dec	hl
		and	7Fh
		jp	z,UnpackLevelD
		cp	40h
		jp	nc,UnpackLevelC
		ld	c,a
		call	UnpackLevelZ
		jp	UnpackLevelA

UnpackLevelC:	and	3Fh
		add	a,2
		jp	UnpackLevelE

UnpackLevelD:	ld	a,(hl)
		dec	hl
		add	a,42h
		jp	nc,UnpackLevelE
		inc	b
UnpackLevelE:	ld	c,a
		ld	a,(hl)
		dec	hl
		ld	(de),a
		push	hl
		ld	h,d
		ld	l,e
		dec	de
UnpackLevelF:	call	UnpackLevelZ
		pop	hl
		ld	a,d
		cp	(LevelBuffer-1) / 256
		jp	nz,UnpackLevelA
		ld	a,e
		cp	(LevelBuffer-1) & 255
		jp	nz,UnpackLevelA
		pop	af
		cp	7
		jp	z,UnpackLevelG
		cp	11
		ret	nz
UnpackLevelG:	ld	a,5
		ld	(LevelBuffer+2),a
		ret

UnpackLevelZ:	ld	a,(hl)
		ld	(de),a
		dec	hl
		dec	de
		dec	bc
		ld	a,b
		or	c
		jp	nz,UnpackLevelZ
		ret

;------------------------------------------------------------------------------
Levels:		defw	Lvl00,Lvl01,Lvl02,Lvl03,Lvl04
		defw	Lvl05,Lvl06,Lvl07,Lvl08,Lvl09
		defw	Lvl10,Lvl11,Lvl12,Lvl13,Lvl14
		defw	Lvl15,Lvl16,Lvl17,Lvl18,Lvl19

;------------------------------------------------------------------------------
		incbin "mmlevel-00.lvl"
Lvl00		equ	$-1
		incbin "mmlevel-01.lvl"
Lvl01		equ	$-1
		incbin "mmlevel-02.lvl"
Lvl02		equ	$-1
		incbin "mmlevel-03.lvl"
Lvl03		equ	$-1
		incbin "mmlevel-04.lvl"
Lvl04		equ	$-1
		incbin "mmlevel-05.lvl"
Lvl05		equ	$-1
		incbin "mmlevel-06.lvl"
Lvl06		equ	$-1
		incbin "mmlevel-07.lvl"
Lvl07		equ	$-1
		incbin "mmlevel-08.lvl"
Lvl08		equ	$-1
		incbin "mmlevel-09.lvl"
Lvl09		equ	$-1
		incbin "mmlevel-10.lvl"
Lvl10		equ	$-1
		incbin "mmlevel-11.lvl"
Lvl11		equ	$-1
		incbin "mmlevel-12.lvl"
Lvl12		equ	$-1
		incbin "mmlevel-13.lvl"
Lvl13		equ	$-1
		incbin "mmlevel-14.lvl"
Lvl14		equ	$-1
		incbin "mmlevel-15.lvl"
Lvl15		equ	$-1
		incbin "mmlevel-16.lvl"
Lvl16		equ	$-1
		incbin "mmlevel-17.lvl"
Lvl17		equ	$-1
		incbin "mmlevel-18.lvl"
Lvl18		equ	$-1
		incbin "mmlevel-19.lvl"
Lvl19		equ	$-1

;------------------------------------------------------------------------------
LevelBuffer:	ds	1024

;------------------------------------------------------------------------------
