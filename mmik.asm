;------------------------------------------------------------------------------
;		cpu	8080
;------------------------------------------------------------------------------
RollReg	equ	0C0h						; ROLL register
ColorReg	equ	0CCh						; Color register
VramAdr	equ	0A000h					; PP 01 VRAM address
CodeAdr	equ	0900h					; start of main program
ZXFont	equ	7424						; start of ZX font in memory
DataStart	equ	4096
Caverns	equ	8192

		org	CodeAdr 
		
		xor	a
		out	(ColorReg),a
		out	(RollReg),a
		
		call	Cls
;		ld	a,0
;		ld	(CurrentCavernNumber),a
;		jp	MainLoop
;		jp	Start
;----------------------------------------------------------------;
; Loading screen immitation
;----------------------------------------------------------------;
		ld	hl,EmptyCavernScreenBuffer	; Clear working area
		ld	de,EmptyCavernScreenBuffer+1
		ld	bc,4096+256
		ld	(hl),0
		call	ldir80
		
		ld	hl,LoadingScreenData          ; Copy attributes to buffer
		ld	de,EmptyCavernAttributeBuffer+256
		ld	bc,256
		call	ldir80
		
		ld	hl,EmptyCavernScreenBuffer
		ld	de,VramAdr
		call	OneThird
		ld	hl,EmptyCavernScreenBuffer+2048
		ld	de,VramAdr+2048
		call	OneThird
	
		ld	c,5
FlashMore:
		push	bc		
		ld	hl,EmptyCavernAttributeBuffer+256
		ld	b,0
FlashAttr:
		ld	a,(hl)
		ld	e,a
		rrca
		rrca
		rrca
		and	$07
		ld	d,a
		ld	a,e
		rlca
		rlca
		rlca
		and	$38
		or	d
		ld	(hl),a
		inc	hl
		dec	b
		jp	nz,FlashAttr	

		ld	hl,EmptyCavernScreenBuffer
		ld	de,VramAdr
		call	OneThird
		ld	hl,EmptyCavernScreenBuffer+2048
		ld	de,VramAdr+2048
		call	OneThird
		
		pop	bc
		dec	c
		jp	nz,FlashMore
		
;----------------------------------------------------------------;
;                                                                ;
; Display the title screen and play the theme tune               ;
;                                                                ;
; The first thing this routine does is initialise some game      ;
; status buffer variables in preparation for the next game.      ;
;                                                                ;
;----------------------------------------------------------------;
Start:	XOR	A                              ; A=0.
		LD	(CurrentCavernNumber),A		; Initialise the current cavern number.
		LD	(KempJoystickIndicator),A	; Initialise the Kempston joystick indicator.
		LD	(GameModeIndicator),A		; Initialise the game mode indicator.
		LD	(MusicNoteIndex),A			; Initialise the in-game music note index.
		LD	(ScreenFlashCounter),A		; Initialise the screen flash counter.
		LD	A,2
		LD	(LivesRemaining),A			; Initialise the number of lives remaining.
		LD	HL,MusicFlags
		;SET 0,(HL)					
		ld	a,(hl)
		or	1
		ld	(hl),a                        ; Initialise the keypress flag in bit 0.

; Next, prepare the screen.
		call	Cls						; Clear the entire display file.

		ld	a,15						; Set white color
		out	(ColorReg),a

ShowTitleScr:
		ld	hl,TitleScreenDataTop 		; Copy the graphic data to the top two-thirds of the display file.
		ld	de,VramAdr
		call	OneThird
		ld	hl,TitleScreenDataMiddle
		ld	de,VramAdr+2048
		call	OneThird

; Originaly was:  Copy the attribute bytes to the bottom two-thirds of the attribute file.
        	ld	a,11						; Replacement routine for last third attribute copy                       	
        	ld	de,45056
        	call	FillArea                		; Draw yellow line at (16,0)
        
        	ld	a,9						; Set red color
        	out	(ColorReg),a

        	ld	de,45312
        	ld	c,10
RedPartLine17:							; Draw red part of AIR line
        	push	de
        	push	bc
        	ld	a,31
        	call	PrintASingleCharacter
        	pop	bc
        	pop	de
        	inc	de
        	dec	c
        	jp	nz,RedPartLine17

        	ld	a,10						; Set green color        
        	out	(ColorReg),a

        	ld      c,22
 GreenPartLine17:						; Draw green part of AIR line
        	push	de
        	push	bc
        	ld	a,31
        	call	PrintASingleCharacter
        	pop	bc
        	pop	de
        	inc	de
        	dec	c
		jp	nz,GreenPartLine17       

		ld	hl,VramAdr+9*256+29			; Draw Willy at (9,29).
		ld	de,WillySpriteData1
		ld	a,d
		ld	(WillySpriteDataPrev),a
        	ld	a,e
        	ld	(WillySpriteDataPrev+1),a	; Store last Willy position
		ld	c,0
		ld	a,15						; In white color
		out	(ColorReg),a
		call	DrawASprite

; And finally, play the theme tune and check for keypresses.
Start3:	ld	hl,TitleScreenTuneData        ; Point HL at the theme tune data.
		call	PlayTheThemeTune              ; Play the theme tune.


		XOR	A						; Initialise the game status buffer variable.
        	LD	(MultiUseCoordinateStore),A	; this will be used as an index for the message scrolled across.
                                             ; the screen.
Start4: 	ld  a,11
        	out (ColorReg),a                   ; Yellow characters on black background

		LD	A,(MultiUseCoordinateStore)	; Pick up the message index.
		LD hl,TitleScreenBanner            ; Point IX at the corresponding location in the message
                                             ; (TitleScreenBanner + MultiUseCoordinateStore).
		LD l,a
		LD DE,VramAdr+19*256			; Print 32 characters of the message at (19,0).
		LD C,32
		CALL PrintAMessage
 
         	LD	BC,30					; Pause for about 0.1s.
Start5: 	dec	b
        	jp	nz,Start5
		DEC	C
		jp	NZ,Start5
		
		ld	hl,VramAdr+9*256+29			; Undraw Willy at (9,29).
		ld	a,(WillySpriteDataPrev)
		ld	d,a
		ld	a,(WillySpriteDataPrev+1)
		ld	e,a
		ld	c,0
		ld	a,9						; In red color
		out	(ColorReg),a
		call	DrawASprite

		in	a,($C1)					; Check whether SPACE or the fire button is being pressed
		rrca
		jp	nc,Start6					; Skip message after SPACE or fire pressed
				
		LD A,(MultiUseCoordinateStore)	; Pick up the message index
		AND 6                              ; Keep only bits 1 and 2, and move them into bits 6 and 7, so
          							; that A holds 0, 64, 128 or 192;
		RRCA                               ; This value determines the animation frame to use for Willy.
		RRCA
		RRCA
		LD	DE,WillySpriteData			; Point DE at the graphic data for Willy's sprite.
                                             ; (WillySpriteData + A).
		LD	E,A
		ld	a,d
		ld	(WillySpriteDataPrev),a
        	ld	a,e
        	ld	(WillySpriteDataPrev+1),a
		ld	hl,VramAdr+9*256+29			; Draw Willy at (9,29).
		ld	c,0
		ld	a,15						; In white color
		out	(ColorReg),a
		call	DrawASprite
        	
		push	de
		ld 	de,45824
		ld	a,8                           ; Clear message area by black
		call	FillArea
        	pop	de
                               
		LD	A,(MultiUseCoordinateStore)   ; Pick up the message index.
		INC	A                             ; Increment it.
		CP	224                           ; Set the zero flag if we've reached the end of the message.
		LD	(MultiUseCoordinateStore),A   ; Store the new message index.
		jp	NZ,Start4                     ; Jump back unless we've finished scrolling the message across
                                             ; the screen.
                                             
		LD	A,64                          ; Initialise the game mode indicator to 64: demo mode.
		LD	(GameModeIndicator),A

; Start the game (or demo mode).
Start6:	push	de
		ld 	de,45824
		ld	a,8                           ; Clear message area by black
		call	FillArea
        	pop	de
        	
		LD	HL,Score1                     ; Initialise the score.
		LD	DE,Score2
		LD	BC,9
		LD	(HL),48
          call	ldir80                        ; LDIR

; This entry point is used when teleporting into a cavern or reinitialising the current cavern after Willy has lost a life.
Start7:	             					; Need to clear the room name on line 16
		ld	a,11						; Replacement routine for last third attribute copy                       	
        	ld	de,45056
        	call	FillArea                		; Draw yellow line at (16,0)

		ld	a,(CurrentCavernNumber)
		call	UnpackLevel		
		
		ld	hl,LevelBuffer
		LD	DE,EmptyCavernAttributeBuffer	; Copy the cavern's attribute bytes into the buffer.
		LD	BC,512
		call	ldir80
;		LDIR

		LD	DE,CavernName				; Copy the rest of the cavern definition into the
		LD	BC,512					; game status buffer.
		call	ldir80
;		LDIR
		pop	af						; Recall the cavern settings
		CALL DrawCurrentCavernToScreenBuffer	; Draw the current cavern to the screen buffer.
		
		ld	a,8
		out	(ColorReg),a
		LD HL,VramAdr+18*256			; Clear the bottom third of the display file.
		LD DE,VramAdr+18*256+1
		LD BC,1536
		LD (HL),255
		call	ldir80
;		LDIR

		ld	a,8
		out	(ColorReg),a
		LD	HL,CavernName				; Print the cavern name at (16,0).
		LD	C,32
		LD	DE,VramAdr+16*256
		CALL PrintAMessage
		ld	a,15
		out	(ColorReg),a
		LD	HL,AirText				; Print 'AIR' at (17,0).
		LD	C,3
		ld	a,15
		out	(ColorReg),a
		LD	DE,VramAdr+17*256
		CALL	PrintAMessage
		
		ld	a,15
		out (ColorReg),a				; Air supply shown in white
		LD	A,0B1h 					; Initialise A to 82; this is the MSB of the display file
									; address at which to start drawing the bar that represents
                                             ; the air supply.
		
Start8:	LD H,A                             ; Prepare HL for drawing a row of pixels in the air bar.
		ld	de,32					; Prepare DE for address increment between lines
		
          LD L,68

		ld	b,4						; How many lines of air supply we draw
AirSupplyBar:
		
		LD	A,(RemainingAirSupply)        ; Pick up the value of the initial air supply.
		;ld	a,64
		SUB	35                            ; Now C determines the length of the air bar (in cell widths).
		LD C,A

		push	hl						; Keep HL to calculate next line start address

AirSupplyLine:
		LD (HL),$FF					; Draw a single row of pixels across C cells.
		inc	hl						; Increment the address of next cell we draw 
		dec	c						; Have we drawn complete line yet?
		jp	nz,AirSupplyLine

		pop	hl						; Increment the display file address in HL
		add	hl,de					; (moving down to the next row of pixels).

		dec	b						; Have we drawn all four rows of pixels in the air bar yet?
		jp	nz,AirSupplyBar			; If not, jump back to draw the next one.		
				
		ld	a,11
		out	(ColorReg),a
		LD	HL,HighScoreText 			; Print 'High Score 000000   Score 000000' at (19,0).
		LD 	DE,VramAdr+19*256
		LD	C,32
		CALL	PrintAMessage
;                                  LD A,(BorderColor)                       ; Pick up the border colour for the current cavern.
;                                  LD C,254                                 ; Set the border colour.
;                                  OUT (C),A
; What to do with border? Last 8 lines of the screen?

		LD	A,(GameModeIndicator)		; Pick up the game mode indicator.
		OR	A                             ; Are we in demo mode?
;                                  JR Z,MainLoop                            ; If not, enter the main loop now.
		LD	A,64                                  ; Reset the game mode indicator to 64 (we're in demo mode).
		LD	(GameModeIndicator),A

;----------------------------------------------------------------;
;                                                                ;
; Main loop                                                      ;
;                                                                ;
;----------------------------------------------------------------;
MainLoop:	LD	A,(LivesRemaining)                    ; Pick up the number of lives remaining.
		LD	HL,VramAdr+4096+5*256				; Set HL to the display file address at which to draw the first
                                                                           ; Willy sprite.
		OR	A                                     ; Are there any lives remaining?
		jp	Z,MainLoop3                           ; Jump if not.
		LD B,A                                   ; Initialise B to the number of lives remaining.

; The following loop draws the remaining lives at the bottom of the screen.
MainLoop2:
		LD	C,0                                   ; C=0; this tells the sprite-drawing routine to overwrite any
                                                                           ; existing graphics.
		PUSH	HL                                  ; Save HL and BC briefly.
		PUSH	BC
		LD	A,(MusicNoteIndex)                    ; Pick up the in-game music note index this will determine the
                                                                           ; animation frame for the Willy sprites.
		RLCA                                     ; Now A=0 (frame 0), 32 (frame 1), 64 (frame 2) or 96
		RLCA                                     ; (frame 3).
		RLCA
		AND	$60
		LD	E,A                                   ; Point DE at the corresponding Willy sprite
		LD	D,high WillySpriteData                ; (at WillySpriteData + A).
		ld	a,14
		out	(ColorReg),a
		CALL DrawASprite                         ; Draw the Willy sprite on the screen.
		POP	BC                                   ; Restore HL and BC.
		POP	HL
		INC	HL                                   ; Move HL along to the location at which to draw the next
		INC	HL                                   ; Willy sprite.
		dec	b
		jp	nz,MainLoop2                           ; Jump back to draw any remaining sprites.

; Now draw a boot if cheat mode has been activated.
MainLoop3:
		LD	A,(KeyCounter)                ; Pick up the 6031769 key counter.
		CP	7                             ; Has 6031769 been keyed in yet?
		JP	NZ,MainLoop4                  ; Jump if not.
		LD	DE,BootGraphicData            ; Point DE at the graphic data for the boot.
		LD	C,0                           ; C=0 (overwrite mode).
		CALL	DrawASprite                   ; Draw the boot at the bottom of the screen next to the
                                                                           ; remaining lives.
MainLoop4:
		LD	HL,EmptyCavernAttributeBuffer	; Copy the contents of the attribute buffer.
                                        	; (the attributes for the empty cavern).
		LD	DE,AttributeBufferCWGI        ; Into the attribute buffer at AttributeBufferCWGI.
		LD	BC,512
;         LDIR
		call	ldir80
		LD	HL,EmptyCavernScreenBuffer	; Copy the contents of the screen buffer.
                                             ; (the tiles for the empty cavern).
		LD	DE,ScreenBufferCWGI			; Into the screen buffer at ScreenBufferCWGI.
		LD	BC,4096
;         LDIR
		call	ldir80
;         CALL MoveHorzGuardians                   ; Move the horizontal guardians in the current cavern.
          LD	A,(GameModeIndicator)                 ; Pick up the game mode indicator.
		OR	A                                     ; Are we in demo mode?
;         CALL Z,MoveWilly1                        ; If not, move Willy.
          LD	A,(GameModeIndicator)                 ; Pick up the game mode indicator.
		OR	A                                     ; Are we in demo mode?
;         CALL Z,CheckSetAttributeForWSIB          ; If not, check and set the attribute bytes for Willy's sprite.
                                                                           ; in the buffer at 23552,and draw Willy to the screen buffer
                                                                           ; at 24576.
;		CALL MoveConveyorInTheCurrentCavern      ; Move the conveyor in the current cavern.
                                                                           ; Willy is touching.
                                                                           
		ld	a,8
		ld	(ScreenFlashCounter),a

MainLoop5:
		CALL DrawThePortal                 ; Draw the portal, or move to the next cavern if Willy has

MainLoop6:	
		LD	HL,ScreenBufferCWGI           ; Copy the contents of the screen buffer at ScreenBufferCWGI to
                                             ; the display file.
		LD	DE,VramAdr
		call	OneThird
		ld	hl,ScreenBufferCWGI+2048
		ld	de,VramAdr+2048
		call	OneThird

		ld	c,1
		CALL DrawHorzontalGuardians              ; Draw the horizontal guardians in the current cavern.

;		CALL DrawCollectItemsWillyTouching       ; Draw the items in the current cavern and collect any that
		
		LD A,(ScreenFlashCounter)                ; Pick up the screen flash counter from ScreenFlashCounter.
		OR A                                     ; Is it zero?
		JP Z,MainLoop7                           ; Jump if so
		
		ld	a,8
		out (ColorReg),a

		ld	b,10
NxtShw:	push	bc
		call	ldelay
		pop	bc
		dec	b
		jp	nz,NxtShw
		

		ld	a,(ScreenFlashCounter)		
WholeBufferShade:
		ld	hl,VramAdr-32
		ld	de,32
NextULine:		
		add	hl,de
		dec	a
		jp	nz,NextULine

		ld	c,16
Ln16:	push	hl
		ld	b,32
ClrLn:		
		ld	(hl),255
		inc	hl
		dec	b
		jp	nz,ClrLn
		pop	hl
		
		inc	h
		dec	c
		jp	nz,Ln16
		call	ldelay			
		ld	a,(ScreenFlashCounter)
		or	a
		jp	z,ScrnShaded
		dec	a
		ld	(ScreenFlashCounter),a
		jp	nz,WholeBufferShade
		
ScrnShaded:			
		ld	a,8
		ld	(ScreenFlashCounter),a
			
;		DEC A                                    ; Decrement the screen flash counter at ScreenFlashCounter.
;		dec a
;		LD (ScreenFlashCounter),A
;		RLCA                                     ; Move bits 0-2 into bits 3-5 and clear all the other bits.
;		RLCA
;		RLCA
;		AND $38
;		LD HL,AttributeBufferCWGI                ; Set every attribute byte in the buffer at AttributeBufferCWGI
;		LD DE,AttributeBufferCWGI+1              ; to this value.
;		LD BC,511
;		LD (HL),A
;		call	ldir80
MainLoop7:
;		LD HL,AttributeBufferCWGI                ; Copy the contents of the attribute buffer at
;		LD DE,22528                              ; AttributeBufferCWGI to the attribute file.
;                                  LD BC,512
;                                  call ldir80	;LDIR
		
;		LD	HL,ScreenBufferCWGI           ; Copy the contents of the screen buffer at ScreenBufferCWGI to
                                             ; the display file.
;		LD	DE,VramAdr
;		call	OneThird
;		ld	hl,ScreenBufferCWGI+2048
;		ld	de,VramAdr+2048
;		call	OneThird

		ld	a,11
		out	(ColorReg),a
		LD	HL,Score2+3                           ; Print the score (Score2 + 3) at (19,26).
		LD	DE,VramAdr+19*256+26	; 20602
		LD	C,6
		CALL	PrintAMessage
		LD HL,HighScore                          ; Print the high score HighScore at (19,11).
		LD DE,VramAdr+19*256+11		; 20587
		LD C,6
		CALL PrintAMessage
;		ret
		
;		ld	a,(ScreenFlashCounter)
;		or	a
;		jp	z,NextCavern
;		jp	MainLoop6
;NextCavern:
		
		
		ld	a,(CurrentCavernNumber)
		inc	a
		ld	(CurrentCavernNumber),a
		cp	20
		jp	z,Start
;		ret	z
		jp	Start7
;		ret

;----------------------------------------------------------------;
;                                                                ;
; Move Willy (1)                                                 ;
;                                                                ;
; This routine deals with Willy if he's jumping or falling.      ;
;                                                                ;
;----------------------------------------------------------------;
MoveWilly1:	ret


;----------------------------------------------------------------;
;                                                                ;
; Kill Willy.                                                    ;
;                                                                ;
; When Willy lands after falling from too great a height.        ;
; When Willy collides with a horizontal guardian.                ;
; When Willy collides with Eugene.                               ;
; When Willy collides with a vertical guardian.                  ;
; When Willy collides with the Kong Beast.                       ;
;                                                                ;
;----------------------------------------------------------------;

KillWilly:                         ;POP HL                                   ; Drop the return address from the stack.
KillWilly1:                        ;POP HL                                   ; Drop the return address from the stack.
		ret
; This entry point is used when a Skylab falls on Willy.

KillWilly2:                        
		ret
		LD A,$FF                                 ; Set the airborne status indicator to $FF.
                                  LD (AirborneStatusIndicator),A           ; (meaning Willy has had a fatal accident).
                                  JP MainLoop6                             ; Jump back into the main loop.

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Move the horizontal guardians in the current cavern            ;
;                                                                ;
;----------------------------------------------------------------;
MoveHorzGuardians:
		LD HL,HorizontalGuardian1         	; Point HL at the first byte of the first horizontal guardian
                                             ; definition at HorizontalGuardian1.
                                                                           
          LD DE,7                            ; Prepare DE for addition.
                                             ; (there are 7 bytes in a guardian definition).
; The guardian-moving loop begins here.
MHG1:	push	de						; Store DE for counter purpose
		LD A,(HL)                      	; Pick up the first byte of the guardian definition (IY+0)
          CP $FF                             ; Have we dealt with all the guardians yet?
          RET Z                              ; Return if so.
          OR A                               ; Is this guardian definition blank?
          jp z,MHG7                          ; If so, skip it and consider the next one.
          LD A,(GameClock)                   ; Pick up the value of the game clock
          AND 4                              ; Move bit 2.
                                             ; which is toggled on each pass through the main loop).
          RRCA                               ; to bit 7 and clear all the other bits.
          RRCA                                 
          RRCA                                 
          AND (HL)                           ; Combine this bit with bit 7 of the first byte of the guardian
                                             ; definition, which specifies the guardian's animation speed:
                                             ; 0=normal, 1=slow.
		jp	nz,MHG7					; Jump to consider the next guardian if this one is not due to
                                             ; be moved on this pass.
; The guardian will be moved on this pass.
		push	hl
		ld	de,4                          ; IY+4
		add	hl,de
		LD A,(hl)                          ; Pick up the current animation frame (0-7).
		ex	de,hl
		pop	hl
		CP 3                               ; Is it 3 (the terminal frame for a guardian moving right)?
		jp z,MHG3                          ; Jump if so to move the guardian right across a cell boundary
									; or turn it round.
		CP 4                                     ; Is the current animation frame 4 (the terminal frame for a
									; guardian moving left)?
		jp z,MHG5                                ; Jump if so to move the guardian left across a cell boundary or
									; turn it round.
		jp nc,MHG2                         ; Jump if the animation frame is 5, 6 or 7
		ex	de,hl
          INC (HL)                           ; Increment the animation frame (this guardian is moving right).
		jp MHG7                            ; Jump forward to consider the next guardian.

MHG2:	push	hl						
		ld	de,4                          ; IY+4
		add	hl,de
		DEC (HL)                           ; Decrement the animation frame (this guardian is moving left).
		pop	hl
		JP MHG7                            ; Jump forward to consider the next guardian.

MHG3:	push	hl
		ld	de,1						; IY+1
		add	hl,de
		ld	a,(hl)                        ; Pick up the LSB of the address of the guardian's location in
									; the attribute buffer at 23552.
		ld	de,5 					; IY+6
		add	hl,de
		cp	(hl)                          ; Has the guardian reached the rightmost point in its path?
		pop	hl
		jp	nz,MHG4					; Jump if not.			

		push	hl
		ld	de,4
		add	hl,de
		ld	(hl),7                        ; Set the animation frame to 7 (turning the guardian round to
									; face left).

		pop	hl											
		
MHG4:	push	hl                            
		ld	de,4                          ; IY+4
		add	hl,de
		ld	(hl),0                        ; Set the animation frame to 0 (the initial frame for a guardian
									; moving right)
		pop	hl									
		inc	hl						; IY+1
		inc	(hl)						; Increment the guardian's x-coordinate (moving it right across
									; a cell boundary).
		jp	MHG7									
MHG5:	push	hl
		inc	hl  						; IY+1
		ld	a,(hl)					; Pick up the LSB of the address of the guardian's location in
									; the attribute buffer at 23552.
		ld	de,4                     	; IY+5
		add	hl,de				
		cp	(hl)                          ; Has the guardian reached the leftmost point in its path?

		pop	hl
		jp	nz,MHG6 					; Jump if not.
		push	hl
		ld	de,4                     	; IY+4
		add	hl,de
		ld	(hl),0                        ; Set the animation frame to 0 (turning the guardian round to
									; face right).
		pop	hl
		jp	MHG7						; Jump forward to consider the next guardian.
MHG6:     push	hl                            
		ld	de,4                          ; IY+4
		add	hl,de
		ld	(hl),7                        ; Set the animation frame to 7 (the initial frame for a guardian
									; moving left)
		dec	hl									
		dec	hl
		dec	hl						; IY+1
		dec	(hl)
		pop	hl
		
		pop	de                            ; Restore the guardian counter
		add	hl,de                         ; Point HL at the first byte of the next horizontal guardian
									; definition.
		jp	MHG1						; Jump back to deal with the next horizontal guardian

; The current guardian definition has been dealt with. Time for the next one.

MHG7:	ADD HL,DE                               ; Point IY at the first byte of the next horizontal guardian
										; definition.
		JP MHG1                                 ; Jump back to deal with the next horizontal guardian


;-------------------------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------------------------;
;                                                                ;
; Draw the horizontal guardians in the current cavern            ;
;                                                                ;
;----------------------------------------------------------------;

DrawHorzontalGuardians:
;		ret							; Safe return before routine is fully ready
		LD HL,HorizontalGuardian1		; Point HL at the first byte of the first horizontal guardian
									; definition.

; The guardian-drawing loop begins here.
DrawHorzontalGuardians2:           
		LD A,(HL)						; Pick up the first byte of the guardian definition.
          CP $FF						; Have we dealt with all the guardians yet?
          RET Z						; Return if so.
		OR A                               ;  Is this guardian definition blank?
          JP Z,DrawHorzontalGuardians4       ; If so, skip it and consider the next one.
;		LD DE,31                           ;  Prepare DE for addition.
;		LD L,(IY+1)                        ;  Point HL at the address of the guardian's location in the
;		LD H,(IY+2)                        ;  attribute buffer at 23552.
;		AND 127                            ;  Reset bit 7 (which specifies the animation speed) of the
									; attribute byte, ensuring no FLASH.
;		LD (HL),A                                ; Set the attribute bytes for the guardian in the buffer
;		INC HL                                   ; at 23552.
;		LD (HL),A
;		ADD HL,DE
;		LD (HL),A
;		INC HL
;		LD (HL),A
		
; C=1 draw guardian; C=0 undraw guardian - revert back the paper as background
		ld	a,c
		or 	a
		ld	a,(hl)					; Get INK for guardian
		jp	nz,DHG2a
				
DHGGetPaper:
		rrca                               ; Get PAPER value
		rrca
		rrca
		
DHG2a:	and	7						; Get INK value )
		rrca
		jp	nc,NoBlue					; Color switch GRB (ZX) to BGR (PP)
		or	4

NoBlue:	or	8                             ; Ensure we set the pixels
		out	(ColorReg),a				; Adjust color register accordingly

		LD C,1                                   ; Prepare C for the call to the drawing routine later on.
		push	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		LD A,(hl)                              ; Pick up the animation frame (0-7).
		pop	hl
		RRCA                                     ; Multiply it by 32.
		RRCA
		RRCA
		ld de,GuardianGraphicData           ; Point DE at the graphic data for the appropriate guardian
;		LD E,A                                   ; Copy the result to E.
		LD A,(CurrentCavernNumber)               ; Pick up the number of the current cavern.
		CP 7                                     ; Are we in one of the first seven caverns?
          JP C,DrawHorzontalGuardians3             ; Jump if so.
          CP 9                                     ; Are we in The Endorian Forest?
          JP Z,DrawHorzontalGuardians3             ; Jump if so.
          CP 15                                    ; Are we in The Sixteenth Cavern?
          JP Z,DrawHorzontalGuardians3             ; Jump if so.
;         SET 7,E                                  ; Add 128 to E (the horizontal guardians in this cavern use
                                                   ; frames 4-7 only).
          ld	a,e
          add	a,128
          ld	e,a

DrawHorzontalGuardians3:
		
     
	                                              ; sprite (at 33024+E).
		push	hl
		inc	hl
		ld	a,(hl)                              ; Point HL at the address of the guardian's location in the
                                                   ; screen buffer at 24576.
		inc	hl
		inc	hl                                                   
		ld	b,(hl)
		ld   l,a
		ld	h,b

;	     ld	de,GuardianGraphicData
		ld	hl,VramAdr+7*256+8			
		CALL DrawASprite                         ; Draw the guardian to the screen buffer at 24576.
		pop	hl
		JP	NZ,KillWilly1                         ; Kill Willy if the guardian collided with him.

; The current guardian definition has been dealt with. Time for the next one.

DrawHorzontalGuardians4:
          LD DE,7                                  ; Point HL at the first byte of the next horizontal guardian
		ADD HL,DE                                ; definition.
		JP DrawHorzontalGuardians2               ; Jump back to deal with the next horizontal guardian

;-------------------------------------------------------------------------------------------------------------------------------------------;


; Fill area: Fills 256 bytes of memory by $FF 
; A - color code
; DE - start address
FillArea: out (ColorReg),a
		ld  b,0
		ld  a,255
 FA1:     ld  (de),a
        	inc de
		dec b
		jp  nz,FA1
		ret

;----------------------------------------------------------------;
;                                                                ;
; Draw the current cavern to the screen buffer                   ;
;                                                                ;
;----------------------------------------------------------------;
DrawCurrentCavernToScreenBuffer:
;		ret
		LD	HL,EmptyCavernAttributeBuffer 		; Point IX at the first byte of the attribute buffer at
                                             		; EmptyCavernAttributeBuffer.
		LD	A,high EmptyCavernScreenBuffer		; Set the operand of the 'LD D,n' instruction
                                             		; DrawCurrentCavernToScreenBuffer4 + 1 (below).
		LD	(DrawCurrentCavernToScreenBuffer4+1),A
		CALL	DrawCurrentCavernToScreenBuffer2    	; Draw the tiles for the top half of the cavern to the screen
                                                       ; buffer at EmptyCavernScreenBuffer

          LD	HL,EmptyCavernAttributeBuffer + 256 	; Point IX at the 256th byte of the attribute buffer at
                                                   	; EmptyCavernAttributeBuffer in preparation for drawing the
                                                   	; bottom half of the cavern; this instruction is redundant, since
                                                   	; IX already holds 24320.
          LD	A,high EmptyCavernScreenBuffer + $800 	; Set the operand of the 'LD D,n' instruction at (35483)
                                                   	; DrawCurrentCavernToScreenBuffer4 + 1 (below)
          LD	(DrawCurrentCavernToScreenBuffer4+1),A
DrawCurrentCavernToScreenBuffer2:
		LD	C,0                                   	; C will count 256 tiles

; The following loop draws 256 tiles (for either the top half or the bottom half of the cavern) to the screen buffer at
; EmptyCavernScreenBuffer.
DrawCurrentCavernToScreenBuffer3:
		LD	E,C                                   	; E holds the LSB of the screen buffer address
		LD	A,(HL)	                            	; Pick up an attribute byte from the buffer at
                                                     	; EmptyCavernAttributeBuffer; this identifies the type of tile
                                                     	; to draw.
		push	hl                                                     	
		LD	HL,BackgroundTile                     	; Move HL through the attribute bytes and graphic data of the
           	                                      	; background, floor, crumbling floor, wall, conveyor and nasty
                                                     	; tiles starting at CavernTiles until we find a byte that matches
                                                     	; the attribute byte of the tile to be drawn.
		LD	BC,72
;		CPIR
CpLoop1:	cp	(hl)
		jp	z,CpCnt1
		inc	hl
		dec	bc
		ld	d,a
		ld	a,b
		or	c
		ld	a,d
		jp	nz,CpLoop1
		
CpCnt1:	inc	hl
		LD	C,E                                   	; Restore the value of the tile counter in C
		LD	B,8                                   	; There are eight bytes in the tile
DrawCurrentCavernToScreenBuffer4:
		LD	D,0                                   	; This instruction is set to the high byte of an address in the
                                                     	; buffer; now DE holds the address in the screen buffer at
                                                     	; EmptyCavernScreenBuffer.
DrawCurrentCavernToScreenBuffer5:
		LD	A,(HL)                                	; Copy the tile graphic data to the screen buffer at
		LD	(DE),A                                	; EmptyCavernScreenBuffer.
		INC	HL
		INC	D
		dec	b
		jp	nz,DrawCurrentCavernToScreenBuffer5
		pop	hl
		INC	HL                                   	; Move IX along to the next byte in the attribute buffer
		INC	C                                    	; Have we drawn 256 tiles yet?
		JP	NZ,DrawCurrentCavernToScreenBuffer3   	; If not, jump back to draw the next one.

; The empty cavern has been drawn to the screen buffer at EmptyCavernScreenBuffer. If we're in The Final Barrier, however, there is further
; work to do.
		LD	A,(CurrentCavernNumber)               	; Pick up the number of the current cavern from
                                                   	; CurrentCavernNumber.
		CP	19                                    	; Is it The Final Barrier?
		RET	NZ                                   	; Return if not
		LD	HL,TitleScreenDataTop                 	; Copy the graphic data from TitleScreenDataTop to the top half
		LD	DE,EmptyCavernScreenBuffer            	; of the screen buffer at EmptyCavernScreenBuffer.
		LD	BC,2048
;		LDIR
		call	ldir80
		RET

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Draw the portal.                                               ;
;                                                                ;
; Move to the next cavern if Willy has entered it.               ;
;                                                                ;
;----------------------------------------------------------------;

; First check whether Willy has entered the portal.
DrawThePortal:
		LD HL,(PortalDefAttributeBuf)            ; Pick up the address of the portal's location in the
                                                   ; attribute buffer at 23552 from PortalDefAttributeBuf.
		ld	a,h                           ; Originally attr buffer was at 5C00h, here is on 3200h
		sub	$2C
		ld	h,a
					                                                   
		jp	DrawThePortal2					 ; Don't check Willy for demo
          LD A,(WillysLocInAttrBuffer)             ; Pick up the LSB of the address of Willy's location in the
                                                   ; attribute buffer at 23552 from WillysLocInAttrBuffer.
          CP L                                     ; Does it match that of the portal?
          JP NZ,DrawThePortal2                     ; Jump if not.
          LD A,(JumpingAnimationCounter - 1)       ; Pick up the MSB of the address of Willy's location in the
                                                   ; attribute buffer at 23552 from (JumpingAnimationCounter - 1).
          CP H                                     ; Does it match that of the portal?
          JR NZ,DrawThePortal2                     ; Jump if not.
          LD A,(PortalDefAttributeByte)            ; Pick up the portal's attribute byte from
                                                   ; PortalDefAttributeByte.
;          BIT 7,A                                  ; Is the portal flashing?
		AND	128
          CP	128	
          JP Z,DrawThePortal2                      ; Jump if not.
          POP HL                                   ; Drop the return address from the stack.
          ;JP MoveToTheNextCavern                   ; Move Willy to the next cavern.

; Willy has not entered the portal, or it's not flashing, so just draw it.
DrawThePortal2:
		LD A,(PortalDefAttributeByte)            ; Pick up the portal's attribute byte from
										 ; PortalDefAttributeByte.
          LD (HL),A                                ; Set the attribute bytes for the portal in the buffer at
                                                   ; 23552
          INC HL
          LD (HL),A
          LD DE,31
          ADD HL,DE
          LD (HL),A
          INC HL
          LD (HL),A
		
          LD DE,PortalDefGraphicData               ; Point DE at the graphic data for the portal at
                                                   ; PortalDefGraphicData.
          LD HL,(PortalDefScreenBuf)               ; Pick up the address of the portal's location in the screen
                                                   ; buffer at 24576 from PortalDefScreenBuf.
		ld	a,h						; Originally screen buffer was at 6000h, here is on 2000h 
		sub	$40
		ld	h,a
			                                                   
          LD C,0                                   ; C = 0: overwrite mode.

;-------------------------------------------------------------------------------------------------------------------------------------------;
SpriteDrawZX:
		ld   b,16
		
SDZX1:	ld	a,(de)
SDZX2:	ld	(hl),a
		inc	l
		inc	de
		ld	a,(de)
SDZX3:	ld	(hl),a
		dec	l
		inc	h
		inc	de
		ld	a,h
		and	7
		jp	nz,SDZX4
		
		ld	a,h
		sub	8
		ld	h,a
		ld	a,l
		add	a,32
		ld	l,a
		and	$E0
		jp	nz,SDZX4
		ld	a,h
		add	a,8
		ld	h,a
SDZX4:	dec	b
		jp	nz,SDZX1		
		xor	a
		ret
								            
;----------------------------------------------------------------;
;                                                                ;
; Draw a sprite.                                                 ;
;                                                                ;
; If C=1 on entry, this routine returns with the zero flag       ;
; reset if any of the set bits in the sprite being drawn         ;
; collides with a set bit in the background.                     ;
;                                                                ;
; Input                                                          ;
; ---------------------------------------------                  ;
;  C = Drawing mode: 0 (overwrite) or 1 (blend)                  ;
; DE = Address of sprite graphic data                            ;
; HL = Address to draw at                                        ;
;                                                                ;
;----------------------------------------------------------------;

DrawASprite:
;		xor  a
;		ld   (RowStorePosition),a
          ld   b,16						; There are 16 rows of pixels to draw.
DaS1:	
		ld	a,c
		cp	1						; Set the zero flag if we're in overwrite mode.
		ld	a,(de)					; Pick up a sprite graphic byte.
       
;		jp	z,DaS2					; Jump if we're in overwrite mode.
;		and	(hl)						; Return with the zero flag reset if any of the set bits in the
;		ret	nz						; sprite graphic byte collide with a set bit in the background	
				        				; (e.g. in Willy's sprite).
;		ld	a,(de)					; Pick up the sprite graphic byte again.
;		or	(hl)						; Blend it with the background byte.
DaS2:	ld	(hl),a					; Copy the graphic byte to its destination cell.
		inc	l						; Move HL along to the next cell on the right.
		inc	de						; Point DE at the next sprite graphic byte.
		ld	a,c
		cp	1						; Set the zero flag if we're in overwrite mode.
		ld   a,(de)					; Pick up a sprite graphic byte.      
;		jp   z,DaS3					; Jump if we're in overwrite mode.                               
;		and  (hl)        				; Return with the zero flag reset if any of the set bits in the  
;		ret  nz          				; sprite graphic byte collide with a set bit in the background   
                        					; (e.g. in Willy's sprite).                                      
;       	ld   a,(de)      				; Pick up the sprite graphic byte again.
;       	or   (hl)        				; Blend it with the background byte.
            
DaS3:	LD	(HL),A					; Copy the graphic byte to its destination cell.
		push	de						; Move HL to the next pixel row down in the cell on the left.
		ld	de,31
		add	hl,de
		pop	de
       
		INC	DE                        	; Point DE at the next sprite graphic byte.
;		ld	a,(RowStorePosition)
;       	LD A,H						; Have we drawn the bottom pixel row in this pair of cells yet?
;		AND	15
;		ret  z
;		JP	NZ,DaS4					; Jump if not.

;       LD A,H                               ; Otherwise move HL to the top pixel row in the cell below.
;       SUB 8
;       LD H,A
;       LD       A,L
;       ADD      A,32
;       LD L,A
;       AND $E0                              ; Was the last pair of cells at y-coordinate 7 or 15?

;       JP NZ,DaS4                           ; Jump if not.
;       LD A,H                               ; Otherwise adjust HL to account for the movement from the top or
                                             ; middle third of the screen to the next one down.
;       ADD  A,8
;       LD   H,A
DaS4:	DEC  B
		JP   NZ,DaS1                       ; Jump back until all 16 rows of pixels have been drawn.
		XOR  A						; Set the zero flag (to indicate no collision).
		RET



;-------------------------------------------------------------------------------------------------------------------------------------------;
; IK: Aux delay loops
;-------------------------------------------------------------------------------------------------------------------------------------------;
ldelay	push	bc
		push	af
		ld	bc,5000
dlp		nop
		dec	bc
		ld	a,b
		or	c
		jp	nz,dlp
		pop	af
		pop	bc
		ret

delay	push	bc
		ld	b,80
sdlp		nop
		dec	b
		jp	nz,sdlp
		pop	bc
		ret



;----------------------------------------------------------------;
;                                                                ;
; Play the theme tune (The Blue Danube)                          ;
;                                                                ;
; Returns with the zero flag reset if ENTER or the fire button   ;
; is pressed while the tune is being played.                     ;
;                                                                ;
; Input                                                          ;
; ------------------------                                       ;
; HL = TitleScreenTuneData                                       ;
;                                                                ;
;----------------------------------------------------------------;
PlayTheThemeTune:
		ld	a,(hl)					; Pick up the next byte of tune data from the table at 846E
		cp	$FF						; Has the tune finished?
		jp	z,SpeakerOff				; Return (with the zero flag set) if so
		rrca
		sub	5
		ld	c,a						; Copy the first byte of data for this note (which determines the duration) to C
		ld	b,0						; Initialise B, which will be used as a delay counter in the note-producing loop
		inc	hl
		ld	d,(hl)					; Pick up the second byte of data for this note
		inc	hl
		ld	a,d						; Copy it to A
		push	hl
		call	CalcAFAForPianoKey			; Calculate the attribute file address for the corresponding piano key
		ld	a,1						; Set the attribute byte for the piano key to 0x50 (INK 0: PAPER 2: BRIGHT 1)
		call	ShowPianoKeyColor
		pop	hl
		ld	e,(hl)					; Pick up the third byte of data for this note
		inc	hl

		ld  a,e						; Copy it to A
		push	hl
		call	CalcAFAForPianoKey			; Calculate the attribute file address for the corresponding piano key
    
		ld	a,6	                    	; Set the attribute byte for the piano key to 0x28 (INK 0: PAPER 5: BRIGHT 0)
		call	ShowPianoKeyColor
		pop	hl

		push	hl
		ld	h,d
		ld	l,e
		ld	a,$80
PlayTheThemeTune1:
		out	($C2),a     				; Produce a sound based on the frequency parameters in the second and third bytes of data for this note (copied into D and E)
		dec	d
		jp	nz,PlayTheThemeTune2
		ld	d,h
		xor	$0F
PlayTheThemeTune2:
		dec	e
		jp	nz,PlayTheThemeTune3
		ld	e,l
		xor	$0F
PlayTheThemeTune3:
		dec	b
		jp	nz,PlayTheThemeTune1
		dec	c
		jp	nz,PlayTheThemeTune1
		pop	hl
    
		in	a,($C1)					; Check whether SPACE or the fire button is being pressed
		rrca
		jp	nc,SpeakerOff				; Return (with the carry flag reset) if it is

		push	hl
		dec	hl
		ld	a,(hl)					; Pick up the third byte of data for this note
    
		ex	de,hl
		call	CalcAFAForPianoKey			; Calculate the attribute file address for the corresponding piano key
		ld	a,7						; Set the attribute byte for the piano key back to 0x38 (INK 0: PAPER 7: BRIGHT 0)
		call	ShowPianoKeyColor
		ex	de,hl
    
		dec	hl
		ld	a,(hl)					; Pick up the second byte of data for this note
		call	CalcAFAForPianoKey			; Calculate the attribute file address for the corresponding piano key
		ld	a,7						; Set the attribute byte for the piano key back to 0x38 (INK 0: PAPER 7: BRIGHT 0)
		call	ShowPianoKeyColor
		pop	hl

		jp	PlayTheThemeTune			; Jump back to play the next note

SpeakerOff:
		ld	a,$90
		out	($C2),a
		ret

; Show attribute    
ShowPianoKeyColor:
		or	8						; Make sure bitplane mode is on		
		out	(ColorReg),a
		ld	a,254
		push	de
		push	bc
		ld	de,32
		ld	c,8    
PKC1:	ld	(hl),a					; Piano Key Color loop
		add	hl,de
		dec	c
		jp	nz,PKC1
		pop	bc
		pop	de
		ret
    
;----------------------------------------------------------------;
;                                                                ;
; Calculate the attribute file address for a piano key.          ;
;                                                                ;
; Returns with the attribute file address in HL.                 ;
;                                                                ;
; Input                                                          ;
; ---------------------------------------------------            ;
; A = Frequency parameter from the tune data table at            ;
;     TitleScreenTuneData                                        ;
;                                                                ;
;----------------------------------------------------------------;
CalcAFAForPianoKey:
		SUB 8						; Compute the piano key index (K) based on the frequency.
		RRCA							; parameter (F) and store it in bits 0-4 of A:
		RRCA							; K = 31 - INT((F - 8) / 8).
		RRCA
		CPL
		and	$1F
		LD	L,A						; Set HL to the attribute file address for the piano key.
		ld	h,$AF
		RET

;----------------------------------------------------------------;
;                                                                ;
; Print a message                                                ;
;                                                                ;
; Input                                                          ;
; ---------------------------                                    ;
; HL = Address of the message                                    ;
;  C = Length of the message                                     ;
; DE = Display file address                                      ;
;                                                                ;
;----------------------------------------------------------------;
PrintAMessage:
		LD	A,(hl)					; Collect a character from the message.
		push	hl
		CALL	PrintASingleCharacter		; Print it.
		pop	hl
		inc	hl						; Point HL at the next character in the message.
		INC	E						; Point DE at the next character cell.
									; Subtracting 8 from D compensates for the operations performed.
									; by the routine at PrintASingleCharacter.
		DEC	C						; Have we printed the entire message yet?
		JP	NZ,PrintAMessage			; If not, jump back to print the next character.
		RET
                                                                    
;----------------------------------------------------------------;
;                                                                ;
; Print a single character.                                      ;
;                                                                ;
; Input                                                          ;
; --------------------------------                               ;
;  A = ASCII code of the character                               ;
; DE = Display file address                                      ;
;                                                                ;
;----------------------------------------------------------------;
PrintASingleCharacter:
		LD	H,3						; Point HL at the bitmap for the character (in the ROM).
		or	128						;SET 7,L
		LD	L,A

		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		LD	B,8						; There are eight pixel rows in a character bitmap.

;----------------------------------------------------------------;
;                                                                ;
; Draw an item in the current cavern.                            ;
;                                                                ;
;----------------------------------------------------------------;
DrawItem:	LD	A,(HL)					; Copy the character bitmap to the screen (or item graphic to
		LD	(DE),A					; the screen buffer).
		INC	HL
		ld	a,e
		add	a,32
		ld	e,a
		dec	b
		jp	nz,DrawItem
		RET

;-------------------------------------------------------------------------------------------------------------------------------------------;
; Clear the whole screen
;-------------------------------------------------------------------------------------------------------------------------------------------;
Cls:		xor	a
		out 	(RollReg),a

		ld	a,8
		out	(ColorReg),a

		ld	bc,8192
		ld	hl,VramAdr
		ld	a,255
		
ClB:		ld	(hl),a
		inc	hl
		dec	c
		jp	nz,ClB
		dec	b
		jp	nz,ClB
		ret
		
;-------------------------------------------------------------------------------------------------------------------------------------------;
; LDIR immitation
;-------------------------------------------------------------------------------------------------------------------------------------------;
ldir80:	push	af
ldi80:	ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		dec	bc
		ld	a,b
		or	c
		jp	nz,ldi80
ldiret:	pop	af
		ret

;==============================================================================
; Vykreslenie jednej tretiny
;==============================================================================
OneThird:	ld 	b,8						; 8 znakovych riadkov v tretime
NextRow:	push	bc

		ld 	c,32						; 32 znakovych stlpcov
NextCol3:	call	OneChar					; vypis jeden znak

		ld 	a,h						; vrat sa na prvy mikroriadok ZX riadku
		sub	8
		ld 	h,a

		inc	hl						; posun na dalsi ZX stlpec
		inc	e						; posun na dalsi PP stlpec

		dec	c						; dalsi stlpec
		jp	nz,NextCol3

		ld 	a,e						; prejdi na dalsi znakovy PP riadok
		sub	32
		ld 	e,a
		inc	d

		pop	bc						; obnov pocitadlo riadkov a tretin

		dec	b						; dalsi riadok
		jp	nz,NextRow
		ret

;------------------------------------------------------------------------------
; Rutina pre vykreslenie jedneho znaku
	
OneChar:	push	bc
		call	PrepareColor				; priprav INK a PAPER z atributu

		; INK
InkColor:	ld	a,0						; vyber farbu pre INK
		out	(ColorReg),a
		ld	b,8						; cyklus pre vykreslenie INK pixelov
InkLoop:	ld	a,(hl)					; v jednom "znaku"
		ld	(de),a
		inc	h						; posun na dalsi ZX mikroriadok
		ld	a,e						; posun na dalsi PP mikroriadok
		add	a,32
		ld	e,a
		dec	b
		jp	nz,InkLoop
		ld 	a,h						; vrat sa na prvy mikroriadok ZX riadku
		sub	8
		ld 	h,a
		; PAPER
PaperColor:	
		ld	a,0						; vyber farbu pre PAPER
		out	(ColorReg),a
		ld	b,8						; cyklus pre vykreslenie INK pixelov
PaperLoop:	
		ld 	a,(hl)					; v jednom "znaku"
		cpl							; zamen bity popredia a pozadia
		ld	(de),a
		inc	h						; posun na dalsi ZX mikroriadok
		ld 	a,e						; posun na dalsi PP mikroriadok
		add	a,32
		ld 	e,a
		dec	b
		jp	nz,PaperLoop
		pop	bc
		ret


;------------------------------------------------------------------------------
; Rutina pre vypocet adresy atributu z pixelovej adresy predpoklada, ze je
; vyssi byte adresy delitelny 8 a teda, ze nie sme "vo vnutri" znaku.
PrepareColor:	
		push	hl
		push	de
          
		ld	a,d
		cp	$A8
		jp	nc,PC2k
		
		ld	de,4096
		jp	PCol1
		
PC2k:	ld	de,2048+256

PCol1:	add	hl,de
				
		ld	a,c						; vytor z pozicie znaku
		sub	32						; adresu atributu
		cpl
		inc	a
		ld	e,a

		ld	a,b		
		sub	8
		cpl
		inc	a
		rrca
		rrca
		rrca
		or	e
		ld	l,a
;		inc	h
		ld 	b,(hl)					; atribut do B
		ld 	h,ColorTab/256				; vyssi byte tabulky farieb
		ld 	a,b						; najprv INK
		and	7
		add	a,ColorTab & 255
		ld 	l,a
		ld 	a,(hl)
		ld	(InkColor+1),a				; a uloz na neskor
		ld 	a,b						; teraz PAPER
		rrca
		rrca
		rrca
		and	7
		add	a,ColorTab & 255
		ld 	l,a
		ld 	a,(hl)
		ld	(PaperColor+1),a			; a uloz na neskor
		pop	de
		pop	hl
		ret



; IK: data
;RowStorePosition			defb	0
WillySpriteDataPrev			defb	0,0


    org DataStart
TitleScreenBanner:
		DEFM ".  .  .  .  .  .  .  .  .  .  . MANIC MINER . . "
		DEFM 127," BUG-BYTE ltd. 1983 . . By Matthew Smith . . . "
		DEFM "Q to P = Left & Right . . Bottom row = Jump . . "
		DEFM "A to G = Pause . . H to L = Tune On/Off . . . "
		DEFM "Guide Miner Willy through 20 lethal caverns"
		DEFM " .  .  .  .  .  .  .  ."  ;

    org DataStart+256
    ;------------------------------------------------------------------------------
; Tabulka transformuje cislo farby, ktore je bitovo vo forme GRB do formy BGR,
; ako to vyzaduje farbovy register ColorReg.
; Tabulka nesmie prekracovat hranicu MOD 256 !!!
; GRB		   BGR
;  0 - cierna	  - 0
;  1 - modra	  - 4
;  2 - cervena	  - 1
;  3 - fialova	  - 5
;  4 - zelena	  - 2
;  5 - bledomodra - 6
;  6 - zlta	  - 3
;  7 - biela	  - 7
ColorTab:	db	0+8,4+8,1+8,5+8,2+8,6+8,3+8,7+8

    org DataStart+512
WillySpriteData:
33280	DEFB 6,0,62,0,124,0,52,0,62,0,60,0,24,0,60,0	
33296	DEFB 126,0,126,0,247,0,251,0,60,0,118,0,110,0,119,0
33312	DEFB 1,128,15,128,31,0,13,0,15,128,15,0,6,0,15,0
33328	DEFB 27,128,27,128,27,128,29,128,15,0,6,0,6,0,7,0	

WillySpriteData1:
33344	DEFB 0,96,3,224,7,192,3,64,3,224,3,192,1,128,3,192	
33360	DEFB 7,224,7,224,15,112,15,176,3,192,7,96,6,224,7,112	

WillySpriteData2:
33376	DEFB 0,24,0,248,1,240,0,208,0,248,0,240,0,96,0,240	
33392	DEFB 1,248,3,252,7,254,6,246,0,248,1,218,3,14,3,132	
33408	DEFB 24,0,31,0,15,128,11,0,31,0,15,0,6,0,15,0	
33424	DEFB 31,128,63,192,127,224,111,96,31,0,91,128,112,192,33,192	
33440	DEFB 6,0,7,192,3,224,2,192,7,192,3,192,1,128,3,192	
33456	DEFB 7,224,7,224,14,240,13,240,3,192,6,224,7,96,14,224	
33472	DEFB 1,128,1,240,0,248,0,176,1,240,0,240,0,96,0,240	
33488	DEFB 1,248,1,216,1,216,1,184,0,240,0,96,0,96,0,224	
33504	DEFB 0,96,0,124,0,62,0,44,0,124,0,60,0,24,0,60	
33520	DEFB 0,126,0,126,0,239,0,223,0,60,0,110,0,118,0,238
    
;----------------------------------------------------------------;
;                                                                ;
; Title screen tune data (The Blue Danube).                      ;
;                                                                ;
; The tune data is organised into 95 groups of three bytes       ;
; each, one group for each note in the tune. The first byte in   ;
; each group determines the duration of the note, and the        ;
; second and third bytes determine the frequency (and also the   ;
; piano keys that light up).                                     ;
;                                                                ;
;----------------------------------------------------------------;

TitleScreenTuneData:              DEFB 80,128,129
                                  DEFB 80,102,103
                                  DEFB 80,86,87
                                  DEFB 50,86,87
                                  DEFB 50,171,203
                                  DEFB 50,43,51
                                  DEFB 50,43,51
                                  DEFB 50,171,203
                                  DEFB 50,51,64
                                  DEFB 50,51,64
                                  DEFB 50,171,203
                                  DEFB 50,128,129
                                  DEFB 50,128,129
                                  DEFB 50,102,103
                                  DEFB 50,86,87
                                  DEFB 50,96,86
                                  DEFB 50,171,192
                                  DEFB 50,43,48
                                  DEFB 50,43,48
                                  DEFB 50,171,192
                                  DEFB 50,48,68
                                  DEFB 50,48,68
                                  DEFB 50,171,192
                                  DEFB 50,136,137
                                  DEFB 50,136,137
                                  DEFB 50,114,115
                                  DEFB 50,76,77
                                  DEFB 50,76,77
                                  DEFB 50,171,192
                                  DEFB 50,38,48
                                  DEFB 50,38,48
                                  DEFB 50,171,192
                                  DEFB 50,48,68
                                  DEFB 50,48,68
                                  DEFB 50,171,192
                                  DEFB 50,136,137
                                  DEFB 50,136,137
                                  DEFB 50,114,115
                                  DEFB 50,76,77
                                  DEFB 50,76,77
                                  DEFB 50,171,203
                                  DEFB 50,38,51
                                  DEFB 50,38,51
                                  DEFB 50,171,203
                                  DEFB 50,51,64
                                  DEFB 50,51,64
                                  DEFB 50,171,203
                                  DEFB 50,128,129
                                  DEFB 50,128,129
                                  DEFB 50,102,103
                                  DEFB 50,86,87
                                  DEFB 50,64,65
                                  DEFB 50,128,171
                                  DEFB 50,32,43
                                  DEFB 50,32,43
                                  DEFB 50,128,171
                                  DEFB 50,43,51
                                  DEFB 50,43,51
                                  DEFB 50,128,171
                                  DEFB 50,128,129
                                  DEFB 50,128,129
                                  DEFB 50,102,103
                                  DEFB 50,86,87
                                  DEFB 50,64,65
                                  DEFB 50,128,152
                                  DEFB 50,32,38
                                  DEFB 50,32,38
                                  DEFB 50,128,152
                                  DEFB 50,38,48
                                  DEFB 50,38,48
                                  DEFB 50,0,0
                                  DEFB 50,114,115
                                  DEFB 50,114,115
                                  DEFB 50,96,97
                                  DEFB 50,76,77
                                  DEFB 50,76,153
                                  DEFB 50,76,77
                                  DEFB 50,76,77
                                  DEFB 50,76,153
                                  DEFB 50,91,92
                                  DEFB 50,86,87
                                  DEFB 50,51,205
                                  DEFB 50,51,52
                                  DEFB 50,51,52
                                  DEFB 50,51,205
                                  DEFB 50,64,65
                                  DEFB 50,102,103
                                  DEFB 100,102,103
                                  DEFB 50,114,115
                                  DEFB 100,76,77
                                  DEFB 50,86,87
                                  DEFB 50,128,203
                                  DEFB 26,128,0
                                  DEFB 26,128,129
                                  DEFB 50,128,203
                                  DEFB $FF                                 ; End marker

		org	1500h
;-------------------------------------------------------------------------------------------------------------------------------------------;
CurrentCavernNumber:              DEFB 0     ; Current cavern number.

;-------------------------------------------------------------------------------------------------------------------------------------------;

AirText:                          DEFM "AIR"                               ; 'AIR'.
HighScore:                        DEFM "000000"                            ; High Score.

; Score
Score1:                           DEFM "0"                                 ; Overflow digits (these may be updated, but are never printed).
Score2:                           DEFM "000000000"

HighScoreText:                    DEFM "High Score 000000   Score 000000"

GameText:                         DEFM "Game"
OverText:                         DEFM "Over"

LivesRemaining:                   DEFB 0     ; Lives remaining.
ScreenFlashCounter:               DEFB 0     ; Screen flash counter.
KempJoystickIndicator:            DEFB 0     ; Kempston joystick indicator Holds 1 if a joystick is present,
                                             ; 0 otherwise.
GameModeIndicator:                DEFB 0     ; Holds 0 when a game is in progress, or a value from 1 to 64
                                             ; when in demo mode.
MusicNoteIndex:                   DEFB 0     ; In-game music note index.

; ---------------------------------------------------------------------------;
;                                                                            ;
; Music flags.                                                               ;
;                                                                            ;
; Bit(s) | Meaning                                                           ;
; -------------------------------------------------------------------------  ;
;      0 | Keypress flag (set=H-ENTER being pressed, reset=no key pressed).  ;
;      1 | In-game music flag (set=music off, reset=music on).               ;
;    2-7 | Unused.                                                           ;
;                                                                            ;
; ---------------------------------------------------------------------------;
MusicFlags:				DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

KeyCounter:				DEFB 0         ; 6031769 Key counter.


; FOLLOWING 512 CONSECUTIVE BYTES FOR LEVEL DEFINITION
;----------------------------------------------------------------;
;                                                                ;
; Cavern name.                                                   ;
;                                                                ;
; The cavern name is copied here.                                ;
;                                                                ;
;----------------------------------------------------------------;
CavernName:				DEFS 32
;-------------------------------------------------------------------------------------------------------------------------------------------;

; --------------------------------------------------------------------;
;                                                                     ;
; Cavern tiles.                                                       ;
;                                                                     ;
; The cavern tiles are copied here and then used to draw the cavern.  ;
; The extra tile behaves like a floor tile, and is used as such       ;
; in The Endorian Forest, Attack of the Mutant Telephones, Ore        ;
; Refinery, Skylab Landing Bay and The Bank. It is also used in The   ;
; Menagerie as spider silk, and in Miner Willy meets the Kong Beast   ;
; and Return of the Alien Kong Beast as a switch.                     ;
;                                                                     ;
; --------------------------------------------------------------------;

BackgroundTile                    DEFS 9                                   ; Background tile.
FloorTile                         DEFS 9                                   ; Floor tile.
CrumblingFloorTile                DEFS 9                                   ; Crumbling floor tile.
WallTile                          DEFS 9                                   ; Wall tile.
ConveyorTile                      DEFS 9                                   ; Conveyor tile.
NastyTile1                        DEFS 9                                   ; Nasty tile 1.
NastyTile2                        DEFS 9                                   ; Nasty tile 2.
ExtraTile                         DEFS 9                                   ; Extra tile.

 ;-------------------------------------------------------------------------------------------------------------------------------------------;

; ---------------------------------------------------------------------;
;                                                                      ;
; Willy's pixel y-coordinate (x2).                                     ;
;                                                                      ;
; Holds the LSB of the address of the entry in the screen buffer       ;
; address lookup table that corresponds to Willy's pixel y-coordinate  ;
; in practice, this is twice Willy's actual pixel y-coordinate.        ;
;                                                                      ;
; ---------------------------------------------------------------------;

WillysPixelYCoord                 DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Willy's animation frame.                                       ;
;                                                                ;
; Initialised upon entry to a cavern or after losing a life and  ;
; updated in game play.                                          ;
;                                                                ;
; Possible values are 0, 1, 2 and 3.                             ;
;                                                                ;
;----------------------------------------------------------------;

WillysAnimationFrame              DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Willy's direction and movement flags.                          ;
;                                                                ;
; Bit(s) | Meaning                                               ;
; -----------------------------------------------------------    ;
;      0 | Direction Willy is facing (reset=right, set=left)).   ;
;      1 | Willy's movement flag (set=moving).                   ;
;    2-7 | Unused (always reset).                                ;
;                                                                ;
;----------------------------------------------------------------;

WillysDirAndMovFlags              DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;
; Airborne status indicator.
;
; Value | Meaning
;     0 | Willy is neither falling nor jumping
;     1 | Willy is jumping
;  2-11 | Willy is falling, and can land safely
;   12+ | Willy is falling, and has fallen too far to land safely
;   255 | Willy has collided with a nasty or a guardian

AirborneStatusIndicator           DEFB 0

WillysLocInAttrBuffer             DEFW 0                                   ; Address of Willy's location in the attribute buffer.

JumpingAnimationCounter           DEFB 0                                   ; Jumping animation counter.

;----------------------------------------------------------------;
;                                                                ;
; Conveyor definitions.                                          ;
;                                                                ;
;----------------------------------------------------------------;

ConveyorDirection                 DEFB 0                                   ; Direction (0=left, 1=right;)
ConveyorAddress                   DEFW 0                                   ; Address of the conveyor's location in the screen buffer
ConveyorLength                    DEFB 0                                   ; Convayor length.

BorderColor                       DEFB 0                                   ; Border color

;----------------------------------------------------------------;
;                                                                ;
; Attribute of the last item drawn.                              ;
;                                                                ;
; Holds the attribute byte of the last item drawn, or 0 if all   ;
; the items have been collected.                                 ;
;----------------------------------------------------------------;

AttrLastItemDrawn                 DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

; ---------------------------------------------------------------------------;
;                                                                            ;
; Item definitions.                                                          ;
;                                                                            ;
; Byte(s) | Content                                                          ;
; -------------------------------------------------------------------------  ;
;       0 | Current attribute.                                               ;
;     1,2 | Address of the item's location in the attribute buffer.          ;
;       3 | MSB of the address of the item's location in the screen buffer.  ;
;       4 | Unused (always 255).                                             ;
;                                                                            ;
; ---------------------------------------------------------------------------;

ItemDef1                          DEFS 5                                   ; Item 1.
ItemDef2                          DEFS 5                                   ; Item 2.
ItemDef3                          DEFS 5                                   ; Item 3.
ItemDef4                          DEFS 5                                   ; Item 4.
ItemDef5                          DEFS 5                                   ; Item 5.
ItemDefTerminator                 DEFB 0                                   ; Terminator (set to 255).

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Portal definitions                                             ;
;                                                                ;
;----------------------------------------------------------------;

PortalDefAttributeByte            DEFB 0                                   ; Attribute byte.
PortalDefGraphicData              DEFS 32                                  ; Graphic data.
PortalDefAttributeBuf             DEFW 0                                   ; Address of the portal's location in the attribute buffer.
PortalDefScreenBuf                DEFW 0                                   ; Address of the portal's location in the screen buffer.

;-------------------------------------------------------------------------------------------------------------------------------------------;

ItemGraphic                       DEFS 8                                   ; Item graphic.

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Remaining air supply.                                          ;
;                                                                ;
; Initialised (always to 63 in practice).                        ;
; Its value ranges from 36 to 63 and is actually the LSB of the  ;
; display file address for the cell at the right                 ;
; end of the air bar. The amount of air to draw in this cell is  ;
; determined by the value of the game clock.                     ;
;                                                                ;
;----------------------------------------------------------------;

RemainingAirSupply                DEFB 0

;----------------------------------------------------------------;
;                                                                ;
; Game clock.                                                    ;
;                                                                ;
; Initialised and, updated on every pass through the main loop   ;
; and used for timing purposes.                                  ;
; Its value (which is always a multiple of 4) is also used to    ;
; compute the amount of air to draw in the cell at the right     ;
; end of the air bar.                                            ;
;                                                                ;
;----------------------------------------------------------------;

GameClock                         DEFB 0

; --------------------------------------------------------------------------------------------------;
;                                                                                                   ;
; Horizontal guardians.                                                                             ;
;                                                                                                   ;
; Byte | Contents                                                                                   ;
; ------------------------------------------------------------------------------------------------  ;
;    0 | Bit 7: animation speed (0=normal, 1=slow).                                                 ;
;      | Bits 0-6: attribute (BRIGHT, PAPER and INK).                                               ;
;  1,2 | Address of the guardian's location in the attribute buffer.                                ;
;    3 | MSB of the address of the guardian's location in the screen buffer.                        ;
;    4 | Animation frame.                                                                           ;
;    5 | LSB of the address of the leftmost point of the guardian's path in the attribute buffer.   ;
;    6 | LSB of the address of the rightmost point of the guardian's path in the attribute buffer.  ;
;                                                                                                   ;
; --------------------------------------------------------------------------------------------------;

HorizontalGuardian1               DEFS 7                                   ; Horizontal Guardian 1.
HorizontalGuardian2               DEFS 7                                   ; Horizontal Guardian 2.
HorizontalGuardian3               DEFS 7                                   ; Horizontal Guardian 3.
HorizontalGuardian4               DEFS 7                                   ; Horizontal Guardian 4.
HorizontalGuardianTerm            DEFB 0                                   ; Terminator (set to 255).

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Eugene's direction or the Kong Beast's status.                 ;
;                                                                ;
; Used to hold Eugene's direction:                               ;
;     0 = Down.                                                  ;
;     1 = Up.                                                    ;
;                                                                ;
; Used to hold the Kong Beast's status:                          ;
;     0 = On the ledge.                                          ;
;     1 = Falling.                                               ;
;     2 = dead.                                                  ;
;                                                                ;
;----------------------------------------------------------------;

EugDirOrKongBeastStatus           DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------------------;
;                                                                                                               ;
; Various Uses.                                                                                                 ;
;                                                                                                               ;
; Used to hold Eugene's or the Kong Beast's pixel y-coordinate.                                                 ;
; Used to hold the index into the message scrolled across the screen after the theme tune has finished playing. ;
; Used to hold the distance of the boot from the top of the screen as it descends onto Willy.                   ;
; Used to hold Eugene's pixel y-coordinate.                                                                     ;
; Used to hold the Kong Beast's pixel y-coordinate.                                                             ;
;                                                                                                               ;
;---------------------------------------------------------------------------------------------------------------;

MultiUseCoordinateStore           DEFB 0

;-------------------------------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------;
;                                                                ;
; Vertical guardians                                             ;
;                                                                ;
; There are four slots, each one seven bytes long, used to hold  ;
; the state of the vertical guardians in the current cavern.     ;
; For each vertical guardian, the seven bytes are used as        ;
; follows:                                                       ;
;                                                                ;
; Byte | Contents                                                ;
; -----------------------------------                            ;
;    0 | Attribute                                               ;
;    1 | Animation frame                                         ;
;    2 | Pixel y-coordinate                                      ;
;    3 | x-coordinate                                            ;
;    4 | Pixel y-coordinate increment                            ;
;    5 | Minimum pixel y-coordinate                              ;
;    6 | Maximum pixel y-coordinate                              ;
;                                                                ;
;----------------------------------------------------------------;

; In most of the caverns that do not have vertical guardians, this area is overwritten by unused bytes from the cavern definition.
; The exception is Eugene's Lair: the routine that copies the graphic data for the Eugene sprite into the last 32 bytes of this
; area, where it is then used by a different routine.

VerticalGuardian1                 DEFS 7                                   ; Vertical Guardian 1.
VerticalGuardian2                 DEFS 7                                   ; Vertical Guardian 2.
VerticalGuardian3                 DEFS 7                                   ; Vertical Guardian 3.
VerticalGuardian4                 DEFS 7                                   ; Vertical Guardian 4.
VerticalGuardianTerm              DEFB 0                                   ; Terminator (set to 255 in caverns that have four
                                                                           ; Vertical Guardians).
VerticalGuardianSpare             DEFS 6                                   ; Spare.

;-------------------------------------------------------------------------------------------------------------------------------------------;

GuardianGraphicData               DEFS 256                                 ; Guardian graphic data.


;-------------------------------------------------------------------------------------------------------------------------------------------;

                                               
;----------------------------------------------------------------;
;                                                                ;
; The graphics data for the top two-thirds of the title screen.  ;
;                                                                ;
;----------------------------------------------------------------;
		org ZXFont-8
		defb    255,255,255,255,255,255,255,255		; Add Graph+Shift+[8]
		incbin "zxfont.bin"
    
		org	Caverns
;----------------------------------------------------------------;
;                                                                ;
; Screen Buffer (cavern + Willy + guardians + items).            ;
;                                                                ;
; Buffer gets initialised with the contents of the screen        ;
; buffer at 28672 (empty cavern), draws Willy, the guardians     ;
; and the items over this background, and then copies the        ;
; result to the display file.                                    ;
;----------------------------------------------------------------;

ScreenBufferCWGI                  DEFS 4096

;----------------------------------------------------------------;
;                                                                ;
; Attribute Buffer (cavern + Willy + guardians + items).         ;
;                                                                ;
; A buffer for the contents of the attribute buffer at 24064     ;
; (empty cavern),the attributes for Willy, the guardians and     ;
; the items.                                                     ;
;                                                                ;
;----------------------------------------------------------------;

AttributeBufferCWGI               DEFS 512

;----------------------------------------------------------------;
;                                                                ;
; Empty Cavern Screen Buffer.                                    ;
;                                                                ;
; Initialised upon entry to a cavern.                            ;
;                                                                ;
;----------------------------------------------------------------;
EmptyCavernScreenBuffer:          DEFS 4096

;----------------------------------------------------------------;
;                                                                ;
; Empty Cavern Attribute Buffer.                                 ;
;                                                                ;
; Initialised upon entry to a cavern and updated throughout      ;
; game.                                                          ;
;                                                                ;
;----------------------------------------------------------------;

EmptyCavernAttributeBuffer:       DEFS 512
		
TitleScreenDataTop:
40960	DEFB 5,0,0,0,0,0,224,0,0,0,0,0,0,0,0,0	
40976	DEFB 0,0,0,0,0,0,1,129,129,128,0,0,0,0,0,0	
40992	DEFB 59,0,8,99,0,0,224,0,0,0,0,0,0,0,0,0	
41008	DEFB 0,0,0,0,0,0,0,255,255,0,0,0,0,7,255,224	
41024	DEFB 3,0,0,84,0,255,0,0,7,224,0,0,15,223,220,0	
41040	DEFB 0,0,0,0,0,0,0,255,255,0,34,34,34,8,224,16	
41056	DEFB 0,255,159,148,243,0,63,192,31,248,3,252,0,0,0,0	
41072	DEFB 0,36,66,66,36,68,0,0,0,0,119,119,119,0,255,0	
41088	DEFB 0,0,0,138,0,7,255,252,7,224,63,255,224,0,0,0	
41104	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41120	DEFB 0,0,0,74,0,0,0,1,255,255,128,0,0,224,0,0
41136	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41152	DEFB 1,0,1,185,128,48,255,255,7,192,255,255,15,255,0,0	
41168	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41184	DEFB 1,36,0,18,64,18,64,18,64,1,34,64,17,65,2,16	
41200	DEFB 36,16,33,0,0,16,0,0,0,0,0,0,0,0,0,33	
41216	DEFB 7,0,0,0,0,0,248,0,0,0,0,0,0,0,0,0	
41232	DEFB 0,0,0,0,0,0,3,66,66,192,0,0,0,0,0,0	
41248	DEFB 22,0,0,0,0,0,200,0,0,0,0,0,1,240,0,0	
41264	DEFB 0,0,0,0,0,0,0,255,255,0,0,0,0,4,0,32	
41280	DEFB 5,0,0,85,0,255,0,0,127,254,0,0,15,239,120,0	
41296	DEFB 0,0,0,0,0,0,0,129,129,0,119,119,119,9,16,16	
41312	DEFB 0,127,15,85,244,0,127,224,31,248,7,254,0,0,0,0	
41328	DEFB 0,36,66,68,34,66,0,0,0,0,119,119,119,49,255,140	
41344	DEFB 0,0,0,82,0,1,255,254,7,224,127,255,128,0,0,15	
41360	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41376	DEFB 0,0,0,82,0,112,0,3,255,255,192,0,14,240,0,0	
41392	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41408	DEFB 7,0,3,16,0,48,127,255,1,240,255,252,1,255,0,0	
41424	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41440	DEFB 1,36,48,33,81,48,36,49,32,32,66,16,52,33,3,18	
41456	DEFB 2,19,64,0,0,66,0,0,0,0,0,0,0,0,0,49	
41472	DEFB 3,0,0,0,0,0,208,0,0,0,0,0,0,0,0,0	
41488	DEFB 0,0,0,0,0,0,7,36,36,224,0,0,0,0,0,0	
41504	DEFB 29,0,0,0,0,0,180,0,0,0,0,0,7,248,0,0	
41520	DEFB 0,0,0,0,0,0,0,129,129,0,0,0,0,4,24,32	
41536	DEFB 5,0,0,148,0,208,0,0,127,254,0,0,31,255,151,128	
41552	DEFB 0,101,118,86,134,86,0,129,129,0,119,119,119,9,80,16	
41568	DEFB 0,62,7,85,192,0,255,224,31,248,7,255,0,0,0,0	
41584	DEFB 0,34,66,68,36,66,0,0,0,0,119,119,119,50,255,76	
41600	DEFB 0,0,0,81,0,0,127,254,0,0,127,254,0,0,0,255	
41616	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41632	DEFB 0,0,6,82,48,127,0,3,255,255,192,0,254,248,0,0	
41648	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41664	DEFB 15,0,0,0,0,0,63,255,7,224,255,240,0,63,0,0	
41680	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41696	DEFB 2,19,21,2,67,96,33,52,80,49,33,80,55,97,80,40	
41712	DEFB 18,3,70,0,0,36,0,0,0,0,0,0,0,0,0,39	
41728	DEFB 1,0,0,0,0,0,224,0,0,0,0,0,0,0,0,0	
41744	DEFB 0,0,0,0,0,0,15,24,24,240,0,0,0,0,0,0	
41760	DEFB 31,0,0,0,0,0,246,0,0,0,0,0,15,252,0,0	
41776	DEFB 0,0,0,0,0,0,0,129,129,0,0,0,0,4,0,32	
41792	DEFB 23,0,0,162,0,248,0,0,127,254,0,0,31,255,239,92	
41808	DEFB 112,133,151,84,104,103,0,129,129,0,255,255,255,63,255,252	
41824	DEFB 0,20,2,84,192,1,255,240,31,248,15,255,128,0,0,0	
41840	DEFB 0,66,68,34,36,34,0,0,0,0,119,119,119,52,255,44	
41856	DEFB 0,0,0,149,0,0,31,255,0,0,255,248,0,0,15,255	
41872	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41888	DEFB 0,0,15,81,248,127,240,7,255,255,224,15,254,248,0,0	
41904	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41920	DEFB 15,0,0,0,0,0,30,127,3,240,255,128,0,3,0,0	
41936	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
41952	DEFB 33,48,84,48,103,40,145,32,52,81,144,36,49,84,97,32	
41968	DEFB 52,81,144,0,0,131,0,0,0,0,0,0,0,0,0,115	
41984	DEFB 6,0,0,0,0,0,228,0,0,0,0,0,0,0,0,0	
42000	DEFB 0,0,0,0,0,0,31,24,24,248,0,0,0,0,0,0	
42016	DEFB 5,0,7,129,192,48,200,0,0,0,0,0,30,59,176,0	
42032	DEFB 0,0,0,0,0,0,0,129,129,0,0,0,0,4,0,32	
42048	DEFB 29,0,0,170,0,192,0,0,63,252,0,0,14,127,238,222	
42064	DEFB 248,102,102,102,102,102,0,129,129,0,255,255,255,127,255,254	
42080	DEFB 0,0,0,146,128,1,255,240,15,240,15,255,128,0,0,0	
42096	DEFB 0,66,36,66,66,68,0,0,0,0,119,119,119,63,255,252	
42112	DEFB 0,0,0,165,0,0,7,255,3,192,255,224,0,0,63,255	
42128	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42144	DEFB 0,0,127,137,252,127,255,7,255,255,224,255,254,252,0,0	
42160	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42176	DEFB 63,0,0,0,0,0,0,31,1,128,254,0,0,0,193,0	
42192	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42208	DEFB 116,17,87,145,81,33,2,70,25,18,2,73,18,6,116,33	
42224	DEFB 52,97,33,0,0,33,0,0,0,0,0,0,0,0,0,67	
42240	DEFB 11,0,0,0,0,0,208,0,0,0,0,0,0,0,0,0	
42256	DEFB 0,0,0,0,0,0,63,36,36,252,0,0,0,0,0,0	
42272	DEFB 3,0,2,195,160,0,208,0,0,0,0,0,29,215,216,0	
42288	DEFB 0,0,0,0,0,0,0,129,129,0,0,0,0,4,0,32	
42304	DEFB 31,0,0,170,0,128,1,128,63,252,1,128,15,191,238,222	
42320	DEFB 248,102,102,102,102,102,0,129,129,0,119,119,119,255,255,255	
42336	DEFB 0,0,0,138,0,3,255,248,15,240,31,255,192,0,0,0	
42352	DEFB 0,36,66,36,36,36,0,0,0,0,119,119,119,48,255,12	
42368	DEFB 0,0,0,169,0,0,1,254,31,248,127,128,0,0,255,255	
42384	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42400	DEFB 0,1,255,170,252,255,255,199,255,255,227,255,255,254,0,0	
42416	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42432	DEFB 255,0,0,0,0,0,0,15,15,192,240,0,0,0,62,0	
42448	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42464	DEFB 248,16,47,70,33,113,21,70,49,38,21,66,19,21,3,36	
42480	DEFB 52,81,81,0,0,81,0,0,0,0,0,0,0,0,0,36	
42496	DEFB 5,0,0,0,0,0,180,0,0,0,0,0,0,0,0,0	
42512	DEFB 0,0,0,0,0,0,127,66,66,254,0,0,0,0,0,0	
42528	DEFB 6,0,1,83,192,0,184,0,0,0,0,0,11,239,232,0	
42544	DEFB 0,0,0,0,0,0,0,129,129,0,0,0,0,0,0,32	
42560	DEFB 10,0,0,170,0,0,7,128,63,252,1,224,7,223,207,111	
42576	DEFB 120,102,102,102,102,102,0,255,255,0,119,119,119,255,255,255	
42592	DEFB 0,0,0,170,0,3,255,248,15,240,31,255,192,0,0,0	
42608	DEFB 0,34,66,68,34,66,0,0,0,0,119,119,119,48,60,12	
42624	DEFB 0,0,0,170,0,0,0,124,127,254,62,0,0,15,255,255	
42640	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42656	DEFB 0,15,255,170,254,255,255,207,255,255,243,255,255,254,0,0	
42672	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42688	DEFB 255,0,0,0,0,0,0,7,3,240,224,0,0,0,0,0	
42704	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42720	DEFB 1,32,49,32,51,32,49,32,2,16,66,16,18,64,16,66	
42736	DEFB 64,16,66,0,0,130,0,0,0,0,0,0,0,0,0,64	
42752	DEFB 42,0,0,0,0,0,248,0,0,0,0,0,0,0,0,0	
42768	DEFB 0,0,0,0,0,0,255,129,129,255,0,0,0,0,0,0	
42784	DEFB 3,0,0,163,0,0,100,0,0,0,0,0,7,223,236,0	
42800	DEFB 0,0,0,0,0,0,0,255,255,0,0,0,0,4,0,32	
42816	DEFB 7,0,0,162,0,0,31,192,63,252,3,248,3,143,135,191	
42832	DEFB 240,102,102,102,102,102,0,255,255,0,119,119,119,255,255,255	
42848	DEFB 0,0,0,170,0,7,255,252,15,240,63,255,224,0,0,0	
42864	DEFB 126,166,246,166,246,166,0,0,0,0,119,119,119,48,0,12	
42880	DEFB 0,0,0,138,0,0,0,24,255,255,24,0,0,255,255,255	
42896	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42912	DEFB 0,63,255,74,255,255,255,207,255,255,243,255,255,255,0,0	
42928	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42944	DEFB 255,0,0,0,0,0,0,1,1,192,128,0,0,0,0,0	
42960	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
42976	DEFB 252,189,254,188,253,190,203,223,235,207,205,239,207,191,254,205	
42992	DEFB 188,206,189,0,0,219,0,0,0,0,0,0,0,0,0,189	

TitleScreenDataMiddle:
43008	DEFB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255	
43024	DEFB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255	
43040	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43056	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43072	DEFB 0,0,130,12,63,134,30,51,128,0,0,34,49,140,60,96	
43088	DEFB 12,96,96,0,0,139,162,251,192,139,160,136,128,0,0,0	
43104	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43120	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43136	DEFB 0,0,0,0,0,0,252,252,254,124,124,0,254,198,254,254	
43152	DEFB 252,0,254,124,0,124,254,16,252,254,0,0,0,0,0,0	
43168	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43184	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43200	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
43216	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
43232	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43248	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43264	DEFB 170,170,170,170,170,170,170,170,170,170,170,170,170,170,170,170	
43280	DEFB 170,170,170,170,170,170,170,170,170,170,170,170,170,170,170,170	
43296	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43312	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43328	DEFB 0,65,0,12,96,198,14,49,129,0,0,32,49,140,28,96	
43344	DEFB 76,48,112,0,0,217,50,130,32,137,32,133,0,0,0,0	
43360	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43376	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43392	DEFB 0,0,0,0,0,0,254,254,254,254,254,0,254,230,254,254	
43408	DEFB 254,0,254,254,0,254,254,56,254,254,0,0,0,0,0,0	
43424	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43440	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43456	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
43472	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
43488	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43504	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43520	DEFB 68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68	
43536	DEFB 68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68	
43552	DEFB 0,3,255,30,4,14,15,120,58,0,7,248,59,220,30,255	
43568	DEFB 159,240,0,0,0,1,255,136,243,206,137,255,128,0,0,0	
43584	DEFB 0,130,0,12,64,198,6,48,198,0,0,32,49,140,12,97	
43600	DEFB 140,24,16,0,0,169,42,227,192,169,32,130,0,0,0,0	
43616	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43632	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43648	DEFB 0,0,0,0,0,0,198,198,192,194,194,0,192,246,48,192	
43664	DEFB 198,0,48,198,0,194,48,108,198,48,0,0,0,0,0,0	
43680	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43696	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43712	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
43728	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
43744	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43760	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
43776	DEFB 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17	
43792	DEFB 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17	
43808	DEFB 0,6,7,60,14,7,6,48,198,0,8,56,113,142,12,97	
43824	DEFB 140,56,0,0,0,2,4,20,138,36,202,0,0,0,0,0	
43840	DEFB 0,140,0,30,225,239,2,120,56,0,0,112,123,222,4,255	
43856	DEFB 158,14,32,0,0,137,38,130,128,169,32,130,0,0,0,0	
43872	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43888	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43904	DEFB 0,0,0,0,0,0,254,254,240,248,248,0,240,246,48,240	
43920	DEFB 254,0,48,198,0,248,48,198,254,48,0,0,0,0,0,0	
43936	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43952	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
43968	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
43984	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
44000	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44016	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44032	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44048	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44064	DEFB 0,9,139,108,14,7,134,49,131,0,16,44,113,143,12,96	
44080	DEFB 76,24,0,0,0,1,196,34,243,196,170,96,0,0,0,0	
44096	DEFB 0,112,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44112	DEFB 0,7,192,0,0,139,162,250,96,83,190,250,0,0,0,0	
44128	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44144	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44160	DEFB 0,0,0,0,0,0,252,252,240,62,62,0,240,222,48,240	
44176	DEFB 252,0,48,198,0,62,48,198,252,48,0,0,0,0,0,0	
44192	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44208	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44224	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
44240	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
44256	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44272	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44288	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44304	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44320	DEFB 0,9,147,204,27,6,198,51,128,0,16,44,177,141,140,98	
44336	DEFB 12,24,0,0,0,0,36,62,162,132,154,32,0,0,0,0	
44352	DEFB 255,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255	
44368	DEFB 255,240,31,240,0,0,0,0,0,0,0,0,0,0,0,0	
44384	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44400	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44416	DEFB 0,0,0,0,0,0,192,216,192,134,134,0,192,222,48,192	
44432	DEFB 216,0,48,198,0,134,48,254,216,48,0,0,0,0,0,0	
44448	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44464	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44480	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
44496	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
44512	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44528	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44544	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44560	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44576	DEFB 0,6,35,140,19,6,102,51,0,0,19,38,177,140,204,126	
44592	DEFB 15,240,0,0,0,255,196,34,154,110,137,192,0,0,0,0	
44608	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44624	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44640	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44656	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44672	DEFB 0,0,0,0,0,0,192,204,254,254,254,0,254,206,48,254	
44688	DEFB 204,0,48,254,0,254,48,254,204,48,0,0,0,0,0,0	
44704	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44720	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44736	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
44752	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
44768	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44784	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
44800	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44816	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44832	DEFB 0,0,67,12,49,134,54,51,0,0,12,39,49,140,108,98	
44848	DEFB 12,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44864	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44880	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44896	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44912	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44928	DEFB 0,0,0,0,0,0,192,198,254,124,124,0,254,198,48,254	
44944	DEFB 198,0,48,124,0,124,48,198,198,48,0,0,0,0,0,0	
44960	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44976	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
44992	DEFB 7,199,199,193,7,199,193,7,199,199,193,7,199,193,7,199	
45008	DEFB 193,7,199,199,193,7,199,193,7,199,199,193,7,199,193,1	
45024	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1	
45040	DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

64512	DEFB 44,34,34,34,34,34,44,40	;Attributes I  (256b)
64520	DEFB 40,40,40,40,47,47,47,47
64528	DEFB 47,40,40,40,40,40,46,50
64536	DEFB 50,46,40,40,40,40,40,40
64544	DEFB 44,34,34,34,34,34,44,40
64552	DEFB 40,47,40,40,47,47,47,47
64560	DEFB 47,40,40,40,40,40,58,56
64568	DEFB 56,58,40,40,40,42,42,42
64576	DEFB 44,34,34,22,34,44,46,46
64584	DEFB 46,46,46,46,47,47,47,47
64592	DEFB 47,46,43,46,43,46,58,56
64600	DEFB 56,58,47,47,47,42,42,42
64608	DEFB 40,44,44,22,44,46,46,46
64616	DEFB 46,46,46,46,46,40,40,40
64624	DEFB 44,44,44,44,44,44,58,58
64632	DEFB 58,58,47,47,47,40,42,40
64640	DEFB 40,47,40,22,40,46,46,46
64648	DEFB 46,46,46,46,46,44,44,44
64656	DEFB 38,38,38,38,38,38,38,38
64664	DEFB 38,38,38,38,38,38,38,38
64672	DEFB 40,44,44,22,44,46,46,46
64680	DEFB 46,46,46,46,46,39,38,38
64688	DEFB 38,38,38,0,0,38,0,0
64696	DEFB 0,0,0,0,0,0,0,38
64704	DEFB 12,38,38,38,38,33,33,33
64712	DEFB 14,14,33,33,33,39,38,38
64720	DEFB 38,38,38,0,0,38,0,0
64728	DEFB 0,0,0,0,0,0,0,38
64736	DEFB 38,38,38,38,38,38,38,38
64744	DEFB 38,38,38,38,38,38,38,38
64752	DEFB 38,38,38,0,0,38,0,0
64760	DEFB 0,0,0,0,0,0,0,38

; Attributes II 	(256b)
40448	DEFB 22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22	
40464	DEFB 22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22	
40480	DEFB 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23	
40496	DEFB 23,23,23,23,23,16,16,16,16,16,16,16,16,23,23,23	
40512	DEFB 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23	
40528	DEFB 23,23,23,23,23,22,22,22,22,22,22,22,22,23,23,23	
40544	DEFB 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19	
40560	DEFB 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19	
40576	DEFB 23,23,23,23,23,23,16,16,16,16,16,16,22,22,22,22	
40592	DEFB 22,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16	
40608	DEFB 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16	
40624	DEFB 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16	
40640	DEFB 56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56	
40656	DEFB 56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56	
40672	DEFB 56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56	
40688	DEFB 56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56	

;64768	DEFB 38,0,0,0,0,0,0,0
;64776	DEFB 0,0,0,0,0,0,0,0
;64784	DEFB 0,0,0,0,0,0,0,0
;64792	DEFB 0,0,0,0,0,66,66,38
;64800	DEFB 38,0,0,0,0,0,0,0
;64808	DEFB 0,0,0,0,0,0,0,0
;64816	DEFB 0,0,0,0,0,0,0,0
;64824	DEFB 0,0,0,0,0,0,0,38
;64832	DEFB 38,5,5,5,5,5,5,5
;64840	DEFB 5,5,5,5,5,5,5,5
;64848	DEFB 5,5,5,5,5,5,5,0
;64856	DEFB 0,0,2,0,0,0,0,38
;64864	DEFB 38,0,0,0,0,0,0,0
;64872	DEFB 0,68,0,0,68,0,0,0
;64880	DEFB 0,68,0,0,0,68,0,0
;64888	DEFB 0,0,0,0,66,0,0,38
;64896	DEFB 38,66,66,0,0,0,0,0
;64904	DEFB 0,0,0,0,0,0,0,0
;64912	DEFB 0,0,0,0,0,0,0,0
;64920	DEFB 0,0,0,0,0,0,0,38
;64928	DEFB 38,0,0,0,0,66,66,0
;64936	DEFB 0,0,0,0,0,0,0,0
;64944	DEFB 0,0,0,0,0,0,0,0
;64952	DEFB 0,0,0,0,0,0,0,38
;64960	DEFB 38,0,0,0,0,0,0,0
;64968	DEFB 0,0,0,0,0,0,0,0
;64976	DEFB 0,0,0,0,0,0,0,0
;64984	DEFB 0,0,0,0,0,0,0,38
;64992	DEFB 38,66,66,66,66,66,66,66
;65000	DEFB 66,66,66,66,66,66,66,66
;65008	DEFB 66,66,66,66,66,66,66,66
;65016	DEFB 66,66,66,66,66,66,66,38


;40704	DEFB 48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48	
;40720	DEFB 48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48	
;40736	DEFB 87,87,87,87,87,87,87,87,87,87,103,103,103,103,103,103	
;40752	DEFB 103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103	
;40768	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40784	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40800	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40816	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40832	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40848	DEFB 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70	
;40864	DEFB 69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69	
;40880	DEFB 69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69	
;40896	DEFB 69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69	
;40912	DEFB 69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69	
;40928	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
;40944	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

LoadingScreenData:
	incbin "mmm.bin"

; Level data are compressed and hanlded by `mmlevels.asm` now

	include "mmlevels.asm"
	
; Boot graphic data

BootGraphicData:
47840 	DEFB 42,192,53,64,63,192,9,0			; Boot graphic data
47848 	DEFB 9,0,31,128,16,128,16,128
47856 	DEFB 17,128,34,64,32,184,89,36
47864 	DEFB 68,66,68,2,68,2,255,255

