;
; File: PlanetaryDefense2012.asm
;
    .OPT NO LIST
; ------------------
; ANALOG Computing's
; PLANETARY  DEFENSE
; ------------------
;
; by Charles Bachand
;   and Tom Hudson
;
; Written with OSS MAC/65
;
; ------------------------------------------------
; Planetary Defense 2012 Version for Android etc.
;
; TODO:
; - save high scores in memory and only submit if highest
; ------------------------------------------------
;
; ------------------
; Input devices
; ------------------
DEVSTICK = 0
DEVKOALA = 1
DEVTOUCH = 2
;
; Status indicator location
STATUS_INDICATOR = MM3
;
; ------------------
; 2012 Bonus Stuff
; ------------------
;
INITLIFEBONUS = 5	;first bonus life at 5,000 points
INITPLANETBONUS = 8 ;first bonus terraforming at 8,000 points
;
; ------------------
; Handy constants
; ------------------
;
LIFECHARACTER = $CA
INITLIVES = 4
TRUE = 1
FALSE = 0
SHOTCOLOR = $8C
LEVELSETS = 30
; Cursor coords
MINCURSOR_X = 48
MAXCURSOR_X = 207
CURSOR_X_RANGE = (MAXCURSOR_X - MINCURSOR_X + 1)
MINCURSOR_Y = 32
MAXCURSOR_Y = 223
CURSOR_Y_RANGE = (MAXCURSOR_Y - MINCURSOR_Y + 1)

;
;Terraforming REPAIRCYCLE values...
;
REPAIRCYCLE_IDLE = 0
REPAIRCYCLE_START = 1
REPAIRCYCLE_COOLDOWN = 2
REPAIRCYCLE_FINISHED = 3


; CIO EQUATES
; ===========
    *=  $0340   ;START OF IOCB
IOCB
;
ICHID *= *+1    ;DEVICE HANDLER
ICDNO *= *+1    ;DEVICE NUMBER
ICCOM *= *+1    ;I/O COMMAND
ICSTA *= *+1    ;I/O STATUS
ICBAL *= *+1    ;LSB BUFFER ADDR
ICBAH *= *+1    ;MSB BUFFER ADDR
ICPTL *= *+1    ;LSB PUT ROUTINE
ICPTH *= *+1    ;MSB PUT ROUTINE
ICBLL *= *+1    ;LSB BUFFER LEN
ICBLH *= *+1    ;MSB BUFFER LEN
ICAX1 *= *+1    ;AUX BYTE 1
ICAX2 *= *+1    ;AUX BYTE 1
;
; CIO Commands...
;
OPEN = 3
OWRITE = 8
PUTREC = 9      ;PUT TEXT RECORD
CLOSE = 12
;
CIOV =  $E456   ;CIO ENTRY VECTOR
EOL   = $9B     ;END OF LINE


;
; ------------------
; Macro definitions
; ------------------

;
; Lookup an Atari keyboard character
; Lookup_Keyboard_Char table,count,base ASCII,SHIFT add
.MACRO Lookup_Keyboard_Char
	sta THEKEYCODE
	ldx #0
; Run thru the table and see if we have a match.
; If we get one, stick the ASCII code into the accum
; and jump to the usage point!
LKCLoop lda THEKEYCODE	; Get the keyboard character
	cmp %1,x		; Is it in the table?
	beq LKCProcess	; Yes!
	clc				; Add the...
	adc #%4			; shift offset (if any)
	cmp %1,x		; Is this the character?
	beq LKCProcess	; Yes!
	inx				; Next index in table
	cpx #%2
	bne LKCLoop		; loop if more
	beq LKCEnd
LKCProcess txa		; Get the index in the accumulator
	clc				; Add the...
	adc #%3			; base ASCII code for the group
	jmp USE_KEY		; And go to the usega point!
LKCEnd lda THEKEYCODE	; Bring the character back in for whatever follows us
.ENDM

; Display 2 hex digits
; Display_2_Hex_Digits addr (value in accum)
.MACRO Display_2_Hex_Digits
	STA DDIG
	LSR A
	LSR A
	LSR A
	LSR A
	TAX
	LDA HEXDIGITS,X
	STA %1
	LDA DDIG
	AND #15
	TAX
	LDA HEXDIGITS,X
	STA %1+1
.ENDM

; Multiply 1 byte x 1 byte and get one-byte high-order result in accumulator
; I use this for remapping the touch point to Atari screen coords
; (Complete result in PRODUCTL, PRODUCTH)
;
; Usage: Do_Multiply_88_H value1(byte mem location), value2(byte mem location)
.MACRO Do_Multiply_88_H
	lda %1
	sta MULTIPLICAND
	lda %2
	sta MULTIPLIER
	jsr MULTIPLY88	; High-order byte is in accumulator!
.ENDM

; Multiply 1 byte x 9-bit multiplier and get one-byte high-order result in accumulator
; I use this for remapping the touch point to Atari screen coords
; (Complete result in PRODUCTL, PRODUCTH)
;
; Usage: Do_Multiply_89_H value1(byte mem location), value2(word [holding 9-bit value] mem location)
.MACRO Do_Multiply_89_H
	lda %1
	sta MULTIPLICAND
	lda %2
	sta MULTIPLIER
	lda %2+1
	STA MULTIPLIER+1
	jsr MULTIPLY89	; High-order byte is in accumulator!
.ENDM

; Divide -- This takes a single-byte dividend and divisor and puts the result in the 16-bit output field DIVRESULTL/H
; that is the result * 256, so the low byte is a pseudo-fraction (0-255 = 0-1)
;
; Usage: Do_DivideIM dividend(Imm),divisor(Mem)
.MACRO Do_DivideIM
	lda #0		; zero out...
	sta DIVIDEND___X	; low order byte ---L of dividend
	sta DIVIDEND_X__	; mid order byte -X-- of dividend
	sta DIVIDENDX___	; high order byte H--- of dividend
	sta DIVISORH	; high order byte of divisor
	lda #%1
	sta DIVIDEND__X_	; put dividend in mid-order byte --X- of dividend
	lda %2
	sta DIVISORL	; put divisor in low-order byte of divisor
	jsr LONG_DIVIDE	; Call the divide routine
.ENDM



; Put a character in the first score line byte and wait for a tap/release
;
.MACRO Tap_Pause
	lda %1
	sta SCOLIN
	JSR WAIT_FOR_NO_TOUCH
	JSR WAIT_FOR_TOUCH
.ENDM

;
; Do a simple delay of n jiffies
; DLI must be running to use this!
; Pause_Jiffies numJiffies
.MACRO Pause_Jiffies
	LDA #%1   ;number of jiffies
    STA DEADTM  ;dead time
PJWT LDA DEADTM ;Timer done?
    BNE PJWT  ;No.
.ENDM

;
; Store BCD-coded two-digit value as ASCII equivalent
; Format: Store2Digits BCDValue,DestinationAddr,CharTable
;
.MACRO Store2Digits
.IF %0 <> 3
.ERROR "*** BAD Call to Store2Digits ***"
.ELSE
	LDA %1
	LSR A
	LSR A
	LSR A
	LSR A
	AND #$0F
	TAX
	LDA %3,X
	STA %2
	LDA %1
	AND #$0F
	TAX
	LDA %3,X
	STA %2+1
.ENDIF
.ENDM

;
; Set P-Code operand base address
;
.MACRO SetPCodeOperand
	LDA #<%$1
	STA OPERANDADDRL	
	LDA #>%$1
	STA OPERANDADDRH
.ENDM

;
; Execute P-Code at address
; ExecutePCode <address>
;
.MACRO ExecutePCode
	LDA #<%$1
	STA PCODEL
	LDA #>%$1
	STA PCODEH
	JSR RUNPCODE
.ENDM
;
; MakePointer label,ptrDest
;
.MACRO MakePointer
.IF %0<>2
.ERROR "DrawPlanet: Wrong number of parameters"
.ELSE
	LDA #<%$1
	STA %$2
	LDA #>%$1
	STA %$2+1
.ENDIF
.ENDM
;
; DrawPlanet type,color,bgonly
;
PLANETFULL = 0
PLANET75 = 1
PLANET50 = 2
PLANET25 = 3

.MACRO DrawPlanet
.IF %0<>3
.ERROR "DrawPlanet: Wrong number of parameters"
.ELSE
	MakePointer PLANETSCRNPOS,INDX1
	LDA #%1
	STA PLANETSIZE
	LDA #%2
	STA PLANETCOLOR
	LDA #%3
	STA PLANETBGONLY
	JSR DRAW_PLANET
.ENDIF
.ENDM

;
; DrawPlanetVar type,color,bgonly
;
.MACRO DrawPlanetVar
.IF %0<>3
.ERROR "DrawPlanetVar: Wrong number of parameters"
.ELSE
	MakePointer PLANETSCRNPOS,INDX1
	LDA %1
	STA PLANETSIZE
	LDA #%2
	STA PLANETCOLOR
	LDA #%3
	STA PLANETBGONLY
	JSR DRAW_PLANET
.ENDIF
.ENDM
;
; Test planet for damage
;
;CheckPlanet type
;
.MACRO CheckPlanetVar
.IF %0<>1
.ERROR "CheckPlanetVar: Wrong number of parameters"
.ELSE
	MakePointer PLANETSCRNPOS,INDX1
	LDA %1
	STA PLANETSIZE
	JSR CHECK_PLANET
.ENDIF
.ENDM

;
; Game Title Line Setting macros...
;
.MACRO SetTerraformingTitle
	LDA TERRAFORMINGTITLEPTR
	STA GTITLE
	LDA TERRAFORMINGTITLEPTR+1
	STA GTITLE+1
.ENDM

.MACRO SetNormalTitle
	LDA GAMETITLEPTR
	STA GTITLE
	LDA GAMETITLEPTR+1
	STA GTITLE+1
.ENDM

.MACRO SetGameOverTitle
	LDA GAMEOVERTITLEPTR
	STA GTITLE
	LDA GAMEOVERTITLEPTR+1
	STA GTITLE+1
.ENDM

;
; Leaderboard randomizer macros
;
.MACRO RandomizeLeaderBoardKey
	LDA RANDOM
	AND #31		;Becomes index into GAMEKEY!
.IF %1 = 1
	STA KEYRANDOM1
.ENDIF
.IF %1 = 2
	STA KEYRANDOM2
.ENDIF
.IF %1 = 3
	STA KEYRANDOM3
.ENDIF
.IF %1 = 4
	STA KEYRANDOM4
.ENDIF
.IF %1 = 5
	STA KEYRANDOM5
.ENDIF
.ENDM

; ------------------
; Hardware Registers
; ------------------
;
HPOSP0 = $D000  ;P0 horizontal
HPOSM0 = $D004  ;M0 horizontal
P0PF =  $D004   ;P0-PField coll.
M0PL =  $D008   ;M0-PLayer coll.
GRAFP0 = $D00D  ;PM graphics
COLPF0 = $D016  ;PF color 0
GRACTL = $D01D  ;graphic control
HITCLR = $D01E  ;clear hit reg
CONSOL = $D01F  ;console buttons
AUDF1 = $D200   ;frequency 1
AUDC1 = $D201   ;volume 1
AUDCTL = $D208  ;audio control
RANDOM = $D20A  ;random numbers
DMACTL = $D400  ;DMA control
PMBASE = $D407  ;PM base address
WSYNC = $D40A   ;wait hor. sync
NMIEN = $D40E   ;interrupt reg.
ATRACT = $4D    ;attract flag
;
; ------------------------
; Operating System Vectors
; ------------------------
;
SETVBV = $E45C  ;set v.blank
XITVBV = $E462  ;exit v.blank
SIOINV = $E465  ;serial I/O init
;
; ----------------
; Shadow Registers
; ----------------
;
VDSLST = $0200  ;DLI vector
SDMCTL = $022F  ;DMA control
SDLSTL = $0230  ;DList pointer
GPRIOR = $026F  ;gr. priority
PADDL0 = $0270  ;paddle 0
PADDL1 = $0271  ;paddle 1
STICK0 = $0278  ;joystick 0
PTRIG0 = $027C  ;paddle trig 0
PTRIG1 = $027D  ;paddle trig 1
STRIG0 = $0284  ;stick trig 0
PCOLR0 = $02C0  ;player colors
COLOR0 = $02C4  ;playfield "
CH  =   $02FC   ;keyboard char
;
; -------------------
; Page Zero Registers
; -------------------
;
    *=  $80     ;top of page 0
;
INDEX *= *+2    ;temp index
INDX1 *= *+2    ;temp index
INDX2 *= *+2    ;temp index
COUNT *= *+1    ;temp register
TEMP *= *+1     ;temp register
SATEMP *= *+1   ;temp register
SCNT *= *+1     ;orbit index
LO  *=  *+1     ;plot low byte
HI  *=  *+1     ;plot high byte
DEADTM *= *+1   ;death timer
EXPTIM *= *+1   ;explosion timer
BOMTIM *= *+1   ;bomb timer
SATPIX *= *+1   ;sat. pic cntr
CURX *= *+1     ;cursor x
CURY *= *+1     ;cursor y
FROMX *= *+1    ;vector from X
FROMY *= *+1    ;vector from Y
TOX *=  *+1     ;vector to X
TOY *=  *+1     ;vector to Y
SATX *= *+1     ;satellite x
SATY *= *+1     ;satellite y
XHOLD *= *+1    ;x reg hold area
LASTRG *= *+1   ;last trigger
LEVEL *= *+1    ;level number
BLEVEL *= *+1   ;binary level #
LIVES *= *+1    ;lives left
SCORE *= *+3    ;score digits - 3 bytes, 2 digits each = 6 digits
SCOADD *= *+3   ;score inc.
SHOBYT *= *+1   ;digit hold
SHCOLR *= *+1   ;digit color
SATLIV *= *+1   ;satellite flag
BOMVL *= *+1    ;bomb value low
BOMVH *= *+1    ;bomb value high
SAUVAL *= *+1   ;saucer value
GAMCTL *= *+1   ;game ctrl flag
DLICNT *= *+1   ;DLI counter
SAUCER *= *+1   ;saucer flag
SAUTIM *= *+1   ;image timer
SAUCHN *= *+1   ;saucer chance
BOMBWT *= *+1   ;bomb wait time
BOMCOL *= *+1   ;bomb collis flg
DEVICE *= *+1   ;koala pad sw - 0=mouse 1=koala 2=touch
PLNCOL *= *+1   ;planet color
PAUSED *= *+1   ;pause flag
AVG *=  *+2     ;average
PTR *=  *+2     ;queue pointer
SSSCNT *= *+1   ;saucer snd cnt
EXSCNT *= *+1   ;expl. snd count
ESSCNT *= *+1   ;enemy shot snd
PSSCNT *= *+1   ;player shot snd
TITLE *= *+1    ;title scrn flag
PENFLG *= *+1   ;pen up/dwn flg
EXPCNT *= *+1   ;explosion counter
NEWX *= *+1     ;explosion x
NEWY *= *+1     ;explosion y
PLOTCLR *= *+1  ;plot/erase flag
COUNTR *= *+1   ;explosion index
PLOTX *= *+1    ;plot x coord
PLOTY *= *+1    ;plot y coord
HIHLD *= *+1    ;plot work area
LOHLD *= *+1    ;plot work area
VXINC *= *+1    ;vector x hold
VYINC *= *+1    ;vector y hold
LR  *=  *+1     ;vector left/right hold
UD  *=  *+1     ;vector up/down hold
DELTAX *= *+1   ;vector work area
DELTAY *= *+1   ;vector work area
XQ  *=  *+5     ;x queue
YQ  *=  *+5     ;y queue
SL  *=  *+5     ;samples lo
SH  *=  *+5     ;samples hi
; Division work areas
DIVN *=*+7
DIVIDEND___X = (DIVN+4)
DIVIDEND__X_ = (DIVN+5)
DIVIDEND_X__ = (DIVN+2)
DIVIDENDX___ = (DIVN+3)
DIVISORH = (DIVN+1)
DIVISORL = (DIVN+0)
DIVRESULTH = (DIVN+5)
DIVRESULTL = (DIVN+4)
REMAINDERH = (DIVN+1)
REMAINDERL = (DIVN+0)
CARRY *=*+1
; Multiply work areas
PRODUCTL *=*+1 
PRODUCTH *=*+1
MULTIPLIER *=*+2
MULTIPLICAND *=*+1
 
;
; ----------------------------
; Screen + Player/Missile Area
; ----------------------------
;
SCRN =  $4000   ;screen area
PLANETSCRNPOS =  SCRN+1935 ;planet pos
;
PM  =   $5000			; on 2K Boundary for single-line resolution (Addr = $5000)
MISL =  PM+$0300 ;missiles
PLR0 =  MISL+$0100 ;player 0
PLR1 =  PLR0+$0100 ;player 1
PLR2 =  PLR1+$0100 ;player 2
PLR3 =  PLR2+$0100 ;player 3
;
ORBX =  $1E00   ;orbit X
ORBY =  $1F00   ;orbit Y
;
; -------------
; Start of game
; -------------
;
    *=  $2010
;
; ------------------
; Intro Display List
; ------------------
;
TLDL .BYTE $70,$70,$70,$70
    .BYTE $70,$70,$70
    .BYTE $46
    .WORD MAGMSG
    .BYTE $70,7,7,$70,6
    .BYTE $10,6,$70,$70
    .BYTE 6,$20,6,$20
    .BYTE 6,$70,$70,$70
    .BYTE 6					; Version line
    .BYTE $41
    .WORD TLDL
;
; ------------------
; Intro Message Text
; ------------------
;
MAGMSG .SBYTE " ANALOG COMPUTING'S "
MM2    .SBYTE +$40," PLANETARY  DEFENSE "
MM3    .SBYTE +$40,"        2012        "
MM4    .SBYTE +$80," BY CHARLES BACHAND "
MM5    .SBYTE +$C0,"   AND TOM HUDSON   "
MM6    .SBYTE +$40,"JOYSTICK ----- START"
MM7    .SBYTE +$C0,"KOALA PAD --- SELECT"
MM8    .SBYTE +$80,"TOUCHSCREEN-- OPTION"
VERM   .SBYTE "    VERSION: 1.0    "
;
; ------------------------------
; Touch calibration Display List
; ------------------------------
;
CALDL .BYTE $70,$70,$70,$70
    .BYTE $70,$70,$70,$70
    .BYTE $70,$70,$70
    .BYTE $46
    .WORD CALMSG
    .BYTE $70
    .BYTE 6
    .BYTE $41
    .WORD CALDL
;
; ------------------------
; Calibration Message Text
; ------------------------
;
CALMSG .SBYTE "  TOUCH CALIBRATION "
    .SBYTE +$40,"TAP TARGET CROSSHAIR"
;
; -----------------
; Game Display List
; -----------------
;
GLIST .BYTE $70,$70,$46
GTITLE .WORD SCOLIN	;top line title
    .BYTE $4D
    .WORD SCRN
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$8D
    .BYTE $8D,$8D,$8D,$41
    .WORD GLIST
;
; -------------------------------
; Leaderboard report display List
; -------------------------------
;
LBRDL .BYTE $70,$70,$70
    .BYTE $47
    .WORD LBRMSG
    .BYTE $40
    .BYTE 6
    .BYTE $40
    .BYTE 7
    .BYTE $70
    .BYTE 6,$10,6,
    .BYTE $70,6,$10,6,$10,6,$10,6
    .BYTE $41
    .WORD LBRDL
;
; -------------------------------
; Leaderboard Report Message Text
; -------------------------------
;
.opt list
LBRMSG
	.SBYTE +$00,"INTERNET LEADERBOARD"
	.SBYTE +$40," ENTER PLAYER NAME: "
PNMSG
	.SBYTE "      "
PNAMESCREEN .SBYTE +$80,"--------"
	.SBYTE "      "
    .SBYTE +$C0,"  ENTER TO SUBMIT,  "
    .SBYTE +$C0,"ESC/BLANK TO CANCEL "
    .SBYTE +$00,"  TO SUBMIT SCORE,  "
    .SBYTE +$00,"CLICK ON 'YES' WHEN "
    .SBYTE +$00," ASKED FOR BROWSER  "
    .SBYTE +$00,"       ACCESS       "
;
; ------------------------
; Display the Intro Screen
; ------------------------
;
PLANET CLD      ;clear decimal
	LDA #0
    STA ATRACT  ;attract mode
	STA GOT_BROWSER			; Assume no web browser
	JSR OPEN_BROWSER_DEVICE	; Try opening the browser
	BMI NO_BROWSER			; No browser!
	LDA #1					; The browser is there!
	STA GOT_BROWSER
	JSR CLOSE_BROWSER_DEVICE	; Close it, we don't need it
NO_BROWSER    LDA #$00    ;get zero
    STA NMIEN   ;display off
    LDX #$7F    ;set index
CLPG0 STA $80,X ;clr top page 0
    DEX         ;dec pointer
    BNE CLPG0   ;done? No.
    INC TITLE   ;title on flag
    LDA #$FF    ;get $FF
    STA LIVES   ;set dead
    STA GAMCTL  ;game control
    STA ESSCNT  ;no enemy shots
    STA PSSCNT  ;no player shots
    JSR SNDOFF  ;no sound on 123
    STA AUDC1+6 ;turn off snd 4
    LDA #$C4    ;medium green
    STA COLOR0  ;score color
    LDA #$84    ;medium blue
    STA COLOR0+1 ;text color
    LDA #$0A    ;bright white
    STA COLOR0+2 ;shot color
    LDA #$98    ;light blue
    STA COLOR0+3 ;text color
    LDA # <DLI  ;set DLI vector
    STA VDSLST  ;low byte
    LDA # >DLI  ;set DLI vector
    STA VDSLST+1 ;high byte
    LDA #$C0    ;enable Display
    STA NMIEN   ;List Interrupts
    LDA #$32    ;PM DMA off
    STA DMACTL  ;DMA control
    STA SDMCTL  ;and shadow reg
    LDA #0      ;get zero
    STA GRACTL  ;graphics ctrl
    STA AUDCTL  ;reset audio
    LDX #4      ;5 PM registers
NOPM STA GRAFP0,X ;clr register
    DEX         ;dec index
    BPL NOPM    ;done? No.
    JSR SIOINV  ;init sound
    LDX # >VBLANK ;vblank high
    LDY # <VBLANK ;vblank low
    LDA #7      ;deferred
    JSR SETVBV  ;set vblank
    LDA #60     ;one second
    STA DEADTM  ;dead time
;
; --------------------------
; Check console and triggers
; --------------------------
;
TITLE_SCREEN
	JSR	CLEAR_PM	; Zero player/missile area
    LDA # <TLDL ;DL addr low
    STA SDLSTL  ;DL pntr low
    LDA # >TLDL ;DL addr high
    STA SDLSTL+1 ;DL pntr high
NO_CONSOLE_WAIT	LDA CONSOL			; Wait for no console keys
	CMP #7
	BNE	NO_CONSOLE_WAIT
START LDA DEADTM ;look dead time
    BNE START   ;alive? No.
CCHK LDA STRIG0 ;stick trig 0
    BNE NOTSTICK ;not using the stick!
SETSTICK LDA #DEVSTICK ;set device to stick
    JMP PDEV    ;now go store it!
SETKOALA LDA #DEVKOALA ;koala
    JMP PDEV
NOTSTICK LDA CONSOL ;get console
    CMP #6      ;start pressed?
    BEQ SETSTICK ;yes! Set joystick
    CMP #5      ;select pressed?
    BEQ SETKOALA ;yes! Set koala pad
    CMP #3      ;option pressed?
    BNE CCHK    ;no! Loop back to keep watching
    LDA #DEVTOUCH ;set touch interface!
PDEV STA DEVICE ;device switch
RELWT Pause_Jiffies 10   ;1/6 second
    LDA CONSOL  ;get console
    CMP #7      ;keys released?
    BNE RELWT   ;No. loop until
; Wait for PTRIG0 release
	JSR WAIT_FOR_NO_TOUCH
;
; ---------------------------
; Clear PM Area and Playfield
; ---------------------------
;
    LDA # >SCRN ;scrn addr high
    STA INDEX+1 ;pointer high
    LDA #0      ;get zero
    STA INDEX   ;pointer low
    LDX #15     ;16 pages 0..15
    TAY         ;use as index
CL0 STA (INDEX),Y ;clear ram
    INY         ;next byte
    BNE CL0     ;page done? No.
    INC INDEX+1 ;next page
    DEX         ;page counter
    BPL CL0     ;scrn done? No.
	JSR	CLEAR_PM	; Zero player/missile area
;
; Init player/missile graphics
;
    LDA # >PM   ;PM address high
    STA PMBASE  ;into hardware
    LDA #$3E    ;enable single
    STA SDMCTL  ;line resolution
    STA DMACTL  ;DMA control
    LDA #3      ;enable player
    STA GRACTL  ;and missile DMA
    LDA #$11    ;set up
    STA GPRIOR  ;P/M priority
;
; If in Touchscreen mode, go do the calibration!
;
	lda DEVICE		; Is device...
	cmp #DEVTOUCH	; Touch screen?
	bne GOTITLES	; No!
	JSR CALIBRATE	; Calibrate the touch interface!
	LDA TOUCH_ABORT	; Did the user abort the calibration?
	BEQ GOTITLES
	JMP TITLE_SCREEN
GOTITLES    SetNormalTitle	; make sure game titles are up
    LDA # <GLIST ;Point to the
    STA SDLSTL  ;game display
    LDA # >GLIST ;list to show
    STA SDLSTL+1 ;the playfield.rate
    LDA #0      ;get zero
    STA TITLE   ;title off
;
; ---------------
; Draw The Planet
; ---------------
;
	DrawPlanet PLANETFULL,1,TRUE
;
; -------------------------
; Setup Orbiter Coordinates
; -------------------------
;
SETUP LDX #64   ;do 65 bytes
    LDY #0      ;quad 2/4 offset
SU1 CLC         ;clear carry
    LDA #96     ;center Y
    ADC OYTBL,X ;add offset Y
    STA ORBY+$40,Y ;quad-2 Y
    STA ORBY+$80,X ;quad-3 Y
    LDA #80     ;center X
    ADC OXTBL,X ;add offset X
    STA ORBX,X  ;quad-1 X
    STA ORBX+$40,Y ;quad-2 X
    SEC         ;set carry
    LDA #80     ;center X
    SBC OXTBL,X ;sub offset X
    STA ORBX+$80,X ;quad-3 X
    STA ORBX+$C0,Y ;quad-4 X
    LDA #96     ;center Y
    SBC OYTBL,X ;sub offset Y
    STA ORBY,X  ;quad-1 Y
    STA ORBY+$C0,Y ;quad-4 Y
    INY         ;quad 2/4 offset
    DEX         ;quad 1/3 offset
    BPL SU1     ;done? No.
    JMP INIT    ;continue
;
; ---------------------------
; Orbiter X,Y Coordinate Data
; ---------------------------
;
OXTBL .BYTE 0,1,2,2,3
    .BYTE 4,5,5,6,7
    .BYTE 8,9,9,10,11
    .BYTE 12,12,13,14,14
    .BYTE 15,16,16,17,18
    .BYTE 18,19,20,20,21
    .BYTE 21,22,23,23,24
    .BYTE 24,25,25,26,26
    .BYTE 27,27,27,28,28
    .BYTE 29,29,29,30,30
    .BYTE 30,30,31,31,31
    .BYTE 31,31,32,32,32
    .BYTE 32,32,32,32,32
;
OYTBL .BYTE 54,54,54,54,54
    .BYTE 54,54,54,53,53
    .BYTE 53,52,52,52,51
    .BYTE 51,50,50,49,49
    .BYTE 48,47,47,46,45
    .BYTE 44,44,43,42,41
    .BYTE 40,39,38,38,37
    .BYTE 36,35,33,32,31
    .BYTE 30,29,28,27,26
    .BYTE 24,23,22,21,20
    .BYTE 18,17,16,15,13
    .BYTE 12,11,9,8,7
    .BYTE 5,4,3,1,0
;
; ----------------------
; Display list interrupt
; ----------------------
;
DLI PHA         ;save Acc
    TXA         ;X --> Acc
    PHA         ;save X register
    INC DLICNT  ;inc counter
    LDA DLICNT  ;get counter
    AND #$07    ;only 3 bits
    TAX         ;use as index
    LDA DLIBRT,X ;planet bright
    ORA PLNCOL  ;planet color
    STA WSYNC   ;start of scan
    STA COLPF0  ;color planet
    LDA REPAIRCYCLE ;in terraforming?
    BNE NOSHOTCOLOR ;yes! Don't alter color register 1 (used in terraforming)
    LDA #SHOTCOLOR    ;bright blue
    STA COLPF0+1 ;shot color
NOSHOTCOLOR CMP #2	;phase 2 terraforming?
	BNE NOTFPHASE2
    LDA DLIBRT2,X ;planet bright
    ORA PLNCOL  ;planet color
    STA COLPF0+1  ;color planet
NOTFPHASE2	PLA         ;restore X
    TAX         ;Acc --> X
    PLA         ;restore Acc
    RTI         ;return
;
;Main planet brightness table
DLIBRT .BYTE 0,2,4,6 ;planet
    .BYTE 8,6,4,2 ;brightness
;Terraforming cool-off brightness table for color 2
DLIBRT2 .BYTE 0,2,4,6 ;planet
    .BYTE 8,6,4,2 ;brightness
;
; ----------------------
; Vertical blank routine
; ----------------------
;
VBLANK CLD      ;clear decimal
    LDX SAUCER  ;saucer flag as
    LDA P3COLR,X ;index 0 or 1
    STA PCOLR0+3 ;saucer color
    LDA #5      ;get 5
    STA DLICNT  ;reset DLI count
    LDA #$C0    ;enable
    STA NMIEN   ;DLI's
    LDA CH      ;keyboard char
    CMP #$21    ;space bar?
    BNE PCHK    ;No. skip it
    LDA PAUSED  ;pause flag
    EOR #$FF    ;invert it
    STA PAUSED  ;save pause flag
    LDA #$FF    ;get $FF
    STA CH      ;reset keyboard
PCHK LDA PAUSED ;pause flag
    BEQ NOPAU   ;paused? No.
    LDA #0      ;get zero
    LDX #7      ;do 8 bytes
NOSND STA AUDF1,X zero sound
    DEX         ;dec index
    BPL NOSND   ;done? No.
    JMP XITVBV  ;exit VBLANK
NOPAU LDA TITLE ;title flag
    BNE NOCYC   ;title? Yes.
    LDA COLOR0+2 ;No. get color
    CLC         ;clear carry
    ADC #$10    ;next color
    STA COLOR0+2 ;explosion col.
NOCYC LDA BONUSLIFESOUND	;don't play explosion sound if bonus sound playing!
	BNE NOPAU2
	LDA EXSCNT ;explosion cnt
    BEQ NOPAU2  ;any? No.
    LSR A       ;count/2
    LSR A       ;count/4
    STA AUDC1+6 ;explo volume
    LDA #40     ;explosion
    STA AUDF1+6 ;explo frequency
    DEC EXSCNT  ;dec count
NOPAU2 LDA GAMCTL ;game control
    BPL CURSOR  ;cursor? Yes.
    JMP TIMERS  ;No. skip
;
; --------------
; Cursor handler
; --------------
;
CURSOR	LDA DEVICE	; Get inpuit device
	CMP #DEVTOUCH	; Touchscreen?
	BNE NOTTOUCH	; No!
	JSR TOUCH		; Get touch screen input
	JMP TIMERS		; Continue at timer processing
NOTTOUCH JSR CLEAR_CURSOR	;Erase the cursor
	LDA DEVICE
	CMP #DEVSTICK
	BNE DOKOALA
    LDA STICK0  ;read joystick
    LDX CURX    ;get X value
    LDY CURY    ;get Y value
    LSR A       ;shift stick right
    BCS NOTN    ;North? No.
    DEY         ;move cursor up
    DEY         ;two scan lines
NOTN LSR A      ;shift right
    BCS NOTS    ;South? No.
    INY         ;cursor down
    INY         ;two scan lines
NOTS LSR A      ;shift right
    BCS NOTW    ;West? No.
    DEX         ;cursor left
NOTW LSR A      ;shift right
    BCS NOTE    ;East? No.
    INX         ;cursor right
NOTE CPX #48    ;too far left?
    BCC BADX    ;Yes. skip next
    CPX #208    ;too far right?
    BCS BADX    ;Yes. skip next
    STX CURX    ;No. it's ok!
BADX CPY #32    ;too far up?
    BCC DOCURSOR    ;Yes. skip next
    CPY #224    ;too far down?
    BCS DOCURSOR    ;Yes. skip next
    STY CURY    ;No. it's ok!
	BCC DOCURSOR	;Go do the cursor
DOKOALA 
    JSR KOALA   ;read the koala pad!
DOCURSOR LDA PENFLG ;koala pen flg
    BNE TIMERS  ;pen up? Yes.
    JSR DRAWCURSOR
;
; -----------------------
; Handle timers and orbit
; -----------------------
;
TIMERS LDA TIMER1
    BEQ NOTIMER1
    DEC TIMER1
NOTIMER1 LDA BOMBWT ;bomb wait cnt
    BEQ NOBWT   ;wait over? Yes.
    DEC BOMBWT  ;dec count
NOBWT LDA DEADTM ;death timer
    BEQ NOTIM0  ;zero? yes.
    DEC DEADTM  ;decrement it!
NOTIM0 LDA EXPTIM ;exp timer
    BEQ NOTIM1  ;zero? Yes.
    DEC EXPTIM  ;decrement it!
NOTIM1 LDA BOMTIM ;get bomb time
    BEQ NOTIM2  ;zero? Yes.
    DEC BOMTIM  ;dec bomb time
NOTIM2 LDA GAMCTL ;game control
    BPL NOTOVR  ;game over? No.
	;flash between GAME OVER and normal title line
	LDA GAMEOVERTIMER
	BEQ TOGGLE_GAMEOVER_TITLE
	DEC GAMEOVERTIMER
	JMP XITVBV	;exit VBLANK
TOGGLE_GAMEOVER_TITLE LDA #90		;reset...
	STA GAMEOVERTIMER
	INC GAMEOVERTITLEFLAG
	LDA GAMEOVERTITLEFLAG
	AND #1
	BEQ NORMALTITLE
	SetGameoverTitle
    JMP XITVBV  ;exit VBLANK
NORMALTITLE SetNormalTitle
	JMP XITVBV
NOTOVR LDA SATLIV ;get satellite
    BEQ NOSAT   ;alive? No.
    INC SCNT    ;inc count
    LDY SCNT    ;orbit index
    CLC         ;clear carry
    LDA ORBX,Y  ;get X coord
    STA SATX    ;save Pfield x
    ADC #47     ;X offset
    STA HPOSM0+1 ;horizontal pos
    ADC #2      ;+2 offset for
    STA HPOSM0  ;right side
    LDA ORBY,Y  ;get Y coord
    LSR A       ;divide by 2
    STA SATY    ;for playfield
    ROL A       ;restore for PM
    ADC #36     ;screen offset
    TAX         ;use as index
    INC SATPIX  ;next sat. image
    LDA SATPIX  ;get number
    AND #$08    ;use bit 3
    TAY         ;use as index
    LDA #8      ;do 8 bytes
    STA SATEMP  ;save count
SSAT LDA MISL,X ;missile graphic
    AND #$F0    ;mask off 1,2
    ORA SATSH,Y ;add sat shape
    STA MISL,X  ;put player #1
    DEX         ;dec position
    INY         ;dec index
    DEC SATEMP  ;dec count
    BNE SSAT    ;done? No.
NOSAT LDA SAUCER ;saucer flag
    BEQ SOUNDS  ;saucer? No.
    LDY BOMBY+3 ;saucer Y pos
    DEY         ;-1
    DEY         ;-2
    LDX #9      ;10 scan lines
SSAULP CPY #32  ;above top?
    BCC NXTSP   ;Yes. skip it
    CPY #223    ;below bottom?
    BCS NXTSP   ;Yes. skip it
    LDA SAUPIC,X ;saucer image
    STA PLR3,Y  ;store player 3
NXTSP DEY       ;next scan line
    DEX         ;dec index
    BPL SSAULP  ;done? No.
    LDA BOMBX+3 ;saucer X pos
    STA HPOSP0+3 ;move it
    INC SAUTIM  ;saucer time
    LDA SAUTIM  ;get counter
    LSR A       ;/2
    AND #$03    ;use only 0..3
    TAX         ;as X index
    LDA SAUMID,X ;saucer middle
    STA SAUPIC+4 ;put in
    STA SAUPIC+5 ;saucer image
SOUNDS LDX BADCOORDSOUND
	BEQ MAYBESHOTSOUND
	DEC BADCOORDSOUND
	LDA #$A6	;volume
    STA AUDC1
    LDA BADCOORDSND,X	;frequency
    STA AUDF1
	JMP MUTEOTHERS
MAYBESHOTSOUND	LDX BONUSLIFESOUND	;playing bonus life sound?
	BEQ DOPLAYERSHOTSOUND
	DEC BONUSLIFESOUND
	DEX
	LDA #$A6	;volume
    STA AUDC1
    LDA BONUSSND,X	;frequency
    STA AUDF1
MUTEOTHERS
    LDA #0		;mute other sounds while playing bonus sound
    STA AUDC1+2
    STA AUDC1+4
    STA AUDC1+6
	JMP TRYS2	;bypass player shot sound
DOPLAYERSHOTSOUND LDX PSSCNT ;shot sound
    BPL DOS1    ;shot? Yes.
    LDA #0      ;No. get zero
    STA AUDC1   ;volume for shot
    BEQ TRYS2   ;skip next
DOS1 LDA #$A6   ;shot sound vol
    STA AUDC1   ;set hardware
    LDA PLSHOT,X ;shot sound
    STA AUDF1   ;frequency
    DEC PSSCNT  ;dec shot snd
TRYS2 LDX ESSCNT ;enemy shots
    BPL DOS2    ;shots? Yes.
    LDA #0      ;No. get zero
    STA AUDC1+2 ;into volume
    BEQ TRYS3   ;skip rest
DOS2 LDA #$A6   ;shot sound vol
    STA AUDC1+2 ;set hardware
    LDA ENSHOT,X ;shot sound
    STA AUDF1+2 ;frequency
    DEC ESSCNT  ;dec shot snd
TRYS3 LDA SAUCER ;saucer flag
    BEQ NOS3    ;saucer? No.
    LDA BOMBY+3 ;saucer Y pos
    CMP #36     ;above top?
    BCC NOS3    ;Yes. skip
    CMP #231    ;below bottom?
    BCC DOS3    ;No. make sound
NOS3 LDA #0     ;get zero
    STA AUDC1+4 ;no saucer snd
    BEQ VBREPAIR  ;skip next
DOS3 INC SSSCNT ;inc saucer cnt
    LDX SSSCNT  ;saucer count
    CPX #12     ;at limit?
    BMI SETS3   ;No. skip next
    LDX #0      ;get zero
    STX SSSCNT  ;zero saucer cnt
SETS3 LDA #$A8  ;saucer volume
    STA AUDC1+4 ;set hardware
    LDA SAUSND,X ;saucer sound
    STA AUDF1+4 ;set hardware
VBREPAIR LDA REPAIRCYCLE	;in a repair cycle?
	BEQ VBDONE	;no!
	CMP #REPAIRCYCLE_COOLDOWN
	BEQ REPAIRCYCLEPHASE2
	;in a repair cycle, we ramp up playfield color 2 to white
	INC REPAIRLEVEL
	LDA REPAIRLEVEL
	CMP #$7F
	BNE REPAIRSETCOLOR
	;prepare for cooldown phase
	LDA #REPAIRCYCLE_COOLDOWN
	STA REPAIRCYCLE
	JMP REPAIRCYCLEPHASE2
REPAIRSETCOLOR LSR A
	LSR A
	LSR A
	and #$0F
	STA COLPF0+1
	JMP VBDONE
; Phase 2 "cools down" the newly-terraformed crust
REPAIRCYCLEPHASE2 DEC REPAIRLEVEL
	LDA REPAIRLEVEL
	LSR A
	LSR A
	LSR A
	CMP #0
	BEQ REPAIRCYCLEDONE
	STA DLIBRT2
	ADC #2
	CMP #16
	BMI COOL2
	LDA #15
COOL2 STA DLIBRT2+1
	STA DLIBRT2+7
	ADC #2
	CMP #16
	BMI COOL3
	LDA #15
COOL3 STA DLIBRT2+2
	STA DLIBRT2+6
	ADC #2
	CMP #16
	BMI COOL4
	LDA #15
COOL4 STA DLIBRT2+3
	STA DLIBRT2+5
	ADC #2
	CMP #16
	BMI COOL5
	LDA #15
COOL5 STA DLIBRT2+4
	JMP VBDONE	
REPAIRCYCLEDONE	LDA #REPAIRCYCLE_FINISHED	;done!
	STA REPAIRCYCLE
VBDONE JMP XITVBV ;continue
;
; ---------------------
; Satellite shape table
; ---------------------
;
SATSH .BYTE 0,0,0,$0A
    .BYTE $04,$0A,0,0
    .BYTE 0,0,0,$04
    .BYTE $0A,$04,0,0
;
;
; File: D:PLANET2.M65
;
    .OPT NO LIST
; ----------------
; Initialize Misc.
; ----------------
;
INIT LDA #0     ;zero out..
	STA CURSOR_UP	; No cursor!
    STA SCORE   ;score byte 0
    STA SCORE+1 ;score byte 1
    STA SCORE+2 ;score byte 2
    STA DEADTM  ;dead timer
    STA PAUSED  ;pause flag
    STA EXPCNT  ;expl. counter
    STA SAUCER  ;no saucer
    STA BLEVEL  ;bomb level
    STA BONUSLIFESOUND	;no bonus life sound!
    STA BADCOORDSOUND	;No bad coord sound
    STA TERRAFORM ;no terraforming scheduled!
    STA TERRAFORMING_IN_PROGRESS ;no terraforming happening
    STA SUPPRESS ; 'no firing allowed' switch
    STA REPORTED	;reset 'score reported' flag
    LDX #11     ;no bombs!
CLRACT STA BOMACT,X ;deactivate
    DEX         ;next bomb
    BPL CLRACT  ;done? No.
    LDX #19     ;zero score line
INISLN LDA SCOINI,X ;get byte
    STA SCOLIN,X ;put score line
    DEX         ;next byte
    BPL INISLN  ;done? No.
    LDA #$01    ;get one
    STA LEVEL   ;game level
    STA SATLIV  ;live satellite
    LDA #INITLIVES      ;get 4
    STA LIVES   ;number of lives
    LDA #$0C    ;set explosion
    STA COLOR0+2 ;brightness
    LDA #$34    ;medium red
    STA PCOLR0  ;bomb 0 color
    STA PCOLR0+1 ;bomb 1 color
    STA PCOLR0+2 ;bomb 2 color
    LDA #127    ;center screen X
    STA CURX    ;cursor X pos
    LDA #129    ;center screen Y
    STA CURY    ;cursor Y pos
    LDA #1      ;get one
    STA GAMCTL  ;game control
    JSR SHOSCO  ;display score
    STA HITCLR  ;reset collision
    LDA #INITLIFEBONUS	;get initial life bonus
    STA NXTBONUSLIFE ;set in scoring test
    LDA #INITPLANETBONUS ;get initial planet terraforming bonus
    STA NXTBONUSPLANET ;set in scoring test
;
; ----------------------
; Set up level variables
; ----------------------
;
SETLVL JSR SHOLVL ;show level
    LDX BLEVEL  ;bomb level
    LDA INIBOM,X ;bombs / level
    STA BOMBS   ;bomb count
    LDA INIBS,X ;bomb speed
    STA BOMTI   ;bomb timer
    LDA INISC,X ;% chance of
    STA SAUCHN  ;saucer in level
    LDA INIPC,X ;planet color
    CMP #$FF    ;level >14?
    BNE SAVEPC  ;No. skip next
    LDA RANDOM  ;random color
    AND #$F0    ;mask off lum.
SAVEPC STA PLNCOL ;planet color
    LDA INIBVL,X ;bomb value low
    STA BOMVL   ;save it
    LDA INIBVH,X ;bomb value hi
    STA BOMVH   ;save it
    LDA INISV,X ;saucer value
    STA SAUVAL  ;save that too
;
; -----------------
; Main program loop
; -----------------
;
LOOP LDA PAUSED ;game paused?
    BNE LOOP    ;Yes. loop here
    LDA #0      ;get zero
    STA ATRACT  ;attract mode
    LDA GAMCTL  ;game done?
    BPL CKCORE  ;No. check core
    LDA EXPCNT  ;Yes. expl count
    BNE CKCORE  ;count done? No.
	JSR	CLEAR_PM	; Zero player/missile area
; Maybe report game score to our server?
    LDA GOT_BROWSER		; Got a browser device?
    BEQ SKIP_REPORT		; Nope!
    LDA REPORTED		; Score already reported?
    BNE SKIP_REPORT		; Yes
    JSR GET_USER_NAME	; Ask user if he wants to report score, maybe report it
SKIP_REPORT
    JMP ENDGAM  ;The End!
;
; --------------------------
; Check planet core for hit!
; --------------------------
;
CKCORE LDA SCRN+1939 ;center LF
    AND #$03    ;RT color clock
    CMP #$03    ;explosion colr?
    BEQ PLDEAD  ;Yes. go dead
;Leaderboard key randomizer 1
	RandomizeLeaderBoardKey 1
;
    LDA SCRN+1940 ;center RT
    AND #$C0    ;LF color clock
    CMP #$C0    ;explosion colr?
    BNE PLANOK  ;No. skip next
;
; ---------------
; Planet is Dead!
; ---------------
;
PLDEAD
	LDA #0   ;get zero
    STA BOMBS   ;zero bombs
    STA SATLIV  ;satellite dead
    LDA #$FF    ;get #$FF
    STA LIVES   ;no lives left
    STA GAMCTL  ;game control
	JSR ERASE_SAT	; Erase the satellite!
	JSR SNDOFF  ;no sound
;
; -------------
; Check console
; -------------
;
PLANOK LDA CONSOL ;get console
    CMP #7      ;any pressed?
    BEQ NORST   ;No. skip next
    JMP PLANET  ;restart game!
;
; -----------------
; Projectile firing
; -----------------
;
NORST JSR BOMINI ;try new bomb
    LDA SATLIV  ;satellite stat
    BEQ NOTRIG  ;alive? No.
    LDA SUPPRESS ;firing suppressed?
    BNE NOTRIG  ;yes. ignore trigger
    LDA DEVICE
    CMP #DEVSTICK
    BNE NOT_STICK_TRIGGER
    LDA STRIG0
    STA TRIGGER
NOT_STICK_TRIGGER
    LDA TRIGGER  ;get trigger
    CMP LASTRG  ;same as last VB
    BEQ NOTRIG  ;Yes. skip next
    STA LASTRG  ;No. save trig
    CMP #0      ;pressed?
    BNE NOTRIG  ;No. skip next
    JSR PROINI  ;strt projectile
NOTRIG JSR BOMADV ;advance bombs
;Leaderboard key randomizer 2
	RandomizeLeaderBoardKey 2
;
    LDA EXPTIM  ;do explosion?
    BNE NOEXP   ;no!
    JSR CHKSAT  ;satellite ok?
    JSR CHKHIT  ;any hits?
    JSR EXPHAN  ;handle expl.
    JSR PROADV  ;advance shots
    LDA SAUCER  ;saucer flag
    BEQ RESTIM  ;saucer? No.
    JSR SSHOOT  ;Yes. let shoot
RESTIM LDA #1   ;get one
    STA EXPTIM  ;reset timer
NOEXP LDA BOMBS ;# bombs to go
    BNE TOLOOP    ;any left? Yes.
    LDA GAMCTL  ;game control
    BMI TOLOOP    ;dead? Yes.
    LDA BOMACT  ;bomb 0 status
    ORA BOMACT+1 ;bomb 1 status
    ORA BOMACT+2 ;bomb 2 status
    ORA BOMACT+3 ;bomb 3 status
    BEQ LEVELDONE ;no more bombs! Wait for idle state.
TOLOOP JMP LOOP	;loop back to main loop!

;We're now at the point where a new level begins.
;If terraforming is pending, wait for all explosions and projectiles
;to complete before starting terraforming!

LEVELDONE	LDA TERRAFORM	;terraforming pending?
	BNE WAITIDLE		;yes! wait for idle state
	JSR INCREMENTLEVEL	;Go to the next level
	JMP SETLVL			;no, go set the new level
WAITIDLE LDA #1
	STA SUPPRESS	;suppress firing!
;any explosions running?	
	LDA EXPCNT  ;get expl count
	BNE TOLOOP
;any projectiles flying?
	LDA #0
	LDX #7
PTEST ORA PROACT,X
	DEX
	BPL PTEST
	CMP #0
	BNE TOLOOP
; At this point we're idle!
	JSR MAYBETERRAFORM	;terraform if the player has earned it!
	LDA #0
	STA SUPPRESS
    JMP SETLVL  ;setup new level
;
; Go to the next level
; Increments BCD level, Binary level if not max level table index
;
INCREMENTLEVEL LDA BLEVEL
    CMP #LEVELSETS     ;at max level set?
    BEQ SAMLVL  ;Yes. skip next
    INC BLEVEL  ;inc bomb level
SAMLVL SED      ;decimal mode
    LDA LEVEL   ;game level #
    CLC         ;clear carry
    ADC #1      ;add one
    STA LEVEL   ;save game level
    CLD         ;clear decimal
	RTS
;
; Draw the cursor using missiles
;
DRAWCURSOR
	LDX #5      ;6 bytes...
    LDY CURY    ;get cursor Y
SHOCUR LDA CURPIC,X ;cursor pic
    ORA MISL-3,Y ;mask missiles
    STA MISL-3,Y ;store missiles
    INY         ;next scan line
    DEX         ;dec count
    BPL SHOCUR  ;done? No.
    LDX CURX    ;get x position,
    DEX         ;1 less for...
    STX HPOSM0+3 ;missile 3
    INX         ;2 more for...
    INX         ;missile 2
    STX HPOSM0+2 ;save position
    LDA #1			; Indicate that...
    STA CURSOR_UP	; cursor is up!
	RTS
	
;
; ------------------------
; Initiate a new explosion
; ------------------------
;
NEWEXP LDA #64  ;1.07 seconds
    STA EXSCNT  ;expl sound cnt
    INC EXPCNT  ;one more expl
    LDY EXPCNT  ;use as index
    LDA NEWX    ;put X coord
    STA XPOS,Y  ;into X table
    LDA NEWY    ;put Y coord
    STA YPOS,Y  ;into Y table
;Leaderboard key randomizer 3
	RandomizeLeaderBoardKey 3
;
    LDA #0      ;init to zero
    STA CNT,Y   ;explosion image
RT1 RTS         ;return
;
; ------------------------------
; Main explosion handler routine
; ------------------------------
;
EXPHAN LDA #0   ;init to zero
    STA COUNTR  ;zero counter
RUNLP INC COUNTR ;nxt explosion
    LDA EXPCNT  ;get explosion #
    CMP COUNTR  ;any more expl?
    BMI RT1     ;No. return
    LDX COUNTR  ;get index
    LDA #0      ;init plotclr
    STA PLOTCLR ;0 = plot block
    LDA CNT,X   ;expl counter
    CMP #37     ;all drawn?
    BMI DOPLOT  ;No. do it
    INC PLOTCLR ;1 = erase block
    SEC         ;set carry
    SBC #37     ;erase cycle
    CMP #37     ;erase done?
    BMI DOPLOT  ;No. erase block
    TXA         ;move index
    TAY         ;to Y register
;
; ---------------------------
; Repack explosion table, get
; rid of finished explosions
; ---------------------------
;
REPACK INX      ;next explosion
    CPX EXPCNT  ;done?
    BEQ RPK2    ;No. repack more
    BPL RPKEND  ;Yes. exit
RPK2 LDA XPOS,X ;get X position
    STA XPOS,Y  ;move back X
    LDA YPOS,X  ;get Y position
    STA YPOS,Y  ;move back Y
    LDA CNT,X   ;get count
    STA CNT,Y   ;move back count
    INY         ;inc index
    BNE REPACK  ;next repack
RPKEND DEC EXPCNT ;dec pointers
    DEC COUNTR  ;due to repack
    JMP RUNLP   ;continue
DOPLOT INC CNT,X ;inc pointer
    TAY         ;exp phase in Y
    LDA XPOS,X  ;get X-coord
    CLC         ;clear carry
    ADC COORD1,Y ;add X offset
    STA PLOTX   ;save it
    CMP #160    ;off screen?
    BCS RUNLP   ;Yes. don't plot
    LDA YPOS,X  ;get Y-coord
    ADC COORD2,Y ;add Y offset
    STA PLOTY   ;save it
    CMP #96     ;off screen?
    BCS RUNLP   ;Yes. don't plot
    JSR PLOT    ;get plot addr
    LDA PLOTCLR ;erase it?
    BNE CLEARIT ;Yes. clear it
    LDA PLOTBL,X ;get plot bits
    ORA (LO),Y  ;alter display
PUTIT STA (LO),Y ;and replot it!
    JMP RUNLP   ;exit
CLEARIT LDA ERABIT,X ;erase bits
    AND (LO),Y  ;turn off pixel
    JMP PUTIT   ;put it back
;
; ------------------------
; Dedicated multiply by 40
; with result in LO and HI
; ------------------------
;
PLOT LDA PLOTY  ;get Y-coord
    ASL A       ;shift it left
    STA LO      ;save low *2
    LDA #0      ;get zero
    STA HI      ;init high byte
    ASL LO      ;shift low byte
    ROL HI      ;rotate high *4
    ASL LO      ;shift low byte
    LDA LO      ;get low byte
    STA LOHLD   ;save low *8
    ROL HI      ;rotate high *8
    LDA HI      ;get high byte
    STA HIHLD   ;save high *8
    ASL LO      ;shift low byte
    ROL HI      ;rotate high *16
    ASL LO      ;shift low byte
    ROL HI      ;rotate high *32
    LDA LO      ;get low *32
    CLC         ;clear carry
    ADC LOHLD   ;add low *8
    STA LO      ;save low *40
    LDA HI      ;get high *32
    ADC HIHLD   ;add high *8
    STA HI      ;save high *40
;
; -----------------------------
; Get offset into screen memory
; -----------------------------
;
    LDA # <SCRN ;screen addr lo
    CLC         ;clear carry
    ADC LO      ;add low offset
    STA LO      ;save addr low
    LDA # >SCRN ;screen addr hi
    ADC HI      ;add high offset
    STA HI      ;save addr hi
    LDA PLOTX   ;mask PLOTX for
    AND #3      ;the plot bits,
    TAX         ;place in X..
    LDA PLOTX   ;get PLOTX and
    LSR A       ;divide
    LSR A       ;by 4
    CLC         ;and add to
    ADC LO      ;plot address
    STA LO      ;for final plot
    BCC PLOT1   ;address.
    INC HI      ;overflow? Yes.
PLOT1 LDY #0    ;zero Y register
    RTS         ;return
;
; ----------------
; Bomb initializer
; ----------------
;
BOMINI LDA BOMBWT ;bomb wait time
    BNE NOBINI  ;done? No.
    LDA BOMBS   ;more bombs?
    BNE CKLIVE  ;Yes. skip RTS
NOBINI RTS      ;No. return
CKLIVE LDX #3   ;find..
CKLVLP LDA BOMACT,X ;an available..
    BEQ GOTBOM  ;bomb? Yes.
    DEX         ;No. dec index
    BPL CKLVLP  ;done? No.
    RTS         ;return
GOTBOM LDA #1   ;this one is..
    STA BOMACT,X ;active now
    DEC BOMBS   ;one less bomb
;Leaderboard key randomizer 4
	RandomizeLeaderBoardKey 4
;
    LDA #0      ;zero out all..
    STA BXHOLD,X ;vector X hold
    STA BYHOLD,X ;vector Y hold
    LDA GAMCTL  ;game control
    BMI NOSAUC  ;saucer possible?
;
; --------------
; Saucer handler
; --------------
;
    CPX #3      ;Yes. bomb #3?
    BNE NOSAUC  ;No. skip next
    LDA RANDOM  ;random number
    CMP SAUCHN  ;compare chances
    BCS NOSAUC  ;put saucer? No.
    LDA #1      ;Yes. get one
    STA SAUCER  ;enable saucer
    LDA RANDOM  ;random number
    AND #$03    ;range: 0..3
    TAY         ;use as index
    LDA STARTX,Y ;saucer start X
    CMP #$FF    ;random flag?
    BNE SAVESX  ;No. use as X
    JSR SAURND  ;random X-coord
    ADC #35     ;add X offset
SAVESX STA FROMX ;from X vector
    STA BOMBX,X ;init X-coord
    LDA STARTY,Y ;saucer start Y
    CMP #$FF    ;random flag?
    BNE SAVESY  ;No. use as Y
    JSR SAURND  ;random Y-coord
    ADC #55     ;add Y offset
SAVESY STA FROMY ;from Y vector
    STA BOMBY,X ;init Y-coord
    LDA ENDX,Y  ;saucer end X
    CMP #$FF    ;random flag?
    BNE SAVEEX  ;No. use as X
    LDA #230    ;screen right
    SEC         ;offset so not
    SBC FROMY   ;to hit planet
SAVEEX STA TOX  ;to X vector
    LDA ENDY,Y  ;saucer end Y
    CMP #$FF    ;random flag?
    BNE SAVEEY  ;No. use as Y
    LDA FROMX   ;use X for Y
SAVEEY STA TOY  ;to Y vector
    JMP GETBV   ;skip next
;
; ------------
; Bomb handler
; ------------
;
NOSAUC LDA RANDOM ;random number
    BMI BXMAX   ;coin flip
    LDA RANDOM  ;random number
    AND #1      ;make 0..1
    TAY         ;use as index
    LDA BMAXS,Y ;top/bottom tbl
    STA BOMBY,X ;bomb Y-coord
SETRBX LDA RANDOM ;random number
    CMP #250    ;compare w/250
    BCS SETRBX  ;less than? No.
    STA BOMBX,X ;bomb X-coord
    JMP BOMVEC  ;skip next
BXMAX LDA RANDOM ;random number
    AND #1      ;make 0..1
    TAY         ;use as index
    LDA BMAXS,Y ;0 or 250
    STA BOMBX,X ;bomb X-coord
SETRBY LDA RANDOM ;random number
    CMP #250    ;compare w/250
    BCS SETRBY  ;less than? No.
    STA BOMBY,X ;bomb Y-coord
BOMVEC LDA BOMBX,X ;bomb X-coord
    STA FROMX   ;shot from X
    LDA BOMBY,X ;bomb Y-coord
    STA FROMY   ;shot from Y
    LDA #128    ;planet center X
    STA TOX     ;shot to X-coord
    LDA #133	;planet center Y
    STA TOY     ;shot to Y-coord
GETBV JSR VECTOR ;calc shot vect
;
; ---------------------
; Store vector in table
; ---------------------
;
    LDA LR      ;bomb L/R flag
    STA BOMBLR,X ;bomb L/R table
    LDA UD      ;bomb U/D flag
    STA BOMBUD,X ;bomb U/D table
    LDA VXINC   ;velocity X inc
    STA BXINC,X ;Vel X table
    LDA VYINC   ;velocity Y inc
    STA BYINC,X ;Vel Y table
    RTS         ;return
;
; -----------------------------
; Saucer random generator 0..99
; -----------------------------
;
SAURND LDA RANDOM ;random number
    AND #$7F    ;0..127
    CMP #100    ;compare w/100
    BCS SAURND  ;less than? No.
    RTS         ;return
;
; --------------------
; Saucer shoot routine
; --------------------
;
SSHOOT LDA RANDOM ;random number
    CMP #6      ;2.3% chance?
    BCS NOSS    ;less than? No.
    LDX #7      ;7 = index
;Leaderboard key randomizer 5
	RandomizeLeaderBoardKey 5
;
    LDA PROACT,X ;projectile #7
    BEQ GOTSS   ;active? No.
    DEX         ;6 = index
    LDA PROACT,X ;projectile #6
    BEQ GOTSS   ;active? No.
NOSS RTS        ;return, no shot
;
; --------------------
; Enable a saucer shot
; --------------------
;
GOTSS LDA #48   ;PF center, Y
    STA TOY     ;shot to Y-coord
    LDA #80     ;PF center X
    STA TOX     ;shot to X-coord
    LDA BOMBX+3 ;saucer x-coord
    SEC         ;set carry
    SBC #44     ;PF offset
    STA FROMX   ;shot from X
    STA PROJX,X ;X-coord table
    CMP #160    ;screen X limit
    BCS NOSS    ;on screen? No.
    LDA BOMBY+3 ;saucer Y-coord
    SBC #37     ;PF offset
    LSR A       ;2 scan lines
    STA FROMY   ;shot from Y
    STA PROJY,X ;Y-coord table
    CMP #95     ;screen Y limit
    BCS NOSS    ;on screen? No.
    LDA #13     ;shot snd time
    STA ESSCNT  ;emeny snd count
    JMP PROVEC  ;continue
;
; ----------------------
; Projectile initializer
; ----------------------
;
PROINI LDX #5   ;6 projectiles
PSCAN LDA PROACT,X ;get status
    BEQ GOTPRO  ;active? No.
    DEX         ;Yes. try again
    BPL PSCAN   ;done? No.
    RTS         ;return
;
; -----------------
; Got a projectile!
; -----------------
;
GOTPRO LDA #13  ;shot snd time
    STA PSSCNT  ;player sht snd
    LDA SATX    ;satellite X
    STA FROMX   ;shot from X
    STA PROJX,X ;proj X table
    LDA SATY    ;satellite Y
    STA FROMY   ;shot from Y
    STA PROJY,X ;proj Y table
    LDA CURX    ;cursor X-coord
    SEC         ;set carry
    SBC #48     ;playfld offset
    STA TOX     ;shot to X-coord
    LDA CURY    ;cursor Y-coord
    SEC         ;set carry
    SBC #32     ;playfld offset
    LSR A       ;2 line res
    STA TOY     ;shot to Y-coord
PROVEC JSR VECTOR ;compute vect
    LDA VXINC   ;X increment
    STA PXINC,X ;X inc table
    LDA VYINC   ;Y increment
    STA PYINC,X ;Y inc table
    LDA LR      ;L/R flag
    STA PROJLR,X ;L/R flag table
    LDA UD      ;U/D flag
    STA PROJUD,X ;U/D flag table
    LDA #1      ;active
    STA PROACT,X ;proj status
RT2 RTS         ;return
;
; --------------------
; Bomb advance handler
; --------------------
;
BOMADV LDA BOMTIM ;bomb timer
    BNE RT2     ;time up? No.
    LDA LIVES   ;any lives?
    BPL REGBT   ;Yes. skip next
    LDA #1      ;speed up bombs
    BNE SETBTM  ;skip next
REGBT LDA BOMTI ;get bomb speed
SETBTM STA BOMTIM ;reset timer
    LDX #3      ;check 4 bombs
ADVBLP LDA BOMACT,X ;bomb on?
    BEQ NXTBOM  ;No. try next
    JSR ADVIT   ;advance bomb
    LDA LIVES   ;any lives left?
    BPL SHOBOM  ;Yes. skip next
    JSR ADVIT   ;No. move bombs
    JSR ADVIT   ;4 times faster
    JSR ADVIT   ;than normal
;
; --------------------------
; We've now got updated bomb
; coordinates for plotting!
; --------------------------
;
SHOBOM LDA BOMBY,X ;bomb Y-coord
    CLC         ;clear carry
    ADC #2      ;bomb center off
    STA INDX1   ;save it
    LDA #0      ;get zero
    STA LO      ;init low byte
    TXA         ;index to Acc
    ORA # >PLR0 ;mask w/address
    STA HI      ;init high byte
    STX INDX2   ;X temp hold
    CPX #3      ;saucer slot?
    BNE NOTSAU  ;No. skip next
    LDA SAUCER  ;saucer in slot?
    BNE NXTBOM  ;Yes. skip bomb
NOTSAU LDY BOMBLR,X ;L/R flag
    LDA #17     ;do 17 bytes
    STA TEMP    ;set counter
    LDX BPSTRT,Y ;start position
    LDY INDX1   ;bomb Y pos
BDRAW CPY #32   ;off screen top?
    BCC NOBDRW  ;Yes. skip next
    CPY #223    ;screen bottom?
    BCS NOBDRW  ;Yes. skip next
    LDA BOMPIC,X ;bomb picture
    STA (LO),Y  ;put in PM area
NOBDRW DEY      ;PM index
    DEX         ;picture index
    DEC TEMP    ;dec count
    BNE BDRAW   ;done? No.
    LDX INDX2   ;restore X
    LDA BOMBX,X ;bomb X-coord
    STA HPOSP0,X ;player pos
NXTBOM DEX      ;more bombs?
    BPL ADVBLP  ;yes!
    RTS         ;all done!
;
; --------------------------
; Projectile advance handler
; --------------------------
;
PROADV LDX #11  ;do 8: 11..4
	LDA #0
	STA ACTIVEPROJCOUNT
PADVLP LDA BOMACT,X ;active?
    BEQ NXTPRO  ;No. skip next
    LDA BOMBX,X ;bomb X-coord
    STA PLOTX   ;plotter X
    LDA BOMBY,X ;bomb Y-coord
    STA PLOTY   ;plotter Y
    STX XHOLD   ;X-reg temporary
    JSR PLOT    ;calc plot addr
    LDA (LO),Y  ;get plot byte
    AND ERABIT,X ;erase bit
    STA (LO),Y  ;replace byte
    LDX XHOLD   ;restore X
    JSR ADVIT   ;advance proj
    LDA BOMBX,X ;bomb X-coord
    CMP #160    ;off screen?
    BCS KILPRO  ;Yes. kill it
    STA PLOTX   ;plotter X
    LDA BOMBY,X ;bomb Y-coord
    CMP #96     ;off screen?
    BCS KILPRO  ;Yes. kill it
    STA PLOTY   ;plotter Y
    JSR PLOT    ;calc plot addr
    LDA PLOTBL,X ;get plot mask
    AND (LO),Y  ;chk collision
    BEQ PROJOK  ;No. plot it
    LDX XHOLD   ;restore X
    LDA PLOTX   ;proj X-coord
    STA NEWX    ;explo X-coord
    LDA PLOTY   ;proj Y-coord
    STA NEWY    ;explo Y-coord
    JSR NEWEXP  ;set off explo
KILPRO LDA #0   ;get zero
    STA BOMACT,X ;kill proj
    JMP NXTPRO  ;skip next
PROJOK LDA PLOTBL,X ;plot mask
    LDX XHOLD   ;restore X
    AND PROMSK,X ;mask color
    ORA (LO),Y  ;add playfield
    STA (LO),Y  ;replace byte
    INC ACTIVEPROJCOUNT	; Increment the number of active projectiles
NXTPRO DEX      ;next projectile
    CPX #3      ;proj #3 yet?
    BNE PADVLP  ;No. continue
    LDA ACTIVEPROJCOUNT
	BEQ NOACTIVEPROJ
    RTS         ;return
NOACTIVEPROJ LDA DEVICE	; If device is touchscreen,
	CMP #DEVTOUCH		; Cursor times out when no shots active
	BNE NO_CURSOR_TIMEOUT	; Not touchscreen!
	JSR CLEAR_CURSOR	; Clear the cursor
NO_CURSOR_TIMEOUT
	RTS
;
;
; File: D:PLANET3.M65
;
    .OPT NO LIST
; ----------------------
; Check satellite status
; ----------------------
;
CHKSAT LDA DEADTM ;satellite ok?
    BEQ LIVE    ;No. skip next
CHKSX RTS       ;return
LIVE LDA LIVES  ;lives left?
    BMI CHKSX   ;No. exit
    LDA #1      ;get one
    STA SATLIV  ;set alive flag
    LDA M0PL    ;did satellite
    ORA M0PL+1  ;hit any bombs?
    BEQ CHKSX   ;No. exit
    LDA #0      ;get zero
    STA SATLIV  ;kill satellite
    STA SCNT    ;init orbit
    LDX LIVES   ;one less life
    STA SCOLIN+14,X ;erase life
    DEC LIVES   ;dec lives count
    BPL MORSAT  ;any left? Yes.
    LDA #255    ;lot of bombs
    STA BOMBS   ;into bomb count
    STA GAMCTL  ;end game
    JSR SNDOFF  ;no sound 1 2 3
MORSAT LDA SATX ;sat X-coord
    STA NEWX    ;explo X-coord
    LDA SATY    ;sat Y-coord
    STA NEWY    ;explo Y-coord
    JSR NEWEXP  ;set off explo
    LDA #80     ;init sat X
    STA SATX    ;sat X-coord
    LDA #21     ;init sat Y
    STA SATY    ;sat Y-coord
    JSR ERASE_SAT
    LDA #$FF    ;4.25 seconds
    STA DEADTM  ;till next life!
    RTS         ;return
;
; Erase satellite
;
ERASE_SAT LDX #0      ;don't show the
CLRSAT LDA MISL,X ;satellite pic
    AND #$F0    ;mask off sat
    STA MISL,X  ;restore data
    DEX         ;dec index
    BNE CLRSAT  ;done? No.
	RTS
;
; ------------------
; Check console keys
; ------------------
;
ENDGAM JSR SNDOFF ;no sound 123
ENDGLP LDA STRIG0 ;stick trigger
    AND PTRIG0  ;mask w/paddle 0
    AND PTRIG1  ;mask w/paddle 1
    BEQ ENDGL1  ;any pushed? No.
    LDA CONSOL  ;chk console
    CMP #7      ;any pushed?
    BEQ ENDGLP  ;No. loop here
ENDGL1 JMP PLANET ;restart game
;
; -------------------------
; Turn off sound regs 1 2 3
; -------------------------
;
SNDOFF LDA #0   ;zero volume
    STA AUDC1   ;to sound #1
    STA AUDC1+2 ;sound #2
    STA AUDC1+4 ;sound #3
    RTS         ;return
;
; -----------------------
; Check for hits on bombs
; -----------------------
;
CHKHIT LDX #3   ;4 bombs 0..3
    LDA SAUCER  ;saucer enabled?
    BEQ CHLOOP  ;No. skip next
    LDA #0      ;get zero
    STA BOMCOL  ;collision count
    LDA GAMCTL  ;game over?
    BMI NOSCOR  ;Yes. skip next
    LDA BOMBX+3 ;saucer X-coord
    CMP #39     ;off screen lf?
    BCC NOSCOR  ;Yes. kill it
    CMP #211    ;off screen rt?
    BCS NOSCOR  ;Yes. kill it
    LDA BOMBY+3 ;saucer Y-coord
    CMP #19     ;off screen up?
    BCC NOSCOR  ;Yes. kill it
    CMP #231    ;off screen dn?
    BCS NOSCOR  ;Yes. kill it
CHLOOP LDA #0   ;get zero
    STA BOMCOL  ;collision count
    LDA P0PF,X  ;playf collision
    AND #$05    ;w/shot+planet
    BEQ NOBHIT  ;hit either? No.
    INC BOMCOL  ;Yes. inc count
    AND #$04    ;hit shot?
    BEQ NOSCOR  ;No. skip next
    LDA GAMCTL  ;game over?
    BMI NOSCOR  ;Yes. skip next
    LDA #2      ;1/30th second
    STA BOMBWT  ;bomb wait time
    CPX #3      ;saucer player?
    BNE ADDBS   ;No. skip this
    LDA SAUCER  ;saucer on?
    BEQ ADDBS   ;No. this this
    LDA SAUVAL  ;saucer value
    STA SCOADD+1 ;point value
    JMP ADDIT   ;add to score
;
; -----------------------
; Add bomb value to score
; -----------------------
;
ADDBS LDA BOMVL ;bomb value low
    STA SCOADD+2 ;score inc low
    LDA BOMVH   ;bomb value high
    STA SCOADD+1 ;score inc high
ADDIT STX XHOLD ;save X register
    JSR ADDSCO  ;add to score
    LDX XHOLD   ;restore X
NOSCOR LDA #0   ;get zero
    STA BOMACT,X ;kill bomb
    LDY BOMBLR,X ;L/R flag
    LDA BOMBX,X ;bomb X-coord
    SEC         ;set carry
    SBC BXOF,Y  ;bomb X offset
    STA NEWX    ;plotter X-coord
    LDA BOMBY,X ;bomb Y-coord
    SEC         ;set carry
    SBC #40     ;bomb Y offset
    LSR A       ;2 line res.
    STA NEWY    ;plotter Y-coord
    LDA SAUCER  ;saucer?
    BEQ EXPBOM  ;No. explode it
    CPX #3      ;bomb player?
    BNE EXPBOM  ;Yes. explode it
    LDA #0      ;get zero
    STA SAUCER  ;kill saucer
    JSR CLRPLR  ;clear player
    LDA GAMCTL  ;game over?
    BMI NOBHIT  ;Yes. skip next
EXPBOM JSR CLRPLR ;clear player
    LDA BOMCOL  ;collisions?
    BEQ NOBHIT  ;No. skip this
    JSR NEWEXP  ;init explosion
NOBHIT DEX      ;dec index
    BPL CHLOOP  ;done? No.
    STA HITCLR  ;reset collision
    RTS         ;return
;
; -------------------------
; Advance bombs/projectiles
; -------------------------
;
ADVIT LDA BXHOLD,X ;bomb X-sum
    CLC         ;clear carry
    ADC BXINC,X ;add X-increment
    STA BXHOLD,X ;replace X-sum
    LDA #0      ;get zero
    ROL A       ;carry = 1
    STA DELTAX  ;X-delta
    LDA BYHOLD,X ;bomb Y-sum
    ADC BYINC,X ;add Y-increment
    STA BYHOLD,X ;replace Y-sum
    LDA #0      ;get zero
    ROL A       ;carry = 1
    STA DELTAY  ;Y-delta
    LDA BOMBLR,X ;bomb L/R flag
    BEQ ADVLFT  ;go left? Yes.
    LDA BOMBX,X ;bomb X-coord
    ADC DELTAX  ;add X-delta
    JMP ADVY    ;skip next
ADVLFT LDA BOMBX,X ;bomb X-coord
    SEC         ;set carry
    SBC DELTAX  ;sub X-delta
ADVY STA BOMBX,X ;save X-coord
    LDA BOMBUD,X ;bomb U/D flag
    BEQ ADVDN   ;go down? Yes.
    LDA BOMBY,X ;bomb Y-coord
    SEC         ;set carry
    SBC DELTAY  ;sub Y-delta
    JMP ADVEND  ;skip next
ADVDN LDA BOMBY,X ;bomb Y-coord
    CLC         ;clear carry
    ADC DELTAY  ;add Y-delta
ADVEND STA BOMBY,X ;save Y-coord
    RTS         ;return
;
; --------------------------
; Clear out player indicated
; by the X register!
; --------------------------
;
CLRPLR LDA #0   ;move player...
    STA HPOSP0,X ;off screen,
    TAY         ;init index
    TXA         ;get X
    ORA # >PLR0 ;mask w/address
    STA HI      ;plr addr high
    TYA         ;Acc = 0
    STA LO      ;plr addr low
CLPLP STA (LO),Y ;zero player
    DEY         ;dec index
    BNE CLPLP   ;done? No.
    RTS         ;return
;
; --------------------------------------
; P-Code work variables
; --------------------------------------
PCACCUMULATOR .BYTE 0	;P-Code accumulator "register"
PCOPERAND .BYTE 0	;P-Code generic operand
PCINDEX .BYTE 0		;P-Code index
; --------------------------------------
; Execute P-Code Sequence
; Place P-Code address in PCODEL, PCODEH
; --------------------------------------
RUNPCODE LDX #0			; Instruction index
PCODELOOP	JSR GETPCBYTEY	; Get opcode in Y reg
	CPY #PCLOAD
	BNE PCL2
; PCLOAD
	JSR GETPCOPERAND	; Operand ends up in "PCOPERAND"
	LDA PCOPERAND		; Put it...
	STA PCACCUMULATOR	; in Accumulator
	JMP	PCODELOOP
PCL2 CPY #PCADD
	BNE PCL3
; PCADD
	JSR GETPCOPERAND
	LDA PCACCUMULATOR
	CLC
	ADC PCOPERAND
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL3 CPY #PCADD31
	BNE PCL4
; PCADD31
	LDA PCACCUMULATOR
	CLC
	ADC #31
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL4 CPY #PCAND
	BNE PCL5
; PCAND
	JSR GETPCOPERAND
	LDA PCACCUMULATOR
	AND PCOPERAND
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL5 CPY #PCAND31
	BNE PCL6
; PCAND31
	LDA PCACCUMULATOR
	AND #31
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL6 CPY #PCSUB
	BNE PCL7
; PCSUB
	JSR GETPCOPERAND
	LDA PCACCUMULATOR
	SEC
	SBC PCOPERAND
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL7 CPY #PCMAKEINDEX
	BNE PCL8
; PCMAKEINDEX
	LDA PCACCUMULATOR
	STA PCINDEX
	JMP PCODELOOP
PCL8 CPY #PCLOADGAMEKEYWITHINDEX
	BNE PCL9
; PCLOADGAMEKEYWITHINDEX
	LDY PCINDEX
	LDA GAMEKEY,Y
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL9 CPY #PCLOADGAMEKEY2WITHINDEX
	BNE PCL10
; PCLOADGAMEKEY2WITHINDEX
	LDY PCINDEX
	LDA GAMEKEY2,Y
	STA PCACCUMULATOR
	JMP PCODELOOP
PCL10 CPY #PCMAKEKEYDIGIT
	BNE PCL11
; PCMAKEKEYDIGIT
	LDY PCACCUMULATOR	; Move accumulator over into Y index for key digit
	LDA KEYDIGITS,y	; Get the key digit
	JSR GETPCBYTEY	; Get offset into output in Y reg
	STA URLKEY,Y
	JMP PCODELOOP
PCL11 ;Add other opcodes here.
; If fall thru to here, default opcode is PCEND - FINISHED!
	RTS
;
; Load up "PCOPERAND" with operand indicated by next two bytes
;
OPERANDADDRL = PCOPPTR+1	;Pointer to operand table base address
OPERANDADDRH = PCOPPTR+2
GETPCOPERAND JSR GETPCBYTEY	; Get next byte in accum (indicates operand type)
	CPY #KR			;KeyRandom?
	BNE GPCO2		;No!
	SetPCodeOperand KEYRANDOM1
	JMP GETPOCOPERANDINDEX
		
GPCO2 CPY #LD		;LevelDigit?
	BNE GPCO3		;No!
	SetPCodeOperand URLLEVEL
	JMP GETPOCOPERANDINDEX	
	
GPCO3 CPY #SD		;ScoreDigit?
	BNE GPCO4		;No!
	SetPCodeOperand URLSCORE
	JMP GETPOCOPERANDINDEX	
	
GPCO4				;NameChar (default)
	SetPCodeOperand URLNAME

GETPOCOPERANDINDEX	JSR GETPCBYTEY			; Get operand index byte in Y reg
	DEY				;Decrement to make it 0-based
PCOPPTR	LDA $0000,Y		;Grab the byte!(self-modifying code)
	STA PCOPERAND	;Store in PCOPERAND variable
	RTS				;and return!
;
; Get next byte in P-Code instruction list, put in Y reg
; Auto-increments instruction pointer index
;
PCODEL = GETPCBYTEY+1
PCODEH = GETPCBYTEY+2
GETPCBYTEY LDY $FFFF,X	; Self-modifying code!  PCODEL/H point to PCode base address
	INX
	RTS
;
; -----------------------
; Calculate target vector
; -----------------------
;
VECTOR LDA #0   ;get zero
    STA LR      ;going left
    LDA FROMX   ;from X-coord
    CMP TOX     ;w/to X-coord
    BCC RIGHT   ;to right? Yes.
    SBC TOX     ;get X-diff
    JMP VECY    ;skip next
RIGHT INC LR    ;going right
    LDA TOX     ;to X-coord
    SEC         ;set carry
    SBC FROMX   ;get X-diff
VECY STA VXINC  ;save difference
    LDA #1      ;get one
    STA UD      ;going up flag
    LDA FROMY   ;from Y-coord
    CMP TOY     ;w/to Y-coord
    BCC DOWN    ;down? Yes.
    SBC TOY     ;get Y-diff
    JMP VECSET  ;skip next
DOWN DEC UD     ;going down flag
    LDA TOY     ;to Y-coord
    SEC         ;set carry
    SBC FROMY   ;get Y-diff
VECSET STA VYINC ;are both
    ORA VXINC   ;distances 0?
    BNE VECLP   ;No. skip next
    LDA #$80    ;set x increment
    STA VXINC   ;to default.
VECLP LDA VXINC ;X vector incre
    BMI VECEND  ;>127? Yes.
    LDA VYINC   ;Y vector incre
    BMI VECEND  ;>127? Yes.
    ASL VXINC   ;times 2 until
    ASL VYINC   ;one is >127
    JMP VECLP   ;continue
VECEND RTS      ;return
;
; ------------
; Add to score
; ------------
;
ADDSCO LDY #0   ;init index
    SED         ;decimal mode
    CLC         ;clear carry
    LDX #2      ;do 3 bytes
ASCLP LDA SCORE,X ;get score
    ADC SCOADD,X ;add bomb value
    STA SCORE,X ;save score
    STY SCOADD,X ;zero value
    DEX         ;next byte
    BPL ASCLP   ;done? No.
    CLD         ;clear decimal
    JSR BONUSLIFE ;check for bonus life
    JSR BONUSPLANET ;check for bonus planet (terraforming)
;
; ----------
; Show score
; ----------
;
SHOSCO LDA #$10 ;put color 0
    STA SHCOLR  ;in hold area
    LDX #1      ;2nd line char
    LDY #0      ;digits 1,2
SSCOLP LDA SCORE,Y ;get digits
    JSR SHOBCD  ;show 'em
    INX         ;advance score
    INX         ;line pointer
    INY         ;next 2 digits
    CPY #3      ;done 6?
    BNE SSCOLP  ;no!
    RTS         ;all done!
;
; -----------------
; Show level number
; -----------------
;
SHOLVL LDY #$50 ;use color 2
    STY SHCOLR  ;save it
    LDA LEVEL   ;get level #
    LDX #11     ;12th char on line
;
; -----------------
; Show 2 BCD digits
; -----------------
;
SHOBCD STA SHOBYT ;save digits
    AND #$0F    ;get lower digit
    ORA SHCOLR  ;add color
    STA SCOLIN+1,X ;show it
    LDA SHOBYT  ;get both again
    LSR A       ;mask...
    LSR A       ;off...
    LSR A       ;upper...
    LSR A       ;digit
    ORA SHCOLR  ;add color
    STA SCOLIN,X ;show it!
    RTS         ;and exit.
;
; Zero out player/missile area
;
CLEAR_PM
    LDX #0      ;now clear P/m
CLPM STA MISL,X ;clear missiles
    STA PLR0,X  ;clear plyr 0
    STA PLR1,X  ;clear plyr 1
    STA PLR2,X  ;clear plyr 2
    STA PLR3,X  ;clear plyr 3
    DEX         ;done 256 bytes?
    BNE CLPM    ;no, loop back!
	RTS
;
; -------------------------------
; Calibrate the touch screen!
; -------------------------------
CALIBRATE
    LDA #0      ;get zero
    STA ATRACT  ;attract mode
	LDA CALIBRATED		; Have we already done the calibration?
	BEQ DO_CALIBRATE	; Nope!
	RTS					; No need to do it again!
DO_CALIBRATE
    LDA # <CALDL ;Point to the
    STA SDLSTL  ;game display
    LDA # >CALDL ;list to show
    STA SDLSTL+1 ;the playfield.
    Pause_Jiffies 10
; Wait for touchscreen button clear
	JSR WAIT_FOR_NO_TOUCH
; Put up a touchscreen target at upper left
	lda #MINCURSOR_X
	sta CURX
	lda #MINCURSOR_Y
	sta CURY
	JSR DRAWCURSOR
WaitCalX1Y1
	JSR WAIT_FOR_TOUCH
	LDA TOUCH_ABORT
	BEQ NO_CAL_ABORT1
	RTS
;store cal target 1
NO_CAL_ABORT1
	lda PADDL0
	cmp #80			; This had better be less than 80 in X and Y -- if not, error and retry
	bcc X1_OK
Bad_Coord1_Warning
	jsr Bad_Coord_Sound
	jmp	WaitCalX1Y1
X1_OK
	sta CAL_MINX
	lda PADDL1
	cmp #80			; This had better be less than 80 in X and Y -- if not, error and retry
	bcs Bad_Coord1_Warning
	sta CAL_MINY
	JSR WAIT_FOR_NO_TOUCH
	JSR CLEAR_CURSOR
; Put up a touchscreen target at lower right
	lda #MAXCURSOR_X
	sta CURX
	lda #MAXCURSOR_Y
	sta CURY
	JSR DRAWCURSOR
WaitCalX2Y2
	JSR WAIT_FOR_TOUCH
	LDA TOUCH_ABORT
	BEQ NO_CAL_ABORT2
	RTS
;store cal target 2
NO_CAL_ABORT2
	LDA PADDL0
	cmp #150			; This had better be greater than 150 in X and Y -- if not, error and retry
	bcs X2_OK
Bad_Coord2_Warning
	jsr Bad_Coord_Sound
	jmp	WaitCalX2Y2
X2_OK
	sta CAL_MAXX
	LDA PADDL1
	cmp #150			; This had better be greater than 150 in X and Y -- if not, error and retry
	bcc Bad_Coord2_Warning
	sta CAL_MAXY
	JSR WAIT_FOR_NO_TOUCH
	JSR CLEAR_CURSOR
; Compute calibration metrics!
; First the X axis...
	LDA CAL_MAXX
	SEC
	SBC CAL_MINX
	CLC
	ADC #1
	STA CAL_XRANGE	; Live area X range
	Do_DivideIM CURSOR_X_RANGE,CAL_XRANGE
	LDA DIVRESULTH
	STA CAL_XMULT+1
	LDA DIVRESULTL
	STA CAL_XMULT
; Now the Y axis...
	LDA CAL_MAXY
	SEC
	SBC CAL_MINY
	CLC
	ADC #1
	STA CAL_YRANGE	; Live area Y range
	Do_DivideIM CURSOR_Y_RANGE,CAL_YRANGE
	LDA DIVRESULTH
	STA CAL_YMULT+1
	LDA DIVRESULTL
	STA CAL_YMULT

	; Calibration complete!  Set a flag so we don't do it again this session

	LDA #1
	STA CALIBRATED	; Only need to do this 1 time
	RTS
;
; -------------------
; KOALA PAD interface
; -------------------
;
; The following filtering
; algorithm is used:
;
; Given 5 points S1,S2,S3,S4,S5
;
; R1=S1+S2+S2+S3
; R2=S2+S3+S3+S4
; R3=S3+S4+S4+S5
;
; AVG=(R1+R2+R2+R3)/16
;
; This reduces to:
;
; AVG=(S1+S2*4+S3*6+S4*4+S5)/16
;
; ---------------------------
; Rotate points through queue
; ---------------------------
;
KOALA LDX #4    ;do 5 bytes
ROT LDA XQ-1,X  ;move X queue
    STA XQ,X    ;up one byte
    LDA YQ-1,X  ;move Y queue
    STA YQ,X    ;up one byte
    DEX         ;dec count
    BNE ROT     ;done? No.
;
; --------------------
; Clear out the cursor
; --------------------
;
	JSR CLEAR_CURSOR
;
; ---------------------------
; Insert new point into queue
; ---------------------------
;
    LDA #1      ;pen up flag
    STA PENFLG  ;set pen up
    LDA PADDL0  ;X input
    STA XQ      ;put in queue
    CMP #5      ;screen boundary
    BCC KOALAX  ;on screen? No.
    LDA PADDL1  ;Y input
    STA YQ      ;put in queue
    CMP #5      ;screen boundary
    BCC KOALAX  ;on screen? No.
;
; ---------------------
; Filter the X-Y queues
; ---------------------
;
    LDA # <XQ   ;queue addr low
    STA PTR     ;pointer low
    LDA # >XQ   ;queue addr high
    STA PTR+1   ;pointer high
    JSR FILTER  ;filter X data
    BCS KOALAX  ;good data? No.
    ADC #16     ;X offset
    CMP #48     ;far left?
    BCS FLF     ;No. skip
    LDA #48     ;screen left
FLF CMP #208    ;far right?
    BCC FRT     ;No. skip
    LDA #207    ;screen right
FRT STA CURX    ;put X coord
    LDA # <YQ   ;queue addr low
    STA PTR     ;pointer low
    LDA # >YQ   ;queue addr high
    STA PTR+1   ;pointer high
    JSR FILTER  ;filter Y data
    BCS KOALAX  ;good data? No.
    ADC #16     ;Y offset
    CMP #32     ;above top?
    BCS FUP     ;No. skip
    LDA #32     ;screen top
FUP CMP #224    ;below bottom?
    BCC FDN     ;No. skip
    LDA #223    ;screen bottom
FDN STA CURY    ;put Y coord
;
; ----------------------
; Paddle trigger handler
; ----------------------
;
    LDA PTRIG0  ;paddle trig 0
    EOR PTRIG1  ;EOR w/PTRIG1
    EOR #1      ;inverse data
    STA TRIGGER  ;put in TRIGGER
    LDA #0      ;pen down flag
    STA PENFLG  ;set pen down
KOALAX RTS      ;continue
;
; Erase the cursor in Missile memory
;
CLEAR_CURSOR
	LDA CURSOR_UP	; Is the cursor drawn?
	BEQ CC_DONE		; No! Do nothing
	LDY CURY    ;get Y coord
    LDX #5      ;do 6 bytes
CCURS LDA MISL-3,Y ;get missiles
    AND #$0F    ;mask off low
    STA MISL-3,Y  ;put back
    INY
    DEX         ;dec count
    BPL CCURS   ;done? No.
    LDA #0		; Indicate that there is...
    STA CURSOR_UP	; no cursor!
CC_DONE	RTS
;
; ----------------------------
; Filter algorithm, initialize
; ----------------------------
;
FILTER LDA #0   ;get zero
    LDX #4      ;do 5 bytes
FILC STA SH,X   ;high byte table
    DEX         ;dec count
    BPL FILC    ;done? No.
    STA AVG     ;average low
    STA AVG+1   ;average high
    TAY         ;xero in Y
    LDX #1      ;one in X
;
; -----------------------
; Process the X-Y samples
; -----------------------
;
    LDA (PTR),Y ;get S1
    STA SL,Y    ;save low byte
    INY         ;inc pointer
    JSR MUL4    ;process S2
    LDA (PTR),Y ;get S3
    ASL A       ;times 2
    ROL SH,X    ;rotate carry
    ADC (PTR),Y ;add = times 3
    BCC FIL2    ;overflow? No.
    INC SH,X    ;inc high byte
FIL2 ASL A      ;times 6
    ROL SH,X    ;rotate carry
    STA SL,X    ;save low byte
    INX         ;inc pointer
    INY         ;inc pointer
    JSR MUL4    ;process S4
    LDA (PTR),Y ;get S5
    STA SL,Y    ;save low byte
;
; -------------
; Total samples
; -------------
;
    LDX #4      ;add 5 elements
ALOOP LDA SL,X  ;get low byte
    ADC AVG     ;add to average
    STA AVG     ;save low byte
    LDA SH,X    ;get high byte
    ADC AVG+1   ;add to average
    STA AVG+1   ;save high byte
    DEX         ;dec pointer
    BPL ALOOP   ;done? No.
;
; ------------------
; Divide total by 16
; ------------------
;
    LDX #4      ;shift 4 bits
    LDA AVG     ;get lo byte
DIV16 LSR AVG+1 ;rotate high
    ROR A       ;rotate low
    DEX         ;dec count
    BNE DIV16   ;done? No.
    TAX         ;save Acc
;
; --------------------------
; Compare average with DELTA
; --------------------------
;
    LDY #4      ;5 byte table
MEAN SEC        ;set carry
    SBC (PTR),Y ;compare points
    BCS POSI    ;negative? No.
    EOR #$FF    ;negate byte and
    ADC #1      ;+1 = ABS value
POSI CMP #24    ;within DELTA?
    BCS FAIL    ;No. abort
    TXA         ;get Acc again
    DEY         ;dec pointer
    BPL MEAN    ;done? No.
FAIL RTS        ;exit
;
; ----------------
; Multply Acc by 4
; ----------------
;
MUL4 LDA (PTR),Y ;get S2
    ASL A       ;times 2
    ROL SH,X    ;rotate carry
    ASL A       ;times 4
    ROL SH,X    ;rotate carry
    STA SL,X    ;save low byte
    INX         ;inc pointer
    INY         ;inc pointer
    RTS         ;return
DISPLAY12 Display_2_Hex_Digits SCOLIN
	RTS
DISPLAY34 Display_2_Hex_Digits SCOLIN+2
	RTS
DISPLAY56 Display_2_Hex_Digits SCOLIN+4
	RTS
DDIG *=*+1
HEXDIGITS .SBYTE "0123456789ABCDEF"
;
; Touchscreen interface
;
TOUCH LDA PTRIG0  ;paddle trig 0
    STA TRIGGER  ;put in TRIGGER; 0 = pressed
    BEQ TOUCHFIRE
    RTS
TOUCHFIRE
	JSR CLEAR_CURSOR
	LDA PADDL0	; Get raw touch
	CMP CAL_MINX
	BCS NOXMINCLAMP
	LDA CAL_MINX	; Clamp to calibration min
	jmp TOUCHXCALC
NOXMINCLAMP	CMP CAL_MAXX
	BCC TOUCHXCALC
	LDA CAL_MAXX	; Clamp to calibration max
TOUCHXCALC
	SEC			; And subtract...
	SBC CAL_MINX	; calibration point minimum x
	STA MULTIPLIER
	Do_Multiply_89_H MULTIPLIER, CAL_XMULT
	CLC
	ADC #MINCURSOR_X
    STA CURX
; Now the Y coord
    LDA PADDL1
	CMP CAL_MINY
	BCS NOYMINCLAMP
	LDA CAL_MINY	; Clamp to calibration min
	jmp TOUCHYCALC
NOYMINCLAMP	CMP CAL_MAXY
	BCC TOUCHYCALC
	LDA CAL_MAXY	; Clamp to calibration max
TOUCHYCALC
	SEC			; And subtract...
	SBC CAL_MINY	; calibration point minimum y
	STA MULTIPLIER
    Do_Multiply_89_H MULTIPLIER, CAL_YMULT
	CLC
    ADC #MINCURSOR_Y
    STA CURY
    JSR DRAWCURSOR
	RTS
    
; wait for no touch
WAIT_FOR_NO_TOUCH LDA PTRIG0
	BEQ WAIT_FOR_NO_TOUCH
	LDA #0
	STA TRIGGER					; Also flush main trigger flag!
	RTS

; wait for touch
WAIT_FOR_TOUCH
	LDA #0				; Clear abort flag (indicates cancel via pressing a console key)
	STA TOUCH_ABORT
WTOUCH
	LDA PTRIG0			; Got a touch?
	BEQ GOT_TOUCH		; Yes!
	LDA	CONSOL			; Got a...
	CMP #7				; console keypress?
	BEQ	WTOUCH			; No!
	LDA #1				; Indicate..
	STA TOUCH_ABORT		; an abort!
	JSR	CLEAR_PM	; Zero player/missile area
GOT_TOUCH
	RTS
	
;
; Check for bonus life
;
BONUSLIFE LDA SCORE+1	;get 2nd score digit set
    LSR A         ;Shift...
    LSR A         ;right...
    LSR A        ;4...
    LSR A        ;bits
    AND #$0F    ;mask off lower 4 bits (thousands digit)
    CMP NXTBONUSLIFE ;reached bonus life yet?
    BEQ DOBONUSLIFE  ;yes!
    RTS         ;no bonus, return
DOBONUSLIFE SED	;Go to decimal mode
    CLC         ;increment...
    ADC #INITLIFEBONUS ;to the next bonus life
    AND #$0F    ;and...
    STA NXTBONUSLIFE ;store it back
    CLD         ;exit decimal mode
    LDX LIVES   ;get life count
    CPX #5      ;got 6 already?
    BEQ NOMORELIVES ;no more, pal!
    INX         ;add 1 life
    STX LIVES   ;store it back
	LDA #LIFECHARACTER
    STA SCOLIN+14,X ;plot life
    LDA #24      ;SET SOUND FLAG for 24 samples
    STA BONUSLIFESOUND
NOMORELIVES RTS
;
; Check for terraforming bonus
;
BONUSPLANET LDA SCORE+1	;get 2nd score digit set
    LSR A         ;Shift...
    LSR A         ;right...
    LSR A         ;4...
    LSR A         ;bits
    AND #$0F    ;mask off lower 4 bits (thousands digit)
    CMP NXTBONUSPLANET ;reached terraforming bonus yet?
    BEQ DOBONUSPLANET  ;yes!
    RTS         ;no bonus, return
DOBONUSPLANET SED	;Go to decimal mode
    CLC         ;increment...
    ADC #INITPLANETBONUS ;to the next bonus life
    AND #$0F    ;and...
    STA NXTBONUSPLANET ;store it back
    CLD         ;exit decimal mode
    LDA #1
    STA TERRAFORM ;set terraforming flag for next level change!
    RTS
;
; Do terraforming if the flag is set
;
MAYBETERRAFORM LDA TERRAFORM	;got a terraforming bonus?
	BNE	DOTERRAFORM				;yes! go do it!
	RTS							;nope.  return.
DOTERRAFORM LDA #0
	STA TERRAFORM				;reset terraforming bonus flag
	LDA #1
	STA TERRAFORMING_IN_PROGRESS
	LDA #250
	STA TIMER1
	SetTerraformingTitle
;Do the terraforming here!
;Test for damage to the planet.  Progress from 25% to 75%
;and if that level is damaged, repair to the next level
	LDA #PLANET25
	STA REPAIRTESTLEVEL
REPAIRLOOP CheckPlanetVar REPAIRTESTLEVEL	;test for damage
	CMP #1						;any damage found?
	BNE NEXTDAMAGETEST			;no damage!			
	JSR STARTREPAIRCYCLE		;start repair cycle
	DrawPlanetVar REPAIRTESTLEVEL,2,TRUE		;repair the damage w/color2
RCWAIT LDA REPAIRCYCLE			;repair cycle done?
	CMP #3
	BNE RCWAIT					;no, loop back!
	DrawPlanetVar REPAIRTESTLEVEL,1,FALSE	;Redraw in normal planet color
	LDA #0
	STA REPAIRCYCLE
	JMP REPAIRDONE
NEXTDAMAGETEST DEC REPAIRTESTLEVEL	;go to the next repair level
	BPL REPAIRLOOP				;if >0, loop back
REPAIRDONE LDA TIMER1
    BNE REPAIRDONE
    SetNormalTitle
	RTS							;all done

STARTREPAIRCYCLE LDA #0
	STA REPAIRLEVEL
	LDA #REPAIRCYCLE_START
	STA REPAIRCYCLE
	LDA #0						;Set color 2 to black
	STA COLPF0+1				;Vblank will ramp it up during terraforming
	RTS
	
Bad_Coord_Sound
    LDA #24      ;SET SOUND FLAG for 24 samples
    STA BADCOORDSOUND
	RTS
;
; ------------------------------
; Planet-drawing subroutine
;  INDEX = SOURCE DATA
;  INDX1 = DEST BYTE ON SCREEN
;  Uses INDX2
; ------------------------------
;
SOURCEINDEX .BYTE 0
DESTINDEX .BYTE 0
PLANETSIZE .BYTE 0
PLANETCOLOR .BYTE 0
PLANETBGONLY .BYTE 0
PIXELMASK .BYTE 0
PLANETBYTE .BYTE 0
DRAW_PLANET LDA PLANETSIZE
	CMP #PLANETFULL
	BNE DPS2
	MakePointer PLANET100DATA,INDEX
	JMP	PDRAWGO
DPS2 CMP #PLANET75
	BNE DPS3
	MakePointer PLANET75DATA,INDEX
	JMP	PDRAWGO
DPS3 CMP #PLANET50
	BNE DPS4
	MakePointer PLANET50DATA,INDEX
	JMP	PDRAWGO
DPS4	MakePointer PLANET25DATA,INDEX
PDRAWGO LDA INDX1   ;pointer #1 low copy...
    STA INDX2   ;pointer #2 low
    LDA INDX1+1 ;pointer #1 high copy...
    STA INDX2+1 ;pointer #2 high
    LDA #0      ;table pointer
    STA SOURCEINDEX
DP0 LDA PLANETSIZE      ;index pointer
	STA DESTINDEX
DP1 LDY SOURCEINDEX
	LDA (INDEX),Y ;table value
    BNE DP2     ;done? No.
    RTS
DP2 BMI DPRPT   ;repeat? Yes.
;Not a repeat byte -- this is a color-1 byte, so
;to get the desired color, shift left 1 bit, OR with
;itself and AND with the GR 7 color bits table
	STA PIXELMASK		;store bits
	ASL A			;shift left 1 bit
	ORA PIXELMASK		;OR with original bits to make 2 bits per pixel
	STA PIXELMASK		;save back the full mask
	LDY PLANETCOLOR		;get the color index
	AND GR7COLORS,y		;mask off the pixels
	LDY PLANETBGONLY	;Overwrite bg only?
	STA PLANETBYTE		;save it
	BEQ PWRITE1			;nope -- write anyway
;BG only! Get the dest byte, OR with its left-shift, XOR that and
;AND it with the byte we're writing to mask off
	LDY DESTINDEX		;get the...
	LDA (INDX1),Y		;destination byte (color 1 pixels)
	ASL A				;shift it left 1
	ORA (INDX1),Y		;OR with itself 
	EOR #$FF			;invert into a mask
	AND PLANETBYTE		;mask it off
	ORA (INDX1),Y		;OR with destination
	STA (INDX1),Y		;write the first byte
	LDA (INDX2),Y		;destination byte (color 1 pixels)
	ASL A				;shift it left 1
	ORA (INDX2),Y		;OR with itself 
	EOR #$FF			;invert into a mask
	AND PLANETBYTE		;mask it off
	ORA (INDX2),Y		;OR with the destination
	STA (INDX2),Y		;onto screen
	JMP RPINC
PWRITE1 LDY DESTINDEX	;get destination index
    LDA PIXELMASK
    EOR #$FF
    STA PIXELMASK
    AND (INDX1),Y	;put values
    ORA PLANETBYTE
    STA (INDX1),Y
    LDA PIXELMASK
    AND (INDX2),Y
    ORA PLANETBYTE
    STA (INDX2),Y ;onto screen
RPINC INC DESTINDEX ;inc index pntr
    INC SOURCEINDEX ;inc table pntr
JDP1 JMP DP1     ;continue
;
; -------------------
; Repeat Byte Handler
; -------------------
;
DPRPT ASL A     ;shift byte
    STA TEMP    ;new line flag
    ASL A       ;NL bit -> carry
    ASL A       ;color -> carry
    LDY PLANETCOLOR
    LDA GR7COLORS,Y ;load appropriate color bits
    BCS FILL1   ;color 1? Yes.
    LDA #0      ;get background
FILL1 STA PLANETBYTE ;save color byte
	LDY SOURCEINDEX
    LDA (INDEX),Y ;table value
    AND #$0F    ;mask 4 bits
    STA COUNT   ;save as count
    LDA PLANETBGONLY
    BEQ FILL2
;Draw only on bgnd pixels
FILLBGONLY LDY DESTINDEX		;get the...
	LDA (INDX1),Y		;destination byte (color 1 pixels)
	ASL A				;shift it left 1
	ORA (INDX1),Y		;OR with itself 
	EOR #$FF			;invert into a mask
	AND PLANETBYTE		;mask it off
	ORA (INDX1),Y		;OR with destination
	STA (INDX1),Y		;write the first byte
	LDA (INDX2),Y		;destination byte (color 1 pixels)
	ASL A				;shift it left 1
	ORA (INDX2),Y		;OR with itself 
	EOR #$FF			;invert into a mask
	AND PLANETBYTE		;mask it off
	ORA (INDX2),Y		;OR with destination
	STA (INDX2),Y		;onto screen
    INC DESTINDEX		;inc index
    DEC COUNT			;dec byte count
    BNE FILLBGONLY		;done? No.
    JMP RPTDONE
FILL2 LDA PLANETBYTE
	BEQ NOPLANETDATA
	LDY DESTINDEX
	STA (INDX1),Y ;put bytes
	STA (INDX2),Y ;onto screen
NOPLANETDATA INC DESTINDEX ;inc index
    DEC COUNT   ;dec byte count
    BNE FILL2   ;done? No.
RPTDONE INC SOURCEINDEX ;inc table index
    LDA TEMP    ;get flag
    BPL JDP1    ;new line? No.
    SEC         ;set carry
    LDA INDX1   ;Yes. get low
    SBC #40     ;subtract 40
    STA INDX1   ;new low
    BCS DPN1    ;overflow? No.
    DEC INDX1+1 ;decrement high
DPN1 CLC        ;clear carry
    LDA INDX2   ;get low
    ADC #40     ;add 40
    STA INDX2   ;new low
    BCC JDP0     ;overflow? No.
    INC INDX2+1 ;increment high
JDP0 JMP DP0     ;continue

;
; Check planet for damage
;
CHECK_PLANET LDA PLANETSIZE
	CMP #PLANETFULL
	BNE CPS2
	MakePointer PLANET100DATA,INDEX
	JMP	PCHECKGO
CPS2 CMP #PLANET75
	BNE CPS3
	MakePointer PLANET75DATA,INDEX
	JMP	PCHECKGO
CPS3 CMP #PLANET50
	BNE CPS4
	MakePointer PLANET50DATA,INDEX
	JMP	PCHECKGO
CPS4	MakePointer PLANET25DATA,INDEX
PCHECKGO LDA INDX1   ;pointer #1 low copy...
    STA INDX2   ;pointer #2 low
    LDA INDX1+1 ;pointer #1 high copy...
    STA INDX2+1 ;pointer #2 high
    LDA #0      ;table pointer
    STA SOURCEINDEX
CP0 LDA PLANETSIZE      ;index pointer
	STA DESTINDEX
CP1 LDY SOURCEINDEX
	LDA (INDEX),Y ;table value
    BNE CP2     ;done? No.
    ;if we reach here, there was no damage found!
    LDA #FALSE
    RTS
CP2 BMI CPRPT   ;repeat? Yes.
;Not a repeat byte -- this is a color-1 byte, so
;to check the planet for damage, just AND it with the dest
;and compare it -- if not the same, there is damage!
	STA PIXELMASK		;store bits
	LDY DESTINDEX		;get the...
	AND (INDX1),Y		;destination byte (color 1 pixels)
	CMP PIXELMASK
	BNE DAMAGED
	LDA PIXELMASK
	AND (INDX2),Y		;destination byte (color 1 pixels)
	CMP PIXELMASK
	BEQ CRPINC
DAMAGED LDA #TRUE
	RTS
CRPINC INC DESTINDEX ;inc index pntr
    INC SOURCEINDEX ;inc table pntr
JCP1 JMP CP1     ;continue
;
; -------------------
; Repeat Byte Handler
; -------------------
;
CPRPT ASL A     ;shift byte
    STA TEMP    ;new line flag
    ASL A       ;NL bit -> carry
    ASL A       ;color -> carry
    LDA #$55	;4 color1 pixels 
    BCS CPFILL1  ;color1? Yes.
    LDA #0
CPFILL1 STA PLANETBYTE ;save color byte
	LDY SOURCEINDEX
    LDA (INDEX),Y ;table value
    AND #$0F    ;mask 4 bits
    STA COUNT   ;save as count
CPFILL2	LDA PLANETBYTE
	BEQ CPNOPLANETDATA
	LDY DESTINDEX		;get the...
	AND (INDX1),Y		;destination byte (color 1 pixels)
	CMP PLANETBYTE
	BNE DAMAGED
	LDA PLANETBYTE
	AND (INDX2),Y		;destination byte (color 1 pixels)
	CMP PLANETBYTE
	BNE DAMAGED
CPNOPLANETDATA INC DESTINDEX		;inc index
    DEC COUNT			;dec byte count
    BNE CPFILL2		;done? No.
CPRPTDONE INC SOURCEINDEX ;inc table index
    LDA TEMP    ;get flag
    BPL JCP1    ;new line? No.
    SEC         ;set carry
    LDA INDX1   ;Yes. get low
    SBC #40     ;subtract 40
    STA INDX1   ;new low
    BCS CPDPN1    ;overflow? No.
    DEC INDX1+1 ;decrement high
CPDPN1 CLC        ;clear carry
    LDA INDX2   ;get low
    ADC #40     ;add 40
    STA INDX2   ;new low
    BCC JCP0     ;overflow? No.
    INC INDX2+1 ;increment high
JCP0 JMP CP0     ;continue

;
; Get user name if he wants to report score to browser
;
GET_USER_NAME
	;Copy user name we have into the leaderboard report display list
	ldx #7
PLAYER_NAME_COPY_IN
	lda PLAYER_NAME,X
	clc
	adc #$80-32
	STA PNAMESCREEN,X
	DEX
	BPL PLAYER_NAME_COPY_IN
	
	;Now point to the leaderboard report display list
    LDA # <LBRDL
    STA SDLSTL
    LDA # >LBRDL
    STA SDLSTL+1
    
    ;Now monitor keyboard for appropriate keypresses
RESET_KEYBOARD
    lda #$FF
    STA CH		; Reset keyboard
READ_KEYBOARD
	LDA CH
	CMP #$FF
	BEQ READ_KEYBOARD
	CMP #KEY_ENTER
	BNE NOT_ENTER_KEY
J_SUBMIT_SCORE JMP	SUBMIT_SCORE
NOT_ENTER_KEY CMP #KEY_ESCAPE
	BNE NOT_ESCAPE
	JMP DONT_SUBMIT
NOT_ESCAPE CMP #KEY_BACKSPACE
	BNE NOT_BACKSPACE
; Delete the last character
	LDX PLAYERNAMEINDEX		; Get insertion point
	BEQ RESET_KEYBOARD		; If already zero, don't do anything
	DEX						;Decrement the index
	STX PLAYERNAMEINDEX		;Save it back
	LDA #'_					;Get the underscore character
	STA PLAYER_NAME,X		;Stuff it into the player name
	JMP GET_USER_NAME		;And go loop!
NOT_BACKSPACE
;	pha	;***********************TEST****************************
;	Store2Digits CH,PNMSG,SCREENHEXCHARS	; Display character hex code on screen
;	pla
	LOOKUP_KEYBOARD_CHAR ALPHA_KEYS,26,65,$40	; Jumps to USE_KEY if key found
	LOOKUP_KEYBOARD_CHAR NUMERIC_KEYS,10,48,0	; Ditto
	lda PLAYERNAMEINDEX	; First character?
	beq RSKB			; If it's not an alphanumeric, punt!
	CMP #KEY_DASH	; Space is the only special char allowed in names
	BNE RSKB		; Not a dash!
	LDA #'-			; Load up the equivalent ASCII code
	JMP USE_KEY		; And go use it
RSKB JMP RESET_KEYBOARD	; Reset keyboard and wait for more!
;
; At this point, the accumulator contains the ASCII code for the character that was typed.
; Place it into the player name field, advance the insert point by 1 character, and loop
;
USE_KEY
	LDX PLAYERNAMEINDEX
	CPX #8				; At end?
	BEQ NOCURSORINC		; No character!  All 8 used!
	STA PLAYER_NAME,X
	INC PLAYERNAMEINDEX
NOCURSORINC
	JMP GET_USER_NAME
SUBMIT_SCORE
	LDA PLAYERNAMEINDEX	; If no characters entered...
	BEQ DONT_SUBMIT		; don't submit!
    JSR BUILDURL		; Build the report URL
	BEQ DONT_SUBMIT		; If URL builder returns zero, it's a blank player name or something else wrong
    JSR URL_TO_BROWSER_DEVICE	; Report score
    LDA #1				; Note that it's been reported!
    STA REPORTED
DONT_SUBMIT
	; Return to main game display list
    LDA # <GLIST ;Point to the
    STA SDLSTL  ;game display
    LDA # >GLIST ;list to show
    STA SDLSTL+1 ;the playfield.	
	RTS

;
; Build game score report URL
;

BUILDURL
; If name is blank, punt!
	lda PLAYER_NAME
	cmp #'_
	bne NOT_BLANK
	lda #0				; Indicate URL build failure
	rts
; Put name into name portion of URL
NOT_BLANK
	LDX #0
URLPLOOP
	LDA PLAYER_NAME,X
	CMP #'_				; Is the character an underscore?
	BNE PNCHAROK		; Nope, it's OK
	LDA #'-				; Make it a dash for the URL
PNCHAROK
	STA URLNAME,X
	INX
	CPX #8
	BNE URLPLOOP
; Put level number into URL
	Store2Digits LEVEL,URLLEVEL,ASCIIHEXCHARS
; Put score into URL
	Store2Digits SCORE+2,URLSCORE+4,ASCIIHEXCHARS
	Store2Digits SCORE+1,URLSCORE+2,ASCIIHEXCHARS
	Store2Digits SCORE,URLSCORE,ASCIIHEXCHARS
; Now create the URL key!
	ExecutePCode URLKEY0
	ExecutePCode URLKEY1
	ExecutePCode URLKEY2
	ExecutePCode URLKEY3
	ExecutePCode URLKEY4
	ExecutePCode URLKEY5
	ExecutePCode URLKEY6
	ExecutePCode URLKEY7
	ExecutePCode URLKEY8
	ExecutePCode URLKEY9
	ExecutePCode URLKEY10
	ExecutePCode URLKEY11
	ExecutePCode URLKEY12
	ExecutePCode URLKEY13
	ExecutePCode URLKEY14
	ExecutePCode URLKEY15
	ExecutePCode URLKEY16
	ExecutePCode URLKEY17
	ExecutePCode URLKEY18
	ExecutePCode URLKEY19
	ExecutePCode URLKEY20
	ExecutePCode URLKEY21
	ExecutePCode URLKEY22
	ExecutePCode URLKEY23
	ExecutePCode URLKEY24
	ExecutePCode URLKEY25
	ExecutePCode URLKEY26
	ExecutePCode URLKEY27
	ExecutePCode URLKEY28
	ExecutePCode URLKEY29
	ExecutePCode URLKEY30
	ExecutePCode URLKEY31
	lda #1					; Indicate URL success!
	RTS
	
;
; Open the B: device.  Return status code in accumulator
;
OPEN_BROWSER_DEVICE
	LDX #$10
	LDA #OPEN
	STA ICCOM,X
	LDA #<DEVICENAME
	STA ICBAL,X
	LDA #>DEVICENAME
	STA ICBAH,X
	LDA #0      ;LENGTH OF MSG
	STA ICBLH,X ; HIGH BYTE
	LDA #3    ;3 CHAR LENGTH
	STA ICBLL,X ; LOW BYTE
	LDA #OWRITE
	STA ICAX1,X
	LDA #0
	STA ICAX2,X
	JSR CIOV
	TYA				; Move status code to accum
	RTS

CLOSE_BROWSER_DEVICE
	LDX #$10    ;IOCB 1
	LDA #CLOSE ;WANT CLOSE
	STA ICCOM,X ;ISSUE CMD
	JSR CIOV
	RTS	

; print the URL to the browser
URL_TO_BROWSER_DEVICE
	JSR OPEN_BROWSER_DEVICE	; Try opening browser
	BPL U2B_GO				; Opened OK
	RTS						; ERROR - return
U2B_GO LDX #$10    ;IOCB 1
	LDA #PUTREC ;WANT OUTPUT
	STA ICCOM,X ;ISSUE CMD
	LDA #<URLSTRING ;LOW BYTE OF MSG
	STA ICBAL,X ; INTO ICBAL
	LDA #>URLSTRING ;HIGH BYTE
	STA ICBAH,X ; INTO ICBAH
	LDA #0      ;LENGTH OF MSG
	STA ICBLH,X ; HIGH BYTE
	LDA #255    ;let EOL terminate it
	STA ICBLL,X ; LOW BYTE
	JSR CIOV    ;CALL CIO
; close the B device
	JSR CLOSE_BROWSER_DEVICE
	RTS
;
; B: "Browser" device name
DEVICENAME .BYTE "B:",EOL

;
; Hex chars in both flavors
SCREENHEXCHARS .SBYTE "0123456789ABCDEF"
ASCIIHEXCHARS .BYTE "0123456789ABCDEF"

; ***********************************************************************************
; Divide routine
;
; Divide a 32-bit dividend in N+3(H), N+2, N+5, N+4(L) by divisor in N+1(H), N+0(L)
;
; result is in N+5(H) and N+4(L), remainder in N+3(H) and N+2(L)
;
; N & N+1 could be called "DIVISOR" (and DIVISOR+1).  Since the answer ends up
; in the same bytes that originally held the 32-bit dividend, it becomes a
; little harder to come up with good variable names for other bytes of N unless
; you have more than one name for the same locations and remember that they are
; indeed the same locations.
;
; This was originally for my 65C02 Forth kernel, where the input and output is
; through the data stack, which is in ZP and indexed by X.  This shows how you'd
; do the actual division if you moved it to the Forth ZP scratchpad area for
; primitives (ie, code definitions).  This area is often simply called N.  In
; Forth, we don't normally move all this to and from this area for division,
; because the time needed to move it is greater than the time saved by not
; having to index by X; but the division process itself takes a little less
; explaining this way, and you don't need to know anything about Forth to use
; this routine.  Refer to the diagrams above for what is in N to N+6.
; ***********************************************************************************

LONG_DIVIDE  SEC             ; Detect overflow or /0 condition.
        LDA     DIVN+2     ; Divisor must be more than high cell of dividend.  To
        SBC     DIVN       ; find out, subtract divisor from high cell of dividend;
        LDA     DIVN+3     ; if carry flag is still set at the end, the divisor was
        SBC     DIVN+1     ; not big enough to avoid overflow. This also takes care
        BCS     div_oflo   ; of any /0 condition.  Branch if overflow or /0 error.
                        ; We will loop 16 times; but since we shift the dividend
        LDX     #$11    ; over at the same time as shifting the answer in, the
                        ; operation must start AND finish with a shift of the
                        ; low cell of the dividend (which ends up holding the
                        ; quotient), so we start with 17 (11H) in X.
div_loop
		ROL     DIVN+4     ; Move low cell of dividend left one bit, also shifting
        ROL     DIVN+5     ; answer in. The 1st rotation brings in a 0, which later
                        ; gets pushed off the other end in the last rotation.
        DEX
        BEQ     div_end    ; Branch to the end if finished.

        ROL     DIVN+2     ; Shift high cell of dividend left one bit, also
        ROL     DIVN+3     ; shifting next bit in from high bit of low cell.
        LDA		#0
        STA     CARRY   ; Zero old bits of CARRY so subtraction works right.
        ROL     CARRY   ; Store old high bit of dividend in CARRY.  (For STZ
                        ; one line up, NMOS 6502 will need LDA #0, STA CARRY.)
        SEC             ; See if divisor will fit into high 17 bits of dividend
        LDA     DIVN+2     ; by subtracting and then looking at carry flag.
        SBC     DIVN       ; First do low byte.
        STA     DIVN+6     ; Save difference low byte until we know if we need it.
        LDA     DIVN+3     ;
        SBC     DIVN+1     ; Then do high byte.
        TAY             ; Save difference high byte until we know if we need it.
        LDA     CARRY   ; Bit 0 of CARRY serves as 17th bit.
        SBC     #0      ; Complete the subtraction by doing the 17th bit before
        BCC     div_loop    ; determining if the divisor fit into the high 17 bits
                        ; of the dividend.  If so, the carry flag remains set.
        LDA     DIVN+6     ; If divisor fit into dividend high 17 bits, update
        STA     DIVN+2     ; dividend high cell to what it would be after
        STY     DIVN+3     ; subtraction.
        JMP     div_loop    ; Always branch.

 div_oflo
		LDA     #$FF    ; If overflow occurred, put FF
        STA     DIVN+2     ; in remainder low byte
        STA     DIVN+3     ; and high byte,
        STA     DIVN+4     ; and in quotient low byte
        STA     DIVN+5     ; and high byte.
 div_end
		RTS
		
; Multiply routine -- multiply bytes, resulting in a word product

MULTIPLY88  LDA #0 
	STA PRODUCTL			; reset product
	LDX #8					; Do 8 bits
MULTIPLY88LOOP LSR MULTIPLIER	; Shift the multiplier
	BCC NOADD88				; Did a bit come off?  No!
	CLC						; Clear the carry...
	ADC MULTIPLICAND		; And add to the product
NOADD88 ROR A				; Rotate to div by 2
	ROR PRODUCTL			; Rotate any carries into the low-order byte
	DEX						; next bit -- more?
	BNE MULTIPLY88LOOP		; Yes!
	STA PRODUCTH			; No more, store in product H
	RTS						; And exit
		
; Multiply routine -- multiply byte by 16-bit operand, resulting in a word product

MULTIPLY89  LDA #0 
	STA PRODUCTL			; reset product
	LDX #9					; Do 9 bits
MULTIPLY89LOOP LSR MULTIPLIER+1	; Shift the multiplier hi byte
	ROR MULTIPLIER			;Rotate thru the multiplier lo byte
	BCC NOADD89				; Did a bit come off?  No!
	CLC						; Clear the carry...
	ADC MULTIPLICAND		; And add to the product
NOADD89
	DEX
	BEQ MULT89DONE
	ROR A				; Rotate to div by 2
	ROR PRODUCTL			; Rotate any carries into the low-order byte
	JMP MULTIPLY89LOOP
MULT89DONE
	STA PRODUCTH			; No more, store in product H
	RTS						; And exit
		
; ----------
; Data areas
; ----------
;
BMAXS .BYTE 0,250 ;bomb limits
BOMPIC .BYTE 0,0,0,0,0,0,$DC,$3E
    .BYTE $7E,$3E,$DC,0,0,0,0
    .BYTE 0,0,$76,$F8,$FC
    .BYTE $F8,$76,0,0,0,0,0,0
BPSTRT .BYTE 27,16
BXOF .BYTE 47,42
CURPIC .BYTE $40,$40,$A0,$A0
    .BYTE $40,$40
P3COLR .BYTE $34,$F8
SAUPIC .BYTE 0,0,$18,$7E,0,0
    .BYTE $7E,$18,0,0
SAUMID .BYTE $92,$49,$24,$92
STARTX .BYTE 40,$FF,210,$FF
STARTY .BYTE $FF,20,$FF,230
ENDX .BYTE $FF,210,$FF,40
ENDY .BYTE 20,$FF,230,$FF
GR7COLORS .BYTE $00,$55,$AA,$FF
;
; ---------------------
; Explosion data tables
; ---------------------
;
PLOTBL .BYTE $C0,$30,$0C,$03
ERABIT .BYTE $3F,$CF,$F3,$FC
PROMSK .BYTE 0,0,0,0
    .BYTE $FF,$FF,$FF,$FF
    .BYTE $FF,$FF,$AA,$AA
COORD1 .BYTE 0,1,255,0,255,1
    .BYTE 0,2,255,254,0,1
    .BYTE 0,254,2,1,1,255
    .BYTE 0,2,254,255,3,0
    .BYTE 253,254,3,2,255,254
    .BYTE 1,255,3,253,1,253,2
COORD2 .BYTE 0,0,1,255,0,1
    .BYTE 1,0,255,1,2,255
    .BYTE 254,255,1,2,254,2
    .BYTE 3,255,0,254,1,253
    .BYTE 0,254,255,2,3,2
    .BYTE 253,253,0,1,3,255,254
;
; ------------------
; Initial score line
; ------------------
;
SCOINI .BYTE $00,$00,$00,$00
    .BYTE $00,$00,$00,$00
    .BYTE $6C,$76,$6C,$00
    .BYTE $00,$00,LIFECHARACTER,LIFECHARACTER
    .BYTE LIFECHARACTER,LIFECHARACTER,LIFECHARACTER,$00
;
; ------------------
; "Terraforming" line
; ------------------
;
TERRAFORM_TITLE .SBYTE " ** TERRAFORMING ** "
;
; ------------------
; "Game Over" line
; ------------------
;
GAMEOVER_TITLE .SBYTE "     GAME OVER!     "
;
; ------------
; Level tables
; 30 sets + 1 that is repeated > level 30
; ------------
;
;Bombs per level...
INIBOM .BYTE 10,15,20,25,20,25
    .BYTE 10,15,20,25,20,25
    .BYTE 10,15,20,25,20,25
    .BYTE 10,15,20,25,20,25
    .BYTE 10,15,20,25,20,25
    .BYTE 20
;Bomb speeds
INIBS .BYTE 12,11,10,9,8,7
    .BYTE 10,9,8,8,7,6
    .BYTE 9,9,9,8,8,8
    .BYTE 8,8,7,6,5,5
    .BYTE 7,6,5,4,3,3
    .BYTE 3
;Saucer chances
INISC .BYTE 0,10,40,70,80,90
    .BYTE 5,20,40,80,90,100
    .BYTE 10,25,45,75,95,105
    .BYTE 30,40,60,90,110,130
    .BYTE 50,70,90,125,160,180
    .BYTE 200
;Planet color
INIPC .BYTE $20,$30,$40,$50,$60,$70
    .BYTE $80,$90,$A0,$B0,$C0,$D0
    .BYTE $E0,$F0,$00,$10,$20,$30
    .BYTE $40,$50,$60,$70,$80,$90
    .BYTE $A0,$B0,$C0,$D0,$E0,$F0
    .BYTE $FF
    
; Bomb values...
INIBVH .BYTE 0,0,0,0,0,0
    .BYTE 0,0,0,$01,$01,$01
    .BYTE $01,$01,$01,$01,$01,$01
    .BYTE $01,$02,$02,$02,$02,$02
    .BYTE $02,$02,$02,$02,$02,$03
    .BYTE $03

INIBVL .BYTE $10,$20,$30,$40,$50,$60
    .BYTE $70,$80,$90,$00,$10,$20
    .BYTE $30,$40,$50,$60,$70,$80
    .BYTE $90,$00,$10,$20,$30,$40
    .BYTE $50,$60,$70,$80,$90,$00
    .BYTE $50
;Saucer values
INISV .BYTE 0,1,1,1,1,1
    .BYTE 2,2,2,2,2,2
    .BYTE 3,3,3,3,3,3
    .BYTE 4,4,4,4,4,4
    .BYTE 5,5,5,5,5,5
    .BYTE 6
;
; ----------
; Sound data
; ----------
;
PLSHOT .BYTE 244,254,210,220
    .BYTE 176,186,142,152,108
    .BYTE 118,74,84,40,50
ENSHOT .BYTE 101,96,85,80,69,64
    .BYTE 53,48,37,32,21,16,5,0
SAUSND .BYTE 10,11,12,14,16,17
    .BYTE 18,17,16,14,12,11
BONUSSND .BYTE 50,60,70,80,90,100,120,140
    .BYTE 80,90,100,120,140,160,180,200
    .BYTE 120,140,160,180,220,240,250,254
BADCOORDSND .BYTE 240,240,240,240,250,250
	.BYTE 240,240,240,240,250,250
	.BYTE 240,240,240,240,250,250
	.BYTE 240,240,240,240,250,250
;
; ----------------
; Planet Draw Data
; ----------------
; bit7=repeat flag - if zero, whole byte is literal insert
; bit6=newline flag
; bit5=color 0=bg 1=fg
; bits0-3=repeat count
;
; color uses color 1
;
;Full planet:
PLANET100DATA
	.BYTE $A4,$54,$15,$E4
	.BYTE $EA,$EA,$EA
    .BYTE $EA,$15,$A8,$54
    .BYTE $C1,$15,$A8,$54
    .BYTE $C1,$05,$A8,$50
    .BYTE $C1,$05,$A8,$50
    .BYTE $C1,$01,$A8,$40
    .BYTE $C1,$81,$E8,$81
    .BYTE $15,$A6,$54,$C1
    .BYTE $81,$05,$A6,$50
    .BYTE $C1,$81,$01,$A6
    .BYTE $40,$C1,$82,$E6
    .BYTE $82,$05,$A4,$50
    .BYTE $C1,$83,$E4,$84
    .BYTE $E2,0
    
; 75% planet: (offset +1)
PLANET75DATA
	.BYTE $15,$A2,$54,$15,$A2,$54,$C1
	.BYTE $15,$A6,$54,$C1
	.BYTE $15,$A6,$54,$C1
	.BYTE $05,$A6,$50,$C1
	.BYTE $05,$A6,$50,$C1
	.BYTE $05,$A6,$50,$C1
	.BYTE $01,$A6,$40,$C1
	.BYTE $01,$A6,$40,$C1
	.BYTE $81,$E6,
	.BYTE $81,$15,$A4,$54,$C1
	.BYTE $81,$05,$A4,$50,$C1
	.BYTE $81,$01,$A4,$40,$C1
	.BYTE $82,$15,$A2,$54,$C1
	.BYTE $83,$A2,$C1
	.BYTE 0

; 50% Planet: (offset +2)
PLANET50DATA
	.BYTE $05,$A1,$54,$15,$A1,$50,$C1
	.BYTE $05,$A4,$50,$C1
	.BYTE $05,$A4,$50,$C1
	.BYTE $01,$A4,$40,$C1
	.BYTE $01,$A4,$40,$C1
	.BYTE $81,$E4
	.BYTE $81,$15,$A2,$54,$C1
	.BYTE $81,$05,$A2,$50,$C1
	.BYTE $82,$E2
	.BYTE $82,$05,$50,$C1
	.BYTE 0
	
; 25% Planet: (offset +3)
PLANET25DATA
	.BYTE $01,$54,$15,$40,$C1
	.BYTE $01,$A2,$40,$C1
	.BYTE $81,$E2
	.BYTE $81,$E2
	.BYTE $81,$15,$54,$C1
	.BYTE $81,$01,$40,$C1
	.BYTE 0
;
; URL for score reporting
;
; 32-digit KEY made from the following algorithm:
; digit 0: KEYDIGITS[GAMEKEY[KEYRANDOM1] & 31]
; digit 1: KEYDIGITS[GAMEKEY2[(SCOREDIGIT1+KEYRANDOM2) & 31] & 31]
; digit 2: KEYDIGITS[KEYRANDOM1]
; digit 3: KEYDIGITS[GAMEKEY2[(NAMECHAR1+LEVELDIGIT1+KEYRANDOM3) & 31] & 31]
; digit 4: KEYDIGITS[GAMEKEY[(NAMECHAR3+31-KEYRANDOM5) & 31] & 31]
; digit 5: KEYDIGITS[GAMEKEY[SCOREDIGIT1+SCOREDIGIT3+SCOREDIGIT5] & KEYRANDOM4]
; digit 6: KEYDIGITS[GAMEKEY[(NAMECHAR6+31-KEYRANDOM5) & 31] & 31]
; digit 7: KEYDIGITS[KEYRANDOM2]
; digit 8: KEYDIGITS[GAMEKEY[(NAMECHAR2+LEVELDIGIT2+KEYRANDOM1) & 31] & 31]
; digit 9: KEYDIGITS[GAMEKEY2[(SCOREDIGIT2+KEYRANDOM1) & 31] & 31]
; digit 10: KEYDIGITS[GAMEKEY[KEYRANDOM2] & 31]
; digit 11: KEYDIGITS[GAMEKEY2[SCOREDIGIT2+SCOREDIGIT4+SCOREDIGIT6] & 31]
; digit 12: KEYDIGITS[GAMEKEY2[(NAMECHAR3+KEYRANDOM1) & 31] & 31]
; digit 13: KEYDIGITS[KEYRANDOM3]
; digit 14: KEYDIGITS[GAMEKEY2[(NAMECHAR5+KEYRANDOM1) & 31] & 31]
; digit 15: KEYDIGITS[GAMEKEY[(SCOREDIGIT3+KEYRANDOM4) & 31] & 31]
; digit 16: KEYDIGITS[KEYRANDOM4]
; digit 17: KEYDIGITS[GAMEKEY2[SCOREDIGIT1] & 31]
; digit 18: KEYDIGITS[GAMEKEY2[(NAMECHAR7+31-KEYRANDOM5) & 31] & 31]
; digit 19: KEYDIGITS[GAMEKEY[KEYRANDOM3] & 31]
; digit 20: KEYDIGITS[GAMEKEY[SCOREDIGIT1+LEVELDIGIT2] & KEYRANDOM3]
; digit 21: KEYDIGITS[GAMEKEY2[KEYRANDOM5] & 31]
; digit 22: KEYDIGITS[GAMEKEY[(NAMECHAR4+31-KEYRANDOM2) & 31] & 31]
; digit 23: KEYDIGITS[GAMEKEY[(SCOREDIGIT3+KEYRANDOM5) & 31] & 31]
; digit 24: KEYDIGITS[GAMEKEY2[(SCOREDIGIT6+KEYRANDOM2) & 31] & 31]
; digit 25: KEYDIGITS[GAMEKEY[(GAMEKEY[NAMECHAR8 & 31]+KEYRANDOM1) & 31] & 31]
; digit 26: KEYDIGITS[GAMEKEY[KEYRANDOM4] & 31]
; digit 27: KEYDIGITS[GAMEKEY2[(SCOREDIGIT4+KEYRANDOM4) & 31] & 31]
; digit 28: KEYDIGITS[KEYRANDOM5]
; digit 29: KEYDIGITS[GAMEKEY2[(NAMECHAR5+31-KEYRANDOM4) & 31] & 31]
; digit 30: KEYDIGITS[GAMEKEY[(SCOREDIGIT5+KEYRANDOM3) & 31] & 31]
; digit 31: KEYDIGITS[GAMEKEY2[LEVELDIGIT1+SCOREDIGIT3+SCOREDIGIT4] & KEYRANDOM2]
;

URLSTRING .BYTE "http://analog.klanky.com/leaderboards/SaveScore.php?game=PD2012&name="
URLNAME	.BYTE "--------"
	.BYTE "&level="
URLLEVEL .BYTE "00"
	.BYTE "&score="
URLSCORE .BYTE "000000"
	.BYTE "&key="
URLKEY	.BYTE "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
URLEND	.BYTE EOL

;
; Title line pointers
;
GAMETITLEPTR .WORD SCOLIN
TERRAFORMINGTITLEPTR .WORD TERRAFORM_TITLE
GAMEOVERTITLEPTR .WORD GAMEOVER_TITLE
;
; -----------------
; Program variables
; -----------------
;
TRIGGER *= *+1	;Trigger flag - 0 = trigger down
XPOS *= *+20    ;all expl. x's
YPOS *= *+20    ;all expl. y's
CNT *=  *+20    ;all expl. counts
BOMACT *= *+4   ;bomb active flags
PROACT *= *+8   ;proj. active flags
BOMBX *= *+4    ;bomb x positions
PROJX *= *+8    ;proj. x positions
BOMBY *= *+4    ;bomb y positions
PROJY *= *+8    ;proj. y positions
BXINC *= *+4    ;bomb x vectors
PXINC *= *+8    ;proj. x vectors
BYINC *= *+4    ;bomb y vectors
PYINC *= *+8    ;proj. y vectors
BXHOLD *= *+12  ;b/p hold areas
BYHOLD *= *+12  ;b/p hold areas
BOMBLR *= *+4   ;bomb left/right
PROJLR *= *+8   ;proj. left/right
BOMBUD *= *+4   ;bomb up/down
PROJUD *= *+8   ;proj. up/down
SCOLIN *= *+20  ;score line
NXTBONUSLIFE *= *+1 ;next bonus life digit (in 1,000 place) -- initially 5
NXTBONUSPLANET *= *+1 ; next bonus terraforming digit (in 10,000 place) -- init 1
BONUSLIFESOUND *= *+1 ;flag to play bonus life sound
BADCOORDSOUND *=*+1 ; flag to play bad coordinate warning sound
BONUSFREQ *= *+1 ;bonus life sound frequency
BONUSVOL *= *+1 ;bonus life volume
TERRAFORM *= *+1 ;terraforming scheduled for next nevel interval!
TERRAFORMING_IN_PROGRESS *= *+1	;set to 1 when terraforming in progress
TIMER1 *= *+1 ;generic timer
SUPPRESS *= *+1 ;firing suppression
REPAIRTESTLEVEL *= *+1 ;Used in testing for damage to planet
REPAIRCYCLE *= *+1 ;Set to REPAIRCYCLE_START to have DLI run repair cycle
REPAIRLEVEL *= *+1 ;Variable for repair cycle
GAMEOVERTIMER *= *+1 ;timer for "GAME OVER" message flash
GAMEOVERTITLEFLAG *= *+1 ;0=normal score line, 1=GAME OVER line
BOMBS *= *+1    ;bombs to come
BOMTI *= *+1    ;bomb speeds
ACTIVEPROJCOUNT *= *+1	; Count of active player projectiles
CURSOR_UP *= *+1
;
; Scoring website stuff
;
;Unique 64-character game key (only used for game ID of "PD2012")
;Used in two 32-character chunks
GAMEKEY	 .BYTE "7kaUVdr!jbX2091Pmvn#D@*bG57zjqPH"
GAMEKEY2 .BYTE "nyVsik^&ghjKCX_70@0SfcVbTYUxGdfp"
KEYRANDOM1 .BYTE 0
KEYRANDOM2 .BYTE 0
KEYRANDOM3 .BYTE 0
KEYRANDOM4 .BYTE 0
KEYRANDOM5 .BYTE 0
KEYDIGITS .BYTE "0123456789ABCDEFGHIJKLMNOPQRSTUV"	;32 characters
;
; --------------------
; P-Code opcodes
; --------------------
;
PCLOAD = 0
PCADD = 1
PCADD31 = 2
PCAND = 3
PCAND31 = 4
PCSUB = 5
PCMAKEINDEX = 6
PCLOADGAMEKEYWITHINDEX = 7
PCLOADGAMEKEY2WITHINDEX = 8
PCMAKEKEYDIGIT = 9
PCEND = 19
; Operands
KR = 20		;KeyRandom (1-5)
LD = 21		;LevelDigit (1-2)
SD = 22		;ScoreDigit (1-6)
NC = 23		;NameChar (1-8)
;
; --------------------------------------------
; P-Code for constructing score URL key string
; --------------------------------------------
;
URLKEYCODE
URLKEY0	.BYTE PCLOAD,KR,1,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,0,PCEND
URLKEY1	.BYTE PCLOAD,KR,2,PCADD,SD,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,1,PCEND
URLKEY2	.BYTE PCLOAD,KR,1,PCMAKEKEYDIGIT,2,PCEND
URLKEY3	.BYTE PCLOAD,NC,1,PCADD,LD,1,PCADD,KR,3,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,3,PCEND
URLKEY4	.BYTE PCLOAD,NC,3,PCADD31,PCSUB,KR,5,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,4,PCEND
URLKEY5	.BYTE PCLOAD,SD,1,PCADD,SD,3,PCADD,SD,5,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND,KR,4,PCMAKEKEYDIGIT,5,PCEND
URLKEY6	.BYTE PCLOAD,NC,6,PCADD31,PCSUB,KR,5,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,6,PCEND
URLKEY7	.BYTE PCLOAD,KR,2,PCMAKEKEYDIGIT,7,PCEND
URLKEY8	.BYTE PCLOAD,NC,2,PCADD,LD,2,PCADD,KR,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,8,PCEND
URLKEY9	.BYTE PCLOAD,SD,2,PCADD,KR,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,9,PCEND
URLKEY10	.BYTE PCLOAD,KR,2,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,10,PCEND
URLKEY11	.BYTE PCLOAD,SD,2,PCADD,SD,4,PCADD,SD,6,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,11,PCEND
URLKEY12	.BYTE PCLOAD,NC,3,PCADD,KR,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,12,PCEND
URLKEY13	.BYTE PCLOAD,KR,3,PCMAKEKEYDIGIT,13,PCEND
URLKEY14	.BYTE PCLOAD,NC,5,PCADD,KR,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,14,PCEND
URLKEY15	.BYTE PCLOAD,SD,3,PCADD,KR,4,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,15,PCEND
URLKEY16	.BYTE PCLOAD,KR,4,PCMAKEKEYDIGIT,16,PCEND
URLKEY17	.BYTE PCLOAD,SD,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,17,PCEND
URLKEY18	.BYTE PCLOAD,NC,7,PCADD31,PCSUB,KR,5,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,18,PCEND
URLKEY19	.BYTE PCLOAD,KR,3,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,19,PCEND
URLKEY20	.BYTE PCLOAD,SD,1,PCADD,LD,2,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND,KR,3,PCMAKEKEYDIGIT,20,PCEND
URLKEY21	.BYTE PCLOAD,KR,5,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,21,PCEND
URLKEY22	.BYTE PCLOAD,NC,4,PCADD31,PCSUB,KR,2,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,22,PCEND
URLKEY23	.BYTE PCLOAD,SD,3,PCADD,KR,5,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,23,PCEND
URLKEY24	.BYTE PCLOAD,SD,6,PCADD,KR,2,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,24,PCEND
URLKEY25	.BYTE PCLOAD,NC,8,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCADD,KR,1,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,25,PCEND
URLKEY26	.BYTE PCLOAD,KR,4,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,26,PCEND
URLKEY27	.BYTE PCLOAD,SD,4,PCADD,KR,4,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,27,PCEND
URLKEY28	.BYTE PCLOAD,KR,5,PCMAKEKEYDIGIT,28,PCEND
URLKEY29	.BYTE PCLOAD,NC,5,PCADD31,PCSUB,KR,4,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND31,PCMAKEKEYDIGIT,29,PCEND
URLKEY30	.BYTE PCLOAD,SD,5,PCADD,KR,3,PCAND31,PCMAKEINDEX,PCLOADGAMEKEYWITHINDEX,PCAND31,PCMAKEKEYDIGIT,30,PCEND
URLKEY31	.BYTE PCLOAD,LD,1,PCADD,SD,3,PCADD,SD,4,PCAND31,PCMAKEINDEX,PCLOADGAMEKEY2WITHINDEX,PCAND,KR,2,PCMAKEKEYDIGIT,31,PCEND
;
REPORTED .BYTE 0		; Set to 1 if score reported to website for this game
CALIBRATED .BYTE 0		; Set to 1 if touch screen calibrated for this session
TOUCH_ABORT .BYTE 0		; Set to 1 if user aborted calibration via console key
GOT_BROWSER .BYTE 0		; Set to 1 if there is web browser capability
;
; Touch calibration stuff
;
CAL_MINX *=	*+1	;touch calibration left
CAL_MINY *=	*+1	;touch calibration top
CAL_MAXX *=	*+1	;touch calibration right
CAL_MAXY *=	*+1	;touch calibration bottom
CAL_XRANGE *=*+1 ; Touch calibration width
CAL_YRANGE *=*+1 ; Touch calibration height
CAL_XMULT *=*+2	; Touch calibration multiplier (atari w / touch range w)
CAL_YMULT *=*+2	; Touch calibration multiplier (atari h / touch range h)

;
;Keyboard codes
;The Atari keyboard codes really suck.  In no order whatsoever, except that they have a $40 difference
;between upper and lower case alpha (a=$3f, A=$7f).  Our type-in player name only allows upper case alpha
;and numerics and dashes.  These tables supply alphas, numerics and special chars
ALPHA_KEYS	;Lower case versions.  Add $40 for upper case
	.BYTE $3f, $15, $12, $3a, $2a	;a-e
	.BYTE $38, $3d, $39, $0d, $01	;f-j
	.BYTE $05, $00, $25, $23, $08	;k-o
	.BYTE $0a, $2f, $28, $3e, $2d	;p-t
	.BYTE $0b, $10, $2e, $16, $2b	;u-y
	.BYTE $17						;z
NUMERIC_KEYS
	.BYTE $32, $1f, $1e, $1a, $18, $1d, $1b, $33, $35, $30

;SPECIAL_KEYS
KEY_ENTER		= $0c	;Enter
KEY_BACKSPACE	= $34	;Backspace
KEY_ESCAPE		= $1c	;Escape
KEY_DASH		= $0e	;Hyphen

; Keyboard reader stuff...
THEKEYCODE *= *+1		;Holding spot for raw key code
PLAYER_NAME .BYTE "________"	; Up to 8 character player name
PLAYERNAMEINDEX .BYTE 0	;0-7 index into player name
;
; --------------
; End of program
; --------------
;
; Point to entry point for Atari DOS
;
    *=  $02E0
    .WORD PLANET
