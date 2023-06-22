;
; Acornsoft Lisp v4.06
;
; Reassembled by TobyLobster in 2023
;
; Made by disassembling the LISP406 binary with py8dis
; then adding original label names and comments.
;
; See https://stardot.org.uk/forums/viewtopic.php?f=2&t=23053
;

; Constants
AREEXT                                 = 0
CHARF                                  = 0
COLDST                                 = 0
FSUBRF                                 = 12
IMALEN                                 = 18
LISTF                                  = 128
NUMF                                   = 4
SUBRF                                  = 8
WARMST                                 = 42
WSBOT                                  = 2
osbyte_acknowledge_escape              = 126
osbyte_enter_language                  = 142
osbyte_inkey                           = 129
osbyte_read_adc_or_get_buffer_status   = 128
osbyte_read_high_order_address         = 130
osbyte_read_himem                      = 132
osbyte_read_himem_for_mode             = 133
osbyte_read_oshwm                      = 131
osbyte_read_tube_presence              = 234
osfile_load                            = 255
osfile_save                            = 0
osfind_close                           = 0
osfind_open_output                     = 128
osword_read_clock                      = 1
osword_read_interval_timer             = 3
osword_read_io_memory                  = 5
osword_read_line                       = 0
osword_read_pixel                      = 9
osword_sound                           = 7
osword_write_clock                     = 2
service_star_help_command              = 9
service_unrecognised_star_command      = 4

; Memory locations
GCNO    = $00
GARX    = $02
GARRY   = $03
GCA     = $04
GABBY   = $06
SA      = $07
l0009   = $09
l000a   = $0a
DISPM   = $0b
ERRCNT  = $0d
TOPBIN  = $0f
ERRNO   = $11
LINEPP  = $12
HANDLE  = $13
NBUFF   = $14
NIND    = $19
LEVEL   = $1a
OLDEXT  = $1b
END     = $1d
AA      = $1e
AB      = $20
YSAV    = $22
REP     = $23
LINENO  = $25
ARG     = $30
TVS     = $30
BINDER  = $32
TVSEXT  = $33
WSA     = $34
WSB     = $36
WSC     = $38
ARGA    = $3a
ARGB    = $3c
ARGC    = $3e
ARGD    = $40
ARGE    = $42
ACL     = $72
XTNDL   = $74
AUXL    = $76
SIGN    = $78
ARETOP  = $79
POINT   = $7a
RELBS   = $7a
ROMBS   = $7a
CELL    = $7c
RAMBS   = $7c
RELOC   = $7c
SP      = $7e
SIZE    = $82
WSD     = $83
RETADD  = $85
AD      = $87
XPR     = $89
DEPTH   = $8b
LSBUFF  = $8c
POPPY   = $8d
TERMCH  = $8e
ACIN    = $8f
ARGINF  = $90
OLDLEV  = $97
CLISTR  = $f2
BRKAD   = $fd
KBD     = $ff
brkv    = $0202
FIRST   = $0400
TUBE    = $0401
ACL40   = $0402
REM40   = $0407
AUX40   = $040c
TEMP40  = $0411
IODCB   = $0416
PWORD   = $041b
GCTIME  = $0420
TIMEW   = $0425
MODEF   = $042a
VECBOT  = $042b
IMBOT   = $042c
AREVAL  = $042d
ESCHF   = $042f
LSCHAR  = $0430
STATYP  = $0431
MEMINV  = $0432
RELOFF  = $0433
GENCNT  = $0434
OSINFO  = $0438
DL      = $0450
DH      = $0520
IMBUF   = $0600
NAMBUF  = $0600
OSWBUF  = $0600
DOSBUF  = $0700
HILISP  = $d700
osfind  = $ffce
osbput  = $ffd4
osbget  = $ffd7
osargs  = $ffda
osfile  = $ffdd
osrdch  = $ffe0
osasci  = $ffe3
oswrch  = $ffee
osword  = $fff1
osbyte  = $fff4
oscli   = $fff7

    * = $8000

; Standard language ROM header
entry_point
LISPST
LISVAL
ROMHDR
pydis_start
    jmp INITUR

    jmp INITSE                                     ; Service entry

    !byte $e2                                      ; Language, service & tube
    !byte CPYOFF-ROMHDR
    !byte 1                                        ; ROM version
TITLE
    !text "LISP"
    !byte 0
VERSN
    !text "4.06"
CPYOFF
    !byte 0
    !text "(C)1983 Acornsoft/1979 Owl Computers"
    !byte 0
TUBOFF
    !byte   0, $80,   0,   0                       ; Tube load addr


    ; ******************************
    ; Initialisation routine
    ; ******************************
INITUR
    cmp #1                                         ; Satisfactory?
    beq CRYON
    rts

CRYON
    cli
    cld
    lda #osbyte_read_high_order_address
    jsr osbyte                                     ; Find out if in tube; Read the filing system 'machine high order address'
    stx TUBE
    txa
    beq COLD
    jmp WRMCHK

COLD
    lda #osbyte_read_oshwm                         ; Find PAGE
    jsr osbyte                                     ; Read top of operating system RAM address (OSHWM)
    sty VECBOT
    cpx #0
    beq PBOUND
    inc VECBOT                                     ; Put on page boundary
PBOUND
    ldy VECBOT
    iny
    iny
    sty IMBOT

    ; ******************************
    ; Now copy down into RAM
    ; ******************************
    lda #<VECTAB
    sta ROMBS
    sta RAMBS
    lda #>VECTAB
    sta ROMBS+1
    lda VECBOT
    sta RAMBS+1
    ldx #IMALEN
    jsr COPY

    ; ******************************
    ; New initialisation routines
    ; ******************************
    lda #0
    sta MEMINV                                     ; Memory OK
    sta MODEF
    lda #$f0
    sta LSBUFF                                     ; Fudge first reads
    lda #$0d
    sta LSCHAR
    ldx #3
    lda #'0'
LOOP5
    sta GENCNT,x
    dex
    bpl LOOP5
    lda TUBE
    beq INTUBE
    lda #WARMST                                    ; No more to do if in I/O
    sta STATYP
    jmp INUREL


    ; **** Copy to HILISP
INTUBE
    lda #<LISPST
    sta ROMBS
    lda #>LISPST
    sta ROMBS+1
    lda #<HILISP
    sta RAMBS
    lda #>HILISP
    sta RAMBS+1
    ldx #(>LISPEN-LISVAL-1)+1
    jsr COPY

    ; **** Change addresses
    lda #<RELTAB
    sta RELBS
    lda #>RELTAB
    sta RELBS+1
    lda #>HILISP-LISVAL
    jsr CHADD

    ; **** Alter vectors for tube
    lda #<VECTAB+2
    sta RELBS
    lda VECBOT
    sta RELBS+1
    ldy #0
LOOP3
    lda (RELBS),y
    clc
    adc #>HILISP-LISVAL
    sta (RELBS),y
    lda RELBS
    clc
    adc #3                                         ; Next vector
    sta RELBS
    lda RELBS+1
    adc #0
    sta RELBS+1
    cmp IMBOT
    bne LOOP3
    lda #0                                         ; Prevent warm start offer
    sta STATYP

    ; Do a *GO WRMCHK (via HILISP)
    lda #<HIWARM
    sta HILISP+1
    lda #>HIWARM
    sta HILISP+2
    ldx #<(GOSTR)
    ldy #>(GOSTR)
    jsr oscli

    ; General copy routine
COPY
    ldy #0
LOOP1
    lda (ROMBS),y
    sta (RAMBS),y
    iny
    bne LOOP1
    inc RAMBS+1
    inc ROMBS+1
    dex
    bne LOOP1
    rts


    ; Address change routine
CHADD
    sta RELOFF
    ldx #0
    ldy #0
LOOP2
    lda (RELBS),y
    sta RELOC
    iny
    lda (RELBS),y
    beq CHRTS
    sta RELOC+1
    lda (RELOC,x)
    clc
    adc RELOFF
    sta (RELOC,x)
    iny
    bne LOOP2
    inc RELBS+1
    jmp LOOP2

CHRTS
    rts


    ; ******************************
    ; Claim as service
    ; ******************************
INITSE
    cmp #service_unrecognised_star_command         ; Reasonable?
    beq OKCALL
    cmp #service_star_help_command                 ; Help?
    beq HELP
    rts

HELP
    pha
    txa
    pha
    tya
    pha                                            ; Mustn't corrupt zero page
    lda HANDLE
    pha
    ldx #HLPOFF
    jsr MESSAH
    pla
    sta HANDLE
    pla
    tay
    pla
    tax
    pla
    rts

OKCALL
    pha
    tya
    pha
    txa
    pha
    lda (CLISTR),y
    cmp #'L'
    bne NOTLSP
    iny
    lda (CLISTR),y
    cmp #'I'
    bne NOTLSP
    iny
    lda (CLISTR),y
    cmp #'S'
    bne NOTLSP
    iny
    lda (CLISTR),y
    cmp #'P'
    bne NOTLSP
    iny
    lda (CLISTR),y
    cmp #$0d
    bne NOTLSP
    lda #osbyte_read_tube_presence
    ldx #0
    ldy #$ff
    jsr osbyte                                     ; Is there a tube?; Read Tube present flag
    txa
    bne STLISP                                     ; If so don't flag
    lda #COLDST
    sta STATYP
STLISP
    pla
    tax                                            ; X=ROM number
    lda #osbyte_enter_language
    jmp osbyte                                     ; Start up LISP; Enter language ROM X

NOTLSP
    pla
    tax
    pla
    tay
    pla
    rts


    ; ******************************
    ; These are LISP's tables
    ; ******************************
    ; **** The text messages
TEXT
    !text $0d, $0d, "Evaluate :", $80+' '
VALTXT
    !text $0d, "Value is :", $80+' '
NILTXT
    !text "NI", $80+'L'
DOTTXT
    !text " .", $80+' '
INSTXT
    !text $0d, "Insufficient memor", $80+'y'
GCTXT
    !text $0d, "G.C. ", $80+'#'
COLTXT
    !text " Bytes collected,", $80+' '
FRTXT
    !text " Bytes free", $80+$0d
SUBTXT
    !text "Subr", $80+'#'
ERRTXT
    !text $0d, "Error number", $80+' '
ARGTXT
    !text $0d, "Arg :", $80+' '
WRMTXT
    !text $0d, "Warm or cold start (W/C) ?", $80+' '
HLPTXT
    !text $0d, "LISP 4.06", $80+$0d

    ; **** Command lines
GOSTR
    !text "GO D700"
    !byte $0d
LISTR
    !text "LISP"
    !byte $0d

    ; **** Readline control block
INCB
    !byte <(DOSBUF), >(DOSBUF)                     ; Buffer address for input (2 bytes)
    !byte $7f                                      ; Max line length; Maximum line length
    !byte $20                                      ; Min. acceptable character value
    !byte $7f                                      ; Max. acceptable character value

    ; **** Terminators
TERMS
    !text "). ("
    !byte $0d, $0a, $21, $27

    ; **** CAR/CDR table
CXXXR
    !byte 0, 2, 2, 2, 0, 4, 2, 2, 0, 2, 4, 2, 0, 4
    !byte 4, 2, 0, 2, 2, 4, 0, 4, 2, 4, 0, 2, 4, 4
    !byte 0, 4, 4, 4

    ; **** Table of CHARS lengths
LENTAB
    !byte   3,   6,   9, $0a,   0

    ; **** Untraceable errors
CATTAB
    !byte   0,   1,   2, $0b, $0c, $ff

    ; **** Zero time
TIMZER
    !byte 0, 0, 0, 0, 0                            ; Five byte clock value (low byte to high byte)


    ; ******************************
    ; Routine to give optionsl warm
    ; start (ie OBLIST retained)
    ; ******************************
WRMCHK
    lda STATYP
    cmp #WARMST
    bne NOTWRM
    jmp WARM                                       ; Offer warm start

NOTWRM
    lda TUBE
    bne COLD1
    lda #WARMST                                    ; We're in tube and
    sta STATYP                                     ; it's first time through
    jmp INUREL

COLD1
    jmp COLD

WARM
    lda MEMINV
    bne REBOOT
    ldx #WRMOFF
    jsr MESSAH
    jsr osrdch                                     ; Read a character from the current input stream
    and #$7f
    cmp #'C'
    beq REBOOT
    cmp #'c'
    beq REBOOT
    lda #'W'
    jsr oswrch                                     ; Write character 87
    jmp INIT


    ; **** Reload from ROM
REBOOT
    lda #'C'
    jsr oswrch                                     ; Write character 67
    jsr CROUT
    jsr CROUT
    ldx #<(LISTR)
    ldy #>(LISTR)
    jmp oscli


    ; ******************************
    ; This is the main LISP
    ; supervisor loop which is
    ; entered when LISP is called.
    ; ******************************
SUPER
    lda #0                                         ; Reset stack
    sta SP
    lda ARETOP
    sta SP+1
    ldx #$ff                                       ; Reset hardware stack
    txs
    lda MODEF                                      ; Check mode flag
    bpl EVPR
    jsr MODCHN                                     ;  Change mode
EVPR
    ldx #EVOFF
    jsr MESSAH                                     ; Evaluate:
    jsr RSREAD                                     ; Read expression
    jsr EVALU                                      ; Evaluate it
    ldx #VALOFF                                    ; Value is:
    jsr MESSAH
    jsr PRINA                                      ; and print it
    jmp SUPER                                      ; Repeat ad infinitum


    ; ******************************
    ; Here are some condition
    ; routines taking advantage
    ; of POP
    ; ******************************
    ; **** ATOM
ATOM
    lda ARGA+1                                     ; Is ARGA atom?
    beq YES                                        ; NIL => yes
    ldy #0
    lda (ARGA),y                                   ; check bit 7
    bpl YES
    bmi NO

    ; **** EQ
EQ
    lda ARGB+1                                     ; ARGA = ARGB?
    beq NULL                                       ; ARGB NIL => ARGA NIL
    cmp ARGA+1
    bne EQUATE                                     ; But they are numeric
    lda ARGB
    cmp ARGA
    beq YES
EQUATE
    ldy #0                                         ; Fudge for nums
    lda (ARGA),y
    cmp #NUMF                                      ; Both must be nums
    bne NO
    cmp (ARGB),y
    bne NO
    iny
    lda (ARGA),y
    cmp (ARGB),y                                   ; Same length?
    bne NO
    tay
    dey
EQUINE
    lda (ARGA),y                                   ; Compare value
    cmp (ARGB),y
    bne NO
    dey
    bne EQUINE
    beq YES

    ; **** NULL
NULL
    lda ARGA+1                                     ; Is ARGA NIL?
    beq YES
NO
    lda #0                                         ; Returns NIL
    beq POPA
YES
    lda #<TRUE                                     ; Returns T
    sta ARG
    lda IMBOT
POPA
    sta ARG+1

    ; ******************************
    ; POP is one of the two LISP
    ; stack handling routines. It
    ; restores old binding values
    ; and WSA, WSB and WSC from the
    ; stack and POP the stack. ARG
    ; is unchanged.
    ; ******************************
POP
    ldy #0                                         ; Get binding size
    lda (SP),y
    beq NOBOUN                                     ; No bindings to do
    tay
POPPLE
    lda (SP),y
    sta RETADD+1                                   ; Get atom
    dey
    lda (SP),y
    sta RETADD
    dey
    lda (SP),y                                     ; Get old value
    tax                                            ; into A,X
    dey
    lda (SP),y
    dey
    sty POPPY                                      ; Save Y
    ldy #2                                         ; Put value back
    sta (RETADD),y                                 ; into atom value
    iny                                            ; cell
    txa
    sta (RETADD),y
    ldy POPPY                                      ; Fetch Y back
    bne POPPLE                                     ; More bindings?
    lda (SP),y
    clc                                            ; Set stack pointer
    adc SP                                         ; to bottom of work-
    sta SP                                         ; Space area as if
    bcc NOBOUN                                     ; there were no binds
    inc SP+1
NOBOUN
    iny                                            ; Now Y = 1
    lda (SP),y
    tay                                            ; Index for top space
    dey
    lda (SP),y                                     ; Push return addr
    pha
    dey
    lda (SP),y
    pha
    dey
MORSP
    lda (SP),y                                     ; Copy back TVS
    sta BINDER,y                                   ; WSA, WSB and WSC
    dey
    bne MORSP
    sec                                            ; Add TVSEXT + 2 to stack
    lda SP                                         ; pointer to POP the stack
    adc TVSEXT
    bcs TVF
    adc #1
    bcc TVG
    clc
TVF
    adc #0
    inc SP+1
TVG
    sta SP
    rts


    ; ******************************
    ; STACK stores:
    ;    Extent of TVS, WSA, WSB,
    ;    WSC, ARG and return addr
    ;     on the LISP stack
    ; ******************************
STACK
    lda SP
    clc
    sbc TVSEXT
    tax
    bne EXTRAM
    clc
EXTRAM
    dex
    lda SP+1
    sbc #0
    cmp AREVAL+1
    bcc SQUAT
    bne SROOM
    cpx AREVAL
    bcs SROOM
SQUAT
    jsr RUBBSH
    bne STACK
STIR
    brk                                            ; None found

    !byte 0
    !text "No room"
    !byte 0

SROOM
    sta SP+1
    stx SP
    pla
    sta RETADD
    pla
    tax
    ldy TVSEXT
    iny
    lda ARG+1                                      ; Store ARG
    sta (SP),y
    dey
    lda ARG
    sta (SP),y
    dey
    dey
    pla                                            ; Store return addr
    sta (SP),y
    iny
    pla
    sta (SP),y
    dey
    dey
PILE
    lda BINDER,y
    sta (SP),y
    dey
    bpl PILE
    txa
    pha
    lda RETADD
    pha
    rts


    ; ******************************
    ; BIND adds a 'new value' to the
    ; bottom of the stack:
    ;   WSD     - Pointer to atom
    ;   TVS + X - New value
    ; ******************************
BIND
    lda SP
    sec
    sbc #4
    tay
    lda SP+1
    sbc #0
    cmp AREVAL+1
    bcc SQUASH
    bne XROOM
    cpy AREVAL
    bcs XROOM
SQUASH
    jsr RUBBSH
    bne BIND
BEAR
    brk                                            ; None found

    !byte 1
    !text "No room"
    !byte 0

XROOM
    sta SP+1
    sty SP
    ldy #4
    lda (SP),y
    adc #3                                         ; Carry is set
    pha
    lda WSD+1
    sta (SP),y
    dey
    lda WSD
    sta (SP),y
    lda (WSD),y                                    ; Old value and atom
    dey
    sta (SP),y                                     ; on stack
    lda (WSD),y
    dey
    sta (SP),y
    dey
    pla
    sta (SP),y                                     ; Bound var size
    ldy #2
    lda TVS,x                                      ; New value in atom
    sta (WSD),y
    iny
    lda TVS+1,x
    sta (WSD),y
    rts


    ; ******************************
    ; Here is the space allocator
    ; routine. If allocates up to
    ; 256 bytes of initialized store
    ; ******************************
ALNUM
    lda #4
ALVEC
    ldx #NUMF
    bne SPACE
ALCHAR
    ldx #CHARF
    beq SPACE
ALFSBR
    ldx #FSUBRF
    lda #6
    bne SPACE
ALSUBR
    ldx #SUBRF
    lda #6
    bne SPACE
ALPAIR
    lda #5
    ldx #$80                                       ; Pointer space
SPACE
    sta SIZE
SPACEB
    clc
    lda AREVAL
    sta POINT
    adc SIZE
    tay
    lda AREVAL+1
    sta POINT+1
    adc #0
    cmp SP+1
    bcc ROOM
    bne SQUID
    cpy SP
    bcc ROOM
SQUID
    jsr RUBBSH
    bne SPACEB
ALLO
    brk                                            ; None found

    !byte 2
    !text "No room"
    !byte 0

ROOM
    sta AREVAL+1
    sty AREVAL
    ldy #0
    txa
    sta (POINT),y
    bmi PINS
    bne OBSCUR
    ldy #5
    lda #0
    sta (POINT),y
    ldy #2
    lda #<ZA                                       ; UNDEFINED
    sta (POINT),y
    iny
    lda IMBOT
    sta (POINT),y
OBSCUR
    ldy #1
    lda SIZE
    sta (POINT),y
    rts

PINS
    lda #0
    ldy #2
    sta (POINT),y                                  ; Initial NIL point
    ldy #4
    sta (POINT),y
    rts


    ; ******************************
    ; Here are the initialisation
    ; routines
    ; ******************************
INIT
    lda #<ERROR
    sta brkv                                       ; Error handling
    lda #>ERROR
    sta brkv+1
    lda #osbyte_read_himem                         ; Find end of memory
    jsr osbyte                                     ; Read top of user memory (HIMEM)
    sty ARETOP

    ; **** Set up various values
    lda #$fc                                       ; Messages except GC
    sta LEVEL
    ldx #0
    stx HANDLE                                     ; Output to screen
    stx GCNO                                       ; Zero collections
    stx GCNO+1
    stx ERRCNT                                     ; Zero error count
    stx ERRCNT+1
    ldy #$10                                       ; Clear TVS etc.
ZLP
    stx TVS,y
    dey
    bpl ZLP
    lda #$0a                                       ; Initially no args
    sta TVSEXT
    jsr GCTIMZ                                     ; Zero GC time
    jsr STCLK                                      ; Zero time
    lda #<KBD                                      ; Set up Escape check
    sta IODCB
    lda #>KBD
    sta IODCB+1
    lda #0
    sta IODCB+2
    sta IODCB+3
    jmp SUPER                                      ; Enter supervisor


    ; **** Message handler
MESSAH
    ldy #0
MESSAI
    sty HANDLE
MESSAG
    lda TEXT,x                                     ; Print message
    php
    and #$7f                                       ; Remove flag bit
    jsr OUT
    inx
    plp                                            ; Retrieve flag
    bpl MESSAG                                     ; Bit 7 not set
    rts

OUT
    ldy HANDLE                                     ; to screen?
    beq OUTSCR
    jmp osbput                                     ; Write a single byte A to an open file Y

OUTSCR
    cmp #$0d
    bne NCR
CROUT
    lda #$0d
NCR
    jmp osasci                                     ; Print the char; Write character 13


    ; ******************************
    ; Main evaluation routine
    ;    arg           -> ARG
    ;    NIL           -> NIL
    ;    number, entry -> same
    ;    char          -> same
    ;    list          -> eval fn
    ; ******************************
NXEVAL
    lda WSA+1
    bne NXEVAM
FSARG
    brk                                            ; No more args

    !byte 3
    !text "Too few arguments"
    !byte 0

NXEVAM
    jsr NXTARH

    ; **** Main evaluator
EVALU
    jsr KBCHK                                      ; Main entry
    beq EVAL1
    brk                                            ; Escape

    !byte 4
    !text "Escape"
    !byte 0

KBCHK
    ldx #<(IODCB)
    ldy #>(IODCB)
    lda #osword_read_io_memory
    jsr osword                                     ; Read byte of I/O processor memory
    lda #$80
    and IODCB+4
    rts

EVAL1
    lda ARG+1
    beq EVARTS                                     ; NIL?
    ldy #0
    lda (ARG),y                                    ; Check type
    bmi EVLIST
    bne EVARTS
    ldy #2
    lda (ARG),y
    tax
    iny                                            ; Get value cell
    lda (ARG),y
    stx ARG
    sta ARG+1
    rts

EVLIST
    jsr STACK
    ldy #4
    lda (ARG),y
    sta WSA+1
    dey
    lda (ARG),y
    sta WSA
    dey
    lda (ARG),y
    tax
    dey
    lda (ARG),y
    stx ARG+1
    sta ARG
    jsr FUN
    jsr EVALU
    jsr FUN
    jsr EVALU
    jsr FUN
FUNERR
    brk                                            ; Can't make function

    !byte 6
    !text "Function expected"
    !byte 0

EVARTS
    rts

FUN
    lda ARG+1
    beq FUNERR
    ldy #0
    lda (ARG),y
    beq EVARTS                                     ; Char atom
    bpl ENT
    iny                                            ; Lambda?
    lda (ARG),y
    cmp #<LAMBDA
    bne EVARTS
    iny
    lda (ARG),y
    cmp IMBOT
    bne EVARTS
    jmp LAMOK

ENT
    tay                                            ; Probably entry
    pla
    pla
    lda ARG
    sta WSC                                        ; Keep fn safe
    lda ARG+1
    sta WSC+1
    ldx #$0a
    stx TVSEXT
    cpy #FSUBRF
    beq ISFSBR
    cpy #SUBRF
    beq ISSUBR
    bne FUNERR                                     ; Oops a number!
MORAG
    jsr NXEVAM
    ldx TVSEXT
    cpx #$42
    bcs NARGER
    lda ARG                                        ; Args eval in TVS
    sta TVS,x
    inx
    lda ARG+1
    sta TVS,x
    inx
    stx TVSEXT
ISSUBR
    lda WSA+1
    bne MORAG
INSUBR
    ldy #1
    lda (WSC),y
    asl
    clc
    adc #$0a
    tax
    cpx TVSEXT
    beq ISFSBR
    bcc ISFSBR
NARGER
    brk                                            ; Wrong

    !byte 6
    !text "Wrong number of arguments"
    !byte 0

ISFSBR
    ldy #3
    lda (WSC),y
    beq GOSUB
    sta WSD+1
    dey
    lda (WSC),y
    sta WSD                                        ; List in WSD
    jmp PLOP

DEFLST
    pha
    dey
    lda (WSD),y
    sta WSD
    pla
    sta WSD+1
PLOP
    cpx TVSEXT                                     ; Default needed
    bcc SKIP
    ldy #1
    lda (WSD),y
    sta TVS,x
    iny
    lda (WSD),y
    sta TVS+1,x
SKIP
    inx
    inx
    ldy #4
    lda (WSD),y
    bne DEFLST
    stx TVSEXT
GOSUB
    ldy #4                                         ; Go and do it!
    lda (WSC),y
    sta RETADD
    iny
    lda (WSC),y
    sta RETADD+1
    jmp (RETADD)                                   ; End of subrs


    ; ******************************
    ; Time for some lambda
    ; ******************************
LAMOK
    pla
    pla
    ldy #4
    lda (ARG),y
    beq LAMERR                                     ; No parms or body
    sta WSD+1
    dey
    lda (ARG),y
    sta WSD
    ldx #$0a
    stx TVSEXT
    ldy #0
    lda (WSD),y
    bpl LAMERR
    iny
    lda (WSD),y                                    ; Parm list
    sta WSB
    iny
    lda (WSD),y
    sta WSB+1
    iny
    lda (WSD),y                                    ; Body in WSC
    sta WSC
    iny
    lda (WSD),y
    sta WSC+1
    lda WSB+1                                      ; NIL parms?
    bne AVX
    jmp XLAM

AVX
    ldy #0
    lda (WSB),y
    bmi ISEXPR
    beq ISFXP1                                     ; NB spelling!
LAMERR
    brk                                            ; Syntax error

    !byte 7
    !text "Lambda syntax"
    !byte 0

ISFXP1
    jmp ISFXPR

MORFAG
    jsr NXEVAM
    ldx TVSEXT                                     ; Spread args for expr
    cpx #$42
    bcc GODARG
    jmp NARGER

GODARG
    lda ARG
    sta TVS,x
    inx
    lda ARG+1
    sta TVS,x
    inx
    stx TVSEXT
ISEXPR
    lda WSA+1
    bne MORFAG
RADON
    ldx #$0a
XENON
    ldy #2
    lda (WSB),y
    beq LAMERR
    sta WSD+1
    dey
    lda (WSB),y
    sta WSD
    dey
    cpx TVSEXT                                     ; Enough args?
    lda (WSD),y
    beq DOBIND
    bpl LAMERR
    bcc NOD
    ldy #3
    lda (WSD),y
    sta TVS,x
    iny                                            ; The default value?
    lda (WSD),y
    sta TVS+1,x
    clc
NOD
    ldy #2
    lda (WSD),y
    beq LAMERR                                     ; Get the atom bind
    pha
    dey
    lda (WSD),y
    sta WSD
    pla
    sta WSD+1
    dey
    lda (WSD),y
    bne LAMERR                                     ; Must be char atom
DOBIND
    bcc GADARG
    jmp NARGER

GADARG
    jsr BIND                                       ; Bind takes atom
    inx                                            ; in WSD and value in TVS + X
    inx
    ldy #4
    lda (WSB),y
    beq XLAMB
    pha
    dey
    lda (WSB),y
    sta WSB
    pla
    sta WSB+1
    ldy #0
    lda (WSB),y
    bmi XENON
    jmp LAMERR

ISFXPR
    lda WSB
    sta WSD
    lda WSB+1
    sta WSD+1
    ldx #4
    jsr BIND
    ldx #$0a
XLAMB
    stx TVSEXT
XLAM
    lda WSC+1
    bne XLAMC
    beq EVPOP
XLAMD
    tax
    dey
    lda (WSC),y
    stx WSC+1
    sta WSC
XLAMC
    ldy #0
    lda (WSC),y
    bmi SYNNED
    jmp LAMERR

SYNNED
    iny
    lda (WSC),y
    sta ARG
    iny
    lda (WSC),y
    sta ARG+1
    jsr EVALU
    ldy #4
    lda (WSC),y
    bne XLAMD
EVPOP
    jmp POP


    ; **** Get a character
GTCHAR
    ldy HANDLE
    beq KEYCH
    lda LSCHAR                                     ; From file
    bpl RENEW
    and #$7f
    bpl REOLD
RENEW
    jsr osbget                                     ; Read a single byte from an open file Y
REOLD
    sta LSCHAR
    rts

KEYCH
    txa                                            ; From screen
    pha
    ldx LSBUFF
    cpx #$f0                                       ; => New line
    bne NNL

    ; **** Read a line
    bit LEVEL                                      ; Prompt masked?
    bpl READON
    lda DEPTH
    tax
PRDEPT
    beq READON
    lda #'['
    jsr oswrch                                     ; Write character 91
    dex
    bpl PRDEPT
READON
    ldx #<(INCB)
    ldy #>(INCB)
    lda #osword_read_line
    jsr osword                                     ; Read line; Read line from input stream (exits with C=1 if ESCAPE pressed)
    bcc OKLINE
RDCHER
    brk                                            ; Escape

    !byte $1a
    !text "Escape"
    !byte 0

OKLINE
    lda #$ff                                       ; Zero => newline
    sta LSBUFF
NNL
    inc LSBUFF                                     ; Fetch char
    pla
    tax                                            ; RESTORE X
    ldy LSBUFF
    lda DOSBUF,y                                   ; Get char form buffer
    cmp #$0d
    bne XYZ
    ldy #$f0
    sty LSBUFF                                     ; New line next time
XYZ
    rts

RSREAD
    ldx #0
    stx DEPTH
    stx LINENO
    stx HANDLE
    ldx #$f0                                       ; Flag for new line
    stx LSBUFF

    ; **** READ
READ
    ldx #$ff
    stx ARG+1                                      ; Flag for rubbish
    jsr STACK
NXCHAR
    jsr GTCHAR
RPT
    cmp #$0d
    beq NXCHAR
    cmp #$0a
    beq NXCHAR
    cmp #' '
    beq NXCHAR
    cmp #'\''
    bne PARQU
    lda #<QUOTE                                    ; It's a quote
    sta WSA
    lda IMBOT
    sta WSA+1
ODDAT
    jsr READ                                       ; Entry for atoms
    jsr ALPAIR                                     ; Get list cell for it
    ldy #2
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    lda POINT+1
    sta WSB+1
    lda POINT
    sta WSB
    jsr ALPAIR
    lda POINT
    sta ARG
    lda POINT+1
    sta ARG+1
    ldy #4
MORODD
    lda TVSEXT,y
    sta (ARG),y
    dey
    bne MORODD
    jmp POP

RDLJMP
    jmp RDLIST

PARQU
    cmp #'('
    beq RDLJMP
    ldx #$ff                                       ; Some sort of atom
    cmp #'!'                                       ; Escape
    beq SPCATM
    ldy #1                                         ; Ordinary char atom
    jsr TERMQ
    bne NORMAL
QUEER
    brk                                            ; Syntax error

    !byte 8
    !text "Read syntax"
    !byte 0

SPCATM
    jsr GTCHAR                                     ; Escaped char
NORMAL
    inx                                            ; Fetch chars
    sta OSWBUF,x
    jsr GTCHAR
    cmp #'!'
    beq SPCATM
    ldy #5
    jsr TERMQ
    bne NORMAL
    jsr PUTBCK

    ; ******************************
    ; Here check if numeric. Find
    ; or set up char atom.
    ; ******************************
    stx END
    jsr MAKNUM
    bcc AMADE
TRYCHR
    jsr MATCH
AMADE
    lda POINT
    sta ARG
    lda POINT+1
    sta ARG+1
    jmp POP


    ; *** Read a list
RDLIST
    lda #0
    sta WSA+1
    sta WSB+1
    inc DEPTH
    jsr LCHAR
    cmp #'.'
    bne LON
    jsr GTCHAR
DOTTY
    brk                                            ; Dot syntax

    !byte 9
    !text "Dot syntax"
    !byte 0

LON
    jsr READ
    jsr ALPAIR
    ldx POINT
    lda POINT+1
    ldy WSB+1                                      ; First element?
    beq NEWLST
    ldy #4
    sta (WSB),y
    txa
    dey
    sta (WSB),y
    lda POINT+1
    bne ALLIST                                     ; Always taken
NEWLST
    stx WSA
    sta WSA+1
ALLIST
    stx WSB
    sta WSB+1
    ldy #1
    lda ARG
    sta (WSB),y
    iny
    lda ARG+1
    sta (WSB),y
    jsr LCHAR
    cmp #'.'
    bne LON
    jsr GTCHAR                                     ; Dotted pair at end
    jsr READ
    ldy #4
    lda ARG+1
    sta (WSB),y
    dey
    lda ARG
    sta (WSB),y
    jsr LCHAR                                      ; Shouldn't return
    jmp DOTTY                                      ; Dot syntax error

READX
    jsr FILGB
    jsr READ
    jmp POP


    ; **** Look for terminators
TERMQ
    cmp TERMS,y
    beq TERRTS
    dey
    bpl TERMQ
TERRTS
    rts


    ; **** Next arg from WSA's list
NXTARG
    lda WSA+1
    bne NXTARH
ARGERR
    jmp FSARG

NXTARH
    ldy #0
    lda (WSA),y
    bpl ARGERR
    iny
    lda (WSA),y
    sta ARG
    iny
    lda (WSA),y
    sta ARG+1
    iny
    lda (WSA),y
    tax
    iny
    lda (WSA),y
    stx WSA
    sta WSA+1
    rts

LCHAR
    jsr GTCHAR
    cmp #' '
    beq LCHAR
    cmp #$0d
    beq LCHAR
    cmp #')'
    beq LISTND
PUTBCK
    pha
    ldy HANDLE
    bne EXFILE                                     ; Reverse file pointer
    ldy LSBUFF
    bpl MIDBUF
    lda #$0d                                       ; Fudge CR into buffer
    sta DOSBUF
    ldy #0
MIDBUF
    dey
    sty LSBUFF
    pla
    rts

EXFILE
    lda LSCHAR                                     ; COS version
    ora #$80
    sta LSCHAR
    pla
    rts

LISTND
    dec DEPTH
    lda WSA
    sta ARG
    lda WSA+1
    sta ARG+1
    pla
    pla
    jmp POP


    ; **** Make number in POINT
MAKNUM
    ldx #0                                         ; Text from IMBUF
    stx SIGN
    stx ACL
    stx ACL+1
    lda OSWBUF,x
    cmp #'-'
    bne PLUSS
    sta SIGN
    inx
    ldy END
    cpy #0
    beq MKRTS
PLUSS
    lda OSWBUF,x
    sec
    sbc #$30
    cmp #$0a
    bcs MKRTS                                      ; Not a digit
    sta ACIN
    ldy #0
    lda ACL                                        ; *10 now
    asl
    sta ACL
    rol ACL+1                                      ; That's * 2
    bcs MKOVFL                                     ; Overflow
    asl
    bcc YIA
    ldy #2
YIA
    asl
    bcc YIB
    iny
    clc
YIB
    adc ACIN
    bcc YIC
    iny
    clc
YIC
    adc ACL
    sta ACL
    bcc YID
    iny
YID
    lda ACL+1
    asl
    bcs MKOVFL
    asl
    bcs MKOVFL
    adc ACL+1
    sta ACL+1
    bcs MKOVFL
    tya
    adc ACL+1
    bcs MKOVFL
    sta ACL+1
    cpx END
    inx
    bcc PLUSS                                      ; Any more digits?
    lda SIGN
    beq PLUSSS
    ldy #0                                         ; Reverse sign if -ve
    ldx #<ACL
    jsr MD
    ldx ACL+1
    bmi NTOVFL
MKOVFL
    sec
    rts                                            ; Not number

PLUSSS
    ldx ACL+1
    bmi MKOVFL
NTOVFL
    jsr ALNUM
    ldy #3
    lda ACL+1
    sta (POINT),y
    dey
    lda ACL
    sta (POINT),y
    clc
MKRTS
    rts

NEXTAD
    inc AD
    bne NEXTAA
    inc AD+1
NEXTAA
    inc AA
    bne NAAB
    inc AA+1
NAAB
    lda AA
    cmp AB
    lda AA+1
    sbc AB+1
    rts


    ; ******************************
    ; MATCH tries to find a string
    ; to match the string in IMBUF.
    ; If it cannot it makes up a
    ; new atom.
    ; ******************************
LETTER
    sta OSWBUF
    ldy #0
    sty END
MATCH
    jsr SETCEL
    inc END
    lda END
    clc
    adc #6
    sta ACL+1
    bcc CHKCLL
LONGER
    brk                                            ; String too long

    !byte $0a
    !text "String too long"
    !byte 0

CHKCLL
    ldy #0
CHKCLM
    lda (CELL),y
    bne NXTCLL
    iny
    lda (CELL),y
    cmp ACL+1
    bne NXTCLL
    lda CELL
    adc #5
    sta AD
    lda CELL+1
    adc #0
    sta AD+1
    ldy END
    bpl TESTY
MCOP
    dey
    lda OSWBUF,y                                   ; Chars the same?
    cmp (AD),y
    bne NXTCLL
    tya
TESTY
    bne MCOP
    lda CELL+1
    ldx CELL                                       ; NIL?
    stx POINT
    cpx #<NIL
    bne BOX
    sbc IMBOT
    beq BOX
    lda CELL+1
BOX
    sta POINT+1
    rts                                            ; It's found

NXTCLL
    jsr NXCELL
    bcc CHKCLM
    lda ACL+1
    jsr ALCHAR
    lda POINT
    clc
    adc #6
    sta AD
    lda POINT+1
    adc #0
    sta AD+1
    ldy END
    bpl TOSTIG
MCAP
    dey
    lda OSWBUF,y
    sta (AD),y
    tya
TOSTIG
    bne MCAP
    rts                                            ; New cell made

NXCELL
    ldy #0                                         ; Given pointer
    lda (CELL),y                                   ; in CELL, finds next
    clc                                            ; CELL
    bmi SIX
    cmp #8
    bcs SIX
    iny
    lda (CELL),y
    dey
    beq GOT
SIX
    lda #5
GOT
    adc CELL
    sta CELL
    bcc HCOK
    inc CELL+1
HCOK
    cmp AREVAL
    lda CELL+1
    sbc AREVAL+1
    rts

FILGB
    ldx TVSEXT
    cpx #$0c
    lda #0
    bcc ZIP
FILGC
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
ZIP
    sta HANDLE
    rts

IPLINE
    jsr FILGB
    ldx #$ff
MOLIN
    jsr GTCHAR
    inx
    sta OSWBUF,x
    cmp #$0d
    bne MOLIN
    dex                                            ; Don't want CR at end.
    stx END
    jmp TRYCHR


    ; ******************************
    ; Here there be the point
    ; routines of various types:
    ; ******************************
    ; **** Intercept return addr
GETSSP
    lda SP                                         ; Find SSP ret addr
    sta POINT
    lda SP+1
    sta POINT+1
    ldy #0
    lda (POINT),y
    clc
    adc POINT
    sta POINT
    bcc STINC
    inc POINT+1
STINC
    iny
    lda (POINT),y
    tay
    dey
    rts


    ; **** WRITE
WRIT
    jsr GETSSP                                     ; Locate ret addr
    lda (POINT),y
    pha
    lda #>WRITGO
    sta (POINT),y
    dey
    lda (POINT),y
    pha
    lda #<WRITGO+2
    sta (POINT),y
WRITGO
    jmp WRTTZ

    lda #$0d
    jsr OUT
    rts


    ; **** WRITE0
WRTTZ
    jsr FILG
    lda #$ff
    sta ESCHF
    ldx #$0c
    bne GENWRI

    ; **** Error entry
PRINTE
    jsr STACK
    jmp PRINTC


    ; **** PRINT
PRINT
    lda #$ff                                       ; Put in esc char
    bmi PRINT1

    ; **** PRIN
PRINZ
    lda #$ff
    bmi PRIN1

    ; **** PRINTC
PRINTC
    lda #0
PRINT1
    sta ESCHF
    jsr GETSSP
    lda (POINT),y
    pha
    lda #>PRINGO
    sta (POINT),y
    dey
    lda (POINT),y
    pha
    lda #<PRINGO+2
    sta (POINT),y
PRINGO
    jmp PRINOK

    jsr CROUT
    rts


    ; **** PRINC
PRINC
    lda #0
PRIN1
    sta ESCHF
PRINOK
    ldy #0
    sty HANDLE
    ldx #$0a
GENWRI
    lda #0
    cpx TVSEXT                                     ; NO ARGS?
    bcs NILFR
PRON
    lda TVS,x
    sta ARG
    inx
    lda TVS,x
    sta ARG+1
    inx
    stx XPR
    jsr PRINB
    ldx XPR
    cpx TVSEXT
    bcc PRON
    lda TVS-2,x
    sta ARG
    lda ARG-1,x
NILFR
    jmp POPA


    ; **** Get file handle
FILG
    lda ARGA+1
    beq FILGER
    ldy #0
    lda (ARGA),y
    cmp #NUMF
    bne FILGER
    ldy #2
    lda (ARGA),y
    sta HANDLE
    rts

FILGER
    jmp NUER

SAVAR
    lda ARG                                        ; Saves ARG in WEB
    sta WSB
    lda ARG+1
    sta WSB+1
    rts

CCPR
    brk                                            ; Escape

    !byte $0b
    !text "Escape"
    !byte 0


    ; **** Just prints ARG
PRINA
    lda #0
    sta ESCHF
PRINB
    jsr KBCHK                                      ; Check for escape
    bne CCPR
    lda ARG+1
    bne NPN                                        ; NIL?
    ldx #NILOFF
    jmp MESSAG                                     ; 'NIL'


    ; **** Main printer
NPN
    ldy #0
    lda (ARG),y
    bpl PATOMB                                     ; Atom or list?
    lda #'('
    jsr OUT
    jsr STACK                                      ; Since recursive
    lda ARG
    sta WSA
    lda ARG+1
    sta WSA+1
    bne NASTY

    ; **** Print list
PLIST
    lda #' '
    jsr OUT
NASTY
    jsr NXTARH
    jsr PRINB
    lda WSA+1
    beq RPP
    ldy #0
    lda (WSA),y
    bmi PLIST
    lda WSA+1
    sta ARG+1
    lda WSA
    sta ARG
    ldx #$1c
    jsr MESSAG
    jsr PATOM
RPP
    lda #')'
    jsr OUT
    jmp POP

BADAT
    brk                                            ; Unknown atom type

    !byte $0c
    !text "Bad atom type"
    !byte 0


    ; **** Print atom
PATOM
    ldy #0
    lda (ARG),y
PATOMB
    beq OKP                                        ; If char atom
    cmp #NUMF
    beq PNUM                                       ; If number atom
    cmp #SUBRF
    beq HASH                                       ; If subr atom
    cmp #FSUBRF
    bne BADAT
    lda #'F'                                       ; Fsubr
    jsr OUT
HASH
    ldx #SUBOFF
    jsr MESSAG
    ldy #5
    lda (ARG),y
    sta ACL+1                                      ; Print entry addr.
    dey
    lda (ARG),y
    sta ACL
    jmp PINT


    ; *** Char atoms
OKP
    jsr GENDS
    ldx #0
    beq INCHP
FOOT
    lda (AA,x)
    ldy ESCHF
    bmi ESCFT
FTOUT
    jsr OUT
    jmp INCHP

ESCFT
    ldy #6
    jsr TERMQ
    bne FTOUT
    pha                                            ; Save character
    lda #'!'                                       ; Escape in char
    jsr OUT
    pla
    jmp FTOUT

INCHP
    jsr NEXTAA
    bcc FOOT
    rts


    ; **** Print a number
PNUM
    ldy #2
    lda (ARG),y
    sta ACL
    iny
    lda (ARG),y
    sta ACL+1
    bpl PINT
    lda #'-'
    jsr OUT
    ldx #ACL
    ldy #0
    jsr MD

    ; **** Actual number printer
PINT
    ldx #5
    lda #0
CLDIV
    sta NBUFF,x
    dex
    bpl CLDIV
    sta XTNDL
    sta XTNDL+1
    sta AUXL+1
    lda #$0a
    sta AUXL
NOMSIN
    jsr DIV
    lda XTNDL
    sty XTNDL
    ora #$30
    ldx NIND
    sta NBUFF,x
    inc NIND
    lda ACL
    ora ACL+1
    bne NOMSIN
    ldx #4
MDIGP
    lda NBUFF,x
    beq NDP
    jsr OUT
NDP
    dex
    bpl MDIGP
    rts


    ; **** Finds end of char atom
GENDS
    lda ARG
    ldy #1                                         ; Char atom
    clc
    adc (ARG),y                                    ; AA +1 TO AB -1
    dey
    sta AB
    lda ARG+1
    adc #0
    sta AB+1
    lda #5
    adc ARG
    sta AA
    lda ARG+1
    adc #0
    sta AA+1
    rts


    ; *** EOF
EOF
    jsr ALLNUM
    ldy #2                                         ; Get file handle
    lda (ARGA),y
    tay                                            ; Y=file handle
    ldx #ACL                                       ; Put pointer into ACL; X=zero page address for result
    lda #0
    jsr osargs                                     ; Get sequential file pointer into zero page address X (A=0)
    ldx #ARGINF                                    ; File extent; X=zero page address for result
    lda #2
    jsr osargs                                     ; Get length of file into zero page address X (A=2)
    ldx #2
EOFCP
    lda ACL,x
    cmp ARGINF,x
    bne EOFNO
    dex
    bpl EOFCP
    jmp YES

EOFNO
    jmp NO


    ; **** CLOSE
CLOS
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
    tay
    lda #osfind_close                              ; Close file (A lost)
    jsr osfind                                     ; Close one or all files
    jmp NO


    ; ******************************
    ; File with CAR, CDR and COND
    ; functions.  Includes PROGN,
    ; PROGNA and POINTN as useful
    ; entry points.
    ; ******************************
PARERR
    brk                                            ; COND syntax

    !byte $0d
    !text "COND syntax"
    !byte 0


    ; **** COND
COND
    lda WSA+1
    bne REMAIN
    jmp POPA

REMAIN
    jsr NXTARH
    ldy #0
    lda (ARG),y
    bpl PARERR
    ldy #4                                         ; Split:
    lda (ARG),y                                    ; Condition - ARG
    sta WSB+1                                      ; Actions   - WSB
    dey                                            ; Rest      - WSA
    lda (ARG),y
    sta WSB
    dey
    lda (ARG),y
    tax
    dey
    lda (ARG),y
    stx ARG+1
    sta ARG
    jsr EVALU
    lda ARG+1                                      ; Condition NIL?
    beq COND                                       ; Next condition duo
    lda WSB+1
    beq PRGEND
    sta WSA+1
    lda WSB
    sta WSA
MTODO
    jsr NXEVAM
PROGN
    lda WSA+1
    bne MTODO
PRGEND
    jmp POP


    ; **** UNTIL
UNTIL
    jsr NXEVAL
    lda ARG+1
    beq PRGEND
    bne WILLY

    ; **** WHILE
WHILE
    jsr NXEVAL
    lda ARG+1
    bne PRGEND
    beq WILLY
WILL
    jsr NXEVAM
WILLY
    lda WSA+1
    bne WILL
    sta REP
    jmp POP


    ; **** QUOTE
QUO
    jsr NXTARG
    jmp POP


    ; ******************************
    ; Now the CAR - CDR complex
    ; ******************************
PERR
    brk                                            ; Atomic arg

    !byte $0e
    !text "CAR/CDR of atom"
    !byte 0

CDDDR
    ldx #$1f
    bne CXR
CADDR
    ldx #$1b
    bne CXR
CDADR
    ldx #$17
    bne CXR
CAADR
    ldx #$13
    bne CXR
CDDAR
    ldx #$0f
    bne CXR
CDDR
    ldx #$0e
    bne CXR
CADAR
    ldx #$0b
    bne CXR
CADR
    ldx #$0a
    bne CXR
CDAAR
    ldx #7
    bne CXR
CDAR
    ldx #6
    bne CXR
CDR
    ldx #5
    bne CXR
CAAAR
    ldx #3
    bne CXR
CAAR
    ldx #2
    bne CXR
CAR
    ldx #1
CXR
    lda ARGA
    sta ARG
    lda ARGA+1
    sta ARG+1
CXLP
    cmp #0
    beq PERR
    ldy #0
    lda (ARG),y
    bpl PERR
    ldy CXXXR,x
    lda (ARG),y
    pha
    dey
    lda (ARG),y
    sta ARG
    pla
    sta ARG+1
    dex
    ldy CXXXR,x
    bne CXLP
    jmp POP


    ; **** ERROR
ERRORL
    jsr PRINTE
LISPER
    brk

    !byte $0f
    !text "ERROR function"
    !byte 0


    ; **** AND
_AND
    lda WSA+1
    bne ANDON
    jmp YES

ANDON
    jsr NXEVAM
    lda ARG+1
    bne _AND
    jmp NO


    ; **** OR
OR
    lda WSA+1
    bne ORON
    jmp NO

ORON
    jsr NXEVAM
    lda ARG+1
    beq OR
    jmp YES


    ; **** LOOOP
LOOP
    lda WSA
    sta WSC
    lda WSA+1
    sta WSC+1
RESTAR
    lda WSC
    sta WSA
    lda WSC+1
    sta WSA+1
RIPON
    lda WSA+1
    beq RESTAR
    sta REP
    jsr NXEVAM
    lda REP
    bne RIPON
    lda #$ff
    sta REP
    jmp POP


    ; ******************************
    ; Extra LISP entry points
    ; ******************************
    ; **** CONS
CONS
    jsr ALPAIR
    ldy #4
CONSLP
    lda WSC+1,y
    sta (POINT),y
    dey
    bne CONSLP
    lda POINT
    sta ARG
    lda POINT+1
    jmp POPA


    ; **** SET
SET
    jsr NXEVAL
    jmp DOSET


    ; **** SETQ
SETQ
    jsr NXTARG
DOSET
    lda ARG+1
    beq SETERR
    ldy #0
    lda (ARG),y
    bne SETERR
    jsr SAVAR
    jsr NXEVAL
    ldy #2                                         ; Alter value cell
    lda ARG
    sta (WSB),y
    iny
    lda ARG+1
    sta (WSB),y
    jmp POP

SETERR
    brk                                            ; SET non-atomic

    !byte $10
    !text "Bad assignment"
    !byte 0


    ; **** LIST
LIST
    lda #0
    sta ARG+1
    ldx TVSEXT
    cpx #$0c
    bcc NOLLY
LL
    stx YSAV
    jsr ALPAIR
    ldx YSAV
    ldy #4
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    dex
    dey
    lda TVS,x
    sta (POINT),y
    dex
    dey
    lda TVS,x
    sta (POINT),y
    lda POINT
    sta ARG
    lda POINT+1
    sta ARG+1
    cpx #$0c
    bcs LL
NOLLY
    jmp POP


    ; **** VDU
VDU
    jsr ALLNUM
    ldx #$0a
MVDU
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    ldy #2
    lda (ARG),y                                    ; LS byte of arg
    jsr oswrch                                     ; Write character
    inx
    inx
    cpx TVSEXT
    bcc MVDU
    jmp POP                                        ; Result is ARG


    ; ******************************
    ; Conditionals on numeric atoms
    ; ******************************
    ; **** CHARP
CHARP
    ldx ARGA+1
    beq YESNIL
    ldy #0
    lda (ARGA),y
    bne NNO
YESNIL
    jmp YES


    ; **** SUBRP
SUBRP
    lda #SUBRF
    bne TYPE

    ; **** FSUBRP
FSUBRP
    lda #FSUBRF
    bne TYPE

    ; **** LISTP
LISTP
    lda #LISTF
    bne TYPE

    ; **** NUMBERP
NUMP
    lda #NUMF
TYPE
    ldx ARGA+1
    beq NNO
    ldy #0
    cmp (ARGA),y
    bne NNO
    jmp YES

NNO
    jmp NO


    ; **** ZEROP
ZEROP
    ldx #0
    beq TSN

    ; **** ONEP
ONEP
    ldx #1
TSN
    lda ARGA+1
    beq NNO                                        ; CHECK IT IS NUM.
    ldy #0
    lda (ARGA),y
    cmp #NUMF
    bne NNO
    ldy #2
    txa
    eor (ARG),y
    iny
    ora (ARG),y
    bne NNO
    jmp YES


    ; **** MINUSP
MINUSP
    lda ARGA+1
    beq NNO
    ldy #0
    lda (ARGA),y
    cmp #NUMF
    bne NNO
    ldy #3
    lda (ARG),y
    bpl NNO
    jmp YES


    ; ******************************
    ; Set up DCB for file I/O
    ; ******************************
SETDCB
    jsr MKNAM                                      ; Pointer to name
    lda #osbyte_read_high_order_address            ; m/c higher order addr
    jsr osbyte                                     ; Read the filing system 'machine high order address'
    lda #0
    sta OSINFO+6
    lda #AREEXT                                    ; Load & save addr
    sta OSINFO+2
    sta OSINFO+10
    lda IMBOT
    sta OSINFO+3
    sta OSINFO+11
    stx OSINFO+4
    stx OSINFO+12
    sty OSINFO+5
    sty OSINFO+13
    lda AREVAL                                     ; End addr
    sta OSINFO+14
    lda AREVAL+1
    sta OSINFO+15
    stx OSINFO+16
    sty OSINFO+17
    rts                                            ; DCB complete


    ; **** LOAD
LOAD
    jsr SETDCB
    lda #osfile_load                               ; Load operation
    ldx #<(OSINFO)
    ldy #>(OSINFO)
    jsr osfile                                     ; Load named file (if XY+6 contains 0, use specified address) (A=255)
INUREL
    lda #AREEXT                                    ; Set up base
    sta RELBS
    lda IMBOT
    sta RELBS+1
    jsr UNREL
    jmp INIT                                       ; Re-start LISP


    ; **** DUMP
DUMP
    jsr RUBBSH                                     ; GC
    jsr SETDCB                                     ; Set up before relat
    jsr RELAT                                      ; Make relocatable
    lda #osfile_save                               ; Save
    ldx #<(OSINFO)
    ldy #>(OSINFO)
    jsr osfile                                     ; Save a block of memory (returning file length and attributes) (A=0)
    jmp POP

CHARQ
    lda ARGA+1
    beq FILERR
CHARQR
    ldy #0
    lda (ARGA),y
    bne FILERR
    rts

FILERR
    brk                                            ; Not char atom

    !byte $11
    !text "Character atom expected"
    !byte 0

ATOA
    lda ARGA
    sta ARG
    lda ARGA+1
    sta ARG+1
    rts

ANUM
    jsr ALLNUM
    ldy #3
    lda (ARGA),y
    sta ACL+1
    dey
    lda (ARGA),y
    sta ACL
    rts


    ; **** CALL
CALL
    jsr ANUM
    lda #>BACALL+1
    pha                                            ; Set up return
    lda #<BACALL+1                                 ; address
    pha
    lda (ARGB),y                                   ; Get A
    jmp (ACL)                                      ; and call


    ; **** PEEK
PEEK
    jsr ANUM
    ldy #0
BACALL
    lda (ACL),y
BECALM
    sta ACL
    lda #0
    sta ACL+1
    jmp ACLRET


    ; **** POKE
POKE
    jsr ANUM
    lda (ARGB),y
    ldy #0
    sta (ACL),y
    lda ARGB
    sta ARG
    lda ARGB+1
    jmp POPA


    ; OBLIST
OBLIST
    jsr SETCEL                                     ; Only want good
    ldy #0                                         ; cells
    sty ARG+1
BOOM
    lda (CELL),y
    bne NEXOS
    jsr USEFUL
    beq NEXOS                                      ; Value UNDEFINED?
    lda CELL
    sta WSA
    lda CELL+1
    sta WSA+1
    jsr ALPAIR
    ldy #4
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    dey
    lda WSA+1
    sta CELL+1
    sta (POINT),y
    dey
    lda WSA
    sta CELL
    sta (POINT),y
    lda POINT+1
    sta ARG+1
    lda POINT
    sta ARG
NEXOS
    jsr NXCELL
    bcc BOOM
    jmp POP

USEFUL
    ldy #2                                         ; Checks for
    lda (CELL),y                                   ; string rather than
    cmp #<ZA                                       ; OBLIST atoms
    bne USABLE
    iny
    lda (CELL),y
    cmp IMBOT
    bne USABLE
    ldy #5                                         ; NIL P-list
    lda (CELL),y
USABLE
    rts


    ; **** Make DOS name
MKNAM
    jsr CHARQ                                      ; FOR DOS
    lda #<NAMBUF
    sta OSINFO
    lda #>NAMBUF
    sta OSINFO+1
    ldy #1
    lda (ARGA),y                                   ; Get name length
    tay
    lda #$0d                                       ; Terminator
MORLET
    sta OSWBUF-6,y
    dey
    cpy #6
    bcc USABLE
    lda (ARGA),y
    bcs MORLET                                     ; Always taken

    ; **** *
STAR
    jsr CHARQ
    ldy #1
    lda (ARGA),y
    cmp #$3e
    bcc SMALLP
    lda #$3e
SMALLP
    tay
    lda #$0d
MORLEZ
    sta DOSBUF-6,y
    dey
    cpy #6
    bcc STARRY
    lda (ARGA),y
    bcs MORLEZ
STARRY
    ldx #<(DOSBUF)
    ldy #>(DOSBUF)
    jsr oscli
    jmp NO


    ; **** OPEN
OPE
    jsr MKNAM
    lda ARGB+1
    cmp #1
    bcc WOPEN
    lda #$c0                                       ; OPEN FOR READ
    jmp ROPEN

WOPEN
    lda #osfind_open_output                        ; OPEN FOR WRITE
ROPEN
    ldx OSINFO
    ldy OSINFO+1
    jsr osfind                                     ; Open file for output (A=128)
    cmp #0
    beq FNERR
    sta ACL
    lda #0
    sta ACL+1
    jmp ACLRET                                     ; RESULT IN A

FNERR
    brk                                            ; File not found

    !byte $d6
    !text "File not found"
    !byte 0


    ; **** Unrelativise
UNREL
    ldy #5
    lda (RELBS),y                                  ; Get old IMBOT value from UNDEFINED
    sec
    sbc IMBOT
    sta RELOFF                                     ; Relativisation constant
    ldy #0
    lda (RELBS),y                                  ; Set up slave
    sta AREVAL
    iny
    lda (RELBS),y                                  ; Length of IMAGE
    sec
    sbc RELOFF
    sta AREVAL+1
    sta (RELBS),y                                  ; Real end of IMAGE
    lda #2                                         ; Get first item
    bpl NXITU
NEXTU
    ldy #0
    lda (RELBS),y                                  ; Sort out type
    cmp #CHARF
    beq URC
    cmp #SUBRF
    beq URS
    cmp #FSUBRF
    beq URS
    cmp #NUMF
    beq URN
    ldy #2                                         ; Must be dotted pair
    jsr ALTADU
    ldy #4
    jsr ALTADU
    lda #5                                         ; Length
    bpl NXITU
URN
    lda #4                                         ; Number
    bpl NXITU                                      ; No alteration
URC
    jsr TWOADD                                     ; Character
    ldy #1
    lda (RELBS),y
    jmp NXITU

URS
URF
    jsr TWOADD                                     ; Subr/Fsubr
    lda #6
    bpl NXITU
TWOADD
    ldy #3
    jsr ALTADU
    ldy #5
ALTADU
    lda (RELBS),y                                  ; Adjust addr
    beq NOTU                                       ; NIL - needn't alter
    sec
    sbc RELOFF
    sta (RELBS),y
NOTU
    rts

NXITU
    jsr NXIT
    bcc NEXTU
    rts

NXIT
    clc                                            ; Step to next item
    adc RELBS
    sta RELBS
    lda #0
    adc RELBS+1
    sta RELBS+1
    cmp AREVAL+1                                   ; End of image
    bcc OKOUT
    bne NOTOK
    lda RELBS
    cmp AREVAL
    bcc OKOUT
NOTOK
    sec
    rts                                            ; End reached

OKOUT
    clc
    rts                                            ; End not reached


    ; **** RELAT (somewhat reduced!)
RELAT
    lda #AREEXT
    sta RELBS                                      ; Reset base
    lda IMBOT
    sta RELBS+1
    ldy #0
    lda AREVAL
    sta (RELBS),y
    iny
    lda AREVAL+1
    sta (RELBS),y
    rts


    ; **** RECLAIM
RECLAM
    jsr RUBBSH                                     ; Force G.C.
    jmp NO


    ; **** QUOTIENT
QUOT
    ldx #ACL
    bne DODO

    ; **** REMAINDER
REM
    ldx #XTNDL
DODO
    stx XPR                                        ; Save index for
    jsr ALLNUM                                     ; returned number
    ldy #0
    sty XTNDL                                      ; Zero initial rem.
    sty XTNDL+1
    jsr ALNUM                                      ; Have cell ready
    ldy #2
    lda (ARGA),y                                   ; Move ARG"s" to
    sta ACL                                        ; workspace
    lda (ARGB),y
    sta AUXL
    iny
    ora (ARGB),y
    beq OVFERR                                     ; Divide by zero!
    lda (ARGA),y
    sta ACL+1
    lda (ARGB),y
    sta AUXL+1
    jsr DIVPM                                      ; Divide
    lsr SIGN                                       ; Check sign
    bcc POSV
    ldx #ACL                                       ; Change ACL sign
    jsr MD
POSV
    ldy #2                                         ; RETREIVE RESULT
    ldx XPR
    lda GCNO,x
    sta (POINT),y                                  ; Put in new cell
    iny
    lda GCNO+1,x
    sta (POINT),y
    jmp AMADE                                      ; return POINT


    ; **** DIFFERENCE
DIFF
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda (ARGA),y
    sec
    sbc (ARGB),y
    sta (POINT),y
    iny
    lda (ARGA),y
    sbc (ARGB),y
    bvs OVFERR
    bvc FINONE

    ; **** MINUS
MINUS
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda #0
    sec
    sbc (ARGA),y
    sta (POINT),y
    iny
    lda #0
    sbc (ARGA),y
FINONE
    sta (POINT),y
    jmp AMADE


    ; **** SUB1
SUBA
    jsr ALLNUM
    jsr ALNUM
    lda #$ff
    clc
    bcc WONE

    ; ADD1
ADDA
    jsr ALLNUM
    jsr ALNUM
    lda #0
    sec
WONE
    pha
    ldy #2
    adc (ARGA),y
    sta (POINT),y
    iny
    pla
    adc (ARGA),y
    bvc FINONE
OVFERR
    brk                                            ; Overflow

    !byte $12
    !text "Arithmetic overflow"
    !byte 0


    ; **** PLUS
PLUS
    jsr ALLNUM
    jsr ALNUM
    lda #0
    ldy #2
    sta (POINT),y
    iny
    ldx TVSEXT
    bne PEX
MPLUS
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    ldy #2
    clc
    lda (POINT),y
    adc (ARG),y
    sta (POINT),y
    iny
    lda (POINT),y
    adc (ARG),y
    bvs OVFERR
PEX
    sta (POINT),y
    dex
    dex
    cpx #$0a
    bcs MPLUS
    jmp AMADE


    ; **** TIMES
TIMES
    jsr ALLNUM
    ldx #1
    stx ACL
    dex
    stx ACL+1
    ldx TVSEXT
    bne TREX
MTIMES
    lda #0
    sta XTNDL
    sta XTNDL+1
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    ldy #2
    lda (ARG),y
    sta AUXL
    iny
    lda (ARG),y
    sta AUXL+1
    stx XPR
    jsr MULPM
    lda XTNDL
    ora XTNDL+1
    bne OVFERR
    lda ACL+1
    bmi OVFERR
    ror SIGN
    bcc POSITV
    ldx #ACL
    jsr MD
POSITV
    ldx XPR
TREX
    dex
    dex
    cpx #$0a
    bcs MTIMES
ACLRET
    jsr ALNUM
    ldy #2
    lda ACL
    sta (POINT),y
    iny
    lda ACL+1
    sta (POINT),y
    jmp AMADE


    ; **** CHARS
CHARS
    jsr ALNUM
    ldx ARGA+1
    beq XGOOD
    ldx #4                                         ; List index
    ldy #0
    lda (ARGA),y
    bmi XGOOD
    bne FIXTY
    iny
    lda (ARGA),y
    sec
    sbc #6
    bcs AGOT
FIXTY
    lsr
    lsr
    tax
XGOOD
    lda LENTAB,x
AGOT
    ldy #2
    sta (POINT),y
    iny
    lda #0
    sta (POINT),y
    jmp AMADE

GPLIST
    jsr CHARQ
    ldy #4
    lda (ARGA),y
    sta ARG
    iny
    lda (ARGA),y
    jmp POPA


    ; **** RPLACA
RPLACA
    ldy #1
    bne PLAQ

    ; **** RPLACD
RPLACD
    ldy #3
PLAQ
    lda ARGA+1
    beq PLAQER
    ldx #0
    lda (ARGA,x)
    bpl PLAQER
    lda ARGB
    sta (ARGA),y
    iny
    lda ARGB+1
    sta (ARGA),y
    jsr ATOA
    jmp POP

PLAQER
    brk                                            ; Wrong 1st arg

    !byte $13
    !text "RPLACA/RPLACD argument"
    !byte 0


    ; **** LESSP
LESSP
    ldx #1
SWIP
    lda ARGA,x
    ldy ARGB,x
    sta ARGB,x
    sty ARGA,x
    dex
    bpl SWIP

    ; **** GREATERP
GT
    jsr ALLNUM
    ldy #3
    lda (ARGB),y
    cmp #$80
    eor (ARGA),y
    bmi DFSGN
    dey
    lda (ARGB),y
    cmp (ARGA),y
    iny
    lda (ARGB),y
    sbc (ARGA),y
    bcs SMALLR
BIGGER
    jmp YES

DFSGN
    bcs BIGGER
SMALLR
    jmp NO

SOCK
    ldy #4
    lda (ARGA),y
    sta WSB
    iny
    lda (ARGA),y
    sta WSB+1                                      ; A-list search
SOCKA
    bne LKG                                        ; routine
    rts

ROCKON
    ldy #3
    lda (WSB),y
    tax
    iny
    lda (WSB),y
    bne LKH
    rts

LKH
    sta WSB+1
    stx WSB
LKG
    ldx #0
    ldy #1
    lda (WSB,x)
    bpl ALERR
    lda (WSB),y
    sta WSD
    iny
    lda (WSB),y
    sta WSD+1
    lda (WSD,x)
    bpl ALERR
    lda ARGB+1
    cmp (WSD),y
    bne ROCKON
    dey
    lda ARGB
    cmp (WSD),y
    bne ROCKON
    lda #$ff
    rts

ALERR
    brk                                            ; P-list error

    !byte $14
    !text "P-list structure"
    !byte 0


    ; **** ASSOC
ASSOC
    lda ARGB
    pha
    sta WSB
    lda ARGB+1
    pha
    sta WSB+1
    lda ARGA                                       ; Fudge arg order
    sta ARGB
    lda ARGA+1
    sta ARGB+1
    jsr SOCKA
    cmp #1                                         ; Save ret code as carry
    pla
    sta ARGB+1
    pla
    sta ARGB
    lda #0
    bcc HAUSE                                      ; If SOCKA returned zero
    lda WSD
    sta ARG
    lda WSD+1
HAUSE
    sta ARG+1
    jmp POP


    ; **** GET
GET
    jsr CHARQ
    jsr SOCK
    beq HAUSE
    ldy #3
    lda (WSD),y
    sta ARG
    iny
    lda (WSD),y
    sta ARG+1
    jmp POP


    ; **** PUT
PUT
    jsr CHARQ
    jsr SOCK
    beq INSERP
    ldy #3
    lda ARGC
    sta (WSD),y
    sta ARG
    iny
    lda ARGC+1
    sta (WSD),y
    jmp POPA

INSERP
    jsr ALPAIR
    lda POINT
    sta WSB
    lda POINT+1
    sta WSB+1
    jsr ALPAIR
    ldy #5
    lda (ARGA),y
    dey
    sta (WSB),y
    lda ARGC+1
    sta ARG+1
    sta (POINT),y
    lda (ARGA),y
    dey
    sta (WSB),y
    lda ARGC
    sta ARG
    sta (POINT),y
    dey
    lda ARGB+1
    sta (POINT),y
    lda POINT+1
    sta (WSB),y
    dey
    lda ARGB
    sta (POINT),y
    lda POINT
    sta (WSB),y
    ldy #5
    lda WSB+1
    sta (ARGA),y
    dey
    lda WSB
    sta (ARGA),y
    jmp POP


    ; **** REMPROP
REMPR
    jsr CHARQ
    jsr SOCK
    beq HOUSE
    ldy #3
    lda (WSB),y
    sta WSC
    iny
    lda (WSB),y
    sta WSC+1
    ldy #5
FRUIT
    lda (ARGA),y
    tax
    cmp WSB+1
    bne FRUT
    dey
    lda (ARGA),y
    cmp WSB
    bne FRUTA
    lda WSC
    sta (ARGA),y
    iny
    lda WSC+1
    sta (ARGA),y
    jmp YES

FRUT
    dey
    lda (ARGA),y
FRUTA
    stx ARGA+1
    sta ARGA
    ldy #4
    bne FRUIT
HOUSE
    jmp NO


    ; **** Check all numeric args
ALLNUM
    lda #NUMF
    ldx TVSEXT
    bne COMPX
MORIX
    ldy TVS+1,x
    beq NUER
    cmp (TVS,x)
    bne NUER
COMPX
    dex
    dex
    cpx #$0a
    bcs MORIX
    rts

NUER
    brk                                            ; Non-numeric args

    !byte $15
    !text "Non-numeric argument"
    !byte 0

EVAL
    jsr ATOA
    jsr EVALU
    jmp POP


    ; ******************************
    ; APPLY/MAP complex
    ; ******************************
APFUN
    lda ARGA+1
    beq APFERR
    ldy #0
    sty WSB+1
    lda (ARGA),y
    bmi GEVIL
    cmp #SUBRF
    beq GFRTS                                      ; It's a subr
APFERR
    brk

    !byte $16
    !text "APPLY argument"
    !byte 0

GEVIL
    iny                                            ; Expr?
    lda (ARGA),y
    cmp #<LAMBDA
    bne APFERR
    iny
    lda (ARGA),y
    cmp IMBOT
    bne APFERR
    iny
    lda (ARGA),y
    sta WSD
    iny
    lda (ARGA),y
    sta WSD+1
    beq APFERR
    ldy #0
    lda (WSD),y
    bpl APFERR
    iny
    lda (WSD),y                                    ; Parm list in WSB
    sta WSB
    iny
    lda (WSD),y
    sta WSB+1
    beq APFERR
    iny
    lda (WSD),y
    sta WSC                                        ; Body in WSC
    iny
    lda (WSD),y
    sta WSC+1
    ldy #0
    lda (WSB),y                                    ; Fexpr?
    bpl APFERR
    rts

GFRTS
    lda ARGA
    sta WSC
    lda ARGA+1
    sta WSC+1
    rts

APERR
    brk                                            ; APPLY list

    !byte $17
    !text "APPLY arguments"
    !byte 0


    ; **** APPLY
APPLY
    jsr APFUN
    ldx #$0a
    lda ARGB+1
    beq APGO
    sta WSA+1
    lda ARGB
    sta WSA
APL
    ldy #0
    lda (WSA),y
    bpl APERR
    iny
    lda (WSA),y
    sta TVS,x
    iny
    inx
    lda (WSA),y
    sta TVS,x
    inx                                            ; Spread args into TVS
    cpx #$42
    bcs APERR
    ldy #4
    lda (WSA),y
    beq APGO
    pha
    dey
    lda (WSA),y
    sta WSA
    pla
    sta WSA+1
    bne APL                                        ; Always taken
APGO
    stx TVSEXT
APGOB
    lda WSB+1
    beq SUBAP
    jmp RADON                                      ; Expr entry

SUBAP
    jmp INSUBR                                     ; Subr entry

MAPSTR
    jsr APFUN                                      ; Prelims for
    ldx #$0c                                       ; MAP functions
MAPMOV
    lda TVS,x
    sta TVS-2,x
    inx
    lda TVS,x
    sta TVS-2,x
    beq MAPRTS
    inx
    cpx TVSEXT
    bcc MAPMOV
    dex
    dex
    stx TVSEXT
MAPRTS
    rts

NILMAP
    jmp POPA


    ; **** MAP
MAP
    jsr MAPSTR
    beq NILMAP
MAPON
    jsr CARAP
    jsr CDRALL
    bne MAPON
    jmp NO


    ; *** MAPC
MAPCAR
    jsr MAPSTR
    beq NILMAP
    jsr ALPAIR
    ldy #4
    lda POINT+1
    sta WSA+1
    sta (POINT),y
    lda POINT
    sta WSA
    dey
    sta (POINT),y
MAPCON
    jsr CARAP
    jsr ALPAIR
    ldy #2
    lda (WSA),y
    bne OLDMAP
    lda POINT+1
    sta (WSA),y
    lda POINT
    dey
    sta (WSA),y
OLDMAP
    ldy #4
    lda (WSA),y
    sta WSD+1
    dey
    lda (WSA),y
    sta WSD
    lda POINT
    sta (WSD),y
    sta (WSA),y
    iny
    lda POINT+1
    sta (WSD),y
    sta (WSA),y
    ldy #2
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    jsr CDRALL
    bne MAPCON
    ldy #1
    lda (WSA),y
    sta ARG
    iny
    lda (WSA),y
    jmp POPA


    ; **** GETCHAR
GETCHA
    ldx TVSEXT                                     ; File arg?
    cpx #$0c
    bcc GETDIR
    jsr FILGC
    jsr GTCHAR
    jmp POST

GETDIR
    jsr osrdch                                     ; Grab a char; Read a character from the current input stream
    bcc POST
    brk

    !byte $1c
    !text "Escape"
    !byte 0


    ; **** CHARACTER
ASCII
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
POST
    jsr LETTER                                     ; Finds char atom
    jmp AMADE


    ; **** ORDINAL
ORDINL
    jsr CHARQ
    jsr ALNUM
    ldy #1
    lda (ARGA),y
    sec
    sbc #6
    beq EMPTYC
    ldy #6
    lda (ARGA),y
EMPTYC
    jmp AGOT

CARAP
    jsr STACK
    ldx TVSEXT
    bne CARAFE
CARAVA
    dex
    lda TVS,x
    sta WSD+1
    beq MAPERR
    lda TVS-1,x
    sta WSD
    ldy #0
    lda (WSD),y
    bpl MAPERR
    iny
    lda (WSD),y
    sta TVS-1,x
    iny
    lda (WSD),y
    sta TVS,x
    dex
CARAFE
    cpx #$0c
    bcs CARAVA
    jmp APGOB

MAPERR
    brk                                            ; MAP args

    !byte $18
    !text "MAP/MAPC arguments"
    !byte 0

CDRALL
    ldx TVSEXT
    bne CDRAWL
CDRAVA
    dex
    lda TVS,x
    sta WSD+1
    lda TVS-1,x
    sta WSD
    ldy #0
    lda (WSD),y
    bpl MAPERR
    ldy #4
    lda (WSD),y
    beq CDREND
    sta TVS,x
    dex
    dey
    lda (WSD),y
    sta TVS,x
CDRAWL
    cpx #$0c
    bcs CDRAVA
CDREND
    rts


    ; **** EXPLODE
EXPLOD
    jsr CHARQ
    sty ARG+1
    iny
    lda (ARGA),y
    bne BANG
THUMP
    sbc #1
    pha
    jsr ALPAIR
    ldy #4
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    lda POINT+1
    sta ARG+1
    lda POINT
    sta ARG
    pla
    pha
    tay
    lda (ARGA),y
    jsr LETTER
    ldy #2
    lda POINT+1
    sta (ARG),y
    dey
    lda POINT
    sta (ARG),y
    pla
BANG
    cmp #7
    bcs THUMP
    jmp POP

IMPERR
    brk                                            ; No space

    !byte $19
    !text "No room for IMPLODE"
    !byte 0


    ; **** IMPLODE
IMPLOD
    ldx #0
    lda ARGA+1
    jmp WSBTST

SQUISH
    iny
    lda (ARGA),y
    sta WSB
    iny
    lda (ARGA),y
    bne NODNIL
    lda #<NIL
    sta WSB
    lda IMBOT
NODNIL
    sta WSB+1
    ldy #0
    lda (WSB),y
    beq ISCH
    jmp FILERR                                     ; Not char err

ISCH
    iny
    lda (WSB),y
    sta YSAV
    ldy #6
    bne EMM
SPLOT
    lda (WSB),y
    sta OSWBUF,x
    iny
    inx
    beq IMPERR
EMM
    cpy YSAV
    bcc SPLOT
    ldy #4
    lda (ARGA),y
    pha
    dey
    lda (ARGA),y
    sta ARGA
    pla
    sta ARGA+1
WSBTST
    beq IMPRET
    ldy #0
    lda (ARGA),y
    bmi SQUISH
IMPRET
    dex                                            ; Show string length
    stx END
    jmp TRYCHR


    ; **** MESSON
MESSON
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
    ora LEVEL
    sta LEVEL
    jmp POP


    ; **** MESSOFF
MESSOF
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
    eor #$ff
    and LEVEL
    sta LEVEL
    jmp POP


    ; **** MODE
MODE
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda (ARGA),y                                   ; Get mode
    and #7                                         ; Mod 8
    sta (POINT),y
    ora #$80                                       ; Flag bit
    sta MODEF
    iny
    lda #0
    sta (POINT),y
    jmp AMADE


    ; **** Actually change mode
MODCHN
    lda MODEF
    and #7
    sta MODEF                                      ; Clear flag bit
    pha
    lda #osbyte_read_high_order_address
    jsr osbyte                                     ; M/c HO addr.; Read the filing system 'machine high order address'
    cpy #$ff
    bne CHANGE                                     ; In 2nd processor?
    pla
    pha
    tax                                            ; X=MODE number
    lda #osbyte_read_himem_for_mode
    jsr osbyte                                     ; Get new aretop; Read top of user memory for a given screen mode X
    cpy AREVAL+1                                   ; Room for OBLIST?
    bcc MODERR
    bne CHARE
    cpx AREVAL
    bcs CHARE
MODERR
    brk                                            ; No room

    !byte $1b
    !text "No room for MODE "
    !byte 0

CHARE
    sty ARETOP                                     ; New ARETOP
    sty SP+1                                       ; Reset software stack
CHANGE
    lda #$16                                       ; Change mode
    jsr oswrch                                     ; Write character 22
    pla
    jmp oswrch                                     ; Write character


    ; **** USR
USR
    jsr ALLNUM
    ldy #3
    lda (ARGA),y
    sta POINT+1                                    ; Call addr
    dey
    lda (ARGA),y
    sta POINT
    lda (ARGE),y                                   ; Get carry
    cmp #1                                         ; And adjust flag
    php
    lda (ARGB),y                                   ; Get A
    pha
    lda (ARGC),y                                   ; Get X
    tax
    lda (ARGD),y                                   ; Get Y
    tay
    pla
    plp
    jsr JUMPAD
    jmp OUTL

JUMPAD
    jmp (POINT)                                    ; Enter routine

OUTL
    php                                            ; Build up result list
    sta WSA+1
    pla
    sta WSA                                        ; Save status
    lda WSA+1                                      ; Restore A
    pha
    txa
    pha
    tya
    pha
    lda #0
    sta WSA+1                                      ; NIL
    lda WSA                                        ; Get status
    jsr BUILD1                                     ; status
    pla
    jsr BUILD1                                     ; Y
    pla
    jsr BUILD1                                     ; X
    pla
    jsr BUILD1                                     ; A
    jmp AMADE                                      ; Return list


    ; **** Put A on list in WSA
BUILD1
    ldx #0

    ; **** Put A & X (msb) on WSA
BUILD2
    jsr SETNUM                                     ; WSB has val
    jsr ALPAIR
    ldy #1
    lda WSB
    sta (POINT),y
    iny
    lda WSB+1
    sta (POINT),y
    iny
    lda WSA
    sta (POINT),y
    iny
    lda WSA+1
    sta (POINT),y
    lda POINT
    sta WSA
    lda POINT+1
    sta WSA+1
    rts

SETNUM
    pha                                            ; Num atom in WSB
    txa
    pha
    jsr ALNUM
    pla
    tax
    pla
    ldy #2
    sta (POINT),y
    txa
    iny
    sta (POINT),y
    lda POINT
    sta WSB
    lda POINT+1
    sta WSB+1
    rts


    ; **** SOUND
SOUND
    jsr ALLNUM
    ldy #2
    ldx #$0a
MORSOU
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    lda (ARG),y
    sta OSWBUF-10,x
    iny
    lda (ARG),y
    sta OSWBUF-9,x
    dey
    inx
    inx
    cpx #$12
    bne MORSOU
    lda #osword_sound
ENTOSW
    ldx #<(OSWBUF)
    ldy #>(OSWBUF)
    jsr osword                                     ; SOUND command
    jmp YES


    ; **** ENVELOPE
ENV
    jsr ALLNUM
    ldy #2
    ldx #$0a
MORENV
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    txa                                            ; Prepare index
    lsr
    tax
    lda (ARG),y
    sta OSWBUF-5,x
    txa
    asl
    tax
    inx
    inx
    cpx #$26
    bne MORENV
    lda #8
    bne ENTOSW
    bne MORENV
    lda #8
    bne ENTOSW

    ; **** Get an argument
GTARG
    lda TVS+10,x
    sta WSA
    inx
    lda TVS+10,x
    sta WSA+1
    inx

    ; ******************************
    ;  Now the clock handling
    ; routines
    ; ******************************
RESET
    jsr STCLK
    jsr GCTIMZ
    jmp YES                                        ; Value is T


    ; **** Zero the clock
STCLK
    lda #osword_write_clock                        ; Write clock
ZERTIM
    ldy #>(TIMZER)
    ldx #<(TIMZER)
    jsr osword                                     ; Write system clock
    ldx #$76                                       ; Restore pointer
    ldy #$82
    rts


    ; **** Zero GC time
GCTIMZ
    lda #0
    ldx #4
MGCTIM
    sta GCTIME,x
    dex
    bpl MGCTIM

    ; **** Read the clock
TIMER
    lda #osword_read_clock
    ldx #<(TIMEW)
    ldy #>(TIMEW)
    jsr osword                                     ; Read the clock; Read system clock
    ldx #<TIMEW                                    ; Restore pointer
    ldy #>TIMEW
    rts


    ; **** TIME
TIME
    jsr TIMER
    jmp TIMPOP


    ; **** GCTIME
GCTIM
    ldx #<GCTIME
    ldy #>GCTIME

    ; **** Return a time
TIMPOP
    stx WSC
    sty WSC+1
    jsr ALNUM
    ldy #0
    lda (WSC),y
    iny
    iny
    sta (POINT),y
    dey
    lda (WSC),y
    iny
    iny
    sta (POINT),y
    jmp AMADE


    ; **** CLOCK
CLOCK
    jsr TIMER                                      ; Time in TIMEW
    ldx #5
    ldy #0
MCLK1
    tya
    sta REM40,x                                    ; Zero remainder
    sta AUX40,x                                    ; Zero divisor
    lda TIMEW,x                                    ; Dividend
    sta ACL40,x
    dex
    bpl MCLK1
    lda #$64                                       ; Throw away cs
    sta AUX40
    jsr DIV40
    lda #0
    sta REM40
    lda #$3c                                       ; Get the seconds
    sta AUX40
    jsr DIV40
    lda #0
    sta WSA+1                                      ; NIL
    lda REM40
    jsr BUILD1
    lda #0
    sta REM40
    lda #$3c                                       ; Get the minutes
    sta AUX40
    jsr DIV40
    lda REM40
    jsr BUILD1
    lda ACL40                                      ; Get the hours
    ldx ACL40+1
    jsr BUILD2
    jmp AMADE


    ; **** POINT
PVAL
    jsr ALLNUM
    ldy #2
    lda (ARGA),y                                   ; X lsb
    sta PWORD
    lda (ARGB),y                                   ; Y msb
    sta PWORD+2
    iny
    lda (ARGA),y                                   ; X lsb
    sta PWORD+1
    lda (ARGB),y                                   ; Y msb
    sta PWORD+3
    lda #osword_read_pixel
    ldx #<(PWORD)
    ldy #>(PWORD)
    jsr osword                                     ; Read pixel value
    jsr ALNUM
    ldy #2
    ldx #0
    lda PWORD+4
    bpl PEXIST                                     ; Point on screen
    dex
PEXIST
    sta (POINT),y
    iny
    txa
    sta (POINT),y
    jmp AMADE


    ; **** ADVAL
ADVAL
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda (ARGA),y
    tax
    iny
    lda (ARGA),y
    tay
    lda #osbyte_read_adc_or_get_buffer_status
    jsr osbyte                                     ; Read buffer status or ADC channel
    tya
    clc
    adc #$80                                       ; Make range +/-2^15
    ldy #3
    sta (POINT),y
    dey
    txa
    sta (POINT),y
    jmp AMADE


    ; **** INKEY (added 18/8/83 RMT)
INKEY
    jsr ALLNUM
    ldy #3
    lda (ARGA),y
    bmi INKEY2
    jsr ALNUM
    ldy #2
    lda (ARGA),y
    tax
    iny
    lda (ARGA),y
    tay
    lda #osbyte_inkey
    jsr osbyte                                     ; Read key within time limit, or read a specific key, or read machine type
    ldy #2
    bcs INKER
    txa
    sta (POINT),y
    lda #0
    iny
    sta (POINT),y
    jmp AMADE

INKER
    lda #$ff
    sta (POINT),y
    iny
    sta (POINT),y
    jmp AMADE

INKEY2
    pha                                            ; Scan keyboard
    dey
    lda (ARGA),y
    tax
    pla
    tay
    lda #osbyte_inkey
    jsr osbyte                                     ; Read key within time limit, or read a specific key, or read machine type
    tya
    bmi INKYES
    jmp NO

INKYES
    jmp YES


    ; **** GENSYM (added 31/8/83 RMT)
GENSYM
    lda #'G'
    sta OSWBUF
    ldx #3
    sec
NXDIGT
    lda GENCNT,x
    adc #0
    cmp #$3a
    bcc NOCAR
    lda #'0'
NOCAR
    sta GENCNT,x
    sta IMBUF+1,x
    dex
    bpl NXDIGT
    ldy #4
    sty END
    jsr MATCH                                      ; Look up Gxxxx
    ldy #2
    lda (POINT),y
    cmp #<ZA                                       ; Is it UNDEFINED ?
    bne GENSYM
    iny
    lda (POINT),y
    cmp IMBOT
    bne GENSYM
    ldy #5
    lda (POINT),y                                  ; No properties ?
    bne GENSYM
    jmp AMADE


    ; ******************************
    ; Useful Boolean routines
    ; ******************************
    ; **** BNOT
BNOT
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda #$ff
    eor (ARGA),y
    sta (POINT),y
    iny
    lda #$ff
    eor (ARGA),y
    sta (POINT),y
    jmp AMADE


    ; **** BAND
BAND
    jsr ALLNUM
    jsr ALNUM
    lda #$ff
    ldy #3
    sta (POINT),y
    dey
    sta (POINT),y
    ldx TVSEXT
MBAND
    dex
    dex
    lda TVS,x
    sta WSA
    lda TVS+1,x
    sta WSA+1
    lda (WSA),y
    and (POINT),y
    sta (POINT),y
    iny
    lda (WSA),y
    and (POINT),y
    sta (POINT),y
    dey
    cpx #$0a
    bne MBAND
    jmp AMADE


    ; **** BOR
BOR
    jsr ALLNUM
    jsr ALNUM
    lda #0
    ldy #3
    sta (POINT),y
    dey
    sta (POINT),y
    ldx TVSEXT
MBOR
    dex
    dex
    lda TVS,x
    sta WSA
    lda TVS+1,x
    sta WSA+1
    lda (WSA),y
    ora (POINT),y
    sta (POINT),y
    iny
    lda (WSA),y
    ora (POINT),y
    sta (POINT),y
    dey
    cpx #$0a
    bne MBOR
    jmp AMADE


    ; ******************************
    ; Garbage collector file.
    ; ******************************
    ; **** Set CELL to image start
SETCEL
    lda #WSBOT
    sta CELL
    lda IMBOT
    sta CELL+1
    rts


    ; **** Clear all G.C. flags
CLEARF
    jsr SETCEL
    ldy #0
RZ
    lda (CELL),y
    and #$fc
    sta (CELL),y
    jsr NXCELL
    bcc RZ
    rts


    ; ******************************
    ; Register a set of cells,
    ; starting from the one in GCA.
    ; ******************************
REGCDR
    lda (GCA,x)
REGPIN
    ror
    bcs REGRTS
    ror
    bcc VIRGIN
    ldy #4
    rol
    sec
    rol
    sta (GCA,x)
    bmi LIN
    iny
LIN
    lda (GCA),y
    beq REGRTS
    tax
    dey
BOT
    lda (GCA),y
    sta GCA
    stx GCA+1
REGIS1
    ldx #0
REGISU
    lda (GCA,x)
    bmi REGPIN
    cmp #4
    bcc REGPIN
    ora #1
    sta (GCA,x)
    ldy #3
    cmp #SUBRF
    bcs LIN
REGRTS
    rts

REGIST
    lda GCA+1                                      ; This code added by RMT
    cmp ARETOP                                     ; 12/9/83 to prevent bug
    bcc REGIS1                                     ; probably caused by ARG
    rts                                            ; being &FFxx

VIRGIN
    clv
    ldy #2
    sec
    rol
    asl
    sta (GCA,x)
    bmi LINJ
    iny
LINJ
    lda (GCA),y
    beq REGCDR
    tax
    dey
    dec l0009
    bpl STOCK
    inc l0009
    beq BOT                                        ; Always taken
STOCK
    lda GCA+1
    pha
    lda GCA
    pha
    lda (GCA),y
    stx GCA+1
    sta GCA
    jsr REGIST
    inc l0009
    pla
    sta GCA
    pla
    sta GCA+1
    jmp REGCDR


    ; **** Garbage Collector entry
RUBBSH
    stx GARX
    sty GARRY
    lda #$ff                                       ; Mark memory invalid
    sta MEMINV
    lda #4                                         ; Start timing
    jsr ZERTIM                                     ; Zero timer
    lda AREVAL
    sta OLDEXT
    lda AREVAL+1
    sta OLDEXT+1
    inc GCNO
    bne HGCA
    inc GCNO+1
HGCA
    lda LEVEL
    and #2
    beq NOMESA
    lda HANDLE
    pha                                            ; Save I/O file handle
    ldx #GCOFF                                     ; 'G.C. no.'
    jsr MESSAH
    lda GCNO
    sta ACL                                        ; Print no. of G.C.'s
    lda GCNO+1
    sta ACL+1
    jsr PINT
    pla
    sta HANDLE
NOMESA
    jsr CLEARF
    lda #$20
    sta l0009                                      ; Stack limit
    ldx TVSEXT
COLLEC
    lda TVS-1,x
    beq SREG                                       ; Register off WSA etc.
CREG
    sta GCA+1
    lda TVS-2,x
    sta GCA
    stx CELL
    jsr REGIST
    ldx CELL
SREG
    dex
    dex
    cpx #6
    bcs COLLEC
    lda ARG+1
    beq STRETH
    sta GCA+1
    lda ARG
    sta GCA
    jsr REGIST
STRETH
    lda SP                                         ; Register off stack
    sta CELL
    lda SP+1
    sta CELL+1
    cmp ARETOP
    bcs LOOKW
STUP
    ldy #0
    lda (CELL),y
    beq BOWDUN
    tay
STCOLL
    jsr SPREG
    bne STCOLL
    lda (CELL),y
BOWDUN
    sec
    adc CELL
    sta CELL
    bcc SPOCK
    inc CELL+1
SPOCK
    lda (CELL),y
    tay
    jsr SPREG
    dey                                            ; Avoid the return addr
    dey
STCOL
    jsr SPREG
    bne STCOL
    lda (CELL),y
    sec
CXH
    adc CELL
    bcc CXHB
    inc CELL+1
CXHB
    sta CELL
    lda CELL+1
    cmp ARETOP
    bcc STUP
LOOKW
    jsr SETCEL                                     ; Now off whole
    bit CXH                                        ; Set overflow
    php
MORWS
    ldx #0
    lda (CELL,x)
    cmp #$82
    beq DOREG
    cmp #2
    beq DOREG                                      ; Register if half
    bcs NOREG                                      ; finished,
    jsr USEFUL
    beq NOREG
    lda (CELL,x)
DOREG
    ldy CELL                                       ; or if char atom
    sty GCA
    ldy CELL+1
    sty GCA+1
    plp
    jsr REGPIN
    php
NOREG
    jsr NXCELL
    bcc MORWS
    plp
    bvc LOOKW                                      ; There's more

    ; ******************************
    ; Having registered the cells,
    ; now to move the memory about
    ;    First calculate the change
    ; in position of all the blocks
    ; ******************************
    jsr SETCEL
    ldy #0
    sty DISPM
    sty DISPM+1
ML
    lda (CELL),y
    lsr
    bcc SETMIN
    jsr NXCELL
    bcc ML
    jmp CLUPB

SETMIN
    lda CELL+1
    sta AD+1
    lda CELL
    sta AD                                         ; Top of static
DLOP
    ldy #2
    lda DISPM
    sta (CELL),y
    iny
    lda DISPM+1
    sta (CELL),y
    lda CELL
    sta AA
    lda CELL+1
    sta AA+1
STFR
    jsr NXCELL
    bcs TADJ
    lda (CELL),y
    lsr
    bcc STFR
    lda CELL
    sta AB
    sbc AA
    tax
    lda CELL+1
    sta AB+1
    sbc AA+1
    tay
    txa
    clc
    adc DISPM
    sta DISPM
    tya
    adc DISPM+1
    sta DISPM+1
    ldy #0
    lda (CELL),y
    lsr
STUSE
    rol
    clc
    bmi SIXG
    cmp #SUBRF
    bcs SIXG
    iny
    lda (CELL),y
    dey
    bcc ADDIN
SIXG
    lda #5
ADDIN
    adc CELL
    sta CELL
    bcc NHCIQ
    ldx CELL+1
    inc CELL+1
    lda DISPM
    sta DL,x
    lda DISPM+1
    sta DH,x
    lda CELL
NHCIQ
    cmp AREVAL
    lda CELL+1
    sbc AREVAL+1
    bcs TADJ
    lda (CELL),y
    lsr
    bcs STUSE
    bcc DLOP
TADJ
    lda DISPM
    sta l0009
    lda DISPM+1
    sta l000a

    ; ******************************
    ; Having calculated the
    ; dispacements, now to alter
    ; the pointers
    ; ******************************
ADJUSS
    lda SP                                         ; Adjust stack
    sta CELL
    lda SP+1
    sta CELL+1
ASP
    ldy #0
    sty GCA
    lda (CELL),y
    beq NOLEAP
    pha
    tay
    jsr AJCELL
    pla
NOLEAP
    sec
    adc CELL
    sta CELL
    bcc LUCK
    inc CELL+1
LUCK
    lda CELL+1
    cmp ARETOP
    bcc ASP
    lda #BINDER                                    ; Adjust workspace
    sta CELL
    lda #0
    sta CELL+1
    ldy TVSEXT
    dey
    dey
    dey
    jsr AJCELL
    lda #TVS-1
    sta CELL
    ldy #2
    jsr AJCELL
    jsr SETCEL
    ldy #0                                         ; Revises pointer over
ADJUT
    lda (CELL),y                                   ; whole of
    lsr
    bcc NXADJ
    ldy #4
    asl
    bmi LICE
    iny
    cmp #SUBRF
    bcs LICE
    cmp #NUMF
    bcs NXADJ
LICE
    jsr AJCELL
NXADJ
    jsr NXCELL
    bcc ADJUT

    ; ******************************
    ; Now move the blocks about
    ; ******************************
    lda AD                                         ; AD has top of static
    sta CELL                                       ; space
    lda AD+1
    sta CELL+1
SAL
    jsr NXCELL
    bcs CLUP
    lda (CELL),y
    lsr
    bcc SAL
    lda CELL
    sta AA
    lda CELL+1
    sta AA+1
MOLE
    jsr NXCELL
    bcs ABGO
    lda (CELL),y
    lsr
    bcs MOLE
ABGO
    lda CELL
    sta AB
    sec
    sbc AA
    sta GCA                                        ; PUT LENGTH IN GCA
    lda CELL+1
    sta AB+1

    ; ******************************
    ; Fast move routine
    ; ******************************
    ldy #0
    sbc AA+1
    beq BITMOV
    sta GCA+1
MORMOV
    lda (AA),y
    sta (AD),y
    iny
    bne MORMOV
    inc AA+1
    inc AD+1
    dec GCA+1
    bne MORMOV
BITMOV
    lda (AA),y
    sta (AD),y
    iny
    cpy GCA
    bcc BITMOV
    bne SAL                                        ; If GCA is zero loop
    tya
    clc
    adc AD
    sta AD
    bcc FINMOV
    inc AD+1
FINMOV
    jmp SAL


    ; ******************************
    ; Now to calculate the gain in
    ; cells etc.
    ; ******************************
CLUP
    lda AD
    sta AREVAL
    lda AD+1
    sta AREVAL+1
CLUPB
    jsr CLEARF
    sec
    lda OLDEXT
    sbc AREVAL
    sta ACL
    sta SA
    lda OLDEXT+1
    sbc AREVAL+1
    sta ACL+1
    sta SA+1
    lda LEVEL
    and #1
    beq NOMESC
    lda HANDLE                                     ; Save file handle
    pha
    lda #0
    sta HANDLE
    jsr CROUT
    lda #'^'
    jsr OUT
    jsr PINT
    ldx #COLOFF
    jsr MESSAG                                     ; ' Bytes collected '
    sec
    lda SP
    sbc AREVAL
    sta ACL
    lda SP+1
    sbc AREVAL+1
    sta ACL+1
    jsr PINT
    ldx #FROFF
    jsr MESSAG                                     ; ' Bytes free<CR>'
    pla
    sta HANDLE                                     ; Restore file handle
NOMESC
    lda #osword_read_interval_timer
    ldx #<(TIMEW)
    ldy #>(TIMEW)
    jsr osword                                     ; Read timer; Read interval timer
    clc
    ldy #0
    ldx #5
MORTIM
    lda TIMEW,y
    adc GCTIME,y
    sta GCTIME,y
    iny
    dex
    bpl MORTIM
    ldx GARX
    ldy GARRY
    lda #0
    sta MEMINV
    lda SA
    ora SA+1
    rts

AJCELL
    lda (CELL),y
    dey
    cmp AD+1
    bcc NXPR
    cmp ARETOP
    bcs NXPR
    cmp AB+1                                       ; Bottom of top block
    bcc ONW
    sta GCA+1
    bne SPECS
    sta SA+1
    lda (CELL),y
    cmp AB
    bcc ONX
    bcs SPECTR
SPECS
    lda (CELL),y
SPECTR
    sbc l0009
    tax
    lda GCA+1
    sbc l000a
    bne CHUGB
ONW
    sta GCA+1
    sta SA+1
    lda (CELL),y
ONX
    tax
    sty GABBY
    tay
    lda (GCA),y
    lsr
GCAON
    rol
    clc
    bmi SIXP
    cmp #SUBRF
    bcs SIXP
    tya
    iny
    beq PAGDIQ
    adc (GCA),y
    bcs PAGDIP
    bcc ADDOUT
SIXP
    tya
    adc #5
    bcs PAGDIP
ADDOUT
    tay
    lda (GCA),y
    lsr
    bcs GCAON
    sty SA
    ldy #2
    txa
    sec
    sbc (SA),y
    tax
    lda GCA+1
    iny
    sbc (SA),y
    bcs CHUG                                       ; Always taken
PAGDIQ
    sec
PAGDIP
    ldy GCA+1
    txa
    sbc DL,y
    tax
    tya
    sbc DH,y
CHUG
    ldy GABBY
CHUGB
    iny
    sta (CELL),y
    dey
    txa
    sta (CELL),y
NXPR
    dey
    cpy #2
    bcs AJCELL
    rts

SPREG
    lda (CELL),y                                   ; Stack register
    beq NULBOW
    sta GCA+1
    sty GABBY
    dey
    lda (CELL),y
    sta GCA
    jsr REGIST
    ldy GABBY
NULBOW
    dey
    dey
    rts


    ; ******************************
    ; The error handler
    ; The stack is "gently" taken
    ; down with diagnostic printout.
    ; ******************************

    ; **** Main error entry
ERRORS
ERROR
    lda #osbyte_acknowledge_escape                 ; Ack escape
    jsr osbyte                                     ; Clear escape condition and perform escape effects
    clc                                            ; Increment error count
    lda ERRCNT
    adc #1
    sta ERRCNT
    lda ERRCNT+1
    adc #0
    sta ERRCNT+1
    lda LEVEL                                      ; Save LEVEL
    sta OLDLEV
    ldy #0
    lda (BRKAD),y                                  ; Get errno.
    sta ERRNO
    sta ACL

    ; **** Traceback wanted?
    ldx #$ff
NEXCAT
    inx
    lda CATTAB,x
    bmi TRCOK                                      ; Allow trackback
    cmp ERRNO
    bne NEXCAT
    lda LEVEL                                      ; Match
    and #$e7
    sta LEVEL                                      ; No traceback
TRCOK
    ldx #$ff
    txs                                            ; Reset stack
    inx
    stx DEPTH
    stx HANDLE
    lda #$0e                                       ; Page mode
    jsr oswrch                                     ; Write character 14
    lda #$18                                       ; Mask for LEVEL
    sta TOPBIN
    lda #4                                         ; Error
    and LEVEL
    beq NERRA
    ldx #ERROFF                                    ; "^Error number "
    jsr MESSAH
    lda #0
    sta ACL+1
    jsr PINT                                       ; Print err no.
NERRA
    lda #$ff
    bit LEVEL                                      ; Err mess mask
    beq NERRB
    jsr PSTRNG                                     ; Print err mess
NERRB
    lda #8                                         ; Top args mask
    and LEVEL
    beq NERRC
    jsr PARG                                       ; Print ARG
NERRC
    lda SP+1
DWARF=NERRC-1
    cmp ARETOP                                     ; Pop return
    bcc WINDER                                     ; stack completely
    lda #$0f
    jsr oswrch                                     ; Page mode off; Write character 15
    lda OLDLEV
    sta LEVEL                                      ; Old LEVEL
    jmp SUPER                                      ; Restart


    ; **** Collapse the stack
WINDER
    ldx #0
    stx ACL+1
    stx WSC+1                                      ; Used bind count
    lda (SP,x)                                     ; Get binding state
    tay
    iny
    clc
    adc (SP),y                                     ; Add work space
    tay
    iny
    lda (SP),y                                     ; To get ARG
    sta WSB+1
    dey                                            ; Put ARG in WSB
    lda (SP),y
    sta WSB
    dey
    lda (SP),y                                     ; Check ret addr for
    dey                                            ;  ERRORSET
    cmp #>ELFIN-1
    bne SLIP
    lda (SP),y                                     ; and low byte
    cmp #<ELFIN-1
    bne SLIP                                       ; Not ERRORSET
    tya
    clc
    adc #4                                         ; Remove ERRORSET
    adc SP                                         ; stack entry
    sta SP
    bcc QZQ
    inc SP+1
QZQ
    lda ERRNO                                      ; Return err no.
    sta ACL
    lda #$0f
    jsr oswrch                                     ; Page mode off; Write character 15
    jmp ACLRET                                     ; POP via ACLRET

SLIP
    lda #<DWARF
    sta (SP),y                                     ; Set up return for POP
    iny
    lda #>DWARF
    sta (SP),y
    lda LEVEL                                      ; Check trace print
    and TOPBIN                                     ; is this second arg
    beq NOPE                                       ; Skip rint
    and #$10                                       ; Clear top arg flag
    sta TOPBIN
    beq BINS
    lda (SP,x)                                     ; Any bound vars?
    beq BINS

    ; **** Do bound var pairs
    tay
ERBIN
    lda (SP),y                                     ; Get var name into
    sta ARG+1                                      ; ARG
    dey
    lda (SP),y
    sta ARG
    dey
    dey
    dey
    sty WSC                                        ; NB. WSC +&01 is &00
    jsr CROUT
    jsr PRINA                                      ; Print var name
    lda #'='
    jsr OUT
    lda #' '                                       ; Print a space
    jsr OUT
    ldy #3                                         ; Get the value of the
    lda (ARG),y                                    ; variable into ARG
    tax
    dey
    lda (ARG),y
    sta ARG
    stx ARG+1
    jsr PRINA                                      ; Print it
    ldy WSC                                        ; Repeat if more
    bne ERBIN

    ; **** Remove rest of stack
BINS
    lda WSB
    sta ARG
    lda WSB+1
    sta ARG+1
    jsr PARG
NOPE
    jmp POP                                        ; And back to NERRC

ERCN
    ldy #0
    ldx #1
ERCND
    lda ERRCNT,x
    sta ACL,x
    sty ERRCNT,x
    dex
    bpl ERCND
    jmp ACLRET

PSTRNG
    ldx #0                                         ; Print err mess
    lda #$0d
NXOUT
    jsr OUT
    inx
    txa
    tay
    lda (BRKAD),y
    bne NXOUT
    jmp CROUT

PARG
    ldx ARG+1                                      ; Look for flag
    inx
    beq ARGRTS
    ldx #ARGOFF                                    ; Print ARG
    jsr MESSAH                                     ; 'Arg :'
    jmp PRINA

ARGRTS
    rts


    ; **** ERRORSET
ERRSET
    jsr STEVAL
ELFIN
    jsr ALPAIR
    ldy #2
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    jmp AMADE

STEVAL
    jsr STACK                                      ; ELFIN on stack
    jsr NXEVAL
    jmp POP


    ; ******************************
    ; Useful arithmetic routines
    ; ******************************
    ; **** 16 bit multiply
MULPM
    jsr MDA
MUL
    ldy #$10                                       ; Index for 16 bits
MULB
    lda ACL
    lsr
    bcc MULD
    clc
    ldx #$fe
MULC
    lda AUXL,x
    adc SIGN,x
    sta AUXL,x
    inx
    bne MULC
MULD
    ldx #3
MULE
    ror ACL,x
    dex
    bpl MULE
    dey
    bne MULB
    rts


    ; **** 16 bit divide
DIVPM
    jsr MDA
DIV
    ldy #$10
DIVB
    asl ACL
    rol ACL+1
    rol XTNDL
    rol XTNDL+1
    sec
    lda XTNDL
    sbc AUXL
    tax
    lda XTNDL+1
    sbc AUXL+1
    bcc DIVC
    stx XTNDL
    sta XTNDL+1
    inc ACL
DIVC
    dey
    bne DIVB
    rts


    ; **** Negation routines
MDA
    ldy #0
    sty SIGN                                       ; Abs value of
    ldx #AUXL                                      ; AUXL with sign in
    jsr MDB
    ldx #ACL                                       ; ls bit of SIGN
MDB
    lda GCNO+1,x
    bpl MDRTS
MD
    sec
    tya
    sbc GCNO,x
    sta GCNO,x
    tya
    sbc GCNO+1,x
    sta GCNO+1,x
    inc SIGN
MDRTS
    rts


    ; **** 40 bit divide
DIV40
    lda #$28
DIV40B
    pha
    asl ACL40
    ldx #1
    ldy #3
MDIV1
    rol ACL40,x
    inx
    dey
    bpl MDIV1
    ldx #0
    ldy #4
MDIV2
    rol REM40,x
    inx
    dey
    bpl MDIV2
    sec
    ldx #0
    ldy #4
MDIV3
    lda REM40,x
    sbc AUX40,x
    sta TEMP40,x
    inx
    dey
    bpl MDIV3
    bcc DIV40C
    ldx #4
MDIV4
    lda TEMP40,x
    sta REM40,x
    dex
    bpl MDIV4
    inc ACL40
DIV40C
    pla
    tay
    dey
    tya
    bne DIV40B
    rts

LISPEN
unused1
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0


    ; ******************************
    ; This file holds the vectored
    ; core routines. These are
    ; held in 2 pages immediately
    ; preceding the image.
    ; ******************************
VECTAB
    jmp NULL

    jmp PRINT

    jmp CONS

    jmp EVAL

    jmp CAR

    jmp EQ

    jmp SETQ

    jmp SET

    jmp ATOM

    jmp READX

    jmp CDR

    jmp PRINZ

    jmp COND

    jmp QUO

    jmp PROGN

    jmp LOOP

    jmp WHILE

    jmp LIST

    jmp CAAR

    jmp CADR

    jmp CDAR

    jmp CDDR

    jmp CAAAR

    jmp CAADR

    jmp CADAR

    jmp CADDR

    jmp CDAAR

    jmp CDADR

    jmp CDDAR

    jmp CDDDR

    jmp _AND

    jmp OR

    jmp ERRORL

    jmp NUMP

    jmp ZEROP

    jmp ONEP

    jmp MINUSP

    jmp DUMP

    jmp LOAD

    jmp PLUS

    jmp DIFF

    jmp MINUS

    jmp SUBRP

    jmp TIMES

    jmp QUOT

    jmp REM

    jmp LESSP

    jmp SUBA

    jmp ADDA

    jmp RECLAM

    jmp RPLACA

    jmp RPLACD

    jmp CHARS

    jmp MESSON

    jmp GET

    jmp PUT

    jmp REMPR

    jmp GT

    jmp GPLIST

    jmp CHARP

    jmp LISTP

    jmp ASSOC

    jmp UNTIL

    jmp FSUBRP

    jmp ERCN

    jmp ERRSET

    jmp CALL

    jmp PEEK

    jmp POKE

    jmp OBLIST

    jmp APPLY

    jmp MAPCAR

    jmp MAP

    jmp ASCII

    jmp ORDINL

    jmp EXPLOD

    jmp IMPLOD

    jmp GETCHA

    jmp STAR

    jmp MESSOF

    jmp IPLINE

    jmp CLOS

    jmp OPE

    jmp WRTTZ

    jmp WRIT

    jmp EOF

    jmp VDU

    jmp PRINTC

    jmp PRINC

    jmp MODE

    jmp USR

    jmp SOUND

    jmp ENV

    jmp GCTIM

    jmp TIME

    jmp RESET

    jmp CLOCK

    jmp PVAL

    jmp ADVAL

    jmp BNOT

    jmp BAND

    jmp BOR

    jmp INKEY

    jmp GENSYM

unused2
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0

    ; Calculated expressions put at the end of the assembly so they resolve on the
    ; first pass which makes the asserts at the end of the file work.
HIWARM  = WRMCHK + HILISP - LISVAL
EVOFF   = TEXT   - TEXT
VALOFF  = VALTXT - TEXT
NILOFF  = NILTXT - TEXT
DOTOFF  = DOTTXT - TEXT
INSOFF  = INSTXT - TEXT
GCOFF   = GCTXT  - TEXT
COLOFF  = COLTXT - TEXT
FROFF   = FRTXT  - TEXT
SUBOFF  = SUBTXT - TEXT
ERROFF  = ERRTXT - TEXT
ARGOFF  = ARGTXT - TEXT
WRMOFF  = WRMTXT - TEXT
HLPOFF  = HLPTXT - TEXT

    ; ******************************
    ; The first LISP image file
    ; ******************************
IMAGE
    !byte $f9, $18

ZA
    !byte CHARF,   $0f
    !word $0a02
    !word $0a02
    !text "UNDEFINED"

TRUE
    !byte CHARF,     7
    !word $0a11
    !word     0
    !text "T"

LAMBDA
    !byte CHARF,   $0c
    !word $0a18
    !word     0
    !text "LAMBDA"

NIL
    !byte CHARF,     9
    !word 0
    !word 0
    !text "NIL"

QUOTE
    !byte CHARF,   $0b
    !word $0b04
    !word     0
    !text "QUOTE"

ZB
    !byte CHARF,   $0a
    !word $0ab6
    !word     0
    !text "NULL"

ZC
    !byte CHARF,   $0b
    !word $0abc
    !word     0
    !text "PRINT"

ZD
    !byte CHARF,   $0a
    !word $0ac2
    !word     0
    !text "CONS"

ZE
    !byte CHARF,   $0a
    !word $0ac8
    !word     0
    !text "EVAL"

ZF
    !byte CHARF,     9
    !word $0ace
    !word     0
    !text "CAR"

ZG
    !byte CHARF,     8
    !word $0ad4
    !word     0
    !text "EQ"

ZH
    !byte CHARF,   $0a
    !word $0ada
    !word     0
    !text "SETQ"

ZI
    !byte CHARF,     9
    !word $0ae0
    !word     0
    !text "SET"

ZJ
    !byte CHARF,   $0a
    !word $0ae6
    !word     0
    !text "ATOM"

ZK
    !byte CHARF,   $0a
    !word $0aec
    !word     0
    !text "READ"

ZL
    !byte CHARF,     9
    !word $0af2
    !word     0
    !text "CDR"

ZM
    !byte CHARF,   $0a
    !word $0af8
    !word     0
    !text "PRIN"

ZN
    !byte CHARF,   $0a
    !word $0afe
    !word     0
    !text "COND"

ZO
    !byte SUBRF,     1
    !word     0
    !word $0800

ZP
    !byte SUBRF,     0
    !word     0
    !word $0803

ZQ
    !byte SUBRF,     2
    !word     0
    !word $0806

ZR
    !byte SUBRF,     1
    !word     0
    !word $0809

ZS
    !byte SUBRF,     1
    !word     0
    !word $080c

ZT
    !byte SUBRF,     2
    !word     0
    !word $080f

ZU
    !byte FSUBRF,      0
    !word     0
    !word $0812

ZV
    !byte FSUBRF,      0
    !word     0
    !word $0815

ZW
    !byte SUBRF,     1
    !word     0
    !word $0818

ZX
    !byte SUBRF,     0
    !word     0
    !word $081b

ZY
    !byte SUBRF,     1
    !word     0
    !word $081e

ZZ
    !byte SUBRF,     0
    !word     0
    !word $0821

ZAA
    !byte FSUBRF,      0
    !word     0
    !word $0824

ZAB
    !byte FSUBRF,      0
    !word     0
    !word $0827

ZBA
    !byte CHARF,   $0b
    !word $0c22
    !word     0
    !text "PROGN"

ZBB
    !byte CHARF,   $0a
    !word $0c28
    !word     0
    !text "LOOP"

ZBC
    !byte CHARF,   $0b
    !word $0c2e
    !word     0
    !text "WHILE"

ZBF
    !byte CHARF,   $0a
    !word $0c34
    !word     0
    !text "LIST"

ZBH
    !byte CHARF,   $0a
    !word $0c3a
    !word     0
    !text "CAAR"

ZBI
    !byte CHARF,   $0a
    !word $0c40
    !word     0
    !text "CADR"

ZBJ
    !byte CHARF,   $0a
    !word $0c46
    !word     0
    !text "CDAR"

ZBK
    !byte CHARF,   $0a
    !word $0c4c
    !word     0
    !text "CDDR"

ZBL
    !byte CHARF,   $0b
    !word $0c52
    !word     0
    !text "CAAAR"

ZBM
    !byte CHARF,   $0b
    !word $0c58
    !word     0
    !text "CAADR"

ZBN
    !byte CHARF,   $0b
    !word $0c5e
    !word     0
    !text "CADAR"

ZBO
    !byte CHARF,   $0b
    !word $0c64
    !word     0
    !text "CADDR"

ZPB
    !byte CHARF,   $0b
    !word $0c6a
    !word     0
    !text "CDAAR"

ZBQ
    !byte CHARF,   $0b
    !word $0c70
    !word     0
    !text "CDADR"

ZBR
    !byte CHARF,   $0b
    !word $0c76
    !word     0
    !text "CDDAR"

ZBS
    !byte CHARF,   $0b
    !word $0c7c
    !word     0
    !text "CDDDR"

ZBT
    !byte CHARF,     9
    !word $0c82
    !word     0
    !text "AND"

ZBU
    !byte CHARF,     8
    !word $0c88
    !word     0
    !text "OR"

ZBV
    !byte CHARF,   $0b
    !word $0c8e
    !word     0
    !text "ERROR"

ZBW
    !byte CHARF,     9
    !word $0ab6
    !word     0
    !text "NOT"

ZBX
    !byte CHARF,   $0d
    !word $0c94
    !word     0
    !text "NUMBERP"

ZBY
    !byte CHARF,   $0b
    !word $0c9a
    !word     0
    !text "ZEROP"

ZBZ
    !byte CHARF,   $0a
    !word $0ca0
    !word     0
    !text "ONEP"

ZCA
    !byte CHARF,   $0c
    !word $0ca6
    !word     0
    !text "MINUSP"

ZCB
    !byte CHARF,     7
    !word 0
    !word 0
    !text "F"

ZCC
    !byte CHARF,   $0a
    !word $0cac
    !word     0
    !text "SAVE"

ZCD
    !byte CHARF,   $0a
    !word $0cb2
    !word     0
    !text "LOAD"

ZEA
    !byte FSUBRF,      0
    !word     0
    !word $082a

ZEB
    !byte FSUBRF,      0
    !word     0
    !word $082d

ZEC
    !byte FSUBRF,      0
    !word     0
    !word $0830

ZEF
    !byte SUBRF,     0
    !word     0
    !word $0833

ZEH
    !byte SUBRF,     1
    !word     0
    !word $0836

ZEI
    !byte SUBRF,     1
    !word     0
    !word $0839

ZEJ
    !byte SUBRF,     1
    !word     0
    !word $083c

ZEK
    !byte SUBRF,     1
    !word     0
    !word $083f

ZEL
    !byte SUBRF,     1
    !word     0
    !word $0842

ZEM
    !byte SUBRF,     1
    !word     0
    !word $0845

ZEN
    !byte SUBRF,     1
    !word     0
    !word $0848

ZEO
    !byte SUBRF,     1
    !word     0
    !word $084b

ZEP
    !byte SUBRF,     1
    !word     0
    !word $084e

ZEQ
    !byte SUBRF,     1
    !word     0
    !word $0851

ZER
    !byte SUBRF,     1
    !word     0
    !word $0854

ZES
    !byte SUBRF,     1
    !word     0
    !word $0857

ZET
    !byte FSUBRF,      0
    !word     0
    !word $085a

ZEU
    !byte FSUBRF,      0
    !word     0
    !word $085d

ZEV
    !byte SUBRF,     0
    !word     0
    !word $0860

ZEX
    !byte SUBRF,     1
    !word     0
    !word $0863

ZEY
    !byte SUBRF,     1
    !word     0
    !word $0866

ZEZ
    !byte SUBRF,     1
    !word     0
    !word $0869

ZFA
    !byte SUBRF,     1
    !word     0
    !word $086c

ZFC
    !byte SUBRF,     1
    !word     0
    !word $086f

ZFD
    !byte SUBRF,     1
    !word     0
    !word $0872

ZFE
    !byte SUBRF,     0
    !word     0
    !word $0875

ZFF
    !byte SUBRF,     2
    !word     0
    !word $0878

ZFG
    !byte SUBRF,     1
    !word     0
    !word $087b

ZFH
    !byte SUBRF,     1
    !word     0
    !word $087e

ZFI
    !byte SUBRF,     0
    !word     0
    !word $0881

ZFJ
    !byte SUBRF,     2
    !word     0
    !word $0884

ZFK
    !byte SUBRF,     2
    !word     0
    !word $0887

ZFL
    !byte SUBRF,     2
    !word     0
    !word $088a

ZFM
    !byte SUBRF,     1
    !word     0
    !word $088d

ZFN
    !byte SUBRF,     1
    !word     0
    !word $0890

ZFQ
    !byte SUBRF,     0
    !word     0
    !word $0893

ZFR
    !byte SUBRF,     2
    !word     0
    !word $0896

ZFS
    !byte SUBRF,     2
    !word     0
    !word $0899

ZFT
    !byte SUBRF,     1
    !word     0
    !word $089c

ZFU
    !byte SUBRF,     0
    !word     0
    !word $089f

ZFV
    !byte SUBRF,     2
    !word     0
    !word $08a2

ZFW
    !byte SUBRF,     3
    !word     0
    !word $08a5

ZFX
    !byte SUBRF,     2
    !word     0
    !word $08a8

ZFY
    !byte SUBRF,     2
    !word     0
    !word $08ab

ZFZ
    !byte SUBRF,     1
    !word     0
    !word $08ae

ZGA
    !byte SUBRF,     1
    !word     0
    !word $08b1

ZGB
    !byte SUBRF,     1
    !word     0
    !word $08b4

ZGD
    !byte SUBRF,     2
    !word     0
    !word $08b7

ZGE
    !byte FSUBRF,      0
    !word     0
    !word $08ba

ZGF
    !byte SUBRF,     1
    !word     0
    !word $08bd

ZGG
    !byte CHARF,   $0a
    !word $0cb8
    !word     0
    !text "PLUS"

    ; ******************************
    ; The second LISP image file
    ; ******************************

    !byte CHARF,   $10
    !word $0cbe
    !word     0
    !text "DIFFERENCE"

    !byte CHARF,   $0b
    !word $0cc4
    !word     0
    !text "MINUS"

    !byte CHARF,   $0b
    !word $0cca
    !word     0
    !text "SUBRP"

    !byte CHARF,   $0b
    !word $0cd0
    !word     0
    !text "TIMES"

    !byte CHARF,   $0e
    !word $0cd6
    !word     0
    !text "QUOTIENT"

    !byte CHARF,   $0f
    !word $0cdc
    !word     0
    !text "REMAINDER"

    !byte CHARF,   $0b
    !word $0ce2
    !word     0
    !text "LESSP"

    !byte CHARF,   $0a
    !word $0ce8
    !word     0
    !text "SUB1"

    !byte CHARF,   $0a
    !word $0cee
    !word     0
    !text "ADD1"

    !byte CHARF,   $0d
    !word $0cf4
    !word     0
    !text "RECLAIM"

    !byte CHARF,   $0c
    !word $0cfa
    !word     0
    !text "RPLACA"

    !byte CHARF,   $0c
    !word $0d00
    !word     0
    !text "RPLACD"

    !byte CHARF,   $0b
    !word $0d06
    !word     0
    !text "CHARS"

    !byte CHARF,   $0c
    !word $0e5a
    !word     0
    !text "DOLLAR"

    !byte CHARF,     8
    !word $0e61
    !word     0
    !text "CR"

    !byte CHARF,   $0c
    !word $0d0c
    !word     0
    !text "MESSON"

    !byte CHARF,   $0d
    !word $1054
    !word     0
    !text "MESSOFF"

    !byte CHARF,     9
    !word $0d12
    !word     0
    !text "GET"

    !byte CHARF,     9
    !word $0d18
    !word     0
    !text "PUT"

    !byte CHARF,   $0d
    !word $0d1e
    !word     0
    !text "REMPROP"

    !byte CHARF,   $0e
    !word $0d24
    !word     0
    !text "GREATERP"

    !byte CHARF,   $0b
    !word $0d2a
    !word     0
    !text "PLIST"

    !byte CHARF,     7
    !word $0e5a
    !word     0
    !text "$"

    !byte CHARF,     7
    !word $0e61
    !word     0
    !byte $0d

    !byte CHARF,   $0b
    !word $0d30
    !word     0
    !text "CHARP"

    !byte CHARF,   $0b
    !word $0d36
    !word     0
    !text "LISTP"

    !byte CHARF,   $0b
    !word $0d3c
    !word     0
    !text "ASSOC"

    !byte CHARF,     7
    !word $104e
    !word     0
    !text "*"

    !byte CHARF,   $0a
    !word $0ebb
    !word     0
    !text "LPAR"

    !byte CHARF,   $0b
    !word $0ec2
    !word     0
    !text "BLANK"

    !byte CHARF,   $0a
    !word $0ec9
    !word     0
    !text "RPAR"

    !byte CHARF,   $0c
    !word $0ed0
    !word     0
    !text "PERIOD"

    !byte CHARF,     7
    !word $0ebb
    !word     0
    !text "("

    !byte CHARF,     7
    !word $0ec2
    !word     0
    !text " "

    !byte CHARF,     7
    !word $0ec9
    !word     0
    !text ")"

    !byte CHARF,     7
    !word $0ed0
    !word     0
    !text "."

    !byte CHARF,   $0b
    !word $0d42
    !word     0
    !text "UNTIL"

    !byte CHARF,   $0c
    !word $0d48
    !word     0
    !text "FSUBRP"

    !byte CHARF,   $10
    !word $0ffa
    !word     0
    !text "ERRORCOUNT"

    !byte CHARF,   $0e
    !word $1000
    !word     0
    !text "ERRORSET"

    !byte CHARF,   $0a
    !word $1006
    !word     0
    !text "CALL"

    !byte CHARF,   $0a
    !word $100c
    !word     0
    !text "PEEK"

    !byte CHARF,   $0a
    !word $1012
    !word     0
    !text "POKE"

    !byte CHARF,   $0c
    !word $1018
    !word     0
    !text "OBLIST"

    !byte CHARF,   $0b
    !word $101e
    !word     0
    !text "APPLY"

    !byte CHARF,   $0a
    !word $1024
    !word     0
    !text "MAPC"

    !byte CHARF,     9
    !word $102a
    !word     0
    !text "MAP"

    !byte CHARF,   $0f
    !word $1030
    !word     0
    !text "CHARACTER"

    !byte CHARF,   $0d
    !word $1036
    !word     0
    !text "ORDINAL"

    !byte CHARF,   $0d
    !word $103c
    !word     0
    !text "EXPLODE"

    !byte CHARF,   $0d
    !word $1042
    !word     0
    !text "IMPLODE"

    !byte CHARF,   $0d
    !word $1048
    !word     0
    !text "GETCHAR"                                ; N.B. ZMQ IS USED

    !byte CHARF,   $0e
    !word $105a
    !word     0
    !text "READLINE"

    !byte CHARF,   $0b
    !word $1060
    !word     0
    !text "CLOSE"

    !byte CHARF,   $0a
    !word $1066
    !word     0
    !text "OPEN"

    !byte CHARF,   $0c
    !word $106c
    !word     0
    !text "WRITE0"

    !byte CHARF,   $0b
    !word $1072
    !word     0
    !text "WRITE"

    !byte CHARF,     9
    !word $1078
    !word     0
    !text "EOF"

    !byte CHARF,     9
    !word $107e
    !word     0
    !text "VDU"

    !byte CHARF,   $0c
    !word $1084
    !word     0
    !text "PRINTC"

    !byte CHARF,   $0b
    !word $108a
    !word     0
    !text "PRINC"

    !byte SUBRF,     0
    !word     0
    !word $08c0

    !byte FSUBRF,      0
    !word     0
    !word $08c3

    !byte SUBRF,     1
    !word     0
    !word $08c6

    !byte SUBRF,     1
    !word     0
    !word $08c9

    !byte SUBRF,     2
    !word     0
    !word $08cc

    !byte SUBRF,     0
    !word     0
    !word $08cf

    !byte SUBRF,     2
    !word     0
    !word $08d2

    !byte SUBRF,     2
    !word     0
    !word $08d5

    !byte SUBRF,     2
    !word     0
    !word $08d8

    !byte SUBRF,     1
    !word     0
    !word $08db

    !byte SUBRF,     1
    !word     0
    !word $08de

    !byte SUBRF,     1
    !word     0
    !word $08e1

    !byte SUBRF,     1
    !word     0
    !word $08e4

    !byte SUBRF,     0
    !word     0
    !word $08e7

    !byte SUBRF,     1
    !word     0
    !word $08ea

    !byte SUBRF,     1
    !word     0
    !word $08ed

    !byte SUBRF,     0
    !word     0
    !word $08f0

    !byte SUBRF,     1
    !word     0
    !word $08f3

    !byte SUBRF,     2
    !word     0
    !word $08f6

    !byte SUBRF,     1
    !word     0
    !word $08f9

    !byte SUBRF,     1
    !word     0
    !word $08fc

    !byte SUBRF,     1
    !word     0
    !word $08ff

    !byte SUBRF,     0
    !word     0
    !word $0902

    !byte SUBRF,     0
    !word     0
    !word $0905

    !byte SUBRF,     0
    !word     0
    !word $0908

    !byte CHARF,   $0a
    !word $1132
    !word     0
    !text "MODE"

    ; ******************************
    ; Routines new to version 2
    ; ******************************

    !byte CHARF,     9
    !word $1138
    !word     0
    !text "USR"

    !byte CHARF,   $0b
    !word $113e
    !word     0
    !text "SOUND"

    !byte CHARF,   $0e
    !word $1144
    !word     0
    !text "ENVELOPE"

    !byte CHARF,   $0c
    !word $114a
    !word     0
    !text "GCTIME"

    !byte CHARF,   $0a
    !word $1150
    !word     0
    !text "TIME"

    !byte CHARF,   $0b
    !word $1156
    !word     0
    !text "RESET"

    !byte CHARF,   $0b
    !word $115c
    !word     0
    !text "POINT"

    !byte CHARF,   $0b
    !word $1162
    !word     0
    !text "ADVAL"

    !byte CHARF,   $0b
    !word $1168
    !word     0
    !text "CLOCK"

    !byte CHARF,   $0a
    !word $116e
    !word     0
    !text "BNOT"

    !byte CHARF,   $0a
    !word $1174
    !word     0
    !text "BAND"

    !byte CHARF,     9
    !word $117a
    !word     0
    !text "BOR"

    !byte CHARF,   $0b
    !word $1180
    !word     0
    !text "INKEY"

    !byte CHARF,   $0c
    !word $1186
    !word     0
    !text "GENSYM"

    !byte SUBRF,     1
    !word     0
    !word $090b

    !byte SUBRF,     5
    !word     0
    !word $090e

    !byte SUBRF,     4
    !word     0
    !word $0911

    !byte SUBRF,   $0e
    !word     0
    !word $0914

    !byte SUBRF,     0
    !word     0
    !word $0917

    !byte SUBRF,     0
    !word     0
    !word $091a

    !byte SUBRF,     0
    !word     0
    !word $091d

    !byte SUBRF,     2
    !word     0
    !word $0923

    !byte SUBRF,     1
    !word     0
    !word $0926

    !byte SUBRF,     0
    !word     0
    !word $0920

    !byte SUBRF,     1
    !word     0
    !word $0929

    !byte SUBRF,     0
    !word     0
    !word $092c

    !byte SUBRF,     0
    !word     0
    !word $092f

    !byte SUBRF,     1
    !word     0
    !word $0932

    !byte SUBRF,     0
    !word     0
    !word $0935

    !byte CHARF,   $0d
    !word $1199
    !word   $2c
    !text "VERSION"
    !byte 4, 4, 4, 0

    !byte CHARF,   $0f
    !word $11ac
    !word   $e3
    !text "LINEWIDTH"

    !byte NUMF,    4
    !word   $1f
    !word $0b00
    !byte $bb, $11, $80,   0
    !text "DEFUN"

    !byte LISTF,   $18
    !word $c70a
    !word   $11
    !byte   7,   2, $0a, $ff,   0, $58

    !byte LISTF,   $c0
    !word $0811
    !word $8012
    !text "|"
    !byte $0a, $db, $11, $80, $61, $0a, $d6, $11
    !byte $80, $c0, $11, $14,   0, $80, $d1, $11
    !byte   3, $12, $80, $4d, $0a, $ef, $11, $80
    !byte $2d, $0a, $ea, $11, $80, $18, $0a,   2
    !byte   0, $80, $e5, $11, $fe, $11, $80, $99
    !byte $0a, $f9, $11, $80, $c0, $11, $80,   0
    !byte $80, $f4, $11, $80,   0, $80, $e0, $11
    !byte $80,   0, $80, $cc, $11, $17, $12, $80
    !byte $61, $0a, $12, $12, $80, $c0, $11, $80
    !byte   0, $80, $0d, $12, $80,   0

    !byte CHARF,   $0f
    !word $1382
    !word   $6f
    !text "CHARCOUNT"

    !byte LISTF,   $c0
    !word $3a11
    !word   $12
    !byte $0a,   2, $0a, $7e,   0
    !text "LEFT"

    !byte LISTF,   $30
    !word $8012
    !word $8000
    !text "+"
    !byte $12, $7d, $13, $80, $ac, $0a, $a3, $12
    !byte $80, $85, $0a, $4e, $12, $80, $c0, $11
    !byte   0,   0, $80, $49, $12, $9e, $12, $80
    !byte $ac, $0a, $99, $12, $80, $41, $0e, $62
    !byte $12, $80, $30, $12, $71, $12, $80, $ea
    !byte $0d, $6c, $12, $80, $c0, $11, $d0,   0
    !byte $80, $67, $12,   0,   0, $80, $5d, $12
    !byte $94, $12, $80, $58, $0d, $80, $12, $80
    !byte $30, $12, $8f, $12, $80, $ea, $0d, $8a
    !byte $12, $80, $c0, $11, $ff,   0, $80, $85
    !byte $12, $ff,   0, $80, $7b, $12, $ff,   0
    !byte $80, $76, $12, $4d,   0, $80, $58, $12
    !byte   2,   0, $80, $53, $12, $78, $13, $80
    !byte $15, $0b, $0f, $13, $80, $d7, $0e, $bc
    !byte $12, $80, $85, $0a, $b7, $12, $80, $c0
    !byte $11, $48,   0, $80, $b2, $12, $0a, $13
    !byte $80, $1c, $12, $c6, $12, $80, $c0, $11
    !byte   5, $13, $80, $58, $0d, $d0, $12, $80
    !byte $30, $12,   0, $13, $80, $ac, $0a, $e8
    !byte $12, $80, $c0, $11, $e3, $12,   4,   4
    !byte   4,   0, $80, $df, $12, $16,   0, $80
    !byte $da, $12, $fb, $12, $80, $11, $0a, $f6
    !byte $12,   4,   4, $fe, $ff, $80, $f2, $12
    !byte $2c,   0, $80, $ed, $12, $2d,   0, $80
    !byte $d5, $12, $c0,   0, $80, $cb, $12, $5c
    !byte   0, $80, $c1, $12, $52,   0, $80, $ad
    !byte $12, $50, $13, $80, $1f, $0b, $4b, $13
    !byte $80, $72, $0a, $1e, $13, $80, $30, $12
    !byte $46, $13, $80, $1c, $12, $32, $13, $80
    !byte $61, $0a, $2d, $13, $80, $c0, $11, $2d
    !byte   0, $80, $28, $13, $41, $13, $80, $b1
    !byte $0d, $3c, $13, $80, $30, $12,   0,   0
    !byte $80, $37, $13,   0,   0, $80, $23, $13
    !byte $16,   0, $80, $19, $13,   0,   0, $80
    !byte $14, $13, $6e, $13, $80, $72, $0a, $5a
    !byte $13, $80, $c0, $11, $69, $13, $80, $99
    !byte $0a, $64, $13, $80, $c0, $11, $14,   0
    !byte $80, $5f, $13, $16,   0, $80, $55, $13
    !byte   0,   0, $80, $a8, $12, $16,   0, $80
    !byte $73, $13,   0,   0, $80, $44, $12, $ff
    !byte   0, $80, $18, $0a, $3f, $12

    !byte CHARF,   $0a
    !word $141b
    !word   $16
    !text "EDIT"

    !byte CHARF,     7
    !word $0a02
    !word   $17
    !text "L"

    !byte LISTF,   $91
    !word $c713
    !word   $13
    !byte $0c, $61, $18, $58,   0
    !text "SPRINT"
    !byte $80, $9d, $13, $c2, $13, $80, $57, $0a
    !byte $bd, $13, $80, $61, $0a, $b8, $13, $80
    !byte $91, $13, $16,   0, $80, $b3, $13, $0f
    !byte   0, $80, $ae, $13, $11,   0, $80, $a9
    !byte $13, $d1, $13, $80, $e3, $0f, $17,   0
    !byte $80, $cc, $13, $16, $14, $80, $7c, $0a
    !byte $e5, $13, $80, $61, $0a, $e0, $13, $80
    !byte $91, $13, $13,   0, $80, $db, $13, $11
    !byte $14

    !byte CHARF,     9
    !word $16c2
    !word   $1a
    !text "SED"

    !byte LISTF,   $ea
    !word $0c13
    !word $8014
    !text "W"
    !byte $0a,   7, $14, $80, $61, $0a,   2, $14
    !byte $80, $91, $13, $cc,   0, $80, $fd, $13
    !byte $4e,   0, $80, $f8, $13, $cc,   0, $80
    !byte $f3, $13,   4,   0, $80, $d6, $13, $17
    !byte   0, $80, $18, $0a, $98, $13,   0,   7
    !byte   2, $0a, $99,   0, $41, $80, $20, $14
    !byte $38, $14,   0,   7,   2, $0a, $80,   0
    !byte $51, $80, $2c, $14, $9e,   0, $80, $33
    !byte $14, $94,   0, $80, $27, $14, $bd, $16
    !byte $80, $15, $0b, $65, $14, $80, $72, $0a
    !byte $4c, $14, $80, $2c, $14, $60, $14, $80
    !byte $ef, $0f, $5b, $14, $80, $8a, $0f, $c1
    !byte   0, $80, $56, $14, $b7,   0, $80, $51
    !byte $14, $ef,   0, $80, $47, $14, $99, $14
    !byte $80, $d7, $0e, $8f, $14, $80, $6a, $0a
    !byte $74, $14, $80, $2c, $14, $8a, $14, $80
    !byte $2d, $0a, $85, $14,   0,   7,   2, $0a
    !byte $0e,   0, $42, $80, $7e, $14, $fd,   0
    !byte $80, $79, $14, $e3,   0, $80, $6f, $14
    !byte $94, $14, $80, $20, $14, $0c,   0, $80
    !byte $6a, $14, $b8, $16, $80, $72, $0a, $a3
    !byte $14, $80, $20, $14, $b3, $16, $80, $ac
    !byte $0a, $e6, $14, $80, $6a, $0a, $b2, $14
    !byte $80, $2c, $14, $c8, $14, $80, $2d, $0a
    !byte $c3, $14,   0,   7,   2, $0a, $18,   0
    !byte $52, $80, $bc, $14, $80,   0, $80, $b7
    !byte $14, $80,   0, $80, $ad, $14, $d7, $14
    !byte $80, $e3, $0f, $80,   0, $80, $d2, $14
    !byte $e1, $14, $80, $8f, $0a, $80,   0, $80
    !byte $dc, $14, $80,   0, $80, $cd, $14, $1d
    !byte $15, $80, $6a, $0a, $f0, $14, $80, $2c
    !byte $14, $f5, $14, $80,   1, $0e,   0,   0
    !byte $80, $eb, $14,   9, $15, $80, $9d, $13
    !byte   4, $15, $80, $20, $14, $80,   0, $80
    !byte $ff, $14, $13, $15, $80, $e3, $0f, $bc
    !byte   0, $80, $0e, $15, $18, $15, $80, $20
    !byte $14, $b2,   0, $80, $fa, $14, $6a, $15
    !byte $80, $6a, $0a, $27, $15, $80, $2c, $14
    !byte $3d, $15, $80, $2d, $0a, $38, $15,   0
    !byte   7,   2, $0a, $80,   0, $43, $80, $31
    !byte $15, $9a,   0, $80, $2c, $15, $90,   0
    !byte $80, $22, $15, $4c, $15, $80, $e3, $0f
    !byte $86,   0, $80, $47, $15, $65, $15, $80
    !byte $4d, $0a, $5b, $15, $80, $8f, $0a, $c1
    !byte   0, $80, $56, $15, $60, $15, $80, $20
    !byte $14, $18,   0, $80, $51, $15, $0e,   0
    !byte $80, $42, $15, $9c, $15, $80, $85, $0a
    !byte $74, $15, $80, $20, $14, $80,   0, $80
    !byte $6f, $15, $92, $15, $80, $ef, $0f, $8d
    !byte $15, $80, $2d, $0a, $88, $15, $80, $89
    !byte $0e,   0,   0, $80, $83, $15,   0,   0
    !byte $80, $7e, $15, $97, $15, $80, $20, $14
    !byte $0a,   0, $80, $79, $15, $f8, $15, $80
    !byte $6a, $0a, $a6, $15, $80, $2c, $14, $bc
    !byte $15, $80, $2d, $0a, $b7, $15,   0,   7
    !byte   2, $0a, $4f,   0, $44, $80, $b0, $15
    !byte $10,   0, $80, $ab, $15, $4c,   0, $80
    !byte $a1, $15, $f3, $15, $80, $4d, $0a, $d5
    !byte $15, $80, $61, $0a, $d0, $15, $80, $20
    !byte $14,   0,   0, $80, $cb, $15, $ee, $15
    !byte $80, $ea, $13, $e9, $15, $80, $99, $0a
    !byte $e4, $15, $80, $20, $14, $43,   0, $80
    !byte $df, $15, $43,   0, $80, $da, $15, $0d
    !byte   0, $80, $c6, $15, $4f,   0, $80, $c1
    !byte $15, $4d, $16, $80, $6a, $0a,   2, $16
    !byte $80, $2c, $14, $11, $16, $80, $2d, $0a
    !byte $0c, $16, $80, $20, $14,   0,   0, $80
    !byte   7, $16, $4f,   0, $80, $fd, $15, $48
    !byte $16, $80, $4d, $0a, $34, $16, $80, $ea
    !byte $13, $2f, $16, $80, $61, $0a, $2a, $16
    !byte $80, $20, $14, $44,   0, $80, $25, $16
    !byte   0,   0, $80, $20, $16, $43, $16, $80
    !byte $99, $0a, $3e, $16, $80, $20, $14,   0
    !byte   0, $80, $39, $16, $4e,   0, $80, $1b
    !byte $16,   0,   0, $80, $16, $16, $7f, $16
    !byte $80, $6a, $0a, $57, $16, $80, $2c, $14
    !byte $66, $16, $80, $2d, $0a, $61, $16, $80
    !byte $c0, $11, $45,   0, $80, $5c, $16, $7e
    !byte   0, $80, $52, $16, $7a, $16, $80, $99
    !byte $0a, $75, $16, $80, $20, $14, $49,   0
    !byte $80, $70, $16, $0b,   0, $80, $6b, $16
    !byte $ae, $16, $80, $11, $0a, $a4, $16, $80
    !byte $ef, $0f, $9f, $16, $80, $2d, $0a, $9a
    !byte $16,   0,   7,   2, $0a,   0,   0, $3f
    !byte $80, $93, $16,   0,   0, $80, $8e, $16
    !byte   0,   0, $80, $89, $16, $a9, $16, $80
    !byte $20, $14,   0,   0, $80, $84, $16,   0
    !byte   0, $80, $a8, $14,   0,   0, $80, $9e
    !byte $14,   0,   0, $80, $42, $14,   0,   0
    !byte $80, $18, $0a, $3d, $14, $80, $c0, $11
    !byte $dc, $16,   0,   7,   2, $0a,   0,   0
    !byte $4e, $80, $cc, $16, $d8, $16,   4,   4
    !byte   0,   0, $80, $d3, $16,   0,   0, $80
    !byte $c7, $16, $5c, $18, $80, $ac, $0a, $36
    !byte $17, $80, $bd, $0b, $fa, $16, $80, $85
    !byte $0a, $f5, $16, $80, $c0, $11,   0,   0
    !byte $80, $f0, $16, $1d, $17, $80, $1c, $12
    !byte   4, $17, $80, $c0, $11, $18, $17, $80
    !byte $58, $0d, $0e, $17, $80, $9d, $11, $13
    !byte $17, $80, $cc, $16,   0,   0, $80,   9
    !byte $17,   0,   0, $80, $ff, $16,   0,   0
    !byte $80, $eb, $16, $31, $17, $80, $ef, $0f
    !byte $2c, $17, $80, $c0, $11,   0,   0, $80
    !byte $27, $17,   0,   0, $80, $22, $17, $57
    !byte $18, $80, $ef, $0f, $40, $17, $80, $90
    !byte $0e,   0,   0, $80, $3b, $17, $63, $17
    !byte $80, $9d, $13, $59, $17, $80, $61, $0a
    !byte $54, $17, $80, $c0, $11,   0,   0, $80
    !byte $4f, $17, $5e, $17, $80, $cc, $16,   0
    !byte   0, $80, $4a, $17, $8a, $17, $80, $72
    !byte $0a, $6d, $17, $80, $cc, $16, $85, $17
    !byte $80, $4e, $0d, $77, $17, $80, $cc, $16
    !byte $80, $17,   4,   4,   3,   0, $80, $7c
    !byte $17,   0,   0, $80, $72, $17,   0,   0
    !byte $80, $68, $17, $52, $18, $80, $15, $0b
    !byte $ad, $17, $80, $72, $0a, $99, $17, $80
    !byte $c0, $11, $a8, $17, $80, $99, $0a, $a3
    !byte $17, $80, $c0, $11,   0,   0, $80, $9e
    !byte $17,   0,   0, $80, $94, $17, $ee, $17
    !byte $80, $ac, $0a, $e9, $17, $80, $b4, $0b
    !byte $bc, $17, $80, $c0, $11, $cb, $17, $80
    !byte $85, $0a, $c6, $17, $80, $c0, $11,   0
    !byte   0, $80, $c1, $17,   0,   0, $80, $b7
    !byte $17, $e4, $17, $80, $ef, $0f, $da, $17
    !byte $80, $af, $0e, $df, $17, $80, $c0, $11
    !byte   0,   0, $80, $d5, $17,   0,   0, $80
    !byte $d0, $17,   0,   0, $80, $b2, $17, $16
    !byte $18, $80, $d7, $0e,   2, $18, $80, $85
    !byte $0a, $fd, $17, $80, $c0, $11,   0,   0
    !byte $80, $f8, $17, $11, $18, $80, $ef, $0f
    !byte $0c, $18, $80, $a5, $0e,   0,   0, $80
    !byte   7, $18,   0,   0, $80, $f3, $17, $2f
    !byte $18

    !byte CHARF,   $0a
    !word $18d5
    !word     0
    !text "XTAB"

    !byte LISTF,   $1b
    !word $2a18
    !word $8018
    !byte $cc, $16,   0,   0, $80, $25, $18, $4d
    !byte $18, $80, $9d, $13, $43, $18, $80, $61
    !byte $0a, $3e, $18, $80, $c0, $11,   0,   0
    !byte $80, $39, $18, $48, $18, $80, $cc, $16
    !byte   0,   0, $80, $34, $18,   0,   0, $80
    !byte $8f, $17,   0,   0, $80, $45, $17,   0
    !byte   0, $80, $e6, $16,   0,   0, $80, $18
    !byte $0a, $e1, $16,   0,   7,   2, $0a,   0
    !byte   0, $53, $80, $66, $18,   0,   0, $80
    !byte $6d, $18, $7c, $18, $80, $e3, $0f,   0
    !byte   0, $80, $77, $18, $d0, $18, $80, $15
    !byte $0b, $bc, $18, $80, $d7, $0e, $b7, $18
    !byte $80, $fb, $0b, $b2, $18, $80, $72, $0a
    !byte $95, $18, $80, $66, $18, $ad, $18, $80
    !byte $58, $0d, $9f, $18, $80, $66, $18, $a8
    !byte $18,   4,   4,   1,   0, $80, $a4, $18
    !byte   0,   0, $80, $9a, $18,   0,   0, $80
    !byte $90, $18,   0,   0, $80, $8b, $18,   0
    !byte   0, $80, $86, $18, $cb, $18, $80, $ef
    !byte $0f, $c6, $18, $80, $9a, $0e,   0,   0
    !byte $80, $c1, $18,   0,   0, $80, $81, $18
    !byte   0,   0, $80, $18, $0a, $72, $18, $80
    !byte $0e, $0c, $f4, $18

    !byte CHARF,   $0b
    !word $0a02
    !word     0
    !text "IMAGE"

    !byte LISTF,   $df
    !word   $18
    !word $8000
    !text "-"
    !byte $0a, $ea, $18, $80, $ef, $18
unused3
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0

; relocation table (entries point to high bytes of addresses)
MOVE_OFFSET = HILISP-LISVAL
RELTAB
    !word MOVE_OFFSET + $8002
    !word MOVE_OFFSET + $8005
    !word MOVE_OFFSET + $8050
    !word MOVE_OFFSET + $807b
    !word MOVE_OFFSET + $80a3
    !word MOVE_OFFSET + $80a9
    !word MOVE_OFFSET + $80b5
    !word MOVE_OFFSET + $80b8
    !word MOVE_OFFSET + $80c5
    !word MOVE_OFFSET + $80fc
    !word MOVE_OFFSET + $8132
    !word MOVE_OFFSET + $8149
    !word MOVE_OFFSET + $8284
    !word MOVE_OFFSET + $8291
    !word MOVE_OFFSET + $8294
    !word MOVE_OFFSET + $829e
    !word MOVE_OFFSET + $82b3
    !word MOVE_OFFSET + $82bb
    !word MOVE_OFFSET + $82be
    !word MOVE_OFFSET + $82c2
    !word MOVE_OFFSET + $82d8
    !word MOVE_OFFSET + $82dd
    !word MOVE_OFFSET + $82e0
    !word MOVE_OFFSET + $82e3
    !word MOVE_OFFSET + $82e8
    !word MOVE_OFFSET + $82eb
    !word MOVE_OFFSET + $82ee
    !word MOVE_OFFSET + $83af
    !word MOVE_OFFSET + $8402
    !word MOVE_OFFSET + $8475
    !word MOVE_OFFSET + $84bb
    !word MOVE_OFFSET + $84e3
    !word MOVE_OFFSET + $84e6
    !word MOVE_OFFSET + $84fb
    !word MOVE_OFFSET + $8502
    !word MOVE_OFFSET + $8508
    !word MOVE_OFFSET + $8538
    !word MOVE_OFFSET + $853b
    !word MOVE_OFFSET + $8571
    !word MOVE_OFFSET + $858a
    !word MOVE_OFFSET + $858d
    !word MOVE_OFFSET + $8590
    !word MOVE_OFFSET + $8593
    !word MOVE_OFFSET + $8596
    !word MOVE_OFFSET + $85c9
    !word MOVE_OFFSET + $85e5
    !word MOVE_OFFSET + $8636
    !word MOVE_OFFSET + $869a
    !word MOVE_OFFSET + $86b5
    !word MOVE_OFFSET + $86b8
    !word MOVE_OFFSET + $86c1
    !word MOVE_OFFSET + $8710
    !word MOVE_OFFSET + $8713
    !word MOVE_OFFSET + $872d
    !word MOVE_OFFSET + $873a
    !word MOVE_OFFSET + $8755
    !word MOVE_OFFSET + $8762
    !word MOVE_OFFSET + $876b
    !word MOVE_OFFSET + $879c
    !word MOVE_OFFSET + $87d5
    !word MOVE_OFFSET + $87d8
    !word MOVE_OFFSET + $87f4
    !word MOVE_OFFSET + $87f7
    !word MOVE_OFFSET + $880d
    !word MOVE_OFFSET + $8822
    !word MOVE_OFFSET + $8825
    !word MOVE_OFFSET + $8834
    !word MOVE_OFFSET + $8847
    !word MOVE_OFFSET + $884e
    !word MOVE_OFFSET + $8857
    !word MOVE_OFFSET + $885c
    !word MOVE_OFFSET + $8861
    !word MOVE_OFFSET + $8866
    !word MOVE_OFFSET + $8871
    !word MOVE_OFFSET + $887c
    !word MOVE_OFFSET + $8883
    !word MOVE_OFFSET + $8893
    !word MOVE_OFFSET + $8896
    !word MOVE_OFFSET + $88c0
    !word MOVE_OFFSET + $88c7
    !word MOVE_OFFSET + $88ca
    !word MOVE_OFFSET + $88d8
    !word MOVE_OFFSET + $88db
    !word MOVE_OFFSET + $88de
    !word MOVE_OFFSET + $88e1
    !word MOVE_OFFSET + $88e4
    !word MOVE_OFFSET + $88e7
    !word MOVE_OFFSET + $88f4
    !word MOVE_OFFSET + $8913
    !word MOVE_OFFSET + $894d
    !word MOVE_OFFSET + $89b8
    !word MOVE_OFFSET + $89c5
    !word MOVE_OFFSET + $89f1
    !word MOVE_OFFSET + $8a4d
    !word MOVE_OFFSET + $8a54
    !word MOVE_OFFSET + $8a9e
    !word MOVE_OFFSET + $8aa8
    !word MOVE_OFFSET + $8aad
    !word MOVE_OFFSET + $8abb
    !word MOVE_OFFSET + $8ad9
    !word MOVE_OFFSET + $8ade
    !word MOVE_OFFSET + $8aeb
    !word MOVE_OFFSET + $8af0
    !word MOVE_OFFSET + $8af4
    !word MOVE_OFFSET + $8b00
    !word MOVE_OFFSET + $8b03
    !word MOVE_OFFSET + $8b13
    !word MOVE_OFFSET + $8b18
    !word MOVE_OFFSET + $8b25
    !word MOVE_OFFSET + $8b28
    !word MOVE_OFFSET + $8b49
    !word MOVE_OFFSET + $8b58
    !word MOVE_OFFSET + $8b6e
    !word MOVE_OFFSET + $8b88
    !word MOVE_OFFSET + $8b93
    !word MOVE_OFFSET + $8b9e
    !word MOVE_OFFSET + $8ba1
    !word MOVE_OFFSET + $8bb0
    !word MOVE_OFFSET + $8bb3
    !word MOVE_OFFSET + $8bb6
    !word MOVE_OFFSET + $8bcd
    !word MOVE_OFFSET + $8bd0
    !word MOVE_OFFSET + $8bd5
    !word MOVE_OFFSET + $8bd8
    !word MOVE_OFFSET + $8bff
    !word MOVE_OFFSET + $8c04
    !word MOVE_OFFSET + $8c12
    !word MOVE_OFFSET + $8c15
    !word MOVE_OFFSET + $8c23
    !word MOVE_OFFSET + $8c26
    !word MOVE_OFFSET + $8c2b
    !word MOVE_OFFSET + $8c33
    !word MOVE_OFFSET + $8c37
    !word MOVE_OFFSET + $8c3a
    !word MOVE_OFFSET + $8c4f
    !word MOVE_OFFSET + $8c56
    !word MOVE_OFFSET + $8c6c
    !word MOVE_OFFSET + $8c87
    !word MOVE_OFFSET + $8cab
    !word MOVE_OFFSET + $8ccc
    !word MOVE_OFFSET + $8ccf
    !word MOVE_OFFSET + $8cd2
    !word MOVE_OFFSET + $8cdf
    !word MOVE_OFFSET + $8cf4
    !word MOVE_OFFSET + $8cf7
    !word MOVE_OFFSET + $8d16
    !word MOVE_OFFSET + $8d27
    !word MOVE_OFFSET + $8d2e
    !word MOVE_OFFSET + $8d31
    !word MOVE_OFFSET + $8d3a
    !word MOVE_OFFSET + $8d43
    !word MOVE_OFFSET + $8d4c
    !word MOVE_OFFSET + $8d4f
    !word MOVE_OFFSET + $8d52
    !word MOVE_OFFSET + $8daf
    !word MOVE_OFFSET + $8dbe
    !word MOVE_OFFSET + $8dc3
    !word MOVE_OFFSET + $8dc6
    !word MOVE_OFFSET + $8dde
    !word MOVE_OFFSET + $8de1
    !word MOVE_OFFSET + $8de8
    !word MOVE_OFFSET + $8def
    !word MOVE_OFFSET + $8df2
    !word MOVE_OFFSET + $8df9
    !word MOVE_OFFSET + $8e12
    !word MOVE_OFFSET + $8e1d
    !word MOVE_OFFSET + $8e20
    !word MOVE_OFFSET + $8e33
    !word MOVE_OFFSET + $8e36
    !word MOVE_OFFSET + $8e39
    !word MOVE_OFFSET + $8e3c
    !word MOVE_OFFSET + $8e49
    !word MOVE_OFFSET + $8e4c
    !word MOVE_OFFSET + $8e5a
    !word MOVE_OFFSET + $8e7a
    !word MOVE_OFFSET + $8ea2
    !word MOVE_OFFSET + $8ea5
    !word MOVE_OFFSET + $8ebf
    !word MOVE_OFFSET + $8ecc
    !word MOVE_OFFSET + $8ee7
    !word MOVE_OFFSET + $8eea
    !word MOVE_OFFSET + $8f09
    !word MOVE_OFFSET + $8f1e
    !word MOVE_OFFSET + $8f21
    !word MOVE_OFFSET + $8f5e
    !word MOVE_OFFSET + $8f73
    !word MOVE_OFFSET + $8f76
    !word MOVE_OFFSET + $8f79
    !word MOVE_OFFSET + $8f7c
    !word MOVE_OFFSET + $8f7f
    !word MOVE_OFFSET + $8f8b
    !word MOVE_OFFSET + $8fbc
    !word MOVE_OFFSET + $8fcb
    !word MOVE_OFFSET + $8fcd
    !word MOVE_OFFSET + $8fd9
    !word MOVE_OFFSET + $8fe6
    !word MOVE_OFFSET + $8fe9
    !word MOVE_OFFSET + $8ff8
    !word MOVE_OFFSET + $8ffb
    !word MOVE_OFFSET + $9006
    !word MOVE_OFFSET + $9013
    !word MOVE_OFFSET + $9037
    !word MOVE_OFFSET + $903c
    !word MOVE_OFFSET + $9054
    !word MOVE_OFFSET + $9074
    !word MOVE_OFFSET + $9097
    !word MOVE_OFFSET + $909a
    !word MOVE_OFFSET + $90a5
    !word MOVE_OFFSET + $90bd
    !word MOVE_OFFSET + $9109
    !word MOVE_OFFSET + $910e
    !word MOVE_OFFSET + $9119
    !word MOVE_OFFSET + $9120
    !word MOVE_OFFSET + $9123
    !word MOVE_OFFSET + $912c
    !word MOVE_OFFSET + $913c
    !word MOVE_OFFSET + $9176
    !word MOVE_OFFSET + $9179
    !word MOVE_OFFSET + $9184
    !word MOVE_OFFSET + $918d
    !word MOVE_OFFSET + $91a7
    !word MOVE_OFFSET + $91b0
    !word MOVE_OFFSET + $91c0
    !word MOVE_OFFSET + $91c3
    !word MOVE_OFFSET + $91c6
    !word MOVE_OFFSET + $91db
    !word MOVE_OFFSET + $91de
    !word MOVE_OFFSET + $91f1
    !word MOVE_OFFSET + $91f4
    !word MOVE_OFFSET + $91f7
    !word MOVE_OFFSET + $91ff
    !word MOVE_OFFSET + $9202
    !word MOVE_OFFSET + $922b
    !word MOVE_OFFSET + $922e
    !word MOVE_OFFSET + $925c
    !word MOVE_OFFSET + $925f
    !word MOVE_OFFSET + $9288
    !word MOVE_OFFSET + $929b
    !word MOVE_OFFSET + $92a6
    !word MOVE_OFFSET + $92b4
    !word MOVE_OFFSET + $92b7
    !word MOVE_OFFSET + $92d3
    !word MOVE_OFFSET + $92df
    !word MOVE_OFFSET + $92e2
    !word MOVE_OFFSET + $92ee
    !word MOVE_OFFSET + $930a
    !word MOVE_OFFSET + $930d
    !word MOVE_OFFSET + $9336
    !word MOVE_OFFSET + $934f
    !word MOVE_OFFSET + $9354
    !word MOVE_OFFSET + $93be
    !word MOVE_OFFSET + $93d5
    !word MOVE_OFFSET + $93d8
    !word MOVE_OFFSET + $93db
    !word MOVE_OFFSET + $93eb
    !word MOVE_OFFSET + $93ee
    !word MOVE_OFFSET + $93f1
    !word MOVE_OFFSET + $9403
    !word MOVE_OFFSET + $9406
    !word MOVE_OFFSET + $9411
    !word MOVE_OFFSET + $9449
    !word MOVE_OFFSET + $944c
    !word MOVE_OFFSET + $944f
    !word MOVE_OFFSET + $9478
    !word MOVE_OFFSET + $9486
    !word MOVE_OFFSET + $94b5
    !word MOVE_OFFSET + $94b8
    !word MOVE_OFFSET + $94bb
    !word MOVE_OFFSET + $9538
    !word MOVE_OFFSET + $9574
    !word MOVE_OFFSET + $9577
    !word MOVE_OFFSET + $957a
    !word MOVE_OFFSET + $9594
    !word MOVE_OFFSET + $9597
    !word MOVE_OFFSET + $959c
    !word MOVE_OFFSET + $959f
    !word MOVE_OFFSET + $95a4
    !word MOVE_OFFSET + $95a7
    !word MOVE_OFFSET + $95ac
    !word MOVE_OFFSET + $95be
    !word MOVE_OFFSET + $95c1
    !word MOVE_OFFSET + $95f6
    !word MOVE_OFFSET + $9604
    !word MOVE_OFFSET + $960d
    !word MOVE_OFFSET + $9610
    !word MOVE_OFFSET + $9613
    !word MOVE_OFFSET + $9624
    !word MOVE_OFFSET + $962b
    !word MOVE_OFFSET + $962e
    !word MOVE_OFFSET + $9631
    !word MOVE_OFFSET + $9634
    !word MOVE_OFFSET + $9644
    !word MOVE_OFFSET + $9647
    !word MOVE_OFFSET + $966e
    !word MOVE_OFFSET + $96ac
    !word MOVE_OFFSET + $96b9
    !word MOVE_OFFSET + $96d4
    !word MOVE_OFFSET + $96e7
    !word MOVE_OFFSET + $9704
    !word MOVE_OFFSET + $9720
    !word MOVE_OFFSET + $9751
    !word MOVE_OFFSET + $9754
    !word MOVE_OFFSET + $975f
    !word MOVE_OFFSET + $9762
    !word MOVE_OFFSET + $976f
    !word MOVE_OFFSET + $9772
    !word MOVE_OFFSET + $9775
    !word MOVE_OFFSET + $978a
    !word MOVE_OFFSET + $97d4
    !word MOVE_OFFSET + $97f2
    !word MOVE_OFFSET + $97f5
    !word MOVE_OFFSET + $980e
    !word MOVE_OFFSET + $9812
    !word MOVE_OFFSET + $9816
    !word MOVE_OFFSET + $981a
    !word MOVE_OFFSET + $981d
    !word MOVE_OFFSET + $9822
    !word MOVE_OFFSET + $9825
    !word MOVE_OFFSET + $9849
    !word MOVE_OFFSET + $9860
    !word MOVE_OFFSET + $988a
    !word MOVE_OFFSET + $988d
    !word MOVE_OFFSET + $98c1
    !word MOVE_OFFSET + $98c4
    !word MOVE_OFFSET + $98c7
    !word MOVE_OFFSET + $98cb
    !word MOVE_OFFSET + $98d4
    !word MOVE_OFFSET + $98f0
    !word MOVE_OFFSET + $98f3
    !word MOVE_OFFSET + $98fe
    !word MOVE_OFFSET + $9910
    !word MOVE_OFFSET + $9913
    !word MOVE_OFFSET + $992f
    !word MOVE_OFFSET + $993c
    !word MOVE_OFFSET + $9946
    !word MOVE_OFFSET + $9953
    !word MOVE_OFFSET + $9959
    !word MOVE_OFFSET + $9962
    !word MOVE_OFFSET + $9965
    !word MOVE_OFFSET + $9968
    !word MOVE_OFFSET + $998b
    !word MOVE_OFFSET + $999e
    !word MOVE_OFFSET + $99a1
    !word MOVE_OFFSET + $99a4
    !word MOVE_OFFSET + $99c1
    !word MOVE_OFFSET + $99c4
    !word MOVE_OFFSET + $99cd
    !word MOVE_OFFSET + $99ea
    !word MOVE_OFFSET + $99f4
    !word MOVE_OFFSET + $9a06
    !word MOVE_OFFSET + $9a09
    !word MOVE_OFFSET + $9a2c
    !word MOVE_OFFSET + $9a45
    !word MOVE_OFFSET + $9a48
    !word MOVE_OFFSET + $9a4b
    !word MOVE_OFFSET + $9a5d
    !word MOVE_OFFSET + $9a60
    !word MOVE_OFFSET + $9a63
    !word MOVE_OFFSET + $9a8d
    !word MOVE_OFFSET + $9a90
    !word MOVE_OFFSET + $9a93
    !word MOVE_OFFSET + $9abd
    !word MOVE_OFFSET + $9aca
    !word MOVE_OFFSET + $9ad5
    !word MOVE_OFFSET + $9b3a
    !word MOVE_OFFSET + $9b45
    !word MOVE_OFFSET + $9b53
    !word MOVE_OFFSET + $9b71
    !word MOVE_OFFSET + $9b7c
    !word MOVE_OFFSET + $9b82
    !word MOVE_OFFSET + $9b97
    !word MOVE_OFFSET + $9bac
    !word MOVE_OFFSET + $9bc2
    !word MOVE_OFFSET + $9bd5
    !word MOVE_OFFSET + $9bda
    !word MOVE_OFFSET + $9bf0
    !word MOVE_OFFSET + $9bf3
    !word MOVE_OFFSET + $9c05
    !word MOVE_OFFSET + $9c15
    !word MOVE_OFFSET + $9c19
    !word MOVE_OFFSET + $9c21
    !word MOVE_OFFSET + $9c2f
    !word MOVE_OFFSET + $9c34
    !word MOVE_OFFSET + $9c52
    !word MOVE_OFFSET + $9ccb
    !word MOVE_OFFSET + $9ceb
    !word MOVE_OFFSET + $9cf4
    !word MOVE_OFFSET + $9cf7
    !word MOVE_OFFSET + $9d0f
    !word MOVE_OFFSET + $9d12
    !word MOVE_OFFSET + $9d1f
    !word MOVE_OFFSET + $9d31
    !word MOVE_OFFSET + $9d74
    !word MOVE_OFFSET + $9d81
    !word MOVE_OFFSET + $9da4
    !word MOVE_OFFSET + $9da9
    !word MOVE_OFFSET + $9dac
    !word MOVE_OFFSET + $9db1
    !word MOVE_OFFSET + $9dc3
    !word MOVE_OFFSET + $9dc8
    !word MOVE_OFFSET + $9e81
    !word MOVE_OFFSET + $9eaa
    !word MOVE_OFFSET + $9ed2
    !word MOVE_OFFSET + $9ed9
    !word MOVE_OFFSET + $9ee2
    !word MOVE_OFFSET + $9eeb
    !word MOVE_OFFSET + $9efd
    !word MOVE_OFFSET + $9f1b
    !word MOVE_OFFSET + $9f3b
    !word MOVE_OFFSET + $9f42
    !word MOVE_OFFSET + $9f66
    !word MOVE_OFFSET + $9f69
    !word MOVE_OFFSET + $9f6e
    !word MOVE_OFFSET + $9f73
    !word MOVE_OFFSET + $9f82
    !word MOVE_OFFSET + $9f91
    !word MOVE_OFFSET + $9f94
    !word MOVE_OFFSET + $9fa4
    !word MOVE_OFFSET + $9fab
    !word MOVE_OFFSET + $9fb5
    !word MOVE_OFFSET + $9fbf
    !word MOVE_OFFSET + $9fc2
    !word MOVE_OFFSET + $9fc6
    !word MOVE_OFFSET + $9fc9
    !word MOVE_OFFSET + $9fd7
    !word MOVE_OFFSET + $9fda
    !word MOVE_OFFSET + $9fdd
    !word MOVE_OFFSET + $9fe0
    !word MOVE_OFFSET + $9fe3
    !word MOVE_OFFSET + $a004
    !word MOVE_OFFSET + $a02d
unused4
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    !byte $ff, $ff,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0,   0,   0,   0,   0,   0,   0
    !byte   0,   0
ROMTOP
pydis_end
!if ($80+$0d) != $8d {
    !error "Assertion failed: $80+$0d == $8d"
}
!if ($80+' ') != $a0 {
    !error "Assertion failed: $80+' ' == $a0"
}
!if ($80+'#') != $a3 {
    !error "Assertion failed: $80+'#' == $a3"
}
!if ($80+'L') != $cc {
    !error "Assertion failed: $80+'L' == $cc"
}
!if ($80+'y') != $f9 {
    !error "Assertion failed: $80+'y' == $f9"
}
!if (' ') != $20 {
    !error "Assertion failed: ' ' == $20"
}
!if ('!') != $21 {
    !error "Assertion failed: '!' == $21"
}
!if ('(') != $28 {
    !error "Assertion failed: '(' == $28"
}
!if (')') != $29 {
    !error "Assertion failed: ')' == $29"
}
!if ('-') != $2d {
    !error "Assertion failed: '-' == $2d"
}
!if ('.') != $2e {
    !error "Assertion failed: '.' == $2e"
}
!if ('0') != $30 {
    !error "Assertion failed: '0' == $30"
}
!if ('=') != $3d {
    !error "Assertion failed: '=' == $3d"
}
!if ('C') != $43 {
    !error "Assertion failed: 'C' == $43"
}
!if ('F') != $46 {
    !error "Assertion failed: 'F' == $46"
}
!if ('G') != $47 {
    !error "Assertion failed: 'G' == $47"
}
!if ('I') != $49 {
    !error "Assertion failed: 'I' == $49"
}
!if ('L') != $4c {
    !error "Assertion failed: 'L' == $4c"
}
!if ('P') != $50 {
    !error "Assertion failed: 'P' == $50"
}
!if ('S') != $53 {
    !error "Assertion failed: 'S' == $53"
}
!if ('W') != $57 {
    !error "Assertion failed: 'W' == $57"
}
!if ('[') != $5b {
    !error "Assertion failed: '[' == $5b"
}
!if ('\'') != $27 {
    !error "Assertion failed: '\'' == $27"
}
!if ('^') != $5e {
    !error "Assertion failed: '^' == $5e"
}
!if ('c') != $63 {
    !error "Assertion failed: 'c' == $63"
}
!if ((>LISPEN-LISVAL-1)+1) != $21 {
    !error "Assertion failed: (>LISPEN-LISVAL-1)+1 == $21"
}
!if (<(DOSBUF)) != $00 {
    !error "Assertion failed: <(DOSBUF) == $00"
}
!if (<(GOSTR)) != $31 {
    !error "Assertion failed: <(GOSTR) == $31"
}
!if (<(INCB)) != $3e {
    !error "Assertion failed: <(INCB) == $3e"
}
!if (<(IODCB)) != $16 {
    !error "Assertion failed: <(IODCB) == $16"
}
!if (<(LISTR)) != $39 {
    !error "Assertion failed: <(LISTR) == $39"
}
!if (<(OSINFO)) != $38 {
    !error "Assertion failed: <(OSINFO) == $38"
}
!if (<(OSWBUF)) != $00 {
    !error "Assertion failed: <(OSWBUF) == $00"
}
!if (<(PWORD)) != $1b {
    !error "Assertion failed: <(PWORD) == $1b"
}
!if (<(TIMEW)) != $25 {
    !error "Assertion failed: <(TIMEW) == $25"
}
!if (<(TIMZER)) != $76 {
    !error "Assertion failed: <(TIMZER) == $76"
}
!if (<ACL) != $72 {
    !error "Assertion failed: <ACL == $72"
}
!if (<BACALL+1) != $dd {
    !error "Assertion failed: <BACALL+1 == $dd"
}
!if (<DWARF) != $eb {
    !error "Assertion failed: <DWARF == $eb"
}
!if (<ELFIN-1) != $c6 {
    !error "Assertion failed: <ELFIN-1 == $c6"
}
!if (<ERROR) != $87 {
    !error "Assertion failed: <ERROR == $87"
}
!if (<GCTIME) != $20 {
    !error "Assertion failed: <GCTIME == $20"
}
!if (<HILISP) != $00 {
    !error "Assertion failed: <HILISP == $00"
}
!if (<HIWARM) != $7b {
    !error "Assertion failed: <HIWARM == $7b"
}
!if (<KBD) != $ff {
    !error "Assertion failed: <KBD == $ff"
}
!if (<LAMBDA) != $18 {
    !error "Assertion failed: <LAMBDA == $18"
}
!if (<LISPST) != $00 {
    !error "Assertion failed: <LISPST == $00"
}
!if (<NAMBUF) != $00 {
    !error "Assertion failed: <NAMBUF == $00"
}
!if (<NIL) != $24 {
    !error "Assertion failed: <NIL == $24"
}
!if (<PRINGO+2) != $25 {
    !error "Assertion failed: <PRINGO+2 == $25"
}
!if (<QUOTE) != $2d {
    !error "Assertion failed: <QUOTE == $2d"
}
!if (<RELTAB) != $00 {
    !error "Assertion failed: <RELTAB == $00"
}
!if (<TIMEW) != $25 {
    !error "Assertion failed: <TIMEW == $25"
}
!if (<TRUE) != $11 {
    !error "Assertion failed: <TRUE == $11"
}
!if (<VECTAB) != $00 {
    !error "Assertion failed: <VECTAB == $00"
}
!if (<VECTAB+2) != $02 {
    !error "Assertion failed: <VECTAB+2 == $02"
}
!if (<WRITGO+2) != $eb {
    !error "Assertion failed: <WRITGO+2 == $eb"
}
!if (<ZA) != $02 {
    !error "Assertion failed: <ZA == $02"
}
!if (>(DOSBUF)) != $07 {
    !error "Assertion failed: >(DOSBUF) == $07"
}
!if (>(GOSTR)) != $82 {
    !error "Assertion failed: >(GOSTR) == $82"
}
!if (>(INCB)) != $82 {
    !error "Assertion failed: >(INCB) == $82"
}
!if (>(IODCB)) != $04 {
    !error "Assertion failed: >(IODCB) == $04"
}
!if (>(LISTR)) != $82 {
    !error "Assertion failed: >(LISTR) == $82"
}
!if (>(OSINFO)) != $04 {
    !error "Assertion failed: >(OSINFO) == $04"
}
!if (>(OSWBUF)) != $06 {
    !error "Assertion failed: >(OSWBUF) == $06"
}
!if (>(PWORD)) != $04 {
    !error "Assertion failed: >(PWORD) == $04"
}
!if (>(TIMEW)) != $04 {
    !error "Assertion failed: >(TIMEW) == $04"
}
!if (>(TIMZER)) != $82 {
    !error "Assertion failed: >(TIMZER) == $82"
}
!if (>BACALL+1) != $8f {
    !error "Assertion failed: >BACALL+1 == $8f"
}
!if (>DWARF) != $9e {
    !error "Assertion failed: >DWARF == $9e"
}
!if (>ELFIN-1) != $9f {
    !error "Assertion failed: >ELFIN-1 == $9f"
}
!if (>ERROR) != $9e {
    !error "Assertion failed: >ERROR == $9e"
}
!if (>GCTIME) != $04 {
    !error "Assertion failed: >GCTIME == $04"
}
!if (>HILISP) != $d7 {
    !error "Assertion failed: >HILISP == $d7"
}
!if (>HILISP-LISVAL) != $57 {
    !error "Assertion failed: >HILISP-LISVAL == $57"
}
!if (>HIWARM) != $d9 {
    !error "Assertion failed: >HIWARM == $d9"
}
!if (>KBD) != $00 {
    !error "Assertion failed: >KBD == $00"
}
!if (>LISPST) != $80 {
    !error "Assertion failed: >LISPST == $80"
}
!if (>NAMBUF) != $06 {
    !error "Assertion failed: >NAMBUF == $06"
}
!if (>PRINGO) != $8b {
    !error "Assertion failed: >PRINGO == $8b"
}
!if (>RELTAB) != $b8 {
    !error "Assertion failed: >RELTAB == $b8"
}
!if (>TIMEW) != $04 {
    !error "Assertion failed: >TIMEW == $04"
}
!if (>VECTAB) != $a4 {
    !error "Assertion failed: >VECTAB == $a4"
}
!if (>WRITGO) != $8a {
    !error "Assertion failed: >WRITGO == $8a"
}
!if (ACL) != $72 {
    !error "Assertion failed: ACL == $72"
}
!if (AREEXT) != $00 {
    !error "Assertion failed: AREEXT == $00"
}
!if (ARGINF) != $90 {
    !error "Assertion failed: ARGINF == $90"
}
!if (ARGOFF) != $6b {
    !error "Assertion failed: ARGOFF == $6b"
}
!if (AUXL) != $76 {
    !error "Assertion failed: AUXL == $76"
}
!if (BINDER) != $32 {
    !error "Assertion failed: BINDER == $32"
}
!if (CHARF) != $00 {
    !error "Assertion failed: CHARF == $00"
}
!if (COLDST) != $00 {
    !error "Assertion failed: COLDST == $00"
}
!if (COLOFF) != $3a {
    !error "Assertion failed: COLOFF == $3a"
}
!if (CPYOFF-ROMHDR) != $12 {
    !error "Assertion failed: CPYOFF-ROMHDR == $12"
}
!if (ERROFF) != $5d {
    !error "Assertion failed: ERROFF == $5d"
}
!if (EVOFF) != $00 {
    !error "Assertion failed: EVOFF == $00"
}
!if (FROFF) != $4c {
    !error "Assertion failed: FROFF == $4c"
}
!if (FSUBRF) != $0c {
    !error "Assertion failed: FSUBRF == $0c"
}
!if (GCOFF) != $33 {
    !error "Assertion failed: GCOFF == $33"
}
!if (HLPOFF) != $8e {
    !error "Assertion failed: HLPOFF == $8e"
}
!if (IMALEN) != $12 {
    !error "Assertion failed: IMALEN == $12"
}
!if (LISTF) != $80 {
    !error "Assertion failed: LISTF == $80"
}
!if (MOVE_OFFSET + $8002) != $d702 {
    !error "Assertion failed: MOVE_OFFSET + $8002 == $d702"
}
!if (MOVE_OFFSET + $8005) != $d705 {
    !error "Assertion failed: MOVE_OFFSET + $8005 == $d705"
}
!if (MOVE_OFFSET + $8050) != $d750 {
    !error "Assertion failed: MOVE_OFFSET + $8050 == $d750"
}
!if (MOVE_OFFSET + $807b) != $d77b {
    !error "Assertion failed: MOVE_OFFSET + $807b == $d77b"
}
!if (MOVE_OFFSET + $80a3) != $d7a3 {
    !error "Assertion failed: MOVE_OFFSET + $80a3 == $d7a3"
}
!if (MOVE_OFFSET + $80a9) != $d7a9 {
    !error "Assertion failed: MOVE_OFFSET + $80a9 == $d7a9"
}
!if (MOVE_OFFSET + $80b5) != $d7b5 {
    !error "Assertion failed: MOVE_OFFSET + $80b5 == $d7b5"
}
!if (MOVE_OFFSET + $80b8) != $d7b8 {
    !error "Assertion failed: MOVE_OFFSET + $80b8 == $d7b8"
}
!if (MOVE_OFFSET + $80c5) != $d7c5 {
    !error "Assertion failed: MOVE_OFFSET + $80c5 == $d7c5"
}
!if (MOVE_OFFSET + $80fc) != $d7fc {
    !error "Assertion failed: MOVE_OFFSET + $80fc == $d7fc"
}
!if (MOVE_OFFSET + $8132) != $d832 {
    !error "Assertion failed: MOVE_OFFSET + $8132 == $d832"
}
!if (MOVE_OFFSET + $8149) != $d849 {
    !error "Assertion failed: MOVE_OFFSET + $8149 == $d849"
}
!if (MOVE_OFFSET + $8284) != $d984 {
    !error "Assertion failed: MOVE_OFFSET + $8284 == $d984"
}
!if (MOVE_OFFSET + $8291) != $d991 {
    !error "Assertion failed: MOVE_OFFSET + $8291 == $d991"
}
!if (MOVE_OFFSET + $8294) != $d994 {
    !error "Assertion failed: MOVE_OFFSET + $8294 == $d994"
}
!if (MOVE_OFFSET + $829e) != $d99e {
    !error "Assertion failed: MOVE_OFFSET + $829e == $d99e"
}
!if (MOVE_OFFSET + $82b3) != $d9b3 {
    !error "Assertion failed: MOVE_OFFSET + $82b3 == $d9b3"
}
!if (MOVE_OFFSET + $82bb) != $d9bb {
    !error "Assertion failed: MOVE_OFFSET + $82bb == $d9bb"
}
!if (MOVE_OFFSET + $82be) != $d9be {
    !error "Assertion failed: MOVE_OFFSET + $82be == $d9be"
}
!if (MOVE_OFFSET + $82c2) != $d9c2 {
    !error "Assertion failed: MOVE_OFFSET + $82c2 == $d9c2"
}
!if (MOVE_OFFSET + $82d8) != $d9d8 {
    !error "Assertion failed: MOVE_OFFSET + $82d8 == $d9d8"
}
!if (MOVE_OFFSET + $82dd) != $d9dd {
    !error "Assertion failed: MOVE_OFFSET + $82dd == $d9dd"
}
!if (MOVE_OFFSET + $82e0) != $d9e0 {
    !error "Assertion failed: MOVE_OFFSET + $82e0 == $d9e0"
}
!if (MOVE_OFFSET + $82e3) != $d9e3 {
    !error "Assertion failed: MOVE_OFFSET + $82e3 == $d9e3"
}
!if (MOVE_OFFSET + $82e8) != $d9e8 {
    !error "Assertion failed: MOVE_OFFSET + $82e8 == $d9e8"
}
!if (MOVE_OFFSET + $82eb) != $d9eb {
    !error "Assertion failed: MOVE_OFFSET + $82eb == $d9eb"
}
!if (MOVE_OFFSET + $82ee) != $d9ee {
    !error "Assertion failed: MOVE_OFFSET + $82ee == $d9ee"
}
!if (MOVE_OFFSET + $83af) != $daaf {
    !error "Assertion failed: MOVE_OFFSET + $83af == $daaf"
}
!if (MOVE_OFFSET + $8402) != $db02 {
    !error "Assertion failed: MOVE_OFFSET + $8402 == $db02"
}
!if (MOVE_OFFSET + $8475) != $db75 {
    !error "Assertion failed: MOVE_OFFSET + $8475 == $db75"
}
!if (MOVE_OFFSET + $84bb) != $dbbb {
    !error "Assertion failed: MOVE_OFFSET + $84bb == $dbbb"
}
!if (MOVE_OFFSET + $84e3) != $dbe3 {
    !error "Assertion failed: MOVE_OFFSET + $84e3 == $dbe3"
}
!if (MOVE_OFFSET + $84e6) != $dbe6 {
    !error "Assertion failed: MOVE_OFFSET + $84e6 == $dbe6"
}
!if (MOVE_OFFSET + $84fb) != $dbfb {
    !error "Assertion failed: MOVE_OFFSET + $84fb == $dbfb"
}
!if (MOVE_OFFSET + $8502) != $dc02 {
    !error "Assertion failed: MOVE_OFFSET + $8502 == $dc02"
}
!if (MOVE_OFFSET + $8508) != $dc08 {
    !error "Assertion failed: MOVE_OFFSET + $8508 == $dc08"
}
!if (MOVE_OFFSET + $8538) != $dc38 {
    !error "Assertion failed: MOVE_OFFSET + $8538 == $dc38"
}
!if (MOVE_OFFSET + $853b) != $dc3b {
    !error "Assertion failed: MOVE_OFFSET + $853b == $dc3b"
}
!if (MOVE_OFFSET + $8571) != $dc71 {
    !error "Assertion failed: MOVE_OFFSET + $8571 == $dc71"
}
!if (MOVE_OFFSET + $858a) != $dc8a {
    !error "Assertion failed: MOVE_OFFSET + $858a == $dc8a"
}
!if (MOVE_OFFSET + $858d) != $dc8d {
    !error "Assertion failed: MOVE_OFFSET + $858d == $dc8d"
}
!if (MOVE_OFFSET + $8590) != $dc90 {
    !error "Assertion failed: MOVE_OFFSET + $8590 == $dc90"
}
!if (MOVE_OFFSET + $8593) != $dc93 {
    !error "Assertion failed: MOVE_OFFSET + $8593 == $dc93"
}
!if (MOVE_OFFSET + $8596) != $dc96 {
    !error "Assertion failed: MOVE_OFFSET + $8596 == $dc96"
}
!if (MOVE_OFFSET + $85c9) != $dcc9 {
    !error "Assertion failed: MOVE_OFFSET + $85c9 == $dcc9"
}
!if (MOVE_OFFSET + $85e5) != $dce5 {
    !error "Assertion failed: MOVE_OFFSET + $85e5 == $dce5"
}
!if (MOVE_OFFSET + $8636) != $dd36 {
    !error "Assertion failed: MOVE_OFFSET + $8636 == $dd36"
}
!if (MOVE_OFFSET + $869a) != $dd9a {
    !error "Assertion failed: MOVE_OFFSET + $869a == $dd9a"
}
!if (MOVE_OFFSET + $86b5) != $ddb5 {
    !error "Assertion failed: MOVE_OFFSET + $86b5 == $ddb5"
}
!if (MOVE_OFFSET + $86b8) != $ddb8 {
    !error "Assertion failed: MOVE_OFFSET + $86b8 == $ddb8"
}
!if (MOVE_OFFSET + $86c1) != $ddc1 {
    !error "Assertion failed: MOVE_OFFSET + $86c1 == $ddc1"
}
!if (MOVE_OFFSET + $8710) != $de10 {
    !error "Assertion failed: MOVE_OFFSET + $8710 == $de10"
}
!if (MOVE_OFFSET + $8713) != $de13 {
    !error "Assertion failed: MOVE_OFFSET + $8713 == $de13"
}
!if (MOVE_OFFSET + $872d) != $de2d {
    !error "Assertion failed: MOVE_OFFSET + $872d == $de2d"
}
!if (MOVE_OFFSET + $873a) != $de3a {
    !error "Assertion failed: MOVE_OFFSET + $873a == $de3a"
}
!if (MOVE_OFFSET + $8755) != $de55 {
    !error "Assertion failed: MOVE_OFFSET + $8755 == $de55"
}
!if (MOVE_OFFSET + $8762) != $de62 {
    !error "Assertion failed: MOVE_OFFSET + $8762 == $de62"
}
!if (MOVE_OFFSET + $876b) != $de6b {
    !error "Assertion failed: MOVE_OFFSET + $876b == $de6b"
}
!if (MOVE_OFFSET + $879c) != $de9c {
    !error "Assertion failed: MOVE_OFFSET + $879c == $de9c"
}
!if (MOVE_OFFSET + $87d5) != $ded5 {
    !error "Assertion failed: MOVE_OFFSET + $87d5 == $ded5"
}
!if (MOVE_OFFSET + $87d8) != $ded8 {
    !error "Assertion failed: MOVE_OFFSET + $87d8 == $ded8"
}
!if (MOVE_OFFSET + $87f4) != $def4 {
    !error "Assertion failed: MOVE_OFFSET + $87f4 == $def4"
}
!if (MOVE_OFFSET + $87f7) != $def7 {
    !error "Assertion failed: MOVE_OFFSET + $87f7 == $def7"
}
!if (MOVE_OFFSET + $880d) != $df0d {
    !error "Assertion failed: MOVE_OFFSET + $880d == $df0d"
}
!if (MOVE_OFFSET + $8822) != $df22 {
    !error "Assertion failed: MOVE_OFFSET + $8822 == $df22"
}
!if (MOVE_OFFSET + $8825) != $df25 {
    !error "Assertion failed: MOVE_OFFSET + $8825 == $df25"
}
!if (MOVE_OFFSET + $8834) != $df34 {
    !error "Assertion failed: MOVE_OFFSET + $8834 == $df34"
}
!if (MOVE_OFFSET + $8847) != $df47 {
    !error "Assertion failed: MOVE_OFFSET + $8847 == $df47"
}
!if (MOVE_OFFSET + $884e) != $df4e {
    !error "Assertion failed: MOVE_OFFSET + $884e == $df4e"
}
!if (MOVE_OFFSET + $8857) != $df57 {
    !error "Assertion failed: MOVE_OFFSET + $8857 == $df57"
}
!if (MOVE_OFFSET + $885c) != $df5c {
    !error "Assertion failed: MOVE_OFFSET + $885c == $df5c"
}
!if (MOVE_OFFSET + $8861) != $df61 {
    !error "Assertion failed: MOVE_OFFSET + $8861 == $df61"
}
!if (MOVE_OFFSET + $8866) != $df66 {
    !error "Assertion failed: MOVE_OFFSET + $8866 == $df66"
}
!if (MOVE_OFFSET + $8871) != $df71 {
    !error "Assertion failed: MOVE_OFFSET + $8871 == $df71"
}
!if (MOVE_OFFSET + $887c) != $df7c {
    !error "Assertion failed: MOVE_OFFSET + $887c == $df7c"
}
!if (MOVE_OFFSET + $8883) != $df83 {
    !error "Assertion failed: MOVE_OFFSET + $8883 == $df83"
}
!if (MOVE_OFFSET + $8893) != $df93 {
    !error "Assertion failed: MOVE_OFFSET + $8893 == $df93"
}
!if (MOVE_OFFSET + $8896) != $df96 {
    !error "Assertion failed: MOVE_OFFSET + $8896 == $df96"
}
!if (MOVE_OFFSET + $88c0) != $dfc0 {
    !error "Assertion failed: MOVE_OFFSET + $88c0 == $dfc0"
}
!if (MOVE_OFFSET + $88c7) != $dfc7 {
    !error "Assertion failed: MOVE_OFFSET + $88c7 == $dfc7"
}
!if (MOVE_OFFSET + $88ca) != $dfca {
    !error "Assertion failed: MOVE_OFFSET + $88ca == $dfca"
}
!if (MOVE_OFFSET + $88d8) != $dfd8 {
    !error "Assertion failed: MOVE_OFFSET + $88d8 == $dfd8"
}
!if (MOVE_OFFSET + $88db) != $dfdb {
    !error "Assertion failed: MOVE_OFFSET + $88db == $dfdb"
}
!if (MOVE_OFFSET + $88de) != $dfde {
    !error "Assertion failed: MOVE_OFFSET + $88de == $dfde"
}
!if (MOVE_OFFSET + $88e1) != $dfe1 {
    !error "Assertion failed: MOVE_OFFSET + $88e1 == $dfe1"
}
!if (MOVE_OFFSET + $88e4) != $dfe4 {
    !error "Assertion failed: MOVE_OFFSET + $88e4 == $dfe4"
}
!if (MOVE_OFFSET + $88e7) != $dfe7 {
    !error "Assertion failed: MOVE_OFFSET + $88e7 == $dfe7"
}
!if (MOVE_OFFSET + $88f4) != $dff4 {
    !error "Assertion failed: MOVE_OFFSET + $88f4 == $dff4"
}
!if (MOVE_OFFSET + $8913) != $e013 {
    !error "Assertion failed: MOVE_OFFSET + $8913 == $e013"
}
!if (MOVE_OFFSET + $894d) != $e04d {
    !error "Assertion failed: MOVE_OFFSET + $894d == $e04d"
}
!if (MOVE_OFFSET + $89b8) != $e0b8 {
    !error "Assertion failed: MOVE_OFFSET + $89b8 == $e0b8"
}
!if (MOVE_OFFSET + $89c5) != $e0c5 {
    !error "Assertion failed: MOVE_OFFSET + $89c5 == $e0c5"
}
!if (MOVE_OFFSET + $89f1) != $e0f1 {
    !error "Assertion failed: MOVE_OFFSET + $89f1 == $e0f1"
}
!if (MOVE_OFFSET + $8a4d) != $e14d {
    !error "Assertion failed: MOVE_OFFSET + $8a4d == $e14d"
}
!if (MOVE_OFFSET + $8a54) != $e154 {
    !error "Assertion failed: MOVE_OFFSET + $8a54 == $e154"
}
!if (MOVE_OFFSET + $8a9e) != $e19e {
    !error "Assertion failed: MOVE_OFFSET + $8a9e == $e19e"
}
!if (MOVE_OFFSET + $8aa8) != $e1a8 {
    !error "Assertion failed: MOVE_OFFSET + $8aa8 == $e1a8"
}
!if (MOVE_OFFSET + $8aad) != $e1ad {
    !error "Assertion failed: MOVE_OFFSET + $8aad == $e1ad"
}
!if (MOVE_OFFSET + $8abb) != $e1bb {
    !error "Assertion failed: MOVE_OFFSET + $8abb == $e1bb"
}
!if (MOVE_OFFSET + $8ad9) != $e1d9 {
    !error "Assertion failed: MOVE_OFFSET + $8ad9 == $e1d9"
}
!if (MOVE_OFFSET + $8ade) != $e1de {
    !error "Assertion failed: MOVE_OFFSET + $8ade == $e1de"
}
!if (MOVE_OFFSET + $8aeb) != $e1eb {
    !error "Assertion failed: MOVE_OFFSET + $8aeb == $e1eb"
}
!if (MOVE_OFFSET + $8af0) != $e1f0 {
    !error "Assertion failed: MOVE_OFFSET + $8af0 == $e1f0"
}
!if (MOVE_OFFSET + $8af4) != $e1f4 {
    !error "Assertion failed: MOVE_OFFSET + $8af4 == $e1f4"
}
!if (MOVE_OFFSET + $8b00) != $e200 {
    !error "Assertion failed: MOVE_OFFSET + $8b00 == $e200"
}
!if (MOVE_OFFSET + $8b03) != $e203 {
    !error "Assertion failed: MOVE_OFFSET + $8b03 == $e203"
}
!if (MOVE_OFFSET + $8b13) != $e213 {
    !error "Assertion failed: MOVE_OFFSET + $8b13 == $e213"
}
!if (MOVE_OFFSET + $8b18) != $e218 {
    !error "Assertion failed: MOVE_OFFSET + $8b18 == $e218"
}
!if (MOVE_OFFSET + $8b25) != $e225 {
    !error "Assertion failed: MOVE_OFFSET + $8b25 == $e225"
}
!if (MOVE_OFFSET + $8b28) != $e228 {
    !error "Assertion failed: MOVE_OFFSET + $8b28 == $e228"
}
!if (MOVE_OFFSET + $8b49) != $e249 {
    !error "Assertion failed: MOVE_OFFSET + $8b49 == $e249"
}
!if (MOVE_OFFSET + $8b58) != $e258 {
    !error "Assertion failed: MOVE_OFFSET + $8b58 == $e258"
}
!if (MOVE_OFFSET + $8b6e) != $e26e {
    !error "Assertion failed: MOVE_OFFSET + $8b6e == $e26e"
}
!if (MOVE_OFFSET + $8b88) != $e288 {
    !error "Assertion failed: MOVE_OFFSET + $8b88 == $e288"
}
!if (MOVE_OFFSET + $8b93) != $e293 {
    !error "Assertion failed: MOVE_OFFSET + $8b93 == $e293"
}
!if (MOVE_OFFSET + $8b9e) != $e29e {
    !error "Assertion failed: MOVE_OFFSET + $8b9e == $e29e"
}
!if (MOVE_OFFSET + $8ba1) != $e2a1 {
    !error "Assertion failed: MOVE_OFFSET + $8ba1 == $e2a1"
}
!if (MOVE_OFFSET + $8bb0) != $e2b0 {
    !error "Assertion failed: MOVE_OFFSET + $8bb0 == $e2b0"
}
!if (MOVE_OFFSET + $8bb3) != $e2b3 {
    !error "Assertion failed: MOVE_OFFSET + $8bb3 == $e2b3"
}
!if (MOVE_OFFSET + $8bb6) != $e2b6 {
    !error "Assertion failed: MOVE_OFFSET + $8bb6 == $e2b6"
}
!if (MOVE_OFFSET + $8bcd) != $e2cd {
    !error "Assertion failed: MOVE_OFFSET + $8bcd == $e2cd"
}
!if (MOVE_OFFSET + $8bd0) != $e2d0 {
    !error "Assertion failed: MOVE_OFFSET + $8bd0 == $e2d0"
}
!if (MOVE_OFFSET + $8bd5) != $e2d5 {
    !error "Assertion failed: MOVE_OFFSET + $8bd5 == $e2d5"
}
!if (MOVE_OFFSET + $8bd8) != $e2d8 {
    !error "Assertion failed: MOVE_OFFSET + $8bd8 == $e2d8"
}
!if (MOVE_OFFSET + $8bff) != $e2ff {
    !error "Assertion failed: MOVE_OFFSET + $8bff == $e2ff"
}
!if (MOVE_OFFSET + $8c04) != $e304 {
    !error "Assertion failed: MOVE_OFFSET + $8c04 == $e304"
}
!if (MOVE_OFFSET + $8c12) != $e312 {
    !error "Assertion failed: MOVE_OFFSET + $8c12 == $e312"
}
!if (MOVE_OFFSET + $8c15) != $e315 {
    !error "Assertion failed: MOVE_OFFSET + $8c15 == $e315"
}
!if (MOVE_OFFSET + $8c23) != $e323 {
    !error "Assertion failed: MOVE_OFFSET + $8c23 == $e323"
}
!if (MOVE_OFFSET + $8c26) != $e326 {
    !error "Assertion failed: MOVE_OFFSET + $8c26 == $e326"
}
!if (MOVE_OFFSET + $8c2b) != $e32b {
    !error "Assertion failed: MOVE_OFFSET + $8c2b == $e32b"
}
!if (MOVE_OFFSET + $8c33) != $e333 {
    !error "Assertion failed: MOVE_OFFSET + $8c33 == $e333"
}
!if (MOVE_OFFSET + $8c37) != $e337 {
    !error "Assertion failed: MOVE_OFFSET + $8c37 == $e337"
}
!if (MOVE_OFFSET + $8c3a) != $e33a {
    !error "Assertion failed: MOVE_OFFSET + $8c3a == $e33a"
}
!if (MOVE_OFFSET + $8c4f) != $e34f {
    !error "Assertion failed: MOVE_OFFSET + $8c4f == $e34f"
}
!if (MOVE_OFFSET + $8c56) != $e356 {
    !error "Assertion failed: MOVE_OFFSET + $8c56 == $e356"
}
!if (MOVE_OFFSET + $8c6c) != $e36c {
    !error "Assertion failed: MOVE_OFFSET + $8c6c == $e36c"
}
!if (MOVE_OFFSET + $8c87) != $e387 {
    !error "Assertion failed: MOVE_OFFSET + $8c87 == $e387"
}
!if (MOVE_OFFSET + $8cab) != $e3ab {
    !error "Assertion failed: MOVE_OFFSET + $8cab == $e3ab"
}
!if (MOVE_OFFSET + $8ccc) != $e3cc {
    !error "Assertion failed: MOVE_OFFSET + $8ccc == $e3cc"
}
!if (MOVE_OFFSET + $8ccf) != $e3cf {
    !error "Assertion failed: MOVE_OFFSET + $8ccf == $e3cf"
}
!if (MOVE_OFFSET + $8cd2) != $e3d2 {
    !error "Assertion failed: MOVE_OFFSET + $8cd2 == $e3d2"
}
!if (MOVE_OFFSET + $8cdf) != $e3df {
    !error "Assertion failed: MOVE_OFFSET + $8cdf == $e3df"
}
!if (MOVE_OFFSET + $8cf4) != $e3f4 {
    !error "Assertion failed: MOVE_OFFSET + $8cf4 == $e3f4"
}
!if (MOVE_OFFSET + $8cf7) != $e3f7 {
    !error "Assertion failed: MOVE_OFFSET + $8cf7 == $e3f7"
}
!if (MOVE_OFFSET + $8d16) != $e416 {
    !error "Assertion failed: MOVE_OFFSET + $8d16 == $e416"
}
!if (MOVE_OFFSET + $8d27) != $e427 {
    !error "Assertion failed: MOVE_OFFSET + $8d27 == $e427"
}
!if (MOVE_OFFSET + $8d2e) != $e42e {
    !error "Assertion failed: MOVE_OFFSET + $8d2e == $e42e"
}
!if (MOVE_OFFSET + $8d31) != $e431 {
    !error "Assertion failed: MOVE_OFFSET + $8d31 == $e431"
}
!if (MOVE_OFFSET + $8d3a) != $e43a {
    !error "Assertion failed: MOVE_OFFSET + $8d3a == $e43a"
}
!if (MOVE_OFFSET + $8d43) != $e443 {
    !error "Assertion failed: MOVE_OFFSET + $8d43 == $e443"
}
!if (MOVE_OFFSET + $8d4c) != $e44c {
    !error "Assertion failed: MOVE_OFFSET + $8d4c == $e44c"
}
!if (MOVE_OFFSET + $8d4f) != $e44f {
    !error "Assertion failed: MOVE_OFFSET + $8d4f == $e44f"
}
!if (MOVE_OFFSET + $8d52) != $e452 {
    !error "Assertion failed: MOVE_OFFSET + $8d52 == $e452"
}
!if (MOVE_OFFSET + $8daf) != $e4af {
    !error "Assertion failed: MOVE_OFFSET + $8daf == $e4af"
}
!if (MOVE_OFFSET + $8dbe) != $e4be {
    !error "Assertion failed: MOVE_OFFSET + $8dbe == $e4be"
}
!if (MOVE_OFFSET + $8dc3) != $e4c3 {
    !error "Assertion failed: MOVE_OFFSET + $8dc3 == $e4c3"
}
!if (MOVE_OFFSET + $8dc6) != $e4c6 {
    !error "Assertion failed: MOVE_OFFSET + $8dc6 == $e4c6"
}
!if (MOVE_OFFSET + $8dde) != $e4de {
    !error "Assertion failed: MOVE_OFFSET + $8dde == $e4de"
}
!if (MOVE_OFFSET + $8de1) != $e4e1 {
    !error "Assertion failed: MOVE_OFFSET + $8de1 == $e4e1"
}
!if (MOVE_OFFSET + $8de8) != $e4e8 {
    !error "Assertion failed: MOVE_OFFSET + $8de8 == $e4e8"
}
!if (MOVE_OFFSET + $8def) != $e4ef {
    !error "Assertion failed: MOVE_OFFSET + $8def == $e4ef"
}
!if (MOVE_OFFSET + $8df2) != $e4f2 {
    !error "Assertion failed: MOVE_OFFSET + $8df2 == $e4f2"
}
!if (MOVE_OFFSET + $8df9) != $e4f9 {
    !error "Assertion failed: MOVE_OFFSET + $8df9 == $e4f9"
}
!if (MOVE_OFFSET + $8e12) != $e512 {
    !error "Assertion failed: MOVE_OFFSET + $8e12 == $e512"
}
!if (MOVE_OFFSET + $8e1d) != $e51d {
    !error "Assertion failed: MOVE_OFFSET + $8e1d == $e51d"
}
!if (MOVE_OFFSET + $8e20) != $e520 {
    !error "Assertion failed: MOVE_OFFSET + $8e20 == $e520"
}
!if (MOVE_OFFSET + $8e33) != $e533 {
    !error "Assertion failed: MOVE_OFFSET + $8e33 == $e533"
}
!if (MOVE_OFFSET + $8e36) != $e536 {
    !error "Assertion failed: MOVE_OFFSET + $8e36 == $e536"
}
!if (MOVE_OFFSET + $8e39) != $e539 {
    !error "Assertion failed: MOVE_OFFSET + $8e39 == $e539"
}
!if (MOVE_OFFSET + $8e3c) != $e53c {
    !error "Assertion failed: MOVE_OFFSET + $8e3c == $e53c"
}
!if (MOVE_OFFSET + $8e49) != $e549 {
    !error "Assertion failed: MOVE_OFFSET + $8e49 == $e549"
}
!if (MOVE_OFFSET + $8e4c) != $e54c {
    !error "Assertion failed: MOVE_OFFSET + $8e4c == $e54c"
}
!if (MOVE_OFFSET + $8e5a) != $e55a {
    !error "Assertion failed: MOVE_OFFSET + $8e5a == $e55a"
}
!if (MOVE_OFFSET + $8e7a) != $e57a {
    !error "Assertion failed: MOVE_OFFSET + $8e7a == $e57a"
}
!if (MOVE_OFFSET + $8ea2) != $e5a2 {
    !error "Assertion failed: MOVE_OFFSET + $8ea2 == $e5a2"
}
!if (MOVE_OFFSET + $8ea5) != $e5a5 {
    !error "Assertion failed: MOVE_OFFSET + $8ea5 == $e5a5"
}
!if (MOVE_OFFSET + $8ebf) != $e5bf {
    !error "Assertion failed: MOVE_OFFSET + $8ebf == $e5bf"
}
!if (MOVE_OFFSET + $8ecc) != $e5cc {
    !error "Assertion failed: MOVE_OFFSET + $8ecc == $e5cc"
}
!if (MOVE_OFFSET + $8ee7) != $e5e7 {
    !error "Assertion failed: MOVE_OFFSET + $8ee7 == $e5e7"
}
!if (MOVE_OFFSET + $8eea) != $e5ea {
    !error "Assertion failed: MOVE_OFFSET + $8eea == $e5ea"
}
!if (MOVE_OFFSET + $8f09) != $e609 {
    !error "Assertion failed: MOVE_OFFSET + $8f09 == $e609"
}
!if (MOVE_OFFSET + $8f1e) != $e61e {
    !error "Assertion failed: MOVE_OFFSET + $8f1e == $e61e"
}
!if (MOVE_OFFSET + $8f21) != $e621 {
    !error "Assertion failed: MOVE_OFFSET + $8f21 == $e621"
}
!if (MOVE_OFFSET + $8f5e) != $e65e {
    !error "Assertion failed: MOVE_OFFSET + $8f5e == $e65e"
}
!if (MOVE_OFFSET + $8f73) != $e673 {
    !error "Assertion failed: MOVE_OFFSET + $8f73 == $e673"
}
!if (MOVE_OFFSET + $8f76) != $e676 {
    !error "Assertion failed: MOVE_OFFSET + $8f76 == $e676"
}
!if (MOVE_OFFSET + $8f79) != $e679 {
    !error "Assertion failed: MOVE_OFFSET + $8f79 == $e679"
}
!if (MOVE_OFFSET + $8f7c) != $e67c {
    !error "Assertion failed: MOVE_OFFSET + $8f7c == $e67c"
}
!if (MOVE_OFFSET + $8f7f) != $e67f {
    !error "Assertion failed: MOVE_OFFSET + $8f7f == $e67f"
}
!if (MOVE_OFFSET + $8f8b) != $e68b {
    !error "Assertion failed: MOVE_OFFSET + $8f8b == $e68b"
}
!if (MOVE_OFFSET + $8fbc) != $e6bc {
    !error "Assertion failed: MOVE_OFFSET + $8fbc == $e6bc"
}
!if (MOVE_OFFSET + $8fcb) != $e6cb {
    !error "Assertion failed: MOVE_OFFSET + $8fcb == $e6cb"
}
!if (MOVE_OFFSET + $8fcd) != $e6cd {
    !error "Assertion failed: MOVE_OFFSET + $8fcd == $e6cd"
}
!if (MOVE_OFFSET + $8fd9) != $e6d9 {
    !error "Assertion failed: MOVE_OFFSET + $8fd9 == $e6d9"
}
!if (MOVE_OFFSET + $8fe6) != $e6e6 {
    !error "Assertion failed: MOVE_OFFSET + $8fe6 == $e6e6"
}
!if (MOVE_OFFSET + $8fe9) != $e6e9 {
    !error "Assertion failed: MOVE_OFFSET + $8fe9 == $e6e9"
}
!if (MOVE_OFFSET + $8ff8) != $e6f8 {
    !error "Assertion failed: MOVE_OFFSET + $8ff8 == $e6f8"
}
!if (MOVE_OFFSET + $8ffb) != $e6fb {
    !error "Assertion failed: MOVE_OFFSET + $8ffb == $e6fb"
}
!if (MOVE_OFFSET + $9006) != $e706 {
    !error "Assertion failed: MOVE_OFFSET + $9006 == $e706"
}
!if (MOVE_OFFSET + $9013) != $e713 {
    !error "Assertion failed: MOVE_OFFSET + $9013 == $e713"
}
!if (MOVE_OFFSET + $9037) != $e737 {
    !error "Assertion failed: MOVE_OFFSET + $9037 == $e737"
}
!if (MOVE_OFFSET + $903c) != $e73c {
    !error "Assertion failed: MOVE_OFFSET + $903c == $e73c"
}
!if (MOVE_OFFSET + $9054) != $e754 {
    !error "Assertion failed: MOVE_OFFSET + $9054 == $e754"
}
!if (MOVE_OFFSET + $9074) != $e774 {
    !error "Assertion failed: MOVE_OFFSET + $9074 == $e774"
}
!if (MOVE_OFFSET + $9097) != $e797 {
    !error "Assertion failed: MOVE_OFFSET + $9097 == $e797"
}
!if (MOVE_OFFSET + $909a) != $e79a {
    !error "Assertion failed: MOVE_OFFSET + $909a == $e79a"
}
!if (MOVE_OFFSET + $90a5) != $e7a5 {
    !error "Assertion failed: MOVE_OFFSET + $90a5 == $e7a5"
}
!if (MOVE_OFFSET + $90bd) != $e7bd {
    !error "Assertion failed: MOVE_OFFSET + $90bd == $e7bd"
}
!if (MOVE_OFFSET + $9109) != $e809 {
    !error "Assertion failed: MOVE_OFFSET + $9109 == $e809"
}
!if (MOVE_OFFSET + $910e) != $e80e {
    !error "Assertion failed: MOVE_OFFSET + $910e == $e80e"
}
!if (MOVE_OFFSET + $9119) != $e819 {
    !error "Assertion failed: MOVE_OFFSET + $9119 == $e819"
}
!if (MOVE_OFFSET + $9120) != $e820 {
    !error "Assertion failed: MOVE_OFFSET + $9120 == $e820"
}
!if (MOVE_OFFSET + $9123) != $e823 {
    !error "Assertion failed: MOVE_OFFSET + $9123 == $e823"
}
!if (MOVE_OFFSET + $912c) != $e82c {
    !error "Assertion failed: MOVE_OFFSET + $912c == $e82c"
}
!if (MOVE_OFFSET + $913c) != $e83c {
    !error "Assertion failed: MOVE_OFFSET + $913c == $e83c"
}
!if (MOVE_OFFSET + $9176) != $e876 {
    !error "Assertion failed: MOVE_OFFSET + $9176 == $e876"
}
!if (MOVE_OFFSET + $9179) != $e879 {
    !error "Assertion failed: MOVE_OFFSET + $9179 == $e879"
}
!if (MOVE_OFFSET + $9184) != $e884 {
    !error "Assertion failed: MOVE_OFFSET + $9184 == $e884"
}
!if (MOVE_OFFSET + $918d) != $e88d {
    !error "Assertion failed: MOVE_OFFSET + $918d == $e88d"
}
!if (MOVE_OFFSET + $91a7) != $e8a7 {
    !error "Assertion failed: MOVE_OFFSET + $91a7 == $e8a7"
}
!if (MOVE_OFFSET + $91b0) != $e8b0 {
    !error "Assertion failed: MOVE_OFFSET + $91b0 == $e8b0"
}
!if (MOVE_OFFSET + $91c0) != $e8c0 {
    !error "Assertion failed: MOVE_OFFSET + $91c0 == $e8c0"
}
!if (MOVE_OFFSET + $91c3) != $e8c3 {
    !error "Assertion failed: MOVE_OFFSET + $91c3 == $e8c3"
}
!if (MOVE_OFFSET + $91c6) != $e8c6 {
    !error "Assertion failed: MOVE_OFFSET + $91c6 == $e8c6"
}
!if (MOVE_OFFSET + $91db) != $e8db {
    !error "Assertion failed: MOVE_OFFSET + $91db == $e8db"
}
!if (MOVE_OFFSET + $91de) != $e8de {
    !error "Assertion failed: MOVE_OFFSET + $91de == $e8de"
}
!if (MOVE_OFFSET + $91f1) != $e8f1 {
    !error "Assertion failed: MOVE_OFFSET + $91f1 == $e8f1"
}
!if (MOVE_OFFSET + $91f4) != $e8f4 {
    !error "Assertion failed: MOVE_OFFSET + $91f4 == $e8f4"
}
!if (MOVE_OFFSET + $91f7) != $e8f7 {
    !error "Assertion failed: MOVE_OFFSET + $91f7 == $e8f7"
}
!if (MOVE_OFFSET + $91ff) != $e8ff {
    !error "Assertion failed: MOVE_OFFSET + $91ff == $e8ff"
}
!if (MOVE_OFFSET + $9202) != $e902 {
    !error "Assertion failed: MOVE_OFFSET + $9202 == $e902"
}
!if (MOVE_OFFSET + $922b) != $e92b {
    !error "Assertion failed: MOVE_OFFSET + $922b == $e92b"
}
!if (MOVE_OFFSET + $922e) != $e92e {
    !error "Assertion failed: MOVE_OFFSET + $922e == $e92e"
}
!if (MOVE_OFFSET + $925c) != $e95c {
    !error "Assertion failed: MOVE_OFFSET + $925c == $e95c"
}
!if (MOVE_OFFSET + $925f) != $e95f {
    !error "Assertion failed: MOVE_OFFSET + $925f == $e95f"
}
!if (MOVE_OFFSET + $9288) != $e988 {
    !error "Assertion failed: MOVE_OFFSET + $9288 == $e988"
}
!if (MOVE_OFFSET + $929b) != $e99b {
    !error "Assertion failed: MOVE_OFFSET + $929b == $e99b"
}
!if (MOVE_OFFSET + $92a6) != $e9a6 {
    !error "Assertion failed: MOVE_OFFSET + $92a6 == $e9a6"
}
!if (MOVE_OFFSET + $92b4) != $e9b4 {
    !error "Assertion failed: MOVE_OFFSET + $92b4 == $e9b4"
}
!if (MOVE_OFFSET + $92b7) != $e9b7 {
    !error "Assertion failed: MOVE_OFFSET + $92b7 == $e9b7"
}
!if (MOVE_OFFSET + $92d3) != $e9d3 {
    !error "Assertion failed: MOVE_OFFSET + $92d3 == $e9d3"
}
!if (MOVE_OFFSET + $92df) != $e9df {
    !error "Assertion failed: MOVE_OFFSET + $92df == $e9df"
}
!if (MOVE_OFFSET + $92e2) != $e9e2 {
    !error "Assertion failed: MOVE_OFFSET + $92e2 == $e9e2"
}
!if (MOVE_OFFSET + $92ee) != $e9ee {
    !error "Assertion failed: MOVE_OFFSET + $92ee == $e9ee"
}
!if (MOVE_OFFSET + $930a) != $ea0a {
    !error "Assertion failed: MOVE_OFFSET + $930a == $ea0a"
}
!if (MOVE_OFFSET + $930d) != $ea0d {
    !error "Assertion failed: MOVE_OFFSET + $930d == $ea0d"
}
!if (MOVE_OFFSET + $9336) != $ea36 {
    !error "Assertion failed: MOVE_OFFSET + $9336 == $ea36"
}
!if (MOVE_OFFSET + $934f) != $ea4f {
    !error "Assertion failed: MOVE_OFFSET + $934f == $ea4f"
}
!if (MOVE_OFFSET + $9354) != $ea54 {
    !error "Assertion failed: MOVE_OFFSET + $9354 == $ea54"
}
!if (MOVE_OFFSET + $93be) != $eabe {
    !error "Assertion failed: MOVE_OFFSET + $93be == $eabe"
}
!if (MOVE_OFFSET + $93d5) != $ead5 {
    !error "Assertion failed: MOVE_OFFSET + $93d5 == $ead5"
}
!if (MOVE_OFFSET + $93d8) != $ead8 {
    !error "Assertion failed: MOVE_OFFSET + $93d8 == $ead8"
}
!if (MOVE_OFFSET + $93db) != $eadb {
    !error "Assertion failed: MOVE_OFFSET + $93db == $eadb"
}
!if (MOVE_OFFSET + $93eb) != $eaeb {
    !error "Assertion failed: MOVE_OFFSET + $93eb == $eaeb"
}
!if (MOVE_OFFSET + $93ee) != $eaee {
    !error "Assertion failed: MOVE_OFFSET + $93ee == $eaee"
}
!if (MOVE_OFFSET + $93f1) != $eaf1 {
    !error "Assertion failed: MOVE_OFFSET + $93f1 == $eaf1"
}
!if (MOVE_OFFSET + $9403) != $eb03 {
    !error "Assertion failed: MOVE_OFFSET + $9403 == $eb03"
}
!if (MOVE_OFFSET + $9406) != $eb06 {
    !error "Assertion failed: MOVE_OFFSET + $9406 == $eb06"
}
!if (MOVE_OFFSET + $9411) != $eb11 {
    !error "Assertion failed: MOVE_OFFSET + $9411 == $eb11"
}
!if (MOVE_OFFSET + $9449) != $eb49 {
    !error "Assertion failed: MOVE_OFFSET + $9449 == $eb49"
}
!if (MOVE_OFFSET + $944c) != $eb4c {
    !error "Assertion failed: MOVE_OFFSET + $944c == $eb4c"
}
!if (MOVE_OFFSET + $944f) != $eb4f {
    !error "Assertion failed: MOVE_OFFSET + $944f == $eb4f"
}
!if (MOVE_OFFSET + $9478) != $eb78 {
    !error "Assertion failed: MOVE_OFFSET + $9478 == $eb78"
}
!if (MOVE_OFFSET + $9486) != $eb86 {
    !error "Assertion failed: MOVE_OFFSET + $9486 == $eb86"
}
!if (MOVE_OFFSET + $94b5) != $ebb5 {
    !error "Assertion failed: MOVE_OFFSET + $94b5 == $ebb5"
}
!if (MOVE_OFFSET + $94b8) != $ebb8 {
    !error "Assertion failed: MOVE_OFFSET + $94b8 == $ebb8"
}
!if (MOVE_OFFSET + $94bb) != $ebbb {
    !error "Assertion failed: MOVE_OFFSET + $94bb == $ebbb"
}
!if (MOVE_OFFSET + $9538) != $ec38 {
    !error "Assertion failed: MOVE_OFFSET + $9538 == $ec38"
}
!if (MOVE_OFFSET + $9574) != $ec74 {
    !error "Assertion failed: MOVE_OFFSET + $9574 == $ec74"
}
!if (MOVE_OFFSET + $9577) != $ec77 {
    !error "Assertion failed: MOVE_OFFSET + $9577 == $ec77"
}
!if (MOVE_OFFSET + $957a) != $ec7a {
    !error "Assertion failed: MOVE_OFFSET + $957a == $ec7a"
}
!if (MOVE_OFFSET + $9594) != $ec94 {
    !error "Assertion failed: MOVE_OFFSET + $9594 == $ec94"
}
!if (MOVE_OFFSET + $9597) != $ec97 {
    !error "Assertion failed: MOVE_OFFSET + $9597 == $ec97"
}
!if (MOVE_OFFSET + $959c) != $ec9c {
    !error "Assertion failed: MOVE_OFFSET + $959c == $ec9c"
}
!if (MOVE_OFFSET + $959f) != $ec9f {
    !error "Assertion failed: MOVE_OFFSET + $959f == $ec9f"
}
!if (MOVE_OFFSET + $95a4) != $eca4 {
    !error "Assertion failed: MOVE_OFFSET + $95a4 == $eca4"
}
!if (MOVE_OFFSET + $95a7) != $eca7 {
    !error "Assertion failed: MOVE_OFFSET + $95a7 == $eca7"
}
!if (MOVE_OFFSET + $95ac) != $ecac {
    !error "Assertion failed: MOVE_OFFSET + $95ac == $ecac"
}
!if (MOVE_OFFSET + $95be) != $ecbe {
    !error "Assertion failed: MOVE_OFFSET + $95be == $ecbe"
}
!if (MOVE_OFFSET + $95c1) != $ecc1 {
    !error "Assertion failed: MOVE_OFFSET + $95c1 == $ecc1"
}
!if (MOVE_OFFSET + $95f6) != $ecf6 {
    !error "Assertion failed: MOVE_OFFSET + $95f6 == $ecf6"
}
!if (MOVE_OFFSET + $9604) != $ed04 {
    !error "Assertion failed: MOVE_OFFSET + $9604 == $ed04"
}
!if (MOVE_OFFSET + $960d) != $ed0d {
    !error "Assertion failed: MOVE_OFFSET + $960d == $ed0d"
}
!if (MOVE_OFFSET + $9610) != $ed10 {
    !error "Assertion failed: MOVE_OFFSET + $9610 == $ed10"
}
!if (MOVE_OFFSET + $9613) != $ed13 {
    !error "Assertion failed: MOVE_OFFSET + $9613 == $ed13"
}
!if (MOVE_OFFSET + $9624) != $ed24 {
    !error "Assertion failed: MOVE_OFFSET + $9624 == $ed24"
}
!if (MOVE_OFFSET + $962b) != $ed2b {
    !error "Assertion failed: MOVE_OFFSET + $962b == $ed2b"
}
!if (MOVE_OFFSET + $962e) != $ed2e {
    !error "Assertion failed: MOVE_OFFSET + $962e == $ed2e"
}
!if (MOVE_OFFSET + $9631) != $ed31 {
    !error "Assertion failed: MOVE_OFFSET + $9631 == $ed31"
}
!if (MOVE_OFFSET + $9634) != $ed34 {
    !error "Assertion failed: MOVE_OFFSET + $9634 == $ed34"
}
!if (MOVE_OFFSET + $9644) != $ed44 {
    !error "Assertion failed: MOVE_OFFSET + $9644 == $ed44"
}
!if (MOVE_OFFSET + $9647) != $ed47 {
    !error "Assertion failed: MOVE_OFFSET + $9647 == $ed47"
}
!if (MOVE_OFFSET + $966e) != $ed6e {
    !error "Assertion failed: MOVE_OFFSET + $966e == $ed6e"
}
!if (MOVE_OFFSET + $96ac) != $edac {
    !error "Assertion failed: MOVE_OFFSET + $96ac == $edac"
}
!if (MOVE_OFFSET + $96b9) != $edb9 {
    !error "Assertion failed: MOVE_OFFSET + $96b9 == $edb9"
}
!if (MOVE_OFFSET + $96d4) != $edd4 {
    !error "Assertion failed: MOVE_OFFSET + $96d4 == $edd4"
}
!if (MOVE_OFFSET + $96e7) != $ede7 {
    !error "Assertion failed: MOVE_OFFSET + $96e7 == $ede7"
}
!if (MOVE_OFFSET + $9704) != $ee04 {
    !error "Assertion failed: MOVE_OFFSET + $9704 == $ee04"
}
!if (MOVE_OFFSET + $9720) != $ee20 {
    !error "Assertion failed: MOVE_OFFSET + $9720 == $ee20"
}
!if (MOVE_OFFSET + $9751) != $ee51 {
    !error "Assertion failed: MOVE_OFFSET + $9751 == $ee51"
}
!if (MOVE_OFFSET + $9754) != $ee54 {
    !error "Assertion failed: MOVE_OFFSET + $9754 == $ee54"
}
!if (MOVE_OFFSET + $975f) != $ee5f {
    !error "Assertion failed: MOVE_OFFSET + $975f == $ee5f"
}
!if (MOVE_OFFSET + $9762) != $ee62 {
    !error "Assertion failed: MOVE_OFFSET + $9762 == $ee62"
}
!if (MOVE_OFFSET + $976f) != $ee6f {
    !error "Assertion failed: MOVE_OFFSET + $976f == $ee6f"
}
!if (MOVE_OFFSET + $9772) != $ee72 {
    !error "Assertion failed: MOVE_OFFSET + $9772 == $ee72"
}
!if (MOVE_OFFSET + $9775) != $ee75 {
    !error "Assertion failed: MOVE_OFFSET + $9775 == $ee75"
}
!if (MOVE_OFFSET + $978a) != $ee8a {
    !error "Assertion failed: MOVE_OFFSET + $978a == $ee8a"
}
!if (MOVE_OFFSET + $97d4) != $eed4 {
    !error "Assertion failed: MOVE_OFFSET + $97d4 == $eed4"
}
!if (MOVE_OFFSET + $97f2) != $eef2 {
    !error "Assertion failed: MOVE_OFFSET + $97f2 == $eef2"
}
!if (MOVE_OFFSET + $97f5) != $eef5 {
    !error "Assertion failed: MOVE_OFFSET + $97f5 == $eef5"
}
!if (MOVE_OFFSET + $980e) != $ef0e {
    !error "Assertion failed: MOVE_OFFSET + $980e == $ef0e"
}
!if (MOVE_OFFSET + $9812) != $ef12 {
    !error "Assertion failed: MOVE_OFFSET + $9812 == $ef12"
}
!if (MOVE_OFFSET + $9816) != $ef16 {
    !error "Assertion failed: MOVE_OFFSET + $9816 == $ef16"
}
!if (MOVE_OFFSET + $981a) != $ef1a {
    !error "Assertion failed: MOVE_OFFSET + $981a == $ef1a"
}
!if (MOVE_OFFSET + $981d) != $ef1d {
    !error "Assertion failed: MOVE_OFFSET + $981d == $ef1d"
}
!if (MOVE_OFFSET + $9822) != $ef22 {
    !error "Assertion failed: MOVE_OFFSET + $9822 == $ef22"
}
!if (MOVE_OFFSET + $9825) != $ef25 {
    !error "Assertion failed: MOVE_OFFSET + $9825 == $ef25"
}
!if (MOVE_OFFSET + $9849) != $ef49 {
    !error "Assertion failed: MOVE_OFFSET + $9849 == $ef49"
}
!if (MOVE_OFFSET + $9860) != $ef60 {
    !error "Assertion failed: MOVE_OFFSET + $9860 == $ef60"
}
!if (MOVE_OFFSET + $988a) != $ef8a {
    !error "Assertion failed: MOVE_OFFSET + $988a == $ef8a"
}
!if (MOVE_OFFSET + $988d) != $ef8d {
    !error "Assertion failed: MOVE_OFFSET + $988d == $ef8d"
}
!if (MOVE_OFFSET + $98c1) != $efc1 {
    !error "Assertion failed: MOVE_OFFSET + $98c1 == $efc1"
}
!if (MOVE_OFFSET + $98c4) != $efc4 {
    !error "Assertion failed: MOVE_OFFSET + $98c4 == $efc4"
}
!if (MOVE_OFFSET + $98c7) != $efc7 {
    !error "Assertion failed: MOVE_OFFSET + $98c7 == $efc7"
}
!if (MOVE_OFFSET + $98cb) != $efcb {
    !error "Assertion failed: MOVE_OFFSET + $98cb == $efcb"
}
!if (MOVE_OFFSET + $98d4) != $efd4 {
    !error "Assertion failed: MOVE_OFFSET + $98d4 == $efd4"
}
!if (MOVE_OFFSET + $98f0) != $eff0 {
    !error "Assertion failed: MOVE_OFFSET + $98f0 == $eff0"
}
!if (MOVE_OFFSET + $98f3) != $eff3 {
    !error "Assertion failed: MOVE_OFFSET + $98f3 == $eff3"
}
!if (MOVE_OFFSET + $98fe) != $effe {
    !error "Assertion failed: MOVE_OFFSET + $98fe == $effe"
}
!if (MOVE_OFFSET + $9910) != $f010 {
    !error "Assertion failed: MOVE_OFFSET + $9910 == $f010"
}
!if (MOVE_OFFSET + $9913) != $f013 {
    !error "Assertion failed: MOVE_OFFSET + $9913 == $f013"
}
!if (MOVE_OFFSET + $992f) != $f02f {
    !error "Assertion failed: MOVE_OFFSET + $992f == $f02f"
}
!if (MOVE_OFFSET + $993c) != $f03c {
    !error "Assertion failed: MOVE_OFFSET + $993c == $f03c"
}
!if (MOVE_OFFSET + $9946) != $f046 {
    !error "Assertion failed: MOVE_OFFSET + $9946 == $f046"
}
!if (MOVE_OFFSET + $9953) != $f053 {
    !error "Assertion failed: MOVE_OFFSET + $9953 == $f053"
}
!if (MOVE_OFFSET + $9959) != $f059 {
    !error "Assertion failed: MOVE_OFFSET + $9959 == $f059"
}
!if (MOVE_OFFSET + $9962) != $f062 {
    !error "Assertion failed: MOVE_OFFSET + $9962 == $f062"
}
!if (MOVE_OFFSET + $9965) != $f065 {
    !error "Assertion failed: MOVE_OFFSET + $9965 == $f065"
}
!if (MOVE_OFFSET + $9968) != $f068 {
    !error "Assertion failed: MOVE_OFFSET + $9968 == $f068"
}
!if (MOVE_OFFSET + $998b) != $f08b {
    !error "Assertion failed: MOVE_OFFSET + $998b == $f08b"
}
!if (MOVE_OFFSET + $999e) != $f09e {
    !error "Assertion failed: MOVE_OFFSET + $999e == $f09e"
}
!if (MOVE_OFFSET + $99a1) != $f0a1 {
    !error "Assertion failed: MOVE_OFFSET + $99a1 == $f0a1"
}
!if (MOVE_OFFSET + $99a4) != $f0a4 {
    !error "Assertion failed: MOVE_OFFSET + $99a4 == $f0a4"
}
!if (MOVE_OFFSET + $99c1) != $f0c1 {
    !error "Assertion failed: MOVE_OFFSET + $99c1 == $f0c1"
}
!if (MOVE_OFFSET + $99c4) != $f0c4 {
    !error "Assertion failed: MOVE_OFFSET + $99c4 == $f0c4"
}
!if (MOVE_OFFSET + $99cd) != $f0cd {
    !error "Assertion failed: MOVE_OFFSET + $99cd == $f0cd"
}
!if (MOVE_OFFSET + $99ea) != $f0ea {
    !error "Assertion failed: MOVE_OFFSET + $99ea == $f0ea"
}
!if (MOVE_OFFSET + $99f4) != $f0f4 {
    !error "Assertion failed: MOVE_OFFSET + $99f4 == $f0f4"
}
!if (MOVE_OFFSET + $9a06) != $f106 {
    !error "Assertion failed: MOVE_OFFSET + $9a06 == $f106"
}
!if (MOVE_OFFSET + $9a09) != $f109 {
    !error "Assertion failed: MOVE_OFFSET + $9a09 == $f109"
}
!if (MOVE_OFFSET + $9a2c) != $f12c {
    !error "Assertion failed: MOVE_OFFSET + $9a2c == $f12c"
}
!if (MOVE_OFFSET + $9a45) != $f145 {
    !error "Assertion failed: MOVE_OFFSET + $9a45 == $f145"
}
!if (MOVE_OFFSET + $9a48) != $f148 {
    !error "Assertion failed: MOVE_OFFSET + $9a48 == $f148"
}
!if (MOVE_OFFSET + $9a4b) != $f14b {
    !error "Assertion failed: MOVE_OFFSET + $9a4b == $f14b"
}
!if (MOVE_OFFSET + $9a5d) != $f15d {
    !error "Assertion failed: MOVE_OFFSET + $9a5d == $f15d"
}
!if (MOVE_OFFSET + $9a60) != $f160 {
    !error "Assertion failed: MOVE_OFFSET + $9a60 == $f160"
}
!if (MOVE_OFFSET + $9a63) != $f163 {
    !error "Assertion failed: MOVE_OFFSET + $9a63 == $f163"
}
!if (MOVE_OFFSET + $9a8d) != $f18d {
    !error "Assertion failed: MOVE_OFFSET + $9a8d == $f18d"
}
!if (MOVE_OFFSET + $9a90) != $f190 {
    !error "Assertion failed: MOVE_OFFSET + $9a90 == $f190"
}
!if (MOVE_OFFSET + $9a93) != $f193 {
    !error "Assertion failed: MOVE_OFFSET + $9a93 == $f193"
}
!if (MOVE_OFFSET + $9abd) != $f1bd {
    !error "Assertion failed: MOVE_OFFSET + $9abd == $f1bd"
}
!if (MOVE_OFFSET + $9aca) != $f1ca {
    !error "Assertion failed: MOVE_OFFSET + $9aca == $f1ca"
}
!if (MOVE_OFFSET + $9ad5) != $f1d5 {
    !error "Assertion failed: MOVE_OFFSET + $9ad5 == $f1d5"
}
!if (MOVE_OFFSET + $9b3a) != $f23a {
    !error "Assertion failed: MOVE_OFFSET + $9b3a == $f23a"
}
!if (MOVE_OFFSET + $9b45) != $f245 {
    !error "Assertion failed: MOVE_OFFSET + $9b45 == $f245"
}
!if (MOVE_OFFSET + $9b53) != $f253 {
    !error "Assertion failed: MOVE_OFFSET + $9b53 == $f253"
}
!if (MOVE_OFFSET + $9b71) != $f271 {
    !error "Assertion failed: MOVE_OFFSET + $9b71 == $f271"
}
!if (MOVE_OFFSET + $9b7c) != $f27c {
    !error "Assertion failed: MOVE_OFFSET + $9b7c == $f27c"
}
!if (MOVE_OFFSET + $9b82) != $f282 {
    !error "Assertion failed: MOVE_OFFSET + $9b82 == $f282"
}
!if (MOVE_OFFSET + $9b97) != $f297 {
    !error "Assertion failed: MOVE_OFFSET + $9b97 == $f297"
}
!if (MOVE_OFFSET + $9bac) != $f2ac {
    !error "Assertion failed: MOVE_OFFSET + $9bac == $f2ac"
}
!if (MOVE_OFFSET + $9bc2) != $f2c2 {
    !error "Assertion failed: MOVE_OFFSET + $9bc2 == $f2c2"
}
!if (MOVE_OFFSET + $9bd5) != $f2d5 {
    !error "Assertion failed: MOVE_OFFSET + $9bd5 == $f2d5"
}
!if (MOVE_OFFSET + $9bda) != $f2da {
    !error "Assertion failed: MOVE_OFFSET + $9bda == $f2da"
}
!if (MOVE_OFFSET + $9bf0) != $f2f0 {
    !error "Assertion failed: MOVE_OFFSET + $9bf0 == $f2f0"
}
!if (MOVE_OFFSET + $9bf3) != $f2f3 {
    !error "Assertion failed: MOVE_OFFSET + $9bf3 == $f2f3"
}
!if (MOVE_OFFSET + $9c05) != $f305 {
    !error "Assertion failed: MOVE_OFFSET + $9c05 == $f305"
}
!if (MOVE_OFFSET + $9c15) != $f315 {
    !error "Assertion failed: MOVE_OFFSET + $9c15 == $f315"
}
!if (MOVE_OFFSET + $9c19) != $f319 {
    !error "Assertion failed: MOVE_OFFSET + $9c19 == $f319"
}
!if (MOVE_OFFSET + $9c21) != $f321 {
    !error "Assertion failed: MOVE_OFFSET + $9c21 == $f321"
}
!if (MOVE_OFFSET + $9c2f) != $f32f {
    !error "Assertion failed: MOVE_OFFSET + $9c2f == $f32f"
}
!if (MOVE_OFFSET + $9c34) != $f334 {
    !error "Assertion failed: MOVE_OFFSET + $9c34 == $f334"
}
!if (MOVE_OFFSET + $9c52) != $f352 {
    !error "Assertion failed: MOVE_OFFSET + $9c52 == $f352"
}
!if (MOVE_OFFSET + $9ccb) != $f3cb {
    !error "Assertion failed: MOVE_OFFSET + $9ccb == $f3cb"
}
!if (MOVE_OFFSET + $9ceb) != $f3eb {
    !error "Assertion failed: MOVE_OFFSET + $9ceb == $f3eb"
}
!if (MOVE_OFFSET + $9cf4) != $f3f4 {
    !error "Assertion failed: MOVE_OFFSET + $9cf4 == $f3f4"
}
!if (MOVE_OFFSET + $9cf7) != $f3f7 {
    !error "Assertion failed: MOVE_OFFSET + $9cf7 == $f3f7"
}
!if (MOVE_OFFSET + $9d0f) != $f40f {
    !error "Assertion failed: MOVE_OFFSET + $9d0f == $f40f"
}
!if (MOVE_OFFSET + $9d12) != $f412 {
    !error "Assertion failed: MOVE_OFFSET + $9d12 == $f412"
}
!if (MOVE_OFFSET + $9d1f) != $f41f {
    !error "Assertion failed: MOVE_OFFSET + $9d1f == $f41f"
}
!if (MOVE_OFFSET + $9d31) != $f431 {
    !error "Assertion failed: MOVE_OFFSET + $9d31 == $f431"
}
!if (MOVE_OFFSET + $9d74) != $f474 {
    !error "Assertion failed: MOVE_OFFSET + $9d74 == $f474"
}
!if (MOVE_OFFSET + $9d81) != $f481 {
    !error "Assertion failed: MOVE_OFFSET + $9d81 == $f481"
}
!if (MOVE_OFFSET + $9da4) != $f4a4 {
    !error "Assertion failed: MOVE_OFFSET + $9da4 == $f4a4"
}
!if (MOVE_OFFSET + $9da9) != $f4a9 {
    !error "Assertion failed: MOVE_OFFSET + $9da9 == $f4a9"
}
!if (MOVE_OFFSET + $9dac) != $f4ac {
    !error "Assertion failed: MOVE_OFFSET + $9dac == $f4ac"
}
!if (MOVE_OFFSET + $9db1) != $f4b1 {
    !error "Assertion failed: MOVE_OFFSET + $9db1 == $f4b1"
}
!if (MOVE_OFFSET + $9dc3) != $f4c3 {
    !error "Assertion failed: MOVE_OFFSET + $9dc3 == $f4c3"
}
!if (MOVE_OFFSET + $9dc8) != $f4c8 {
    !error "Assertion failed: MOVE_OFFSET + $9dc8 == $f4c8"
}
!if (MOVE_OFFSET + $9e81) != $f581 {
    !error "Assertion failed: MOVE_OFFSET + $9e81 == $f581"
}
!if (MOVE_OFFSET + $9eaa) != $f5aa {
    !error "Assertion failed: MOVE_OFFSET + $9eaa == $f5aa"
}
!if (MOVE_OFFSET + $9ed2) != $f5d2 {
    !error "Assertion failed: MOVE_OFFSET + $9ed2 == $f5d2"
}
!if (MOVE_OFFSET + $9ed9) != $f5d9 {
    !error "Assertion failed: MOVE_OFFSET + $9ed9 == $f5d9"
}
!if (MOVE_OFFSET + $9ee2) != $f5e2 {
    !error "Assertion failed: MOVE_OFFSET + $9ee2 == $f5e2"
}
!if (MOVE_OFFSET + $9eeb) != $f5eb {
    !error "Assertion failed: MOVE_OFFSET + $9eeb == $f5eb"
}
!if (MOVE_OFFSET + $9efd) != $f5fd {
    !error "Assertion failed: MOVE_OFFSET + $9efd == $f5fd"
}
!if (MOVE_OFFSET + $9f1b) != $f61b {
    !error "Assertion failed: MOVE_OFFSET + $9f1b == $f61b"
}
!if (MOVE_OFFSET + $9f3b) != $f63b {
    !error "Assertion failed: MOVE_OFFSET + $9f3b == $f63b"
}
!if (MOVE_OFFSET + $9f42) != $f642 {
    !error "Assertion failed: MOVE_OFFSET + $9f42 == $f642"
}
!if (MOVE_OFFSET + $9f66) != $f666 {
    !error "Assertion failed: MOVE_OFFSET + $9f66 == $f666"
}
!if (MOVE_OFFSET + $9f69) != $f669 {
    !error "Assertion failed: MOVE_OFFSET + $9f69 == $f669"
}
!if (MOVE_OFFSET + $9f6e) != $f66e {
    !error "Assertion failed: MOVE_OFFSET + $9f6e == $f66e"
}
!if (MOVE_OFFSET + $9f73) != $f673 {
    !error "Assertion failed: MOVE_OFFSET + $9f73 == $f673"
}
!if (MOVE_OFFSET + $9f82) != $f682 {
    !error "Assertion failed: MOVE_OFFSET + $9f82 == $f682"
}
!if (MOVE_OFFSET + $9f91) != $f691 {
    !error "Assertion failed: MOVE_OFFSET + $9f91 == $f691"
}
!if (MOVE_OFFSET + $9f94) != $f694 {
    !error "Assertion failed: MOVE_OFFSET + $9f94 == $f694"
}
!if (MOVE_OFFSET + $9fa4) != $f6a4 {
    !error "Assertion failed: MOVE_OFFSET + $9fa4 == $f6a4"
}
!if (MOVE_OFFSET + $9fab) != $f6ab {
    !error "Assertion failed: MOVE_OFFSET + $9fab == $f6ab"
}
!if (MOVE_OFFSET + $9fb5) != $f6b5 {
    !error "Assertion failed: MOVE_OFFSET + $9fb5 == $f6b5"
}
!if (MOVE_OFFSET + $9fbf) != $f6bf {
    !error "Assertion failed: MOVE_OFFSET + $9fbf == $f6bf"
}
!if (MOVE_OFFSET + $9fc2) != $f6c2 {
    !error "Assertion failed: MOVE_OFFSET + $9fc2 == $f6c2"
}
!if (MOVE_OFFSET + $9fc6) != $f6c6 {
    !error "Assertion failed: MOVE_OFFSET + $9fc6 == $f6c6"
}
!if (MOVE_OFFSET + $9fc9) != $f6c9 {
    !error "Assertion failed: MOVE_OFFSET + $9fc9 == $f6c9"
}
!if (MOVE_OFFSET + $9fd7) != $f6d7 {
    !error "Assertion failed: MOVE_OFFSET + $9fd7 == $f6d7"
}
!if (MOVE_OFFSET + $9fda) != $f6da {
    !error "Assertion failed: MOVE_OFFSET + $9fda == $f6da"
}
!if (MOVE_OFFSET + $9fdd) != $f6dd {
    !error "Assertion failed: MOVE_OFFSET + $9fdd == $f6dd"
}
!if (MOVE_OFFSET + $9fe0) != $f6e0 {
    !error "Assertion failed: MOVE_OFFSET + $9fe0 == $f6e0"
}
!if (MOVE_OFFSET + $9fe3) != $f6e3 {
    !error "Assertion failed: MOVE_OFFSET + $9fe3 == $f6e3"
}
!if (MOVE_OFFSET + $a004) != $f704 {
    !error "Assertion failed: MOVE_OFFSET + $a004 == $f704"
}
!if (MOVE_OFFSET + $a02d) != $f72d {
    !error "Assertion failed: MOVE_OFFSET + $a02d == $f72d"
}
!if (NILOFF) != $19 {
    !error "Assertion failed: NILOFF == $19"
}
!if (NUMF) != $04 {
    !error "Assertion failed: NUMF == $04"
}
!if (RAMBS) != $7c {
    !error "Assertion failed: RAMBS == $7c"
}
!if (RAMBS+1) != $7d {
    !error "Assertion failed: RAMBS+1 == $7d"
}
!if (RELBS) != $7a {
    !error "Assertion failed: RELBS == $7a"
}
!if (RELBS+1) != $7b {
    !error "Assertion failed: RELBS+1 == $7b"
}
!if (RELOC) != $7c {
    !error "Assertion failed: RELOC == $7c"
}
!if (RELOC+1) != $7d {
    !error "Assertion failed: RELOC+1 == $7d"
}
!if (ROMBS) != $7a {
    !error "Assertion failed: ROMBS == $7a"
}
!if (ROMBS+1) != $7b {
    !error "Assertion failed: ROMBS+1 == $7b"
}
!if (SUBOFF) != $58 {
    !error "Assertion failed: SUBOFF == $58"
}
!if (SUBRF) != $08 {
    !error "Assertion failed: SUBRF == $08"
}
!if (TVS) != $30 {
    !error "Assertion failed: TVS == $30"
}
!if (TVS+1) != $31 {
    !error "Assertion failed: TVS+1 == $31"
}
!if (TVS+10) != $3a {
    !error "Assertion failed: TVS+10 == $3a"
}
!if (TVS-1) != $2f {
    !error "Assertion failed: TVS-1 == $2f"
}
!if (VALOFF) != $0d {
    !error "Assertion failed: VALOFF == $0d"
}
!if (WARMST) != $2a {
    !error "Assertion failed: WARMST == $2a"
}
!if (WRMOFF) != $72 {
    !error "Assertion failed: WRMOFF == $72"
}
!if (WSBOT) != $02 {
    !error "Assertion failed: WSBOT == $02"
}
!if (XTNDL) != $74 {
    !error "Assertion failed: XTNDL == $74"
}
!if (osbyte_acknowledge_escape) != $7e {
    !error "Assertion failed: osbyte_acknowledge_escape == $7e"
}
!if (osbyte_enter_language) != $8e {
    !error "Assertion failed: osbyte_enter_language == $8e"
}
!if (osbyte_inkey) != $81 {
    !error "Assertion failed: osbyte_inkey == $81"
}
!if (osbyte_read_adc_or_get_buffer_status) != $80 {
    !error "Assertion failed: osbyte_read_adc_or_get_buffer_status == $80"
}
!if (osbyte_read_high_order_address) != $82 {
    !error "Assertion failed: osbyte_read_high_order_address == $82"
}
!if (osbyte_read_himem) != $84 {
    !error "Assertion failed: osbyte_read_himem == $84"
}
!if (osbyte_read_himem_for_mode) != $85 {
    !error "Assertion failed: osbyte_read_himem_for_mode == $85"
}
!if (osbyte_read_oshwm) != $83 {
    !error "Assertion failed: osbyte_read_oshwm == $83"
}
!if (osbyte_read_tube_presence) != $ea {
    !error "Assertion failed: osbyte_read_tube_presence == $ea"
}
!if (osfile_load) != $ff {
    !error "Assertion failed: osfile_load == $ff"
}
!if (osfile_save) != $00 {
    !error "Assertion failed: osfile_save == $00"
}
!if (osfind_close) != $00 {
    !error "Assertion failed: osfind_close == $00"
}
!if (osfind_open_output) != $80 {
    !error "Assertion failed: osfind_open_output == $80"
}
!if (osword_read_clock) != $01 {
    !error "Assertion failed: osword_read_clock == $01"
}
!if (osword_read_interval_timer) != $03 {
    !error "Assertion failed: osword_read_interval_timer == $03"
}
!if (osword_read_io_memory) != $05 {
    !error "Assertion failed: osword_read_io_memory == $05"
}
!if (osword_read_line) != $00 {
    !error "Assertion failed: osword_read_line == $00"
}
!if (osword_read_pixel) != $09 {
    !error "Assertion failed: osword_read_pixel == $09"
}
!if (osword_sound) != $07 {
    !error "Assertion failed: osword_sound == $07"
}
!if (osword_write_clock) != $02 {
    !error "Assertion failed: osword_write_clock == $02"
}
!if (service_star_help_command) != $09 {
    !error "Assertion failed: service_star_help_command == $09"
}
!if (service_unrecognised_star_command) != $04 {
    !error "Assertion failed: service_unrecognised_star_command == $04"
}
