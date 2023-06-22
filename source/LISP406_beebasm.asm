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
GCNO    = &0000
GARX    = &0002
GARRY   = &0003
GCA     = &0004
GABBY   = &0006
SA      = &0007
l0009   = &0009
l000a   = &000a
DISPM   = &000b
ERRCNT  = &000d
TOPBIN  = &000f
ERRNO   = &0011
LINEPP  = &0012
HANDLE  = &0013
NBUFF   = &0014
NIND    = &0019
LEVEL   = &001a
OLDEXT  = &001b
END     = &001d
AA      = &001e
AB      = &0020
YSAV    = &0022
REP     = &0023
LINENO  = &0025
ARG     = &0030
TVS     = &0030
BINDER  = &0032
TVSEXT  = &0033
WSA     = &0034
WSB     = &0036
WSC     = &0038
ARGA    = &003a
ARGB    = &003c
ARGC    = &003e
ARGD    = &0040
ARGE    = &0042
ACL     = &0072
XTNDL   = &0074
AUXL    = &0076
SIGN    = &0078
ARETOP  = &0079
POINT   = &007a
RELBS   = &007a
ROMBS   = &007a
CELL    = &007c
RAMBS   = &007c
RELOC   = &007c
SP      = &007e
SIZE    = &0082
WSD     = &0083
RETADD  = &0085
AD      = &0087
XPR     = &0089
DEPTH   = &008b
LSBUFF  = &008c
POPPY   = &008d
TERMCH  = &008e
ACIN    = &008f
ARGINF  = &0090
OLDLEV  = &0097
CLISTR  = &00f2
BRKAD   = &00fd
KBD     = &00ff
brkv    = &0202
FIRST   = &0400
TUBE    = &0401
ACL40   = &0402
REM40   = &0407
AUX40   = &040c
TEMP40  = &0411
IODCB   = &0416
PWORD   = &041b
GCTIME  = &0420
TIMEW   = &0425
MODEF   = &042a
VECBOT  = &042b
IMBOT   = &042c
AREVAL  = &042d
ESCHF   = &042f
LSCHAR  = &0430
STATYP  = &0431
MEMINV  = &0432
RELOFF  = &0433
GENCNT  = &0434
OSINFO  = &0438
DL      = &0450
DH      = &0520
IMBUF   = &0600
NAMBUF  = &0600
OSWBUF  = &0600
DOSBUF  = &0700
HILISP  = &d700
osfind  = &ffce
osbput  = &ffd4
osbget  = &ffd7
osargs  = &ffda
osfile  = &ffdd
osrdch  = &ffe0
osasci  = &ffe3
oswrch  = &ffee
osword  = &fff1
osbyte  = &fff4
oscli   = &fff7

    org &8000

; Standard language ROM header
.entry_point
.LISPST
.LISVAL
.ROMHDR
.pydis_start
    jmp INITUR

    jmp INITSE                                     ; Service entry

    equb &e2                                       ; Language, service & tube
    equb CPYOFF-ROMHDR
    equb 1                                         ; ROM version
.TITLE
    equs "LISP"
    equb 0
.VERSN
    equs "4.06"
.CPYOFF
    equb 0
    equs "(C)1983 Acornsoft/1979 Owl Computers"
    equb 0
.TUBOFF
    equb   0, &80,   0,   0                        ; Tube load addr


    ; ******************************
    ; Initialisation routine
    ; ******************************
.INITUR
    cmp #1                                         ; Satisfactory?
    beq CRYON
    rts

.CRYON
    cli
    cld
    lda #osbyte_read_high_order_address
    jsr osbyte                                     ; Find out if in tube; Read the filing system 'machine high order address'
    stx TUBE
    txa
    beq COLD
    jmp WRMCHK

.COLD
    lda #osbyte_read_oshwm                         ; Find PAGE
    jsr osbyte                                     ; Read top of operating system RAM address (OSHWM)
    sty VECBOT
    cpx #0
    beq PBOUND
    inc VECBOT                                     ; Put on page boundary
.PBOUND
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
    lda #&f0
    sta LSBUFF                                     ; Fudge first reads
    lda #&0d
    sta LSCHAR
    ldx #3
    lda #'0'
.LOOP5
    sta GENCNT,x
    dex
    bpl LOOP5
    lda TUBE
    beq INTUBE
    lda #WARMST                                    ; No more to do if in I/O
    sta STATYP
    jmp INUREL


    ; **** Copy to HILISP
.INTUBE
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
.LOOP3
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
.COPY
    ldy #0
.LOOP1
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
.CHADD
    sta RELOFF
    ldx #0
    ldy #0
.LOOP2
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

.CHRTS
    rts


    ; ******************************
    ; Claim as service
    ; ******************************
.INITSE
    cmp #service_unrecognised_star_command         ; Reasonable?
    beq OKCALL
    cmp #service_star_help_command                 ; Help?
    beq HELP
    rts

.HELP
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

.OKCALL
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
    cmp #&0d
    bne NOTLSP
    lda #osbyte_read_tube_presence
    ldx #0
    ldy #&ff
    jsr osbyte                                     ; Is there a tube?; Read Tube present flag
    txa
    bne STLISP                                     ; If so don't flag
    lda #COLDST
    sta STATYP
.STLISP
    pla
    tax                                            ; X=ROM number
    lda #osbyte_enter_language
    jmp osbyte                                     ; Start up LISP; Enter language ROM X

.NOTLSP
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
.TEXT
    equs &0d, &0d, "Evaluate :", &80+' '
.VALTXT
    equs &0d, "Value is :", &80+' '
.NILTXT
    equs "NI", &80+'L'
.DOTTXT
    equs " .", &80+' '
.INSTXT
    equs &0d, "Insufficient memor", &80+'y'
.GCTXT
    equs &0d, "G.C. ", &80+'#'
.COLTXT
    equs " Bytes collected,", &80+' '
.FRTXT
    equs " Bytes free", &80+&0d
.SUBTXT
    equs "Subr", &80+'#'
.ERRTXT
    equs &0d, "Error number", &80+' '
.ARGTXT
    equs &0d, "Arg :", &80+' '
.WRMTXT
    equs &0d, "Warm or cold start (W/C) ?", &80+' '
.HLPTXT
    equs &0d, "LISP 4.06", &80+&0d

    ; **** Command lines
.GOSTR
    equs "GO D700"
    equb &0d
.LISTR
    equs "LISP"
    equb &0d

    ; **** Readline control block
.INCB
    equb <(DOSBUF), >(DOSBUF)                      ; Buffer address for input (2 bytes)
    equb &7f                                       ; Max line length; Maximum line length
    equb &20                                       ; Min. acceptable character value
    equb &7f                                       ; Max. acceptable character value

    ; **** Terminators
.TERMS
    equs "). ("
    equb &0d, &0a, &21, &27

    ; **** CAR/CDR table
.CXXXR
    equb 0, 2, 2, 2, 0, 4, 2, 2, 0, 2, 4, 2, 0, 4
    equb 4, 2, 0, 2, 2, 4, 0, 4, 2, 4, 0, 2, 4, 4
    equb 0, 4, 4, 4

    ; **** Table of CHARS lengths
.LENTAB
    equb   3,   6,   9, &0a,   0

    ; **** Untraceable errors
.CATTAB
    equb   0,   1,   2, &0b, &0c, &ff

    ; **** Zero time
.TIMZER
    equb 0, 0, 0, 0, 0                             ; Five byte clock value (low byte to high byte)


    ; ******************************
    ; Routine to give optionsl warm
    ; start (ie OBLIST retained)
    ; ******************************
.WRMCHK
    lda STATYP
    cmp #WARMST
    bne NOTWRM
    jmp WARM                                       ; Offer warm start

.NOTWRM
    lda TUBE
    bne COLD1
    lda #WARMST                                    ; We're in tube and
    sta STATYP                                     ; it's first time through
    jmp INUREL

.COLD1
    jmp COLD

.WARM
    lda MEMINV
    bne REBOOT
    ldx #WRMOFF
    jsr MESSAH
    jsr osrdch                                     ; Read a character from the current input stream
    and #&7f
    cmp #'C'
    beq REBOOT
    cmp #'c'
    beq REBOOT
    lda #'W'
    jsr oswrch                                     ; Write character 87
    jmp INIT


    ; **** Reload from ROM
.REBOOT
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
.SUPER
    lda #0                                         ; Reset stack
    sta SP
    lda ARETOP
    sta SP+1
    ldx #&ff                                       ; Reset hardware stack
    txs
    lda MODEF                                      ; Check mode flag
    bpl EVPR
    jsr MODCHN                                     ;  Change mode
.EVPR
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
.ATOM
    lda ARGA+1                                     ; Is ARGA atom?
    beq YES                                        ; NIL => yes
    ldy #0
    lda (ARGA),y                                   ; check bit 7
    bpl YES
    bmi NO

    ; **** EQ
.EQ
    lda ARGB+1                                     ; ARGA = ARGB?
    beq NULL                                       ; ARGB NIL => ARGA NIL
    cmp ARGA+1
    bne EQUATE                                     ; But they are numeric
    lda ARGB
    cmp ARGA
    beq YES
.EQUATE
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
.EQUINE
    lda (ARGA),y                                   ; Compare value
    cmp (ARGB),y
    bne NO
    dey
    bne EQUINE
    beq YES

    ; **** NULL
.NULL
    lda ARGA+1                                     ; Is ARGA NIL?
    beq YES
.NO
    lda #0                                         ; Returns NIL
    beq POPA
.YES
    lda #<TRUE                                     ; Returns T
    sta ARG
    lda IMBOT
.POPA
    sta ARG+1

    ; ******************************
    ; POP is one of the two LISP
    ; stack handling routines. It
    ; restores old binding values
    ; and WSA, WSB and WSC from the
    ; stack and POP the stack. ARG
    ; is unchanged.
    ; ******************************
.POP
    ldy #0                                         ; Get binding size
    lda (SP),y
    beq NOBOUN                                     ; No bindings to do
    tay
.POPPLE
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
.NOBOUN
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
.MORSP
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
.TVF
    adc #0
    inc SP+1
.TVG
    sta SP
    rts


    ; ******************************
    ; STACK stores:
    ;    Extent of TVS, WSA, WSB,
    ;    WSC, ARG and return addr
    ;     on the LISP stack
    ; ******************************
.STACK
    lda SP
    clc
    sbc TVSEXT
    tax
    bne EXTRAM
    clc
.EXTRAM
    dex
    lda SP+1
    sbc #0
    cmp AREVAL+1
    bcc SQUAT
    bne SROOM
    cpx AREVAL
    bcs SROOM
.SQUAT
    jsr RUBBSH
    bne STACK
.STIR
    brk                                            ; None found

    equb 0
    equs "No room"
    equb 0

.SROOM
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
.PILE
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
.BIND
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
.SQUASH
    jsr RUBBSH
    bne BIND
.BEAR
    brk                                            ; None found

    equb 1
    equs "No room"
    equb 0

.XROOM
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
.ALNUM
    lda #4
.ALVEC
    ldx #NUMF
    bne SPACE
.ALCHAR
    ldx #CHARF
    beq SPACE
.ALFSBR
    ldx #FSUBRF
    lda #6
    bne SPACE
.ALSUBR
    ldx #SUBRF
    lda #6
    bne SPACE
.ALPAIR
    lda #5
    ldx #&80                                       ; Pointer space
.SPACE
    sta SIZE
.SPACEB
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
.SQUID
    jsr RUBBSH
    bne SPACEB
.ALLO
    brk                                            ; None found

    equb 2
    equs "No room"
    equb 0

.ROOM
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
.OBSCUR
    ldy #1
    lda SIZE
    sta (POINT),y
    rts

.PINS
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
.INIT
    lda #<ERROR
    sta brkv                                       ; Error handling
    lda #>ERROR
    sta brkv+1
    lda #osbyte_read_himem                         ; Find end of memory
    jsr osbyte                                     ; Read top of user memory (HIMEM)
    sty ARETOP

    ; **** Set up various values
    lda #&fc                                       ; Messages except GC
    sta LEVEL
    ldx #0
    stx HANDLE                                     ; Output to screen
    stx GCNO                                       ; Zero collections
    stx GCNO+1
    stx ERRCNT                                     ; Zero error count
    stx ERRCNT+1
    ldy #&10                                       ; Clear TVS etc.
.ZLP
    stx TVS,y
    dey
    bpl ZLP
    lda #&0a                                       ; Initially no args
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
.MESSAH
    ldy #0
.MESSAI
    sty HANDLE
.MESSAG
    lda TEXT,x                                     ; Print message
    php
    and #&7f                                       ; Remove flag bit
    jsr OUT
    inx
    plp                                            ; Retrieve flag
    bpl MESSAG                                     ; Bit 7 not set
    rts

.OUT
    ldy HANDLE                                     ; to screen?
    beq OUTSCR
    jmp osbput                                     ; Write a single byte A to an open file Y

.OUTSCR
    cmp #&0d
    bne NCR
.CROUT
    lda #&0d
.NCR
    jmp osasci                                     ; Print the char; Write character 13


    ; ******************************
    ; Main evaluation routine
    ;    arg           -> ARG
    ;    NIL           -> NIL
    ;    number, entry -> same
    ;    char          -> same
    ;    list          -> eval fn
    ; ******************************
.NXEVAL
    lda WSA+1
    bne NXEVAM
.FSARG
    brk                                            ; No more args

    equb 3
    equs "Too few arguments"
    equb 0

.NXEVAM
    jsr NXTARH

    ; **** Main evaluator
.EVALU
    jsr KBCHK                                      ; Main entry
    beq EVAL1
    brk                                            ; Escape

    equb 4
    equs "Escape"
    equb 0

.KBCHK
    ldx #<(IODCB)
    ldy #>(IODCB)
    lda #osword_read_io_memory
    jsr osword                                     ; Read byte of I/O processor memory
    lda #&80
    and IODCB+4
    rts

.EVAL1
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

.EVLIST
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
.FUNERR
    brk                                            ; Can't make function

    equb 6
    equs "Function expected"
    equb 0

.EVARTS
    rts

.FUN
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

.ENT
    tay                                            ; Probably entry
    pla
    pla
    lda ARG
    sta WSC                                        ; Keep fn safe
    lda ARG+1
    sta WSC+1
    ldx #&0a
    stx TVSEXT
    cpy #FSUBRF
    beq ISFSBR
    cpy #SUBRF
    beq ISSUBR
    bne FUNERR                                     ; Oops a number!
.MORAG
    jsr NXEVAM
    ldx TVSEXT
    cpx #&42
    bcs NARGER
    lda ARG                                        ; Args eval in TVS
    sta TVS,x
    inx
    lda ARG+1
    sta TVS,x
    inx
    stx TVSEXT
.ISSUBR
    lda WSA+1
    bne MORAG
.INSUBR
    ldy #1
    lda (WSC),y
    asl a
    clc
    adc #&0a
    tax
    cpx TVSEXT
    beq ISFSBR
    bcc ISFSBR
.NARGER
    brk                                            ; Wrong

    equb 6
    equs "Wrong number of arguments"
    equb 0

.ISFSBR
    ldy #3
    lda (WSC),y
    beq GOSUB
    sta WSD+1
    dey
    lda (WSC),y
    sta WSD                                        ; List in WSD
    jmp PLOP

.DEFLST
    pha
    dey
    lda (WSD),y
    sta WSD
    pla
    sta WSD+1
.PLOP
    cpx TVSEXT                                     ; Default needed
    bcc SKIP
    ldy #1
    lda (WSD),y
    sta TVS,x
    iny
    lda (WSD),y
    sta TVS+1,x
.SKIP
    inx
    inx
    ldy #4
    lda (WSD),y
    bne DEFLST
    stx TVSEXT
.GOSUB
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
.LAMOK
    pla
    pla
    ldy #4
    lda (ARG),y
    beq LAMERR                                     ; No parms or body
    sta WSD+1
    dey
    lda (ARG),y
    sta WSD
    ldx #&0a
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

.AVX
    ldy #0
    lda (WSB),y
    bmi ISEXPR
    beq ISFXP1                                     ; NB spelling!
.LAMERR
    brk                                            ; Syntax error

    equb 7
    equs "Lambda syntax"
    equb 0

.ISFXP1
    jmp ISFXPR

.MORFAG
    jsr NXEVAM
    ldx TVSEXT                                     ; Spread args for expr
    cpx #&42
    bcc GODARG
    jmp NARGER

.GODARG
    lda ARG
    sta TVS,x
    inx
    lda ARG+1
    sta TVS,x
    inx
    stx TVSEXT
.ISEXPR
    lda WSA+1
    bne MORFAG
.RADON
    ldx #&0a
.XENON
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
.NOD
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
.DOBIND
    bcc GADARG
    jmp NARGER

.GADARG
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

.ISFXPR
    lda WSB
    sta WSD
    lda WSB+1
    sta WSD+1
    ldx #4
    jsr BIND
    ldx #&0a
.XLAMB
    stx TVSEXT
.XLAM
    lda WSC+1
    bne XLAMC
    beq EVPOP
.XLAMD
    tax
    dey
    lda (WSC),y
    stx WSC+1
    sta WSC
.XLAMC
    ldy #0
    lda (WSC),y
    bmi SYNNED
    jmp LAMERR

.SYNNED
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
.EVPOP
    jmp POP


    ; **** Get a character
.GTCHAR
    ldy HANDLE
    beq KEYCH
    lda LSCHAR                                     ; From file
    bpl RENEW
    and #&7f
    bpl REOLD
.RENEW
    jsr osbget                                     ; Read a single byte from an open file Y
.REOLD
    sta LSCHAR
    rts

.KEYCH
    txa                                            ; From screen
    pha
    ldx LSBUFF
    cpx #&f0                                       ; => New line
    bne NNL

    ; **** Read a line
    bit LEVEL                                      ; Prompt masked?
    bpl READON
    lda DEPTH
    tax
.PRDEPT
    beq READON
    lda #'['
    jsr oswrch                                     ; Write character 91
    dex
    bpl PRDEPT
.READON
    ldx #<(INCB)
    ldy #>(INCB)
    lda #osword_read_line
    jsr osword                                     ; Read line; Read line from input stream (exits with C=1 if ESCAPE pressed)
    bcc OKLINE
.RDCHER
    brk                                            ; Escape

    equb &1a
    equs "Escape"
    equb 0

.OKLINE
    lda #&ff                                       ; Zero => newline
    sta LSBUFF
.NNL
    inc LSBUFF                                     ; Fetch char
    pla
    tax                                            ; RESTORE X
    ldy LSBUFF
    lda DOSBUF,y                                   ; Get char form buffer
    cmp #&0d
    bne XYZ
    ldy #&f0
    sty LSBUFF                                     ; New line next time
.XYZ
    rts

.RSREAD
    ldx #0
    stx DEPTH
    stx LINENO
    stx HANDLE
    ldx #&f0                                       ; Flag for new line
    stx LSBUFF

    ; **** READ
.READ
    ldx #&ff
    stx ARG+1                                      ; Flag for rubbish
    jsr STACK
.NXCHAR
    jsr GTCHAR
.RPT
    cmp #&0d
    beq NXCHAR
    cmp #&0a
    beq NXCHAR
    cmp #' '
    beq NXCHAR
    cmp #'\''
    bne PARQU
    lda #<QUOTE                                    ; It's a quote
    sta WSA
    lda IMBOT
    sta WSA+1
.ODDAT
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
.MORODD
    lda TVSEXT,y
    sta (ARG),y
    dey
    bne MORODD
    jmp POP

.RDLJMP
    jmp RDLIST

.PARQU
    cmp #'('
    beq RDLJMP
    ldx #&ff                                       ; Some sort of atom
    cmp #'!'                                       ; Escape
    beq SPCATM
    ldy #1                                         ; Ordinary char atom
    jsr TERMQ
    bne NORMAL
.QUEER
    brk                                            ; Syntax error

    equb 8
    equs "Read syntax"
    equb 0

.SPCATM
    jsr GTCHAR                                     ; Escaped char
.NORMAL
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
.TRYCHR
    jsr MATCH
.AMADE
    lda POINT
    sta ARG
    lda POINT+1
    sta ARG+1
    jmp POP


    ; *** Read a list
.RDLIST
    lda #0
    sta WSA+1
    sta WSB+1
    inc DEPTH
    jsr LCHAR
    cmp #'.'
    bne LON
    jsr GTCHAR
.DOTTY
    brk                                            ; Dot syntax

    equb 9
    equs "Dot syntax"
    equb 0

.LON
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
.NEWLST
    stx WSA
    sta WSA+1
.ALLIST
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

.READX
    jsr FILGB
    jsr READ
    jmp POP


    ; **** Look for terminators
.TERMQ
    cmp TERMS,y
    beq TERRTS
    dey
    bpl TERMQ
.TERRTS
    rts


    ; **** Next arg from WSA's list
.NXTARG
    lda WSA+1
    bne NXTARH
.ARGERR
    jmp FSARG

.NXTARH
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

.LCHAR
    jsr GTCHAR
    cmp #' '
    beq LCHAR
    cmp #&0d
    beq LCHAR
    cmp #')'
    beq LISTND
.PUTBCK
    pha
    ldy HANDLE
    bne EXFILE                                     ; Reverse file pointer
    ldy LSBUFF
    bpl MIDBUF
    lda #&0d                                       ; Fudge CR into buffer
    sta DOSBUF
    ldy #0
.MIDBUF
    dey
    sty LSBUFF
    pla
    rts

.EXFILE
    lda LSCHAR                                     ; COS version
    ora #&80
    sta LSCHAR
    pla
    rts

.LISTND
    dec DEPTH
    lda WSA
    sta ARG
    lda WSA+1
    sta ARG+1
    pla
    pla
    jmp POP


    ; **** Make number in POINT
.MAKNUM
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
.PLUSS
    lda OSWBUF,x
    sec
    sbc #&30
    cmp #&0a
    bcs MKRTS                                      ; Not a digit
    sta ACIN
    ldy #0
    lda ACL                                        ; *10 now
    asl a
    sta ACL
    rol ACL+1                                      ; That's * 2
    bcs MKOVFL                                     ; Overflow
    asl a
    bcc YIA
    ldy #2
.YIA
    asl a
    bcc YIB
    iny
    clc
.YIB
    adc ACIN
    bcc YIC
    iny
    clc
.YIC
    adc ACL
    sta ACL
    bcc YID
    iny
.YID
    lda ACL+1
    asl a
    bcs MKOVFL
    asl a
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
.MKOVFL
    sec
    rts                                            ; Not number

.PLUSSS
    ldx ACL+1
    bmi MKOVFL
.NTOVFL
    jsr ALNUM
    ldy #3
    lda ACL+1
    sta (POINT),y
    dey
    lda ACL
    sta (POINT),y
    clc
.MKRTS
    rts

.NEXTAD
    inc AD
    bne NEXTAA
    inc AD+1
.NEXTAA
    inc AA
    bne NAAB
    inc AA+1
.NAAB
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
.LETTER
    sta OSWBUF
    ldy #0
    sty END
.MATCH
    jsr SETCEL
    inc END
    lda END
    clc
    adc #6
    sta ACL+1
    bcc CHKCLL
.LONGER
    brk                                            ; String too long

    equb &0a
    equs "String too long"
    equb 0

.CHKCLL
    ldy #0
.CHKCLM
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
.MCOP
    dey
    lda OSWBUF,y                                   ; Chars the same?
    cmp (AD),y
    bne NXTCLL
    tya
.TESTY
    bne MCOP
    lda CELL+1
    ldx CELL                                       ; NIL?
    stx POINT
    cpx #<NIL
    bne BOX
    sbc IMBOT
    beq BOX
    lda CELL+1
.BOX
    sta POINT+1
    rts                                            ; It's found

.NXTCLL
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
.MCAP
    dey
    lda OSWBUF,y
    sta (AD),y
    tya
.TOSTIG
    bne MCAP
    rts                                            ; New cell made

.NXCELL
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
.SIX
    lda #5
.GOT
    adc CELL
    sta CELL
    bcc HCOK
    inc CELL+1
.HCOK
    cmp AREVAL
    lda CELL+1
    sbc AREVAL+1
    rts

.FILGB
    ldx TVSEXT
    cpx #&0c
    lda #0
    bcc ZIP
.FILGC
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
.ZIP
    sta HANDLE
    rts

.IPLINE
    jsr FILGB
    ldx #&ff
.MOLIN
    jsr GTCHAR
    inx
    sta OSWBUF,x
    cmp #&0d
    bne MOLIN
    dex                                            ; Don't want CR at end.
    stx END
    jmp TRYCHR


    ; ******************************
    ; Here there be the point
    ; routines of various types:
    ; ******************************
    ; **** Intercept return addr
.GETSSP
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
.STINC
    iny
    lda (POINT),y
    tay
    dey
    rts


    ; **** WRITE
.WRIT
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
.WRITGO
    jmp WRTTZ

    lda #&0d
    jsr OUT
    rts


    ; **** WRITE0
.WRTTZ
    jsr FILG
    lda #&ff
    sta ESCHF
    ldx #&0c
    bne GENWRI

    ; **** Error entry
.PRINTE
    jsr STACK
    jmp PRINTC


    ; **** PRINT
.PRINT
    lda #&ff                                       ; Put in esc char
    bmi PRINT1

    ; **** PRIN
.PRINZ
    lda #&ff
    bmi PRIN1

    ; **** PRINTC
.PRINTC
    lda #0
.PRINT1
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
.PRINGO
    jmp PRINOK

    jsr CROUT
    rts


    ; **** PRINC
.PRINC
    lda #0
.PRIN1
    sta ESCHF
.PRINOK
    ldy #0
    sty HANDLE
    ldx #&0a
.GENWRI
    lda #0
    cpx TVSEXT                                     ; NO ARGS?
    bcs NILFR
.PRON
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
.NILFR
    jmp POPA


    ; **** Get file handle
.FILG
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

.FILGER
    jmp NUER

.SAVAR
    lda ARG                                        ; Saves ARG in WEB
    sta WSB
    lda ARG+1
    sta WSB+1
    rts

.CCPR
    brk                                            ; Escape

    equb &0b
    equs "Escape"
    equb 0


    ; **** Just prints ARG
.PRINA
    lda #0
    sta ESCHF
.PRINB
    jsr KBCHK                                      ; Check for escape
    bne CCPR
    lda ARG+1
    bne NPN                                        ; NIL?
    ldx #NILOFF
    jmp MESSAG                                     ; 'NIL'


    ; **** Main printer
.NPN
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
.PLIST
    lda #' '
    jsr OUT
.NASTY
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
    ldx #&1c
    jsr MESSAG
    jsr PATOM
.RPP
    lda #')'
    jsr OUT
    jmp POP

.BADAT
    brk                                            ; Unknown atom type

    equb &0c
    equs "Bad atom type"
    equb 0


    ; **** Print atom
.PATOM
    ldy #0
    lda (ARG),y
.PATOMB
    beq OKP                                        ; If char atom
    cmp #NUMF
    beq PNUM                                       ; If number atom
    cmp #SUBRF
    beq HASH                                       ; If subr atom
    cmp #FSUBRF
    bne BADAT
    lda #'F'                                       ; Fsubr
    jsr OUT
.HASH
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
.OKP
    jsr GENDS
    ldx #0
    beq INCHP
.FOOT
    lda (AA,x)
    ldy ESCHF
    bmi ESCFT
.FTOUT
    jsr OUT
    jmp INCHP

.ESCFT
    ldy #6
    jsr TERMQ
    bne FTOUT
    pha                                            ; Save character
    lda #'!'                                       ; Escape in char
    jsr OUT
    pla
    jmp FTOUT

.INCHP
    jsr NEXTAA
    bcc FOOT
    rts


    ; **** Print a number
.PNUM
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
.PINT
    ldx #5
    lda #0
.CLDIV
    sta NBUFF,x
    dex
    bpl CLDIV
    sta XTNDL
    sta XTNDL+1
    sta AUXL+1
    lda #&0a
    sta AUXL
.NOMSIN
    jsr DIV
    lda XTNDL
    sty XTNDL
    ora #&30
    ldx NIND
    sta NBUFF,x
    inc NIND
    lda ACL
    ora ACL+1
    bne NOMSIN
    ldx #4
.MDIGP
    lda NBUFF,x
    beq NDP
    jsr OUT
.NDP
    dex
    bpl MDIGP
    rts


    ; **** Finds end of char atom
.GENDS
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
.EOF
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
.EOFCP
    lda ACL,x
    cmp ARGINF,x
    bne EOFNO
    dex
    bpl EOFCP
    jmp YES

.EOFNO
    jmp NO


    ; **** CLOSE
.CLOS
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
.PARERR
    brk                                            ; COND syntax

    equb &0d
    equs "COND syntax"
    equb 0


    ; **** COND
.COND
    lda WSA+1
    bne REMAIN
    jmp POPA

.REMAIN
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
.MTODO
    jsr NXEVAM
.PROGN
    lda WSA+1
    bne MTODO
.PRGEND
    jmp POP


    ; **** UNTIL
.UNTIL
    jsr NXEVAL
    lda ARG+1
    beq PRGEND
    bne WILLY

    ; **** WHILE
.WHILE
    jsr NXEVAL
    lda ARG+1
    bne PRGEND
    beq WILLY
.WILL
    jsr NXEVAM
.WILLY
    lda WSA+1
    bne WILL
    sta REP
    jmp POP


    ; **** QUOTE
.QUO
    jsr NXTARG
    jmp POP


    ; ******************************
    ; Now the CAR - CDR complex
    ; ******************************
.PERR
    brk                                            ; Atomic arg

    equb &0e
    equs "CAR/CDR of atom"
    equb 0

.CDDDR
    ldx #&1f
    bne CXR
.CADDR
    ldx #&1b
    bne CXR
.CDADR
    ldx #&17
    bne CXR
.CAADR
    ldx #&13
    bne CXR
.CDDAR
    ldx #&0f
    bne CXR
.CDDR
    ldx #&0e
    bne CXR
.CADAR
    ldx #&0b
    bne CXR
.CADR
    ldx #&0a
    bne CXR
.CDAAR
    ldx #7
    bne CXR
.CDAR
    ldx #6
    bne CXR
.CDR
    ldx #5
    bne CXR
.CAAAR
    ldx #3
    bne CXR
.CAAR
    ldx #2
    bne CXR
.CAR
    ldx #1
.CXR
    lda ARGA
    sta ARG
    lda ARGA+1
    sta ARG+1
.CXLP
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
.ERRORL
    jsr PRINTE
.LISPER
    brk

    equb &0f
    equs "ERROR function"
    equb 0


    ; **** AND
._AND
    lda WSA+1
    bne ANDON
    jmp YES

.ANDON
    jsr NXEVAM
    lda ARG+1
    bne _AND
    jmp NO


    ; **** OR
.OR
    lda WSA+1
    bne ORON
    jmp NO

.ORON
    jsr NXEVAM
    lda ARG+1
    beq OR
    jmp YES


    ; **** LOOOP
.LOOP
    lda WSA
    sta WSC
    lda WSA+1
    sta WSC+1
.RESTAR
    lda WSC
    sta WSA
    lda WSC+1
    sta WSA+1
.RIPON
    lda WSA+1
    beq RESTAR
    sta REP
    jsr NXEVAM
    lda REP
    bne RIPON
    lda #&ff
    sta REP
    jmp POP


    ; ******************************
    ; Extra LISP entry points
    ; ******************************
    ; **** CONS
.CONS
    jsr ALPAIR
    ldy #4
.CONSLP
    lda WSC+1,y
    sta (POINT),y
    dey
    bne CONSLP
    lda POINT
    sta ARG
    lda POINT+1
    jmp POPA


    ; **** SET
.SET
    jsr NXEVAL
    jmp DOSET


    ; **** SETQ
.SETQ
    jsr NXTARG
.DOSET
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

.SETERR
    brk                                            ; SET non-atomic

    equb &10
    equs "Bad assignment"
    equb 0


    ; **** LIST
.LIST
    lda #0
    sta ARG+1
    ldx TVSEXT
    cpx #&0c
    bcc NOLLY
.LL
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
    cpx #&0c
    bcs LL
.NOLLY
    jmp POP


    ; **** VDU
.VDU
    jsr ALLNUM
    ldx #&0a
.MVDU
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
.CHARP
    ldx ARGA+1
    beq YESNIL
    ldy #0
    lda (ARGA),y
    bne NNO
.YESNIL
    jmp YES


    ; **** SUBRP
.SUBRP
    lda #SUBRF
    bne TYPE

    ; **** FSUBRP
.FSUBRP
    lda #FSUBRF
    bne TYPE

    ; **** LISTP
.LISTP
    lda #LISTF
    bne TYPE

    ; **** NUMBERP
.NUMP
    lda #NUMF
.TYPE
    ldx ARGA+1
    beq NNO
    ldy #0
    cmp (ARGA),y
    bne NNO
    jmp YES

.NNO
    jmp NO


    ; **** ZEROP
.ZEROP
    ldx #0
    beq TSN

    ; **** ONEP
.ONEP
    ldx #1
.TSN
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
.MINUSP
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
.SETDCB
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
.LOAD
    jsr SETDCB
    lda #osfile_load                               ; Load operation
    ldx #<(OSINFO)
    ldy #>(OSINFO)
    jsr osfile                                     ; Load named file (if XY+6 contains 0, use specified address) (A=255)
.INUREL
    lda #AREEXT                                    ; Set up base
    sta RELBS
    lda IMBOT
    sta RELBS+1
    jsr UNREL
    jmp INIT                                       ; Re-start LISP


    ; **** DUMP
.DUMP
    jsr RUBBSH                                     ; GC
    jsr SETDCB                                     ; Set up before relat
    jsr RELAT                                      ; Make relocatable
    lda #osfile_save                               ; Save
    ldx #<(OSINFO)
    ldy #>(OSINFO)
    jsr osfile                                     ; Save a block of memory (returning file length and attributes) (A=0)
    jmp POP

.CHARQ
    lda ARGA+1
    beq FILERR
.CHARQR
    ldy #0
    lda (ARGA),y
    bne FILERR
    rts

.FILERR
    brk                                            ; Not char atom

    equb &11
    equs "Character atom expected"
    equb 0

.ATOA
    lda ARGA
    sta ARG
    lda ARGA+1
    sta ARG+1
    rts

.ANUM
    jsr ALLNUM
    ldy #3
    lda (ARGA),y
    sta ACL+1
    dey
    lda (ARGA),y
    sta ACL
    rts


    ; **** CALL
.CALL
    jsr ANUM
    lda #>BACALL+1
    pha                                            ; Set up return
    lda #<BACALL+1                                 ; address
    pha
    lda (ARGB),y                                   ; Get A
    jmp (ACL)                                      ; and call


    ; **** PEEK
.PEEK
    jsr ANUM
    ldy #0
.BACALL
    lda (ACL),y
.BECALM
    sta ACL
    lda #0
    sta ACL+1
    jmp ACLRET


    ; **** POKE
.POKE
    jsr ANUM
    lda (ARGB),y
    ldy #0
    sta (ACL),y
    lda ARGB
    sta ARG
    lda ARGB+1
    jmp POPA


    ; OBLIST
.OBLIST
    jsr SETCEL                                     ; Only want good
    ldy #0                                         ; cells
    sty ARG+1
.BOOM
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
.NEXOS
    jsr NXCELL
    bcc BOOM
    jmp POP

.USEFUL
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
.USABLE
    rts


    ; **** Make DOS name
.MKNAM
    jsr CHARQ                                      ; FOR DOS
    lda #<NAMBUF
    sta OSINFO
    lda #>NAMBUF
    sta OSINFO+1
    ldy #1
    lda (ARGA),y                                   ; Get name length
    tay
    lda #&0d                                       ; Terminator
.MORLET
    sta OSWBUF-6,y
    dey
    cpy #6
    bcc USABLE
    lda (ARGA),y
    bcs MORLET                                     ; Always taken

    ; **** *
.STAR
    jsr CHARQ
    ldy #1
    lda (ARGA),y
    cmp #&3e
    bcc SMALLP
    lda #&3e
.SMALLP
    tay
    lda #&0d
.MORLEZ
    sta DOSBUF-6,y
    dey
    cpy #6
    bcc STARRY
    lda (ARGA),y
    bcs MORLEZ
.STARRY
    ldx #<(DOSBUF)
    ldy #>(DOSBUF)
    jsr oscli
    jmp NO


    ; **** OPEN
.OPE
    jsr MKNAM
    lda ARGB+1
    cmp #1
    bcc WOPEN
    lda #&c0                                       ; OPEN FOR READ
    jmp ROPEN

.WOPEN
    lda #osfind_open_output                        ; OPEN FOR WRITE
.ROPEN
    ldx OSINFO
    ldy OSINFO+1
    jsr osfind                                     ; Open file for output (A=128)
    cmp #0
    beq FNERR
    sta ACL
    lda #0
    sta ACL+1
    jmp ACLRET                                     ; RESULT IN A

.FNERR
    brk                                            ; File not found

    equb &d6
    equs "File not found"
    equb 0


    ; **** Unrelativise
.UNREL
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
.NEXTU
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
.URN
    lda #4                                         ; Number
    bpl NXITU                                      ; No alteration
.URC
    jsr TWOADD                                     ; Character
    ldy #1
    lda (RELBS),y
    jmp NXITU

.URS
.URF
    jsr TWOADD                                     ; Subr/Fsubr
    lda #6
    bpl NXITU
.TWOADD
    ldy #3
    jsr ALTADU
    ldy #5
.ALTADU
    lda (RELBS),y                                  ; Adjust addr
    beq NOTU                                       ; NIL - needn't alter
    sec
    sbc RELOFF
    sta (RELBS),y
.NOTU
    rts

.NXITU
    jsr NXIT
    bcc NEXTU
    rts

.NXIT
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
.NOTOK
    sec
    rts                                            ; End reached

.OKOUT
    clc
    rts                                            ; End not reached


    ; **** RELAT (somewhat reduced!)
.RELAT
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
.RECLAM
    jsr RUBBSH                                     ; Force G.C.
    jmp NO


    ; **** QUOTIENT
.QUOT
    ldx #ACL
    bne DODO

    ; **** REMAINDER
.REM
    ldx #XTNDL
.DODO
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
.POSV
    ldy #2                                         ; RETREIVE RESULT
    ldx XPR
    lda GCNO,x
    sta (POINT),y                                  ; Put in new cell
    iny
    lda GCNO+1,x
    sta (POINT),y
    jmp AMADE                                      ; return POINT


    ; **** DIFFERENCE
.DIFF
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
.MINUS
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
.FINONE
    sta (POINT),y
    jmp AMADE


    ; **** SUB1
.SUBA
    jsr ALLNUM
    jsr ALNUM
    lda #&ff
    clc
    bcc WONE

    ; ADD1
.ADDA
    jsr ALLNUM
    jsr ALNUM
    lda #0
    sec
.WONE
    pha
    ldy #2
    adc (ARGA),y
    sta (POINT),y
    iny
    pla
    adc (ARGA),y
    bvc FINONE
.OVFERR
    brk                                            ; Overflow

    equb &12
    equs "Arithmetic overflow"
    equb 0


    ; **** PLUS
.PLUS
    jsr ALLNUM
    jsr ALNUM
    lda #0
    ldy #2
    sta (POINT),y
    iny
    ldx TVSEXT
    bne PEX
.MPLUS
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
.PEX
    sta (POINT),y
    dex
    dex
    cpx #&0a
    bcs MPLUS
    jmp AMADE


    ; **** TIMES
.TIMES
    jsr ALLNUM
    ldx #1
    stx ACL
    dex
    stx ACL+1
    ldx TVSEXT
    bne TREX
.MTIMES
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
.POSITV
    ldx XPR
.TREX
    dex
    dex
    cpx #&0a
    bcs MTIMES
.ACLRET
    jsr ALNUM
    ldy #2
    lda ACL
    sta (POINT),y
    iny
    lda ACL+1
    sta (POINT),y
    jmp AMADE


    ; **** CHARS
.CHARS
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
.FIXTY
    lsr a
    lsr a
    tax
.XGOOD
    lda LENTAB,x
.AGOT
    ldy #2
    sta (POINT),y
    iny
    lda #0
    sta (POINT),y
    jmp AMADE

.GPLIST
    jsr CHARQ
    ldy #4
    lda (ARGA),y
    sta ARG
    iny
    lda (ARGA),y
    jmp POPA


    ; **** RPLACA
.RPLACA
    ldy #1
    bne PLAQ

    ; **** RPLACD
.RPLACD
    ldy #3
.PLAQ
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

.PLAQER
    brk                                            ; Wrong 1st arg

    equb &13
    equs "RPLACA/RPLACD argument"
    equb 0


    ; **** LESSP
.LESSP
    ldx #1
.SWIP
    lda ARGA,x
    ldy ARGB,x
    sta ARGB,x
    sty ARGA,x
    dex
    bpl SWIP

    ; **** GREATERP
.GT
    jsr ALLNUM
    ldy #3
    lda (ARGB),y
    cmp #&80
    eor (ARGA),y
    bmi DFSGN
    dey
    lda (ARGB),y
    cmp (ARGA),y
    iny
    lda (ARGB),y
    sbc (ARGA),y
    bcs SMALLR
.BIGGER
    jmp YES

.DFSGN
    bcs BIGGER
.SMALLR
    jmp NO

.SOCK
    ldy #4
    lda (ARGA),y
    sta WSB
    iny
    lda (ARGA),y
    sta WSB+1                                      ; A-list search
.SOCKA
    bne LKG                                        ; routine
    rts

.ROCKON
    ldy #3
    lda (WSB),y
    tax
    iny
    lda (WSB),y
    bne LKH
    rts

.LKH
    sta WSB+1
    stx WSB
.LKG
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
    lda #&ff
    rts

.ALERR
    brk                                            ; P-list error

    equb &14
    equs "P-list structure"
    equb 0


    ; **** ASSOC
.ASSOC
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
.HAUSE
    sta ARG+1
    jmp POP


    ; **** GET
.GET
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
.PUT
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

.INSERP
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
.REMPR
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
.FRUIT
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

.FRUT
    dey
    lda (ARGA),y
.FRUTA
    stx ARGA+1
    sta ARGA
    ldy #4
    bne FRUIT
.HOUSE
    jmp NO


    ; **** Check all numeric args
.ALLNUM
    lda #NUMF
    ldx TVSEXT
    bne COMPX
.MORIX
    ldy TVS+1,x
    beq NUER
    cmp (TVS,x)
    bne NUER
.COMPX
    dex
    dex
    cpx #&0a
    bcs MORIX
    rts

.NUER
    brk                                            ; Non-numeric args

    equb &15
    equs "Non-numeric argument"
    equb 0

.EVAL
    jsr ATOA
    jsr EVALU
    jmp POP


    ; ******************************
    ; APPLY/MAP complex
    ; ******************************
.APFUN
    lda ARGA+1
    beq APFERR
    ldy #0
    sty WSB+1
    lda (ARGA),y
    bmi GEVIL
    cmp #SUBRF
    beq GFRTS                                      ; It's a subr
.APFERR
    brk

    equb &16
    equs "APPLY argument"
    equb 0

.GEVIL
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

.GFRTS
    lda ARGA
    sta WSC
    lda ARGA+1
    sta WSC+1
    rts

.APERR
    brk                                            ; APPLY list

    equb &17
    equs "APPLY arguments"
    equb 0


    ; **** APPLY
.APPLY
    jsr APFUN
    ldx #&0a
    lda ARGB+1
    beq APGO
    sta WSA+1
    lda ARGB
    sta WSA
.APL
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
    cpx #&42
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
.APGO
    stx TVSEXT
.APGOB
    lda WSB+1
    beq SUBAP
    jmp RADON                                      ; Expr entry

.SUBAP
    jmp INSUBR                                     ; Subr entry

.MAPSTR
    jsr APFUN                                      ; Prelims for
    ldx #&0c                                       ; MAP functions
.MAPMOV
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
.MAPRTS
    rts

.NILMAP
    jmp POPA


    ; **** MAP
.MAP
    jsr MAPSTR
    beq NILMAP
.MAPON
    jsr CARAP
    jsr CDRALL
    bne MAPON
    jmp NO


    ; *** MAPC
.MAPCAR
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
.MAPCON
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
.OLDMAP
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
.GETCHA
    ldx TVSEXT                                     ; File arg?
    cpx #&0c
    bcc GETDIR
    jsr FILGC
    jsr GTCHAR
    jmp POST

.GETDIR
    jsr osrdch                                     ; Grab a char; Read a character from the current input stream
    bcc POST
    brk

    equb &1c
    equs "Escape"
    equb 0


    ; **** CHARACTER
.ASCII
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
.POST
    jsr LETTER                                     ; Finds char atom
    jmp AMADE


    ; **** ORDINAL
.ORDINL
    jsr CHARQ
    jsr ALNUM
    ldy #1
    lda (ARGA),y
    sec
    sbc #6
    beq EMPTYC
    ldy #6
    lda (ARGA),y
.EMPTYC
    jmp AGOT

.CARAP
    jsr STACK
    ldx TVSEXT
    bne CARAFE
.CARAVA
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
.CARAFE
    cpx #&0c
    bcs CARAVA
    jmp APGOB

.MAPERR
    brk                                            ; MAP args

    equb &18
    equs "MAP/MAPC arguments"
    equb 0

.CDRALL
    ldx TVSEXT
    bne CDRAWL
.CDRAVA
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
.CDRAWL
    cpx #&0c
    bcs CDRAVA
.CDREND
    rts


    ; **** EXPLODE
.EXPLOD
    jsr CHARQ
    sty ARG+1
    iny
    lda (ARGA),y
    bne BANG
.THUMP
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
.BANG
    cmp #7
    bcs THUMP
    jmp POP

.IMPERR
    brk                                            ; No space

    equb &19
    equs "No room for IMPLODE"
    equb 0


    ; **** IMPLODE
.IMPLOD
    ldx #0
    lda ARGA+1
    jmp WSBTST

.SQUISH
    iny
    lda (ARGA),y
    sta WSB
    iny
    lda (ARGA),y
    bne NODNIL
    lda #<NIL
    sta WSB
    lda IMBOT
.NODNIL
    sta WSB+1
    ldy #0
    lda (WSB),y
    beq ISCH
    jmp FILERR                                     ; Not char err

.ISCH
    iny
    lda (WSB),y
    sta YSAV
    ldy #6
    bne EMM
.SPLOT
    lda (WSB),y
    sta OSWBUF,x
    iny
    inx
    beq IMPERR
.EMM
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
.WSBTST
    beq IMPRET
    ldy #0
    lda (ARGA),y
    bmi SQUISH
.IMPRET
    dex                                            ; Show string length
    stx END
    jmp TRYCHR


    ; **** MESSON
.MESSON
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
    ora LEVEL
    sta LEVEL
    jmp POP


    ; **** MESSOFF
.MESSOF
    jsr ALLNUM
    ldy #2
    lda (ARGA),y
    eor #&ff
    and LEVEL
    sta LEVEL
    jmp POP


    ; **** MODE
.MODE
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda (ARGA),y                                   ; Get mode
    and #7                                         ; Mod 8
    sta (POINT),y
    ora #&80                                       ; Flag bit
    sta MODEF
    iny
    lda #0
    sta (POINT),y
    jmp AMADE


    ; **** Actually change mode
.MODCHN
    lda MODEF
    and #7
    sta MODEF                                      ; Clear flag bit
    pha
    lda #osbyte_read_high_order_address
    jsr osbyte                                     ; M/c HO addr.; Read the filing system 'machine high order address'
    cpy #&ff
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
.MODERR
    brk                                            ; No room

    equb &1b
    equs "No room for MODE "
    equb 0

.CHARE
    sty ARETOP                                     ; New ARETOP
    sty SP+1                                       ; Reset software stack
.CHANGE
    lda #&16                                       ; Change mode
    jsr oswrch                                     ; Write character 22
    pla
    jmp oswrch                                     ; Write character


    ; **** USR
.USR
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

.JUMPAD
    jmp (POINT)                                    ; Enter routine

.OUTL
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
.BUILD1
    ldx #0

    ; **** Put A & X (msb) on WSA
.BUILD2
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

.SETNUM
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
.SOUND
    jsr ALLNUM
    ldy #2
    ldx #&0a
.MORSOU
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
    cpx #&12
    bne MORSOU
    lda #osword_sound
.ENTOSW
    ldx #<(OSWBUF)
    ldy #>(OSWBUF)
    jsr osword                                     ; SOUND command
    jmp YES


    ; **** ENVELOPE
.ENV
    jsr ALLNUM
    ldy #2
    ldx #&0a
.MORENV
    lda TVS,x
    sta ARG
    lda TVS+1,x
    sta ARG+1
    txa                                            ; Prepare index
    lsr a
    tax
    lda (ARG),y
    sta OSWBUF-5,x
    txa
    asl a
    tax
    inx
    inx
    cpx #&26
    bne MORENV
    lda #8
    bne ENTOSW
    bne MORENV
    lda #8
    bne ENTOSW

    ; **** Get an argument
.GTARG
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
.RESET
    jsr STCLK
    jsr GCTIMZ
    jmp YES                                        ; Value is T


    ; **** Zero the clock
.STCLK
    lda #osword_write_clock                        ; Write clock
.ZERTIM
    ldy #>(TIMZER)
    ldx #<(TIMZER)
    jsr osword                                     ; Write system clock
    ldx #&76                                       ; Restore pointer
    ldy #&82
    rts


    ; **** Zero GC time
.GCTIMZ
    lda #0
    ldx #4
.MGCTIM
    sta GCTIME,x
    dex
    bpl MGCTIM

    ; **** Read the clock
.TIMER
    lda #osword_read_clock
    ldx #<(TIMEW)
    ldy #>(TIMEW)
    jsr osword                                     ; Read the clock; Read system clock
    ldx #<TIMEW                                    ; Restore pointer
    ldy #>TIMEW
    rts


    ; **** TIME
.TIME
    jsr TIMER
    jmp TIMPOP


    ; **** GCTIME
.GCTIM
    ldx #<GCTIME
    ldy #>GCTIME

    ; **** Return a time
.TIMPOP
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
.CLOCK
    jsr TIMER                                      ; Time in TIMEW
    ldx #5
    ldy #0
.MCLK1
    tya
    sta REM40,x                                    ; Zero remainder
    sta AUX40,x                                    ; Zero divisor
    lda TIMEW,x                                    ; Dividend
    sta ACL40,x
    dex
    bpl MCLK1
    lda #&64                                       ; Throw away cs
    sta AUX40
    jsr DIV40
    lda #0
    sta REM40
    lda #&3c                                       ; Get the seconds
    sta AUX40
    jsr DIV40
    lda #0
    sta WSA+1                                      ; NIL
    lda REM40
    jsr BUILD1
    lda #0
    sta REM40
    lda #&3c                                       ; Get the minutes
    sta AUX40
    jsr DIV40
    lda REM40
    jsr BUILD1
    lda ACL40                                      ; Get the hours
    ldx ACL40+1
    jsr BUILD2
    jmp AMADE


    ; **** POINT
.PVAL
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
.PEXIST
    sta (POINT),y
    iny
    txa
    sta (POINT),y
    jmp AMADE


    ; **** ADVAL
.ADVAL
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
    adc #&80                                       ; Make range +/-2^15
    ldy #3
    sta (POINT),y
    dey
    txa
    sta (POINT),y
    jmp AMADE


    ; **** INKEY (added 18/8/83 RMT)
.INKEY
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

.INKER
    lda #&ff
    sta (POINT),y
    iny
    sta (POINT),y
    jmp AMADE

.INKEY2
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

.INKYES
    jmp YES


    ; **** GENSYM (added 31/8/83 RMT)
.GENSYM
    lda #'G'
    sta OSWBUF
    ldx #3
    sec
.NXDIGT
    lda GENCNT,x
    adc #0
    cmp #&3a
    bcc NOCAR
    lda #'0'
.NOCAR
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
.BNOT
    jsr ALLNUM
    jsr ALNUM
    ldy #2
    lda #&ff
    eor (ARGA),y
    sta (POINT),y
    iny
    lda #&ff
    eor (ARGA),y
    sta (POINT),y
    jmp AMADE


    ; **** BAND
.BAND
    jsr ALLNUM
    jsr ALNUM
    lda #&ff
    ldy #3
    sta (POINT),y
    dey
    sta (POINT),y
    ldx TVSEXT
.MBAND
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
    cpx #&0a
    bne MBAND
    jmp AMADE


    ; **** BOR
.BOR
    jsr ALLNUM
    jsr ALNUM
    lda #0
    ldy #3
    sta (POINT),y
    dey
    sta (POINT),y
    ldx TVSEXT
.MBOR
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
    cpx #&0a
    bne MBOR
    jmp AMADE


    ; ******************************
    ; Garbage collector file.
    ; ******************************
    ; **** Set CELL to image start
.SETCEL
    lda #WSBOT
    sta CELL
    lda IMBOT
    sta CELL+1
    rts


    ; **** Clear all G.C. flags
.CLEARF
    jsr SETCEL
    ldy #0
.RZ
    lda (CELL),y
    and #&fc
    sta (CELL),y
    jsr NXCELL
    bcc RZ
    rts


    ; ******************************
    ; Register a set of cells,
    ; starting from the one in GCA.
    ; ******************************
.REGCDR
    lda (GCA,x)
.REGPIN
    ror a
    bcs REGRTS
    ror a
    bcc VIRGIN
    ldy #4
    rol a
    sec
    rol a
    sta (GCA,x)
    bmi LIN
    iny
.LIN
    lda (GCA),y
    beq REGRTS
    tax
    dey
.BOT
    lda (GCA),y
    sta GCA
    stx GCA+1
.REGIS1
    ldx #0
.REGISU
    lda (GCA,x)
    bmi REGPIN
    cmp #4
    bcc REGPIN
    ora #1
    sta (GCA,x)
    ldy #3
    cmp #SUBRF
    bcs LIN
.REGRTS
    rts

.REGIST
    lda GCA+1                                      ; This code added by RMT
    cmp ARETOP                                     ; 12/9/83 to prevent bug
    bcc REGIS1                                     ; probably caused by ARG
    rts                                            ; being &FFxx

.VIRGIN
    clv
    ldy #2
    sec
    rol a
    asl a
    sta (GCA,x)
    bmi LINJ
    iny
.LINJ
    lda (GCA),y
    beq REGCDR
    tax
    dey
    dec l0009
    bpl STOCK
    inc l0009
    beq BOT                                        ; Always taken
.STOCK
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
.RUBBSH
    stx GARX
    sty GARRY
    lda #&ff                                       ; Mark memory invalid
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
.HGCA
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
.NOMESA
    jsr CLEARF
    lda #&20
    sta l0009                                      ; Stack limit
    ldx TVSEXT
.COLLEC
    lda TVS-1,x
    beq SREG                                       ; Register off WSA etc.
.CREG
    sta GCA+1
    lda TVS-2,x
    sta GCA
    stx CELL
    jsr REGIST
    ldx CELL
.SREG
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
.STRETH
    lda SP                                         ; Register off stack
    sta CELL
    lda SP+1
    sta CELL+1
    cmp ARETOP
    bcs LOOKW
.STUP
    ldy #0
    lda (CELL),y
    beq BOWDUN
    tay
.STCOLL
    jsr SPREG
    bne STCOLL
    lda (CELL),y
.BOWDUN
    sec
    adc CELL
    sta CELL
    bcc SPOCK
    inc CELL+1
.SPOCK
    lda (CELL),y
    tay
    jsr SPREG
    dey                                            ; Avoid the return addr
    dey
.STCOL
    jsr SPREG
    bne STCOL
    lda (CELL),y
    sec
.CXH
    adc CELL
    bcc CXHB
    inc CELL+1
.CXHB
    sta CELL
    lda CELL+1
    cmp ARETOP
    bcc STUP
.LOOKW
    jsr SETCEL                                     ; Now off whole
    bit CXH                                        ; Set overflow
    php
.MORWS
    ldx #0
    lda (CELL,x)
    cmp #&82
    beq DOREG
    cmp #2
    beq DOREG                                      ; Register if half
    bcs NOREG                                      ; finished,
    jsr USEFUL
    beq NOREG
    lda (CELL,x)
.DOREG
    ldy CELL                                       ; or if char atom
    sty GCA
    ldy CELL+1
    sty GCA+1
    plp
    jsr REGPIN
    php
.NOREG
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
.ML
    lda (CELL),y
    lsr a
    bcc SETMIN
    jsr NXCELL
    bcc ML
    jmp CLUPB

.SETMIN
    lda CELL+1
    sta AD+1
    lda CELL
    sta AD                                         ; Top of static
.DLOP
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
.STFR
    jsr NXCELL
    bcs TADJ
    lda (CELL),y
    lsr a
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
    lsr a
.STUSE
    rol a
    clc
    bmi SIXG
    cmp #SUBRF
    bcs SIXG
    iny
    lda (CELL),y
    dey
    bcc ADDIN
.SIXG
    lda #5
.ADDIN
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
.NHCIQ
    cmp AREVAL
    lda CELL+1
    sbc AREVAL+1
    bcs TADJ
    lda (CELL),y
    lsr a
    bcs STUSE
    bcc DLOP
.TADJ
    lda DISPM
    sta l0009
    lda DISPM+1
    sta l000a

    ; ******************************
    ; Having calculated the
    ; dispacements, now to alter
    ; the pointers
    ; ******************************
.ADJUSS
    lda SP                                         ; Adjust stack
    sta CELL
    lda SP+1
    sta CELL+1
.ASP
    ldy #0
    sty GCA
    lda (CELL),y
    beq NOLEAP
    pha
    tay
    jsr AJCELL
    pla
.NOLEAP
    sec
    adc CELL
    sta CELL
    bcc LUCK
    inc CELL+1
.LUCK
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
.ADJUT
    lda (CELL),y                                   ; whole of
    lsr a
    bcc NXADJ
    ldy #4
    asl a
    bmi LICE
    iny
    cmp #SUBRF
    bcs LICE
    cmp #NUMF
    bcs NXADJ
.LICE
    jsr AJCELL
.NXADJ
    jsr NXCELL
    bcc ADJUT

    ; ******************************
    ; Now move the blocks about
    ; ******************************
    lda AD                                         ; AD has top of static
    sta CELL                                       ; space
    lda AD+1
    sta CELL+1
.SAL
    jsr NXCELL
    bcs CLUP
    lda (CELL),y
    lsr a
    bcc SAL
    lda CELL
    sta AA
    lda CELL+1
    sta AA+1
.MOLE
    jsr NXCELL
    bcs ABGO
    lda (CELL),y
    lsr a
    bcs MOLE
.ABGO
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
.MORMOV
    lda (AA),y
    sta (AD),y
    iny
    bne MORMOV
    inc AA+1
    inc AD+1
    dec GCA+1
    bne MORMOV
.BITMOV
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
.FINMOV
    jmp SAL


    ; ******************************
    ; Now to calculate the gain in
    ; cells etc.
    ; ******************************
.CLUP
    lda AD
    sta AREVAL
    lda AD+1
    sta AREVAL+1
.CLUPB
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
.NOMESC
    lda #osword_read_interval_timer
    ldx #<(TIMEW)
    ldy #>(TIMEW)
    jsr osword                                     ; Read timer; Read interval timer
    clc
    ldy #0
    ldx #5
.MORTIM
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

.AJCELL
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
.SPECS
    lda (CELL),y
.SPECTR
    sbc l0009
    tax
    lda GCA+1
    sbc l000a
    bne CHUGB
.ONW
    sta GCA+1
    sta SA+1
    lda (CELL),y
.ONX
    tax
    sty GABBY
    tay
    lda (GCA),y
    lsr a
.GCAON
    rol a
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
.SIXP
    tya
    adc #5
    bcs PAGDIP
.ADDOUT
    tay
    lda (GCA),y
    lsr a
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
.PAGDIQ
    sec
.PAGDIP
    ldy GCA+1
    txa
    sbc DL,y
    tax
    tya
    sbc DH,y
.CHUG
    ldy GABBY
.CHUGB
    iny
    sta (CELL),y
    dey
    txa
    sta (CELL),y
.NXPR
    dey
    cpy #2
    bcs AJCELL
    rts

.SPREG
    lda (CELL),y                                   ; Stack register
    beq NULBOW
    sta GCA+1
    sty GABBY
    dey
    lda (CELL),y
    sta GCA
    jsr REGIST
    ldy GABBY
.NULBOW
    dey
    dey
    rts


    ; ******************************
    ; The error handler
    ; The stack is "gently" taken
    ; down with diagnostic printout.
    ; ******************************

    ; **** Main error entry
.ERRORS
.ERROR
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
    ldx #&ff
.NEXCAT
    inx
    lda CATTAB,x
    bmi TRCOK                                      ; Allow trackback
    cmp ERRNO
    bne NEXCAT
    lda LEVEL                                      ; Match
    and #&e7
    sta LEVEL                                      ; No traceback
.TRCOK
    ldx #&ff
    txs                                            ; Reset stack
    inx
    stx DEPTH
    stx HANDLE
    lda #&0e                                       ; Page mode
    jsr oswrch                                     ; Write character 14
    lda #&18                                       ; Mask for LEVEL
    sta TOPBIN
    lda #4                                         ; Error
    and LEVEL
    beq NERRA
    ldx #ERROFF                                    ; "^Error number "
    jsr MESSAH
    lda #0
    sta ACL+1
    jsr PINT                                       ; Print err no.
.NERRA
    lda #&ff
    bit LEVEL                                      ; Err mess mask
    beq NERRB
    jsr PSTRNG                                     ; Print err mess
.NERRB
    lda #8                                         ; Top args mask
    and LEVEL
    beq NERRC
    jsr PARG                                       ; Print ARG
.NERRC
    lda SP+1
DWARF=NERRC-1
    cmp ARETOP                                     ; Pop return
    bcc WINDER                                     ; stack completely
    lda #&0f
    jsr oswrch                                     ; Page mode off; Write character 15
    lda OLDLEV
    sta LEVEL                                      ; Old LEVEL
    jmp SUPER                                      ; Restart


    ; **** Collapse the stack
.WINDER
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
.QZQ
    lda ERRNO                                      ; Return err no.
    sta ACL
    lda #&0f
    jsr oswrch                                     ; Page mode off; Write character 15
    jmp ACLRET                                     ; POP via ACLRET

.SLIP
    lda #<DWARF
    sta (SP),y                                     ; Set up return for POP
    iny
    lda #>DWARF
    sta (SP),y
    lda LEVEL                                      ; Check trace print
    and TOPBIN                                     ; is this second arg
    beq NOPE                                       ; Skip rint
    and #&10                                       ; Clear top arg flag
    sta TOPBIN
    beq BINS
    lda (SP,x)                                     ; Any bound vars?
    beq BINS

    ; **** Do bound var pairs
    tay
.ERBIN
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
.BINS
    lda WSB
    sta ARG
    lda WSB+1
    sta ARG+1
    jsr PARG
.NOPE
    jmp POP                                        ; And back to NERRC

.ERCN
    ldy #0
    ldx #1
.ERCND
    lda ERRCNT,x
    sta ACL,x
    sty ERRCNT,x
    dex
    bpl ERCND
    jmp ACLRET

.PSTRNG
    ldx #0                                         ; Print err mess
    lda #&0d
.NXOUT
    jsr OUT
    inx
    txa
    tay
    lda (BRKAD),y
    bne NXOUT
    jmp CROUT

.PARG
    ldx ARG+1                                      ; Look for flag
    inx
    beq ARGRTS
    ldx #ARGOFF                                    ; Print ARG
    jsr MESSAH                                     ; 'Arg :'
    jmp PRINA

.ARGRTS
    rts


    ; **** ERRORSET
.ERRSET
    jsr STEVAL
.ELFIN
    jsr ALPAIR
    ldy #2
    lda ARG+1
    sta (POINT),y
    dey
    lda ARG
    sta (POINT),y
    jmp AMADE

.STEVAL
    jsr STACK                                      ; ELFIN on stack
    jsr NXEVAL
    jmp POP


    ; ******************************
    ; Useful arithmetic routines
    ; ******************************
    ; **** 16 bit multiply
.MULPM
    jsr MDA
.MUL
    ldy #&10                                       ; Index for 16 bits
.MULB
    lda ACL
    lsr a
    bcc MULD
    clc
    ldx #&fe
.MULC
    lda AUXL,x
    adc SIGN,x
    sta AUXL,x
    inx
    bne MULC
.MULD
    ldx #3
.MULE
    ror ACL,x
    dex
    bpl MULE
    dey
    bne MULB
    rts


    ; **** 16 bit divide
.DIVPM
    jsr MDA
.DIV
    ldy #&10
.DIVB
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
.DIVC
    dey
    bne DIVB
    rts


    ; **** Negation routines
.MDA
    ldy #0
    sty SIGN                                       ; Abs value of
    ldx #AUXL                                      ; AUXL with sign in
    jsr MDB
    ldx #ACL                                       ; ls bit of SIGN
.MDB
    lda GCNO+1,x
    bpl MDRTS
.MD
    sec
    tya
    sbc GCNO,x
    sta GCNO,x
    tya
    sbc GCNO+1,x
    sta GCNO+1,x
    inc SIGN
.MDRTS
    rts


    ; **** 40 bit divide
.DIV40
    lda #&28
.DIV40B
    pha
    asl ACL40
    ldx #1
    ldy #3
.MDIV1
    rol ACL40,x
    inx
    dey
    bpl MDIV1
    ldx #0
    ldy #4
.MDIV2
    rol REM40,x
    inx
    dey
    bpl MDIV2
    sec
    ldx #0
    ldy #4
.MDIV3
    lda REM40,x
    sbc AUX40,x
    sta TEMP40,x
    inx
    dey
    bpl MDIV3
    bcc DIV40C
    ldx #4
.MDIV4
    lda TEMP40,x
    sta REM40,x
    dex
    bpl MDIV4
    inc ACL40
.DIV40C
    pla
    tay
    dey
    tya
    bne DIV40B
    rts

.LISPEN
.unused1
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0


    ; ******************************
    ; This file holds the vectored
    ; core routines. These are
    ; held in 2 pages immediately
    ; preceding the image.
    ; ******************************
.VECTAB
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

.unused2
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0

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
.IMAGE
    equb &f9, &18

.ZA
    equb CHARF,   &0f
    equw &0a02
    equw &0a02
    equs "UNDEFINED"

.TRUE
    equb CHARF,     7
    equw &0a11
    equw     0
    equs "T"

.LAMBDA
    equb CHARF,   &0c
    equw &0a18
    equw     0
    equs "LAMBDA"

.NIL
    equb CHARF,     9
    equw 0
    equw 0
    equs "NIL"

.QUOTE
    equb CHARF,   &0b
    equw &0b04
    equw     0
    equs "QUOTE"

.ZB
    equb CHARF,   &0a
    equw &0ab6
    equw     0
    equs "NULL"

.ZC
    equb CHARF,   &0b
    equw &0abc
    equw     0
    equs "PRINT"

.ZD
    equb CHARF,   &0a
    equw &0ac2
    equw     0
    equs "CONS"

.ZE
    equb CHARF,   &0a
    equw &0ac8
    equw     0
    equs "EVAL"

.ZF
    equb CHARF,     9
    equw &0ace
    equw     0
    equs "CAR"

.ZG
    equb CHARF,     8
    equw &0ad4
    equw     0
    equs "EQ"

.ZH
    equb CHARF,   &0a
    equw &0ada
    equw     0
    equs "SETQ"

.ZI
    equb CHARF,     9
    equw &0ae0
    equw     0
    equs "SET"

.ZJ
    equb CHARF,   &0a
    equw &0ae6
    equw     0
    equs "ATOM"

.ZK
    equb CHARF,   &0a
    equw &0aec
    equw     0
    equs "READ"

.ZL
    equb CHARF,     9
    equw &0af2
    equw     0
    equs "CDR"

.ZM
    equb CHARF,   &0a
    equw &0af8
    equw     0
    equs "PRIN"

.ZN
    equb CHARF,   &0a
    equw &0afe
    equw     0
    equs "COND"

.ZO
    equb SUBRF,     1
    equw     0
    equw &0800

.ZP
    equb SUBRF,     0
    equw     0
    equw &0803

.ZQ
    equb SUBRF,     2
    equw     0
    equw &0806

.ZR
    equb SUBRF,     1
    equw     0
    equw &0809

.ZS
    equb SUBRF,     1
    equw     0
    equw &080c

.ZT
    equb SUBRF,     2
    equw     0
    equw &080f

.ZU
    equb FSUBRF,      0
    equw     0
    equw &0812

.ZV
    equb FSUBRF,      0
    equw     0
    equw &0815

.ZW
    equb SUBRF,     1
    equw     0
    equw &0818

.ZX
    equb SUBRF,     0
    equw     0
    equw &081b

.ZY
    equb SUBRF,     1
    equw     0
    equw &081e

.ZZ
    equb SUBRF,     0
    equw     0
    equw &0821

.ZAA
    equb FSUBRF,      0
    equw     0
    equw &0824

.ZAB
    equb FSUBRF,      0
    equw     0
    equw &0827

.ZBA
    equb CHARF,   &0b
    equw &0c22
    equw     0
    equs "PROGN"

.ZBB
    equb CHARF,   &0a
    equw &0c28
    equw     0
    equs "LOOP"

.ZBC
    equb CHARF,   &0b
    equw &0c2e
    equw     0
    equs "WHILE"

.ZBF
    equb CHARF,   &0a
    equw &0c34
    equw     0
    equs "LIST"

.ZBH
    equb CHARF,   &0a
    equw &0c3a
    equw     0
    equs "CAAR"

.ZBI
    equb CHARF,   &0a
    equw &0c40
    equw     0
    equs "CADR"

.ZBJ
    equb CHARF,   &0a
    equw &0c46
    equw     0
    equs "CDAR"

.ZBK
    equb CHARF,   &0a
    equw &0c4c
    equw     0
    equs "CDDR"

.ZBL
    equb CHARF,   &0b
    equw &0c52
    equw     0
    equs "CAAAR"

.ZBM
    equb CHARF,   &0b
    equw &0c58
    equw     0
    equs "CAADR"

.ZBN
    equb CHARF,   &0b
    equw &0c5e
    equw     0
    equs "CADAR"

.ZBO
    equb CHARF,   &0b
    equw &0c64
    equw     0
    equs "CADDR"

.ZPB
    equb CHARF,   &0b
    equw &0c6a
    equw     0
    equs "CDAAR"

.ZBQ
    equb CHARF,   &0b
    equw &0c70
    equw     0
    equs "CDADR"

.ZBR
    equb CHARF,   &0b
    equw &0c76
    equw     0
    equs "CDDAR"

.ZBS
    equb CHARF,   &0b
    equw &0c7c
    equw     0
    equs "CDDDR"

.ZBT
    equb CHARF,     9
    equw &0c82
    equw     0
    equs "AND"

.ZBU
    equb CHARF,     8
    equw &0c88
    equw     0
    equs "OR"

.ZBV
    equb CHARF,   &0b
    equw &0c8e
    equw     0
    equs "ERROR"

.ZBW
    equb CHARF,     9
    equw &0ab6
    equw     0
    equs "NOT"

.ZBX
    equb CHARF,   &0d
    equw &0c94
    equw     0
    equs "NUMBERP"

.ZBY
    equb CHARF,   &0b
    equw &0c9a
    equw     0
    equs "ZEROP"

.ZBZ
    equb CHARF,   &0a
    equw &0ca0
    equw     0
    equs "ONEP"

.ZCA
    equb CHARF,   &0c
    equw &0ca6
    equw     0
    equs "MINUSP"

.ZCB
    equb CHARF,     7
    equw 0
    equw 0
    equs "F"

.ZCC
    equb CHARF,   &0a
    equw &0cac
    equw     0
    equs "SAVE"

.ZCD
    equb CHARF,   &0a
    equw &0cb2
    equw     0
    equs "LOAD"

.ZEA
    equb FSUBRF,      0
    equw     0
    equw &082a

.ZEB
    equb FSUBRF,      0
    equw     0
    equw &082d

.ZEC
    equb FSUBRF,      0
    equw     0
    equw &0830

.ZEF
    equb SUBRF,     0
    equw     0
    equw &0833

.ZEH
    equb SUBRF,     1
    equw     0
    equw &0836

.ZEI
    equb SUBRF,     1
    equw     0
    equw &0839

.ZEJ
    equb SUBRF,     1
    equw     0
    equw &083c

.ZEK
    equb SUBRF,     1
    equw     0
    equw &083f

.ZEL
    equb SUBRF,     1
    equw     0
    equw &0842

.ZEM
    equb SUBRF,     1
    equw     0
    equw &0845

.ZEN
    equb SUBRF,     1
    equw     0
    equw &0848

.ZEO
    equb SUBRF,     1
    equw     0
    equw &084b

.ZEP
    equb SUBRF,     1
    equw     0
    equw &084e

.ZEQ
    equb SUBRF,     1
    equw     0
    equw &0851

.ZER
    equb SUBRF,     1
    equw     0
    equw &0854

.ZES
    equb SUBRF,     1
    equw     0
    equw &0857

.ZET
    equb FSUBRF,      0
    equw     0
    equw &085a

.ZEU
    equb FSUBRF,      0
    equw     0
    equw &085d

.ZEV
    equb SUBRF,     0
    equw     0
    equw &0860

.ZEX
    equb SUBRF,     1
    equw     0
    equw &0863

.ZEY
    equb SUBRF,     1
    equw     0
    equw &0866

.ZEZ
    equb SUBRF,     1
    equw     0
    equw &0869

.ZFA
    equb SUBRF,     1
    equw     0
    equw &086c

.ZFC
    equb SUBRF,     1
    equw     0
    equw &086f

.ZFD
    equb SUBRF,     1
    equw     0
    equw &0872

.ZFE
    equb SUBRF,     0
    equw     0
    equw &0875

.ZFF
    equb SUBRF,     2
    equw     0
    equw &0878

.ZFG
    equb SUBRF,     1
    equw     0
    equw &087b

.ZFH
    equb SUBRF,     1
    equw     0
    equw &087e

.ZFI
    equb SUBRF,     0
    equw     0
    equw &0881

.ZFJ
    equb SUBRF,     2
    equw     0
    equw &0884

.ZFK
    equb SUBRF,     2
    equw     0
    equw &0887

.ZFL
    equb SUBRF,     2
    equw     0
    equw &088a

.ZFM
    equb SUBRF,     1
    equw     0
    equw &088d

.ZFN
    equb SUBRF,     1
    equw     0
    equw &0890

.ZFQ
    equb SUBRF,     0
    equw     0
    equw &0893

.ZFR
    equb SUBRF,     2
    equw     0
    equw &0896

.ZFS
    equb SUBRF,     2
    equw     0
    equw &0899

.ZFT
    equb SUBRF,     1
    equw     0
    equw &089c

.ZFU
    equb SUBRF,     0
    equw     0
    equw &089f

.ZFV
    equb SUBRF,     2
    equw     0
    equw &08a2

.ZFW
    equb SUBRF,     3
    equw     0
    equw &08a5

.ZFX
    equb SUBRF,     2
    equw     0
    equw &08a8

.ZFY
    equb SUBRF,     2
    equw     0
    equw &08ab

.ZFZ
    equb SUBRF,     1
    equw     0
    equw &08ae

.ZGA
    equb SUBRF,     1
    equw     0
    equw &08b1

.ZGB
    equb SUBRF,     1
    equw     0
    equw &08b4

.ZGD
    equb SUBRF,     2
    equw     0
    equw &08b7

.ZGE
    equb FSUBRF,      0
    equw     0
    equw &08ba

.ZGF
    equb SUBRF,     1
    equw     0
    equw &08bd

.ZGG
    equb CHARF,   &0a
    equw &0cb8
    equw     0
    equs "PLUS"

    ; ******************************
    ; The second LISP image file
    ; ******************************

    equb CHARF,   &10
    equw &0cbe
    equw     0
    equs "DIFFERENCE"

    equb CHARF,   &0b
    equw &0cc4
    equw     0
    equs "MINUS"

    equb CHARF,   &0b
    equw &0cca
    equw     0
    equs "SUBRP"

    equb CHARF,   &0b
    equw &0cd0
    equw     0
    equs "TIMES"

    equb CHARF,   &0e
    equw &0cd6
    equw     0
    equs "QUOTIENT"

    equb CHARF,   &0f
    equw &0cdc
    equw     0
    equs "REMAINDER"

    equb CHARF,   &0b
    equw &0ce2
    equw     0
    equs "LESSP"

    equb CHARF,   &0a
    equw &0ce8
    equw     0
    equs "SUB1"

    equb CHARF,   &0a
    equw &0cee
    equw     0
    equs "ADD1"

    equb CHARF,   &0d
    equw &0cf4
    equw     0
    equs "RECLAIM"

    equb CHARF,   &0c
    equw &0cfa
    equw     0
    equs "RPLACA"

    equb CHARF,   &0c
    equw &0d00
    equw     0
    equs "RPLACD"

    equb CHARF,   &0b
    equw &0d06
    equw     0
    equs "CHARS"

    equb CHARF,   &0c
    equw &0e5a
    equw     0
    equs "DOLLAR"

    equb CHARF,     8
    equw &0e61
    equw     0
    equs "CR"

    equb CHARF,   &0c
    equw &0d0c
    equw     0
    equs "MESSON"

    equb CHARF,   &0d
    equw &1054
    equw     0
    equs "MESSOFF"

    equb CHARF,     9
    equw &0d12
    equw     0
    equs "GET"

    equb CHARF,     9
    equw &0d18
    equw     0
    equs "PUT"

    equb CHARF,   &0d
    equw &0d1e
    equw     0
    equs "REMPROP"

    equb CHARF,   &0e
    equw &0d24
    equw     0
    equs "GREATERP"

    equb CHARF,   &0b
    equw &0d2a
    equw     0
    equs "PLIST"

    equb CHARF,     7
    equw &0e5a
    equw     0
    equs "$"

    equb CHARF,     7
    equw &0e61
    equw     0
    equb &0d

    equb CHARF,   &0b
    equw &0d30
    equw     0
    equs "CHARP"

    equb CHARF,   &0b
    equw &0d36
    equw     0
    equs "LISTP"

    equb CHARF,   &0b
    equw &0d3c
    equw     0
    equs "ASSOC"

    equb CHARF,     7
    equw &104e
    equw     0
    equs "*"

    equb CHARF,   &0a
    equw &0ebb
    equw     0
    equs "LPAR"

    equb CHARF,   &0b
    equw &0ec2
    equw     0
    equs "BLANK"

    equb CHARF,   &0a
    equw &0ec9
    equw     0
    equs "RPAR"

    equb CHARF,   &0c
    equw &0ed0
    equw     0
    equs "PERIOD"

    equb CHARF,     7
    equw &0ebb
    equw     0
    equs "("

    equb CHARF,     7
    equw &0ec2
    equw     0
    equs " "

    equb CHARF,     7
    equw &0ec9
    equw     0
    equs ")"

    equb CHARF,     7
    equw &0ed0
    equw     0
    equs "."

    equb CHARF,   &0b
    equw &0d42
    equw     0
    equs "UNTIL"

    equb CHARF,   &0c
    equw &0d48
    equw     0
    equs "FSUBRP"

    equb CHARF,   &10
    equw &0ffa
    equw     0
    equs "ERRORCOUNT"

    equb CHARF,   &0e
    equw &1000
    equw     0
    equs "ERRORSET"

    equb CHARF,   &0a
    equw &1006
    equw     0
    equs "CALL"

    equb CHARF,   &0a
    equw &100c
    equw     0
    equs "PEEK"

    equb CHARF,   &0a
    equw &1012
    equw     0
    equs "POKE"

    equb CHARF,   &0c
    equw &1018
    equw     0
    equs "OBLIST"

    equb CHARF,   &0b
    equw &101e
    equw     0
    equs "APPLY"

    equb CHARF,   &0a
    equw &1024
    equw     0
    equs "MAPC"

    equb CHARF,     9
    equw &102a
    equw     0
    equs "MAP"

    equb CHARF,   &0f
    equw &1030
    equw     0
    equs "CHARACTER"

    equb CHARF,   &0d
    equw &1036
    equw     0
    equs "ORDINAL"

    equb CHARF,   &0d
    equw &103c
    equw     0
    equs "EXPLODE"

    equb CHARF,   &0d
    equw &1042
    equw     0
    equs "IMPLODE"

    equb CHARF,   &0d
    equw &1048
    equw     0
    equs "GETCHAR"                                 ; N.B. ZMQ IS USED

    equb CHARF,   &0e
    equw &105a
    equw     0
    equs "READLINE"

    equb CHARF,   &0b
    equw &1060
    equw     0
    equs "CLOSE"

    equb CHARF,   &0a
    equw &1066
    equw     0
    equs "OPEN"

    equb CHARF,   &0c
    equw &106c
    equw     0
    equs "WRITE0"

    equb CHARF,   &0b
    equw &1072
    equw     0
    equs "WRITE"

    equb CHARF,     9
    equw &1078
    equw     0
    equs "EOF"

    equb CHARF,     9
    equw &107e
    equw     0
    equs "VDU"

    equb CHARF,   &0c
    equw &1084
    equw     0
    equs "PRINTC"

    equb CHARF,   &0b
    equw &108a
    equw     0
    equs "PRINC"

    equb SUBRF,     0
    equw     0
    equw &08c0

    equb FSUBRF,      0
    equw     0
    equw &08c3

    equb SUBRF,     1
    equw     0
    equw &08c6

    equb SUBRF,     1
    equw     0
    equw &08c9

    equb SUBRF,     2
    equw     0
    equw &08cc

    equb SUBRF,     0
    equw     0
    equw &08cf

    equb SUBRF,     2
    equw     0
    equw &08d2

    equb SUBRF,     2
    equw     0
    equw &08d5

    equb SUBRF,     2
    equw     0
    equw &08d8

    equb SUBRF,     1
    equw     0
    equw &08db

    equb SUBRF,     1
    equw     0
    equw &08de

    equb SUBRF,     1
    equw     0
    equw &08e1

    equb SUBRF,     1
    equw     0
    equw &08e4

    equb SUBRF,     0
    equw     0
    equw &08e7

    equb SUBRF,     1
    equw     0
    equw &08ea

    equb SUBRF,     1
    equw     0
    equw &08ed

    equb SUBRF,     0
    equw     0
    equw &08f0

    equb SUBRF,     1
    equw     0
    equw &08f3

    equb SUBRF,     2
    equw     0
    equw &08f6

    equb SUBRF,     1
    equw     0
    equw &08f9

    equb SUBRF,     1
    equw     0
    equw &08fc

    equb SUBRF,     1
    equw     0
    equw &08ff

    equb SUBRF,     0
    equw     0
    equw &0902

    equb SUBRF,     0
    equw     0
    equw &0905

    equb SUBRF,     0
    equw     0
    equw &0908

    equb CHARF,   &0a
    equw &1132
    equw     0
    equs "MODE"

    ; ******************************
    ; Routines new to version 2
    ; ******************************

    equb CHARF,     9
    equw &1138
    equw     0
    equs "USR"

    equb CHARF,   &0b
    equw &113e
    equw     0
    equs "SOUND"

    equb CHARF,   &0e
    equw &1144
    equw     0
    equs "ENVELOPE"

    equb CHARF,   &0c
    equw &114a
    equw     0
    equs "GCTIME"

    equb CHARF,   &0a
    equw &1150
    equw     0
    equs "TIME"

    equb CHARF,   &0b
    equw &1156
    equw     0
    equs "RESET"

    equb CHARF,   &0b
    equw &115c
    equw     0
    equs "POINT"

    equb CHARF,   &0b
    equw &1162
    equw     0
    equs "ADVAL"

    equb CHARF,   &0b
    equw &1168
    equw     0
    equs "CLOCK"

    equb CHARF,   &0a
    equw &116e
    equw     0
    equs "BNOT"

    equb CHARF,   &0a
    equw &1174
    equw     0
    equs "BAND"

    equb CHARF,     9
    equw &117a
    equw     0
    equs "BOR"

    equb CHARF,   &0b
    equw &1180
    equw     0
    equs "INKEY"

    equb CHARF,   &0c
    equw &1186
    equw     0
    equs "GENSYM"

    equb SUBRF,     1
    equw     0
    equw &090b

    equb SUBRF,     5
    equw     0
    equw &090e

    equb SUBRF,     4
    equw     0
    equw &0911

    equb SUBRF,   &0e
    equw     0
    equw &0914

    equb SUBRF,     0
    equw     0
    equw &0917

    equb SUBRF,     0
    equw     0
    equw &091a

    equb SUBRF,     0
    equw     0
    equw &091d

    equb SUBRF,     2
    equw     0
    equw &0923

    equb SUBRF,     1
    equw     0
    equw &0926

    equb SUBRF,     0
    equw     0
    equw &0920

    equb SUBRF,     1
    equw     0
    equw &0929

    equb SUBRF,     0
    equw     0
    equw &092c

    equb SUBRF,     0
    equw     0
    equw &092f

    equb SUBRF,     1
    equw     0
    equw &0932

    equb SUBRF,     0
    equw     0
    equw &0935

    equb CHARF,   &0d
    equw &1199
    equw &002c
    equs "VERSION"
    equb 4, 4, 4, 0

    equb CHARF,   &0f
    equw &11ac
    equw &00e3
    equs "LINEWIDTH"

    equb NUMF,    4
    equw &001f
    equw &0b00
    equb &bb, &11, &80,   0
    equs "DEFUN"

    equb LISTF,   &18
    equw &c70a
    equw &0011
    equb   7,   2, &0a, &ff,   0, &58

    equb LISTF,   &c0
    equw &0811
    equw &8012
    equs "|"
    equb &0a, &db, &11, &80, &61, &0a, &d6, &11
    equb &80, &c0, &11, &14,   0, &80, &d1, &11
    equb   3, &12, &80, &4d, &0a, &ef, &11, &80
    equb &2d, &0a, &ea, &11, &80, &18, &0a,   2
    equb   0, &80, &e5, &11, &fe, &11, &80, &99
    equb &0a, &f9, &11, &80, &c0, &11, &80,   0
    equb &80, &f4, &11, &80,   0, &80, &e0, &11
    equb &80,   0, &80, &cc, &11, &17, &12, &80
    equb &61, &0a, &12, &12, &80, &c0, &11, &80
    equb   0, &80, &0d, &12, &80,   0

    equb CHARF,   &0f
    equw &1382
    equw &006f
    equs "CHARCOUNT"

    equb LISTF,   &c0
    equw &3a11
    equw &0012
    equb &0a,   2, &0a, &7e,   0
    equs "LEFT"

    equb LISTF,   &30
    equw &8012
    equw &8000
    equs "+"
    equb &12, &7d, &13, &80, &ac, &0a, &a3, &12
    equb &80, &85, &0a, &4e, &12, &80, &c0, &11
    equb   0,   0, &80, &49, &12, &9e, &12, &80
    equb &ac, &0a, &99, &12, &80, &41, &0e, &62
    equb &12, &80, &30, &12, &71, &12, &80, &ea
    equb &0d, &6c, &12, &80, &c0, &11, &d0,   0
    equb &80, &67, &12,   0,   0, &80, &5d, &12
    equb &94, &12, &80, &58, &0d, &80, &12, &80
    equb &30, &12, &8f, &12, &80, &ea, &0d, &8a
    equb &12, &80, &c0, &11, &ff,   0, &80, &85
    equb &12, &ff,   0, &80, &7b, &12, &ff,   0
    equb &80, &76, &12, &4d,   0, &80, &58, &12
    equb   2,   0, &80, &53, &12, &78, &13, &80
    equb &15, &0b, &0f, &13, &80, &d7, &0e, &bc
    equb &12, &80, &85, &0a, &b7, &12, &80, &c0
    equb &11, &48,   0, &80, &b2, &12, &0a, &13
    equb &80, &1c, &12, &c6, &12, &80, &c0, &11
    equb   5, &13, &80, &58, &0d, &d0, &12, &80
    equb &30, &12,   0, &13, &80, &ac, &0a, &e8
    equb &12, &80, &c0, &11, &e3, &12,   4,   4
    equb   4,   0, &80, &df, &12, &16,   0, &80
    equb &da, &12, &fb, &12, &80, &11, &0a, &f6
    equb &12,   4,   4, &fe, &ff, &80, &f2, &12
    equb &2c,   0, &80, &ed, &12, &2d,   0, &80
    equb &d5, &12, &c0,   0, &80, &cb, &12, &5c
    equb   0, &80, &c1, &12, &52,   0, &80, &ad
    equb &12, &50, &13, &80, &1f, &0b, &4b, &13
    equb &80, &72, &0a, &1e, &13, &80, &30, &12
    equb &46, &13, &80, &1c, &12, &32, &13, &80
    equb &61, &0a, &2d, &13, &80, &c0, &11, &2d
    equb   0, &80, &28, &13, &41, &13, &80, &b1
    equb &0d, &3c, &13, &80, &30, &12,   0,   0
    equb &80, &37, &13,   0,   0, &80, &23, &13
    equb &16,   0, &80, &19, &13,   0,   0, &80
    equb &14, &13, &6e, &13, &80, &72, &0a, &5a
    equb &13, &80, &c0, &11, &69, &13, &80, &99
    equb &0a, &64, &13, &80, &c0, &11, &14,   0
    equb &80, &5f, &13, &16,   0, &80, &55, &13
    equb   0,   0, &80, &a8, &12, &16,   0, &80
    equb &73, &13,   0,   0, &80, &44, &12, &ff
    equb   0, &80, &18, &0a, &3f, &12

    equb CHARF,   &0a
    equw &141b
    equw &0016
    equs "EDIT"

    equb CHARF,     7
    equw &0a02
    equw &0017
    equs "L"

    equb LISTF,   &91
    equw &c713
    equw &0013
    equb &0c, &61, &18, &58,   0
    equs "SPRINT"
    equb &80, &9d, &13, &c2, &13, &80, &57, &0a
    equb &bd, &13, &80, &61, &0a, &b8, &13, &80
    equb &91, &13, &16,   0, &80, &b3, &13, &0f
    equb   0, &80, &ae, &13, &11,   0, &80, &a9
    equb &13, &d1, &13, &80, &e3, &0f, &17,   0
    equb &80, &cc, &13, &16, &14, &80, &7c, &0a
    equb &e5, &13, &80, &61, &0a, &e0, &13, &80
    equb &91, &13, &13,   0, &80, &db, &13, &11
    equb &14

    equb CHARF,     9
    equw &16c2
    equw &001a
    equs "SED"

    equb LISTF,   &ea
    equw &0c13
    equw &8014
    equs "W"
    equb &0a,   7, &14, &80, &61, &0a,   2, &14
    equb &80, &91, &13, &cc,   0, &80, &fd, &13
    equb &4e,   0, &80, &f8, &13, &cc,   0, &80
    equb &f3, &13,   4,   0, &80, &d6, &13, &17
    equb   0, &80, &18, &0a, &98, &13,   0,   7
    equb   2, &0a, &99,   0, &41, &80, &20, &14
    equb &38, &14,   0,   7,   2, &0a, &80,   0
    equb &51, &80, &2c, &14, &9e,   0, &80, &33
    equb &14, &94,   0, &80, &27, &14, &bd, &16
    equb &80, &15, &0b, &65, &14, &80, &72, &0a
    equb &4c, &14, &80, &2c, &14, &60, &14, &80
    equb &ef, &0f, &5b, &14, &80, &8a, &0f, &c1
    equb   0, &80, &56, &14, &b7,   0, &80, &51
    equb &14, &ef,   0, &80, &47, &14, &99, &14
    equb &80, &d7, &0e, &8f, &14, &80, &6a, &0a
    equb &74, &14, &80, &2c, &14, &8a, &14, &80
    equb &2d, &0a, &85, &14,   0,   7,   2, &0a
    equb &0e,   0, &42, &80, &7e, &14, &fd,   0
    equb &80, &79, &14, &e3,   0, &80, &6f, &14
    equb &94, &14, &80, &20, &14, &0c,   0, &80
    equb &6a, &14, &b8, &16, &80, &72, &0a, &a3
    equb &14, &80, &20, &14, &b3, &16, &80, &ac
    equb &0a, &e6, &14, &80, &6a, &0a, &b2, &14
    equb &80, &2c, &14, &c8, &14, &80, &2d, &0a
    equb &c3, &14,   0,   7,   2, &0a, &18,   0
    equb &52, &80, &bc, &14, &80,   0, &80, &b7
    equb &14, &80,   0, &80, &ad, &14, &d7, &14
    equb &80, &e3, &0f, &80,   0, &80, &d2, &14
    equb &e1, &14, &80, &8f, &0a, &80,   0, &80
    equb &dc, &14, &80,   0, &80, &cd, &14, &1d
    equb &15, &80, &6a, &0a, &f0, &14, &80, &2c
    equb &14, &f5, &14, &80,   1, &0e,   0,   0
    equb &80, &eb, &14,   9, &15, &80, &9d, &13
    equb   4, &15, &80, &20, &14, &80,   0, &80
    equb &ff, &14, &13, &15, &80, &e3, &0f, &bc
    equb   0, &80, &0e, &15, &18, &15, &80, &20
    equb &14, &b2,   0, &80, &fa, &14, &6a, &15
    equb &80, &6a, &0a, &27, &15, &80, &2c, &14
    equb &3d, &15, &80, &2d, &0a, &38, &15,   0
    equb   7,   2, &0a, &80,   0, &43, &80, &31
    equb &15, &9a,   0, &80, &2c, &15, &90,   0
    equb &80, &22, &15, &4c, &15, &80, &e3, &0f
    equb &86,   0, &80, &47, &15, &65, &15, &80
    equb &4d, &0a, &5b, &15, &80, &8f, &0a, &c1
    equb   0, &80, &56, &15, &60, &15, &80, &20
    equb &14, &18,   0, &80, &51, &15, &0e,   0
    equb &80, &42, &15, &9c, &15, &80, &85, &0a
    equb &74, &15, &80, &20, &14, &80,   0, &80
    equb &6f, &15, &92, &15, &80, &ef, &0f, &8d
    equb &15, &80, &2d, &0a, &88, &15, &80, &89
    equb &0e,   0,   0, &80, &83, &15,   0,   0
    equb &80, &7e, &15, &97, &15, &80, &20, &14
    equb &0a,   0, &80, &79, &15, &f8, &15, &80
    equb &6a, &0a, &a6, &15, &80, &2c, &14, &bc
    equb &15, &80, &2d, &0a, &b7, &15,   0,   7
    equb   2, &0a, &4f,   0, &44, &80, &b0, &15
    equb &10,   0, &80, &ab, &15, &4c,   0, &80
    equb &a1, &15, &f3, &15, &80, &4d, &0a, &d5
    equb &15, &80, &61, &0a, &d0, &15, &80, &20
    equb &14,   0,   0, &80, &cb, &15, &ee, &15
    equb &80, &ea, &13, &e9, &15, &80, &99, &0a
    equb &e4, &15, &80, &20, &14, &43,   0, &80
    equb &df, &15, &43,   0, &80, &da, &15, &0d
    equb   0, &80, &c6, &15, &4f,   0, &80, &c1
    equb &15, &4d, &16, &80, &6a, &0a,   2, &16
    equb &80, &2c, &14, &11, &16, &80, &2d, &0a
    equb &0c, &16, &80, &20, &14,   0,   0, &80
    equb   7, &16, &4f,   0, &80, &fd, &15, &48
    equb &16, &80, &4d, &0a, &34, &16, &80, &ea
    equb &13, &2f, &16, &80, &61, &0a, &2a, &16
    equb &80, &20, &14, &44,   0, &80, &25, &16
    equb   0,   0, &80, &20, &16, &43, &16, &80
    equb &99, &0a, &3e, &16, &80, &20, &14,   0
    equb   0, &80, &39, &16, &4e,   0, &80, &1b
    equb &16,   0,   0, &80, &16, &16, &7f, &16
    equb &80, &6a, &0a, &57, &16, &80, &2c, &14
    equb &66, &16, &80, &2d, &0a, &61, &16, &80
    equb &c0, &11, &45,   0, &80, &5c, &16, &7e
    equb   0, &80, &52, &16, &7a, &16, &80, &99
    equb &0a, &75, &16, &80, &20, &14, &49,   0
    equb &80, &70, &16, &0b,   0, &80, &6b, &16
    equb &ae, &16, &80, &11, &0a, &a4, &16, &80
    equb &ef, &0f, &9f, &16, &80, &2d, &0a, &9a
    equb &16,   0,   7,   2, &0a,   0,   0, &3f
    equb &80, &93, &16,   0,   0, &80, &8e, &16
    equb   0,   0, &80, &89, &16, &a9, &16, &80
    equb &20, &14,   0,   0, &80, &84, &16,   0
    equb   0, &80, &a8, &14,   0,   0, &80, &9e
    equb &14,   0,   0, &80, &42, &14,   0,   0
    equb &80, &18, &0a, &3d, &14, &80, &c0, &11
    equb &dc, &16,   0,   7,   2, &0a,   0,   0
    equb &4e, &80, &cc, &16, &d8, &16,   4,   4
    equb   0,   0, &80, &d3, &16,   0,   0, &80
    equb &c7, &16, &5c, &18, &80, &ac, &0a, &36
    equb &17, &80, &bd, &0b, &fa, &16, &80, &85
    equb &0a, &f5, &16, &80, &c0, &11,   0,   0
    equb &80, &f0, &16, &1d, &17, &80, &1c, &12
    equb   4, &17, &80, &c0, &11, &18, &17, &80
    equb &58, &0d, &0e, &17, &80, &9d, &11, &13
    equb &17, &80, &cc, &16,   0,   0, &80,   9
    equb &17,   0,   0, &80, &ff, &16,   0,   0
    equb &80, &eb, &16, &31, &17, &80, &ef, &0f
    equb &2c, &17, &80, &c0, &11,   0,   0, &80
    equb &27, &17,   0,   0, &80, &22, &17, &57
    equb &18, &80, &ef, &0f, &40, &17, &80, &90
    equb &0e,   0,   0, &80, &3b, &17, &63, &17
    equb &80, &9d, &13, &59, &17, &80, &61, &0a
    equb &54, &17, &80, &c0, &11,   0,   0, &80
    equb &4f, &17, &5e, &17, &80, &cc, &16,   0
    equb   0, &80, &4a, &17, &8a, &17, &80, &72
    equb &0a, &6d, &17, &80, &cc, &16, &85, &17
    equb &80, &4e, &0d, &77, &17, &80, &cc, &16
    equb &80, &17,   4,   4,   3,   0, &80, &7c
    equb &17,   0,   0, &80, &72, &17,   0,   0
    equb &80, &68, &17, &52, &18, &80, &15, &0b
    equb &ad, &17, &80, &72, &0a, &99, &17, &80
    equb &c0, &11, &a8, &17, &80, &99, &0a, &a3
    equb &17, &80, &c0, &11,   0,   0, &80, &9e
    equb &17,   0,   0, &80, &94, &17, &ee, &17
    equb &80, &ac, &0a, &e9, &17, &80, &b4, &0b
    equb &bc, &17, &80, &c0, &11, &cb, &17, &80
    equb &85, &0a, &c6, &17, &80, &c0, &11,   0
    equb   0, &80, &c1, &17,   0,   0, &80, &b7
    equb &17, &e4, &17, &80, &ef, &0f, &da, &17
    equb &80, &af, &0e, &df, &17, &80, &c0, &11
    equb   0,   0, &80, &d5, &17,   0,   0, &80
    equb &d0, &17,   0,   0, &80, &b2, &17, &16
    equb &18, &80, &d7, &0e,   2, &18, &80, &85
    equb &0a, &fd, &17, &80, &c0, &11,   0,   0
    equb &80, &f8, &17, &11, &18, &80, &ef, &0f
    equb &0c, &18, &80, &a5, &0e,   0,   0, &80
    equb   7, &18,   0,   0, &80, &f3, &17, &2f
    equb &18

    equb CHARF,   &0a
    equw &18d5
    equw     0
    equs "XTAB"

    equb LISTF,   &1b
    equw &2a18
    equw &8018
    equb &cc, &16,   0,   0, &80, &25, &18, &4d
    equb &18, &80, &9d, &13, &43, &18, &80, &61
    equb &0a, &3e, &18, &80, &c0, &11,   0,   0
    equb &80, &39, &18, &48, &18, &80, &cc, &16
    equb   0,   0, &80, &34, &18,   0,   0, &80
    equb &8f, &17,   0,   0, &80, &45, &17,   0
    equb   0, &80, &e6, &16,   0,   0, &80, &18
    equb &0a, &e1, &16,   0,   7,   2, &0a,   0
    equb   0, &53, &80, &66, &18,   0,   0, &80
    equb &6d, &18, &7c, &18, &80, &e3, &0f,   0
    equb   0, &80, &77, &18, &d0, &18, &80, &15
    equb &0b, &bc, &18, &80, &d7, &0e, &b7, &18
    equb &80, &fb, &0b, &b2, &18, &80, &72, &0a
    equb &95, &18, &80, &66, &18, &ad, &18, &80
    equb &58, &0d, &9f, &18, &80, &66, &18, &a8
    equb &18,   4,   4,   1,   0, &80, &a4, &18
    equb   0,   0, &80, &9a, &18,   0,   0, &80
    equb &90, &18,   0,   0, &80, &8b, &18,   0
    equb   0, &80, &86, &18, &cb, &18, &80, &ef
    equb &0f, &c6, &18, &80, &9a, &0e,   0,   0
    equb &80, &c1, &18,   0,   0, &80, &81, &18
    equb   0,   0, &80, &18, &0a, &72, &18, &80
    equb &0e, &0c, &f4, &18

    equb CHARF,   &0b
    equw &0a02
    equw     0
    equs "IMAGE"

    equb LISTF,   &df
    equw &0018
    equw &8000
    equs "-"
    equb &0a, &ea, &18, &80, &ef, &18
.unused3
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0

; relocation table (entries point to high bytes of addresses)
MOVE_OFFSET = HILISP-LISVAL
.RELTAB
    equw MOVE_OFFSET + $8002
    equw MOVE_OFFSET + $8005
    equw MOVE_OFFSET + $8050
    equw MOVE_OFFSET + $807b
    equw MOVE_OFFSET + $80a3
    equw MOVE_OFFSET + $80a9
    equw MOVE_OFFSET + $80b5
    equw MOVE_OFFSET + $80b8
    equw MOVE_OFFSET + $80c5
    equw MOVE_OFFSET + $80fc
    equw MOVE_OFFSET + $8132
    equw MOVE_OFFSET + $8149
    equw MOVE_OFFSET + $8284
    equw MOVE_OFFSET + $8291
    equw MOVE_OFFSET + $8294
    equw MOVE_OFFSET + $829e
    equw MOVE_OFFSET + $82b3
    equw MOVE_OFFSET + $82bb
    equw MOVE_OFFSET + $82be
    equw MOVE_OFFSET + $82c2
    equw MOVE_OFFSET + $82d8
    equw MOVE_OFFSET + $82dd
    equw MOVE_OFFSET + $82e0
    equw MOVE_OFFSET + $82e3
    equw MOVE_OFFSET + $82e8
    equw MOVE_OFFSET + $82eb
    equw MOVE_OFFSET + $82ee
    equw MOVE_OFFSET + $83af
    equw MOVE_OFFSET + $8402
    equw MOVE_OFFSET + $8475
    equw MOVE_OFFSET + $84bb
    equw MOVE_OFFSET + $84e3
    equw MOVE_OFFSET + $84e6
    equw MOVE_OFFSET + $84fb
    equw MOVE_OFFSET + $8502
    equw MOVE_OFFSET + $8508
    equw MOVE_OFFSET + $8538
    equw MOVE_OFFSET + $853b
    equw MOVE_OFFSET + $8571
    equw MOVE_OFFSET + $858a
    equw MOVE_OFFSET + $858d
    equw MOVE_OFFSET + $8590
    equw MOVE_OFFSET + $8593
    equw MOVE_OFFSET + $8596
    equw MOVE_OFFSET + $85c9
    equw MOVE_OFFSET + $85e5
    equw MOVE_OFFSET + $8636
    equw MOVE_OFFSET + $869a
    equw MOVE_OFFSET + $86b5
    equw MOVE_OFFSET + $86b8
    equw MOVE_OFFSET + $86c1
    equw MOVE_OFFSET + $8710
    equw MOVE_OFFSET + $8713
    equw MOVE_OFFSET + $872d
    equw MOVE_OFFSET + $873a
    equw MOVE_OFFSET + $8755
    equw MOVE_OFFSET + $8762
    equw MOVE_OFFSET + $876b
    equw MOVE_OFFSET + $879c
    equw MOVE_OFFSET + $87d5
    equw MOVE_OFFSET + $87d8
    equw MOVE_OFFSET + $87f4
    equw MOVE_OFFSET + $87f7
    equw MOVE_OFFSET + $880d
    equw MOVE_OFFSET + $8822
    equw MOVE_OFFSET + $8825
    equw MOVE_OFFSET + $8834
    equw MOVE_OFFSET + $8847
    equw MOVE_OFFSET + $884e
    equw MOVE_OFFSET + $8857
    equw MOVE_OFFSET + $885c
    equw MOVE_OFFSET + $8861
    equw MOVE_OFFSET + $8866
    equw MOVE_OFFSET + $8871
    equw MOVE_OFFSET + $887c
    equw MOVE_OFFSET + $8883
    equw MOVE_OFFSET + $8893
    equw MOVE_OFFSET + $8896
    equw MOVE_OFFSET + $88c0
    equw MOVE_OFFSET + $88c7
    equw MOVE_OFFSET + $88ca
    equw MOVE_OFFSET + $88d8
    equw MOVE_OFFSET + $88db
    equw MOVE_OFFSET + $88de
    equw MOVE_OFFSET + $88e1
    equw MOVE_OFFSET + $88e4
    equw MOVE_OFFSET + $88e7
    equw MOVE_OFFSET + $88f4
    equw MOVE_OFFSET + $8913
    equw MOVE_OFFSET + $894d
    equw MOVE_OFFSET + $89b8
    equw MOVE_OFFSET + $89c5
    equw MOVE_OFFSET + $89f1
    equw MOVE_OFFSET + $8a4d
    equw MOVE_OFFSET + $8a54
    equw MOVE_OFFSET + $8a9e
    equw MOVE_OFFSET + $8aa8
    equw MOVE_OFFSET + $8aad
    equw MOVE_OFFSET + $8abb
    equw MOVE_OFFSET + $8ad9
    equw MOVE_OFFSET + $8ade
    equw MOVE_OFFSET + $8aeb
    equw MOVE_OFFSET + $8af0
    equw MOVE_OFFSET + $8af4
    equw MOVE_OFFSET + $8b00
    equw MOVE_OFFSET + $8b03
    equw MOVE_OFFSET + $8b13
    equw MOVE_OFFSET + $8b18
    equw MOVE_OFFSET + $8b25
    equw MOVE_OFFSET + $8b28
    equw MOVE_OFFSET + $8b49
    equw MOVE_OFFSET + $8b58
    equw MOVE_OFFSET + $8b6e
    equw MOVE_OFFSET + $8b88
    equw MOVE_OFFSET + $8b93
    equw MOVE_OFFSET + $8b9e
    equw MOVE_OFFSET + $8ba1
    equw MOVE_OFFSET + $8bb0
    equw MOVE_OFFSET + $8bb3
    equw MOVE_OFFSET + $8bb6
    equw MOVE_OFFSET + $8bcd
    equw MOVE_OFFSET + $8bd0
    equw MOVE_OFFSET + $8bd5
    equw MOVE_OFFSET + $8bd8
    equw MOVE_OFFSET + $8bff
    equw MOVE_OFFSET + $8c04
    equw MOVE_OFFSET + $8c12
    equw MOVE_OFFSET + $8c15
    equw MOVE_OFFSET + $8c23
    equw MOVE_OFFSET + $8c26
    equw MOVE_OFFSET + $8c2b
    equw MOVE_OFFSET + $8c33
    equw MOVE_OFFSET + $8c37
    equw MOVE_OFFSET + $8c3a
    equw MOVE_OFFSET + $8c4f
    equw MOVE_OFFSET + $8c56
    equw MOVE_OFFSET + $8c6c
    equw MOVE_OFFSET + $8c87
    equw MOVE_OFFSET + $8cab
    equw MOVE_OFFSET + $8ccc
    equw MOVE_OFFSET + $8ccf
    equw MOVE_OFFSET + $8cd2
    equw MOVE_OFFSET + $8cdf
    equw MOVE_OFFSET + $8cf4
    equw MOVE_OFFSET + $8cf7
    equw MOVE_OFFSET + $8d16
    equw MOVE_OFFSET + $8d27
    equw MOVE_OFFSET + $8d2e
    equw MOVE_OFFSET + $8d31
    equw MOVE_OFFSET + $8d3a
    equw MOVE_OFFSET + $8d43
    equw MOVE_OFFSET + $8d4c
    equw MOVE_OFFSET + $8d4f
    equw MOVE_OFFSET + $8d52
    equw MOVE_OFFSET + $8daf
    equw MOVE_OFFSET + $8dbe
    equw MOVE_OFFSET + $8dc3
    equw MOVE_OFFSET + $8dc6
    equw MOVE_OFFSET + $8dde
    equw MOVE_OFFSET + $8de1
    equw MOVE_OFFSET + $8de8
    equw MOVE_OFFSET + $8def
    equw MOVE_OFFSET + $8df2
    equw MOVE_OFFSET + $8df9
    equw MOVE_OFFSET + $8e12
    equw MOVE_OFFSET + $8e1d
    equw MOVE_OFFSET + $8e20
    equw MOVE_OFFSET + $8e33
    equw MOVE_OFFSET + $8e36
    equw MOVE_OFFSET + $8e39
    equw MOVE_OFFSET + $8e3c
    equw MOVE_OFFSET + $8e49
    equw MOVE_OFFSET + $8e4c
    equw MOVE_OFFSET + $8e5a
    equw MOVE_OFFSET + $8e7a
    equw MOVE_OFFSET + $8ea2
    equw MOVE_OFFSET + $8ea5
    equw MOVE_OFFSET + $8ebf
    equw MOVE_OFFSET + $8ecc
    equw MOVE_OFFSET + $8ee7
    equw MOVE_OFFSET + $8eea
    equw MOVE_OFFSET + $8f09
    equw MOVE_OFFSET + $8f1e
    equw MOVE_OFFSET + $8f21
    equw MOVE_OFFSET + $8f5e
    equw MOVE_OFFSET + $8f73
    equw MOVE_OFFSET + $8f76
    equw MOVE_OFFSET + $8f79
    equw MOVE_OFFSET + $8f7c
    equw MOVE_OFFSET + $8f7f
    equw MOVE_OFFSET + $8f8b
    equw MOVE_OFFSET + $8fbc
    equw MOVE_OFFSET + $8fcb
    equw MOVE_OFFSET + $8fcd
    equw MOVE_OFFSET + $8fd9
    equw MOVE_OFFSET + $8fe6
    equw MOVE_OFFSET + $8fe9
    equw MOVE_OFFSET + $8ff8
    equw MOVE_OFFSET + $8ffb
    equw MOVE_OFFSET + $9006
    equw MOVE_OFFSET + $9013
    equw MOVE_OFFSET + $9037
    equw MOVE_OFFSET + $903c
    equw MOVE_OFFSET + $9054
    equw MOVE_OFFSET + $9074
    equw MOVE_OFFSET + $9097
    equw MOVE_OFFSET + $909a
    equw MOVE_OFFSET + $90a5
    equw MOVE_OFFSET + $90bd
    equw MOVE_OFFSET + $9109
    equw MOVE_OFFSET + $910e
    equw MOVE_OFFSET + $9119
    equw MOVE_OFFSET + $9120
    equw MOVE_OFFSET + $9123
    equw MOVE_OFFSET + $912c
    equw MOVE_OFFSET + $913c
    equw MOVE_OFFSET + $9176
    equw MOVE_OFFSET + $9179
    equw MOVE_OFFSET + $9184
    equw MOVE_OFFSET + $918d
    equw MOVE_OFFSET + $91a7
    equw MOVE_OFFSET + $91b0
    equw MOVE_OFFSET + $91c0
    equw MOVE_OFFSET + $91c3
    equw MOVE_OFFSET + $91c6
    equw MOVE_OFFSET + $91db
    equw MOVE_OFFSET + $91de
    equw MOVE_OFFSET + $91f1
    equw MOVE_OFFSET + $91f4
    equw MOVE_OFFSET + $91f7
    equw MOVE_OFFSET + $91ff
    equw MOVE_OFFSET + $9202
    equw MOVE_OFFSET + $922b
    equw MOVE_OFFSET + $922e
    equw MOVE_OFFSET + $925c
    equw MOVE_OFFSET + $925f
    equw MOVE_OFFSET + $9288
    equw MOVE_OFFSET + $929b
    equw MOVE_OFFSET + $92a6
    equw MOVE_OFFSET + $92b4
    equw MOVE_OFFSET + $92b7
    equw MOVE_OFFSET + $92d3
    equw MOVE_OFFSET + $92df
    equw MOVE_OFFSET + $92e2
    equw MOVE_OFFSET + $92ee
    equw MOVE_OFFSET + $930a
    equw MOVE_OFFSET + $930d
    equw MOVE_OFFSET + $9336
    equw MOVE_OFFSET + $934f
    equw MOVE_OFFSET + $9354
    equw MOVE_OFFSET + $93be
    equw MOVE_OFFSET + $93d5
    equw MOVE_OFFSET + $93d8
    equw MOVE_OFFSET + $93db
    equw MOVE_OFFSET + $93eb
    equw MOVE_OFFSET + $93ee
    equw MOVE_OFFSET + $93f1
    equw MOVE_OFFSET + $9403
    equw MOVE_OFFSET + $9406
    equw MOVE_OFFSET + $9411
    equw MOVE_OFFSET + $9449
    equw MOVE_OFFSET + $944c
    equw MOVE_OFFSET + $944f
    equw MOVE_OFFSET + $9478
    equw MOVE_OFFSET + $9486
    equw MOVE_OFFSET + $94b5
    equw MOVE_OFFSET + $94b8
    equw MOVE_OFFSET + $94bb
    equw MOVE_OFFSET + $9538
    equw MOVE_OFFSET + $9574
    equw MOVE_OFFSET + $9577
    equw MOVE_OFFSET + $957a
    equw MOVE_OFFSET + $9594
    equw MOVE_OFFSET + $9597
    equw MOVE_OFFSET + $959c
    equw MOVE_OFFSET + $959f
    equw MOVE_OFFSET + $95a4
    equw MOVE_OFFSET + $95a7
    equw MOVE_OFFSET + $95ac
    equw MOVE_OFFSET + $95be
    equw MOVE_OFFSET + $95c1
    equw MOVE_OFFSET + $95f6
    equw MOVE_OFFSET + $9604
    equw MOVE_OFFSET + $960d
    equw MOVE_OFFSET + $9610
    equw MOVE_OFFSET + $9613
    equw MOVE_OFFSET + $9624
    equw MOVE_OFFSET + $962b
    equw MOVE_OFFSET + $962e
    equw MOVE_OFFSET + $9631
    equw MOVE_OFFSET + $9634
    equw MOVE_OFFSET + $9644
    equw MOVE_OFFSET + $9647
    equw MOVE_OFFSET + $966e
    equw MOVE_OFFSET + $96ac
    equw MOVE_OFFSET + $96b9
    equw MOVE_OFFSET + $96d4
    equw MOVE_OFFSET + $96e7
    equw MOVE_OFFSET + $9704
    equw MOVE_OFFSET + $9720
    equw MOVE_OFFSET + $9751
    equw MOVE_OFFSET + $9754
    equw MOVE_OFFSET + $975f
    equw MOVE_OFFSET + $9762
    equw MOVE_OFFSET + $976f
    equw MOVE_OFFSET + $9772
    equw MOVE_OFFSET + $9775
    equw MOVE_OFFSET + $978a
    equw MOVE_OFFSET + $97d4
    equw MOVE_OFFSET + $97f2
    equw MOVE_OFFSET + $97f5
    equw MOVE_OFFSET + $980e
    equw MOVE_OFFSET + $9812
    equw MOVE_OFFSET + $9816
    equw MOVE_OFFSET + $981a
    equw MOVE_OFFSET + $981d
    equw MOVE_OFFSET + $9822
    equw MOVE_OFFSET + $9825
    equw MOVE_OFFSET + $9849
    equw MOVE_OFFSET + $9860
    equw MOVE_OFFSET + $988a
    equw MOVE_OFFSET + $988d
    equw MOVE_OFFSET + $98c1
    equw MOVE_OFFSET + $98c4
    equw MOVE_OFFSET + $98c7
    equw MOVE_OFFSET + $98cb
    equw MOVE_OFFSET + $98d4
    equw MOVE_OFFSET + $98f0
    equw MOVE_OFFSET + $98f3
    equw MOVE_OFFSET + $98fe
    equw MOVE_OFFSET + $9910
    equw MOVE_OFFSET + $9913
    equw MOVE_OFFSET + $992f
    equw MOVE_OFFSET + $993c
    equw MOVE_OFFSET + $9946
    equw MOVE_OFFSET + $9953
    equw MOVE_OFFSET + $9959
    equw MOVE_OFFSET + $9962
    equw MOVE_OFFSET + $9965
    equw MOVE_OFFSET + $9968
    equw MOVE_OFFSET + $998b
    equw MOVE_OFFSET + $999e
    equw MOVE_OFFSET + $99a1
    equw MOVE_OFFSET + $99a4
    equw MOVE_OFFSET + $99c1
    equw MOVE_OFFSET + $99c4
    equw MOVE_OFFSET + $99cd
    equw MOVE_OFFSET + $99ea
    equw MOVE_OFFSET + $99f4
    equw MOVE_OFFSET + $9a06
    equw MOVE_OFFSET + $9a09
    equw MOVE_OFFSET + $9a2c
    equw MOVE_OFFSET + $9a45
    equw MOVE_OFFSET + $9a48
    equw MOVE_OFFSET + $9a4b
    equw MOVE_OFFSET + $9a5d
    equw MOVE_OFFSET + $9a60
    equw MOVE_OFFSET + $9a63
    equw MOVE_OFFSET + $9a8d
    equw MOVE_OFFSET + $9a90
    equw MOVE_OFFSET + $9a93
    equw MOVE_OFFSET + $9abd
    equw MOVE_OFFSET + $9aca
    equw MOVE_OFFSET + $9ad5
    equw MOVE_OFFSET + $9b3a
    equw MOVE_OFFSET + $9b45
    equw MOVE_OFFSET + $9b53
    equw MOVE_OFFSET + $9b71
    equw MOVE_OFFSET + $9b7c
    equw MOVE_OFFSET + $9b82
    equw MOVE_OFFSET + $9b97
    equw MOVE_OFFSET + $9bac
    equw MOVE_OFFSET + $9bc2
    equw MOVE_OFFSET + $9bd5
    equw MOVE_OFFSET + $9bda
    equw MOVE_OFFSET + $9bf0
    equw MOVE_OFFSET + $9bf3
    equw MOVE_OFFSET + $9c05
    equw MOVE_OFFSET + $9c15
    equw MOVE_OFFSET + $9c19
    equw MOVE_OFFSET + $9c21
    equw MOVE_OFFSET + $9c2f
    equw MOVE_OFFSET + $9c34
    equw MOVE_OFFSET + $9c52
    equw MOVE_OFFSET + $9ccb
    equw MOVE_OFFSET + $9ceb
    equw MOVE_OFFSET + $9cf4
    equw MOVE_OFFSET + $9cf7
    equw MOVE_OFFSET + $9d0f
    equw MOVE_OFFSET + $9d12
    equw MOVE_OFFSET + $9d1f
    equw MOVE_OFFSET + $9d31
    equw MOVE_OFFSET + $9d74
    equw MOVE_OFFSET + $9d81
    equw MOVE_OFFSET + $9da4
    equw MOVE_OFFSET + $9da9
    equw MOVE_OFFSET + $9dac
    equw MOVE_OFFSET + $9db1
    equw MOVE_OFFSET + $9dc3
    equw MOVE_OFFSET + $9dc8
    equw MOVE_OFFSET + $9e81
    equw MOVE_OFFSET + $9eaa
    equw MOVE_OFFSET + $9ed2
    equw MOVE_OFFSET + $9ed9
    equw MOVE_OFFSET + $9ee2
    equw MOVE_OFFSET + $9eeb
    equw MOVE_OFFSET + $9efd
    equw MOVE_OFFSET + $9f1b
    equw MOVE_OFFSET + $9f3b
    equw MOVE_OFFSET + $9f42
    equw MOVE_OFFSET + $9f66
    equw MOVE_OFFSET + $9f69
    equw MOVE_OFFSET + $9f6e
    equw MOVE_OFFSET + $9f73
    equw MOVE_OFFSET + $9f82
    equw MOVE_OFFSET + $9f91
    equw MOVE_OFFSET + $9f94
    equw MOVE_OFFSET + $9fa4
    equw MOVE_OFFSET + $9fab
    equw MOVE_OFFSET + $9fb5
    equw MOVE_OFFSET + $9fbf
    equw MOVE_OFFSET + $9fc2
    equw MOVE_OFFSET + $9fc6
    equw MOVE_OFFSET + $9fc9
    equw MOVE_OFFSET + $9fd7
    equw MOVE_OFFSET + $9fda
    equw MOVE_OFFSET + $9fdd
    equw MOVE_OFFSET + $9fe0
    equw MOVE_OFFSET + $9fe3
    equw MOVE_OFFSET + $a004
    equw MOVE_OFFSET + $a02d
.unused4
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff, &ff, &ff, &ff, &ff, &ff, &ff
    equb &ff, &ff,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0,   0,   0,   0,   0,   0,   0
    equb   0,   0
.ROMTOP
.pydis_end
    assert &80+&0d == &8d
    assert &80+' ' == &a0
    assert &80+'#' == &a3
    assert &80+'L' == &cc
    assert &80+'y' == &f9
    assert ' ' == &20
    assert '!' == &21
    assert '(' == &28
    assert ')' == &29
    assert '-' == &2d
    assert '.' == &2e
    assert '0' == &30
    assert '=' == &3d
    assert 'C' == &43
    assert 'F' == &46
    assert 'G' == &47
    assert 'I' == &49
    assert 'L' == &4c
    assert 'P' == &50
    assert 'S' == &53
    assert 'W' == &57
    assert '[' == &5b
    assert '\'' == &27
    assert '^' == &5e
    assert 'c' == &63
    assert (>LISPEN-LISVAL-1)+1 == &21
    assert <(DOSBUF) == &00
    assert <(GOSTR) == &31
    assert <(INCB) == &3e
    assert <(IODCB) == &16
    assert <(LISTR) == &39
    assert <(OSINFO) == &38
    assert <(OSWBUF) == &00
    assert <(PWORD) == &1b
    assert <(TIMEW) == &25
    assert <(TIMZER) == &76
    assert <ACL == &72
    assert <BACALL+1 == &dd
    assert <DWARF == &eb
    assert <ELFIN-1 == &c6
    assert <ERROR == &87
    assert <GCTIME == &20
    assert <HILISP == &00
    assert <HIWARM == &7b
    assert <KBD == &ff
    assert <LAMBDA == &18
    assert <LISPST == &00
    assert <NAMBUF == &00
    assert <NIL == &24
    assert <PRINGO+2 == &25
    assert <QUOTE == &2d
    assert <RELTAB == &00
    assert <TIMEW == &25
    assert <TRUE == &11
    assert <VECTAB == &00
    assert <VECTAB+2 == &02
    assert <WRITGO+2 == &eb
    assert <ZA == &02
    assert >(DOSBUF) == &07
    assert >(GOSTR) == &82
    assert >(INCB) == &82
    assert >(IODCB) == &04
    assert >(LISTR) == &82
    assert >(OSINFO) == &04
    assert >(OSWBUF) == &06
    assert >(PWORD) == &04
    assert >(TIMEW) == &04
    assert >(TIMZER) == &82
    assert >BACALL+1 == &8f
    assert >DWARF == &9e
    assert >ELFIN-1 == &9f
    assert >ERROR == &9e
    assert >GCTIME == &04
    assert >HILISP == &d7
    assert >HILISP-LISVAL == &57
    assert >HIWARM == &d9
    assert >KBD == &00
    assert >LISPST == &80
    assert >NAMBUF == &06
    assert >PRINGO == &8b
    assert >RELTAB == &b8
    assert >TIMEW == &04
    assert >VECTAB == &a4
    assert >WRITGO == &8a
    assert ACL == &72
    assert AREEXT == &00
    assert ARGINF == &90
    assert ARGOFF == &6b
    assert AUXL == &76
    assert BINDER == &32
    assert CHARF == &00
    assert COLDST == &00
    assert COLOFF == &3a
    assert CPYOFF-ROMHDR == &12
    assert ERROFF == &5d
    assert EVOFF == &00
    assert FROFF == &4c
    assert FSUBRF == &0c
    assert GCOFF == &33
    assert HLPOFF == &8e
    assert IMALEN == &12
    assert LISTF == &80
    assert MOVE_OFFSET + $8002 == &d702
    assert MOVE_OFFSET + $8005 == &d705
    assert MOVE_OFFSET + $8050 == &d750
    assert MOVE_OFFSET + $807b == &d77b
    assert MOVE_OFFSET + $80a3 == &d7a3
    assert MOVE_OFFSET + $80a9 == &d7a9
    assert MOVE_OFFSET + $80b5 == &d7b5
    assert MOVE_OFFSET + $80b8 == &d7b8
    assert MOVE_OFFSET + $80c5 == &d7c5
    assert MOVE_OFFSET + $80fc == &d7fc
    assert MOVE_OFFSET + $8132 == &d832
    assert MOVE_OFFSET + $8149 == &d849
    assert MOVE_OFFSET + $8284 == &d984
    assert MOVE_OFFSET + $8291 == &d991
    assert MOVE_OFFSET + $8294 == &d994
    assert MOVE_OFFSET + $829e == &d99e
    assert MOVE_OFFSET + $82b3 == &d9b3
    assert MOVE_OFFSET + $82bb == &d9bb
    assert MOVE_OFFSET + $82be == &d9be
    assert MOVE_OFFSET + $82c2 == &d9c2
    assert MOVE_OFFSET + $82d8 == &d9d8
    assert MOVE_OFFSET + $82dd == &d9dd
    assert MOVE_OFFSET + $82e0 == &d9e0
    assert MOVE_OFFSET + $82e3 == &d9e3
    assert MOVE_OFFSET + $82e8 == &d9e8
    assert MOVE_OFFSET + $82eb == &d9eb
    assert MOVE_OFFSET + $82ee == &d9ee
    assert MOVE_OFFSET + $83af == &daaf
    assert MOVE_OFFSET + $8402 == &db02
    assert MOVE_OFFSET + $8475 == &db75
    assert MOVE_OFFSET + $84bb == &dbbb
    assert MOVE_OFFSET + $84e3 == &dbe3
    assert MOVE_OFFSET + $84e6 == &dbe6
    assert MOVE_OFFSET + $84fb == &dbfb
    assert MOVE_OFFSET + $8502 == &dc02
    assert MOVE_OFFSET + $8508 == &dc08
    assert MOVE_OFFSET + $8538 == &dc38
    assert MOVE_OFFSET + $853b == &dc3b
    assert MOVE_OFFSET + $8571 == &dc71
    assert MOVE_OFFSET + $858a == &dc8a
    assert MOVE_OFFSET + $858d == &dc8d
    assert MOVE_OFFSET + $8590 == &dc90
    assert MOVE_OFFSET + $8593 == &dc93
    assert MOVE_OFFSET + $8596 == &dc96
    assert MOVE_OFFSET + $85c9 == &dcc9
    assert MOVE_OFFSET + $85e5 == &dce5
    assert MOVE_OFFSET + $8636 == &dd36
    assert MOVE_OFFSET + $869a == &dd9a
    assert MOVE_OFFSET + $86b5 == &ddb5
    assert MOVE_OFFSET + $86b8 == &ddb8
    assert MOVE_OFFSET + $86c1 == &ddc1
    assert MOVE_OFFSET + $8710 == &de10
    assert MOVE_OFFSET + $8713 == &de13
    assert MOVE_OFFSET + $872d == &de2d
    assert MOVE_OFFSET + $873a == &de3a
    assert MOVE_OFFSET + $8755 == &de55
    assert MOVE_OFFSET + $8762 == &de62
    assert MOVE_OFFSET + $876b == &de6b
    assert MOVE_OFFSET + $879c == &de9c
    assert MOVE_OFFSET + $87d5 == &ded5
    assert MOVE_OFFSET + $87d8 == &ded8
    assert MOVE_OFFSET + $87f4 == &def4
    assert MOVE_OFFSET + $87f7 == &def7
    assert MOVE_OFFSET + $880d == &df0d
    assert MOVE_OFFSET + $8822 == &df22
    assert MOVE_OFFSET + $8825 == &df25
    assert MOVE_OFFSET + $8834 == &df34
    assert MOVE_OFFSET + $8847 == &df47
    assert MOVE_OFFSET + $884e == &df4e
    assert MOVE_OFFSET + $8857 == &df57
    assert MOVE_OFFSET + $885c == &df5c
    assert MOVE_OFFSET + $8861 == &df61
    assert MOVE_OFFSET + $8866 == &df66
    assert MOVE_OFFSET + $8871 == &df71
    assert MOVE_OFFSET + $887c == &df7c
    assert MOVE_OFFSET + $8883 == &df83
    assert MOVE_OFFSET + $8893 == &df93
    assert MOVE_OFFSET + $8896 == &df96
    assert MOVE_OFFSET + $88c0 == &dfc0
    assert MOVE_OFFSET + $88c7 == &dfc7
    assert MOVE_OFFSET + $88ca == &dfca
    assert MOVE_OFFSET + $88d8 == &dfd8
    assert MOVE_OFFSET + $88db == &dfdb
    assert MOVE_OFFSET + $88de == &dfde
    assert MOVE_OFFSET + $88e1 == &dfe1
    assert MOVE_OFFSET + $88e4 == &dfe4
    assert MOVE_OFFSET + $88e7 == &dfe7
    assert MOVE_OFFSET + $88f4 == &dff4
    assert MOVE_OFFSET + $8913 == &e013
    assert MOVE_OFFSET + $894d == &e04d
    assert MOVE_OFFSET + $89b8 == &e0b8
    assert MOVE_OFFSET + $89c5 == &e0c5
    assert MOVE_OFFSET + $89f1 == &e0f1
    assert MOVE_OFFSET + $8a4d == &e14d
    assert MOVE_OFFSET + $8a54 == &e154
    assert MOVE_OFFSET + $8a9e == &e19e
    assert MOVE_OFFSET + $8aa8 == &e1a8
    assert MOVE_OFFSET + $8aad == &e1ad
    assert MOVE_OFFSET + $8abb == &e1bb
    assert MOVE_OFFSET + $8ad9 == &e1d9
    assert MOVE_OFFSET + $8ade == &e1de
    assert MOVE_OFFSET + $8aeb == &e1eb
    assert MOVE_OFFSET + $8af0 == &e1f0
    assert MOVE_OFFSET + $8af4 == &e1f4
    assert MOVE_OFFSET + $8b00 == &e200
    assert MOVE_OFFSET + $8b03 == &e203
    assert MOVE_OFFSET + $8b13 == &e213
    assert MOVE_OFFSET + $8b18 == &e218
    assert MOVE_OFFSET + $8b25 == &e225
    assert MOVE_OFFSET + $8b28 == &e228
    assert MOVE_OFFSET + $8b49 == &e249
    assert MOVE_OFFSET + $8b58 == &e258
    assert MOVE_OFFSET + $8b6e == &e26e
    assert MOVE_OFFSET + $8b88 == &e288
    assert MOVE_OFFSET + $8b93 == &e293
    assert MOVE_OFFSET + $8b9e == &e29e
    assert MOVE_OFFSET + $8ba1 == &e2a1
    assert MOVE_OFFSET + $8bb0 == &e2b0
    assert MOVE_OFFSET + $8bb3 == &e2b3
    assert MOVE_OFFSET + $8bb6 == &e2b6
    assert MOVE_OFFSET + $8bcd == &e2cd
    assert MOVE_OFFSET + $8bd0 == &e2d0
    assert MOVE_OFFSET + $8bd5 == &e2d5
    assert MOVE_OFFSET + $8bd8 == &e2d8
    assert MOVE_OFFSET + $8bff == &e2ff
    assert MOVE_OFFSET + $8c04 == &e304
    assert MOVE_OFFSET + $8c12 == &e312
    assert MOVE_OFFSET + $8c15 == &e315
    assert MOVE_OFFSET + $8c23 == &e323
    assert MOVE_OFFSET + $8c26 == &e326
    assert MOVE_OFFSET + $8c2b == &e32b
    assert MOVE_OFFSET + $8c33 == &e333
    assert MOVE_OFFSET + $8c37 == &e337
    assert MOVE_OFFSET + $8c3a == &e33a
    assert MOVE_OFFSET + $8c4f == &e34f
    assert MOVE_OFFSET + $8c56 == &e356
    assert MOVE_OFFSET + $8c6c == &e36c
    assert MOVE_OFFSET + $8c87 == &e387
    assert MOVE_OFFSET + $8cab == &e3ab
    assert MOVE_OFFSET + $8ccc == &e3cc
    assert MOVE_OFFSET + $8ccf == &e3cf
    assert MOVE_OFFSET + $8cd2 == &e3d2
    assert MOVE_OFFSET + $8cdf == &e3df
    assert MOVE_OFFSET + $8cf4 == &e3f4
    assert MOVE_OFFSET + $8cf7 == &e3f7
    assert MOVE_OFFSET + $8d16 == &e416
    assert MOVE_OFFSET + $8d27 == &e427
    assert MOVE_OFFSET + $8d2e == &e42e
    assert MOVE_OFFSET + $8d31 == &e431
    assert MOVE_OFFSET + $8d3a == &e43a
    assert MOVE_OFFSET + $8d43 == &e443
    assert MOVE_OFFSET + $8d4c == &e44c
    assert MOVE_OFFSET + $8d4f == &e44f
    assert MOVE_OFFSET + $8d52 == &e452
    assert MOVE_OFFSET + $8daf == &e4af
    assert MOVE_OFFSET + $8dbe == &e4be
    assert MOVE_OFFSET + $8dc3 == &e4c3
    assert MOVE_OFFSET + $8dc6 == &e4c6
    assert MOVE_OFFSET + $8dde == &e4de
    assert MOVE_OFFSET + $8de1 == &e4e1
    assert MOVE_OFFSET + $8de8 == &e4e8
    assert MOVE_OFFSET + $8def == &e4ef
    assert MOVE_OFFSET + $8df2 == &e4f2
    assert MOVE_OFFSET + $8df9 == &e4f9
    assert MOVE_OFFSET + $8e12 == &e512
    assert MOVE_OFFSET + $8e1d == &e51d
    assert MOVE_OFFSET + $8e20 == &e520
    assert MOVE_OFFSET + $8e33 == &e533
    assert MOVE_OFFSET + $8e36 == &e536
    assert MOVE_OFFSET + $8e39 == &e539
    assert MOVE_OFFSET + $8e3c == &e53c
    assert MOVE_OFFSET + $8e49 == &e549
    assert MOVE_OFFSET + $8e4c == &e54c
    assert MOVE_OFFSET + $8e5a == &e55a
    assert MOVE_OFFSET + $8e7a == &e57a
    assert MOVE_OFFSET + $8ea2 == &e5a2
    assert MOVE_OFFSET + $8ea5 == &e5a5
    assert MOVE_OFFSET + $8ebf == &e5bf
    assert MOVE_OFFSET + $8ecc == &e5cc
    assert MOVE_OFFSET + $8ee7 == &e5e7
    assert MOVE_OFFSET + $8eea == &e5ea
    assert MOVE_OFFSET + $8f09 == &e609
    assert MOVE_OFFSET + $8f1e == &e61e
    assert MOVE_OFFSET + $8f21 == &e621
    assert MOVE_OFFSET + $8f5e == &e65e
    assert MOVE_OFFSET + $8f73 == &e673
    assert MOVE_OFFSET + $8f76 == &e676
    assert MOVE_OFFSET + $8f79 == &e679
    assert MOVE_OFFSET + $8f7c == &e67c
    assert MOVE_OFFSET + $8f7f == &e67f
    assert MOVE_OFFSET + $8f8b == &e68b
    assert MOVE_OFFSET + $8fbc == &e6bc
    assert MOVE_OFFSET + $8fcb == &e6cb
    assert MOVE_OFFSET + $8fcd == &e6cd
    assert MOVE_OFFSET + $8fd9 == &e6d9
    assert MOVE_OFFSET + $8fe6 == &e6e6
    assert MOVE_OFFSET + $8fe9 == &e6e9
    assert MOVE_OFFSET + $8ff8 == &e6f8
    assert MOVE_OFFSET + $8ffb == &e6fb
    assert MOVE_OFFSET + $9006 == &e706
    assert MOVE_OFFSET + $9013 == &e713
    assert MOVE_OFFSET + $9037 == &e737
    assert MOVE_OFFSET + $903c == &e73c
    assert MOVE_OFFSET + $9054 == &e754
    assert MOVE_OFFSET + $9074 == &e774
    assert MOVE_OFFSET + $9097 == &e797
    assert MOVE_OFFSET + $909a == &e79a
    assert MOVE_OFFSET + $90a5 == &e7a5
    assert MOVE_OFFSET + $90bd == &e7bd
    assert MOVE_OFFSET + $9109 == &e809
    assert MOVE_OFFSET + $910e == &e80e
    assert MOVE_OFFSET + $9119 == &e819
    assert MOVE_OFFSET + $9120 == &e820
    assert MOVE_OFFSET + $9123 == &e823
    assert MOVE_OFFSET + $912c == &e82c
    assert MOVE_OFFSET + $913c == &e83c
    assert MOVE_OFFSET + $9176 == &e876
    assert MOVE_OFFSET + $9179 == &e879
    assert MOVE_OFFSET + $9184 == &e884
    assert MOVE_OFFSET + $918d == &e88d
    assert MOVE_OFFSET + $91a7 == &e8a7
    assert MOVE_OFFSET + $91b0 == &e8b0
    assert MOVE_OFFSET + $91c0 == &e8c0
    assert MOVE_OFFSET + $91c3 == &e8c3
    assert MOVE_OFFSET + $91c6 == &e8c6
    assert MOVE_OFFSET + $91db == &e8db
    assert MOVE_OFFSET + $91de == &e8de
    assert MOVE_OFFSET + $91f1 == &e8f1
    assert MOVE_OFFSET + $91f4 == &e8f4
    assert MOVE_OFFSET + $91f7 == &e8f7
    assert MOVE_OFFSET + $91ff == &e8ff
    assert MOVE_OFFSET + $9202 == &e902
    assert MOVE_OFFSET + $922b == &e92b
    assert MOVE_OFFSET + $922e == &e92e
    assert MOVE_OFFSET + $925c == &e95c
    assert MOVE_OFFSET + $925f == &e95f
    assert MOVE_OFFSET + $9288 == &e988
    assert MOVE_OFFSET + $929b == &e99b
    assert MOVE_OFFSET + $92a6 == &e9a6
    assert MOVE_OFFSET + $92b4 == &e9b4
    assert MOVE_OFFSET + $92b7 == &e9b7
    assert MOVE_OFFSET + $92d3 == &e9d3
    assert MOVE_OFFSET + $92df == &e9df
    assert MOVE_OFFSET + $92e2 == &e9e2
    assert MOVE_OFFSET + $92ee == &e9ee
    assert MOVE_OFFSET + $930a == &ea0a
    assert MOVE_OFFSET + $930d == &ea0d
    assert MOVE_OFFSET + $9336 == &ea36
    assert MOVE_OFFSET + $934f == &ea4f
    assert MOVE_OFFSET + $9354 == &ea54
    assert MOVE_OFFSET + $93be == &eabe
    assert MOVE_OFFSET + $93d5 == &ead5
    assert MOVE_OFFSET + $93d8 == &ead8
    assert MOVE_OFFSET + $93db == &eadb
    assert MOVE_OFFSET + $93eb == &eaeb
    assert MOVE_OFFSET + $93ee == &eaee
    assert MOVE_OFFSET + $93f1 == &eaf1
    assert MOVE_OFFSET + $9403 == &eb03
    assert MOVE_OFFSET + $9406 == &eb06
    assert MOVE_OFFSET + $9411 == &eb11
    assert MOVE_OFFSET + $9449 == &eb49
    assert MOVE_OFFSET + $944c == &eb4c
    assert MOVE_OFFSET + $944f == &eb4f
    assert MOVE_OFFSET + $9478 == &eb78
    assert MOVE_OFFSET + $9486 == &eb86
    assert MOVE_OFFSET + $94b5 == &ebb5
    assert MOVE_OFFSET + $94b8 == &ebb8
    assert MOVE_OFFSET + $94bb == &ebbb
    assert MOVE_OFFSET + $9538 == &ec38
    assert MOVE_OFFSET + $9574 == &ec74
    assert MOVE_OFFSET + $9577 == &ec77
    assert MOVE_OFFSET + $957a == &ec7a
    assert MOVE_OFFSET + $9594 == &ec94
    assert MOVE_OFFSET + $9597 == &ec97
    assert MOVE_OFFSET + $959c == &ec9c
    assert MOVE_OFFSET + $959f == &ec9f
    assert MOVE_OFFSET + $95a4 == &eca4
    assert MOVE_OFFSET + $95a7 == &eca7
    assert MOVE_OFFSET + $95ac == &ecac
    assert MOVE_OFFSET + $95be == &ecbe
    assert MOVE_OFFSET + $95c1 == &ecc1
    assert MOVE_OFFSET + $95f6 == &ecf6
    assert MOVE_OFFSET + $9604 == &ed04
    assert MOVE_OFFSET + $960d == &ed0d
    assert MOVE_OFFSET + $9610 == &ed10
    assert MOVE_OFFSET + $9613 == &ed13
    assert MOVE_OFFSET + $9624 == &ed24
    assert MOVE_OFFSET + $962b == &ed2b
    assert MOVE_OFFSET + $962e == &ed2e
    assert MOVE_OFFSET + $9631 == &ed31
    assert MOVE_OFFSET + $9634 == &ed34
    assert MOVE_OFFSET + $9644 == &ed44
    assert MOVE_OFFSET + $9647 == &ed47
    assert MOVE_OFFSET + $966e == &ed6e
    assert MOVE_OFFSET + $96ac == &edac
    assert MOVE_OFFSET + $96b9 == &edb9
    assert MOVE_OFFSET + $96d4 == &edd4
    assert MOVE_OFFSET + $96e7 == &ede7
    assert MOVE_OFFSET + $9704 == &ee04
    assert MOVE_OFFSET + $9720 == &ee20
    assert MOVE_OFFSET + $9751 == &ee51
    assert MOVE_OFFSET + $9754 == &ee54
    assert MOVE_OFFSET + $975f == &ee5f
    assert MOVE_OFFSET + $9762 == &ee62
    assert MOVE_OFFSET + $976f == &ee6f
    assert MOVE_OFFSET + $9772 == &ee72
    assert MOVE_OFFSET + $9775 == &ee75
    assert MOVE_OFFSET + $978a == &ee8a
    assert MOVE_OFFSET + $97d4 == &eed4
    assert MOVE_OFFSET + $97f2 == &eef2
    assert MOVE_OFFSET + $97f5 == &eef5
    assert MOVE_OFFSET + $980e == &ef0e
    assert MOVE_OFFSET + $9812 == &ef12
    assert MOVE_OFFSET + $9816 == &ef16
    assert MOVE_OFFSET + $981a == &ef1a
    assert MOVE_OFFSET + $981d == &ef1d
    assert MOVE_OFFSET + $9822 == &ef22
    assert MOVE_OFFSET + $9825 == &ef25
    assert MOVE_OFFSET + $9849 == &ef49
    assert MOVE_OFFSET + $9860 == &ef60
    assert MOVE_OFFSET + $988a == &ef8a
    assert MOVE_OFFSET + $988d == &ef8d
    assert MOVE_OFFSET + $98c1 == &efc1
    assert MOVE_OFFSET + $98c4 == &efc4
    assert MOVE_OFFSET + $98c7 == &efc7
    assert MOVE_OFFSET + $98cb == &efcb
    assert MOVE_OFFSET + $98d4 == &efd4
    assert MOVE_OFFSET + $98f0 == &eff0
    assert MOVE_OFFSET + $98f3 == &eff3
    assert MOVE_OFFSET + $98fe == &effe
    assert MOVE_OFFSET + $9910 == &f010
    assert MOVE_OFFSET + $9913 == &f013
    assert MOVE_OFFSET + $992f == &f02f
    assert MOVE_OFFSET + $993c == &f03c
    assert MOVE_OFFSET + $9946 == &f046
    assert MOVE_OFFSET + $9953 == &f053
    assert MOVE_OFFSET + $9959 == &f059
    assert MOVE_OFFSET + $9962 == &f062
    assert MOVE_OFFSET + $9965 == &f065
    assert MOVE_OFFSET + $9968 == &f068
    assert MOVE_OFFSET + $998b == &f08b
    assert MOVE_OFFSET + $999e == &f09e
    assert MOVE_OFFSET + $99a1 == &f0a1
    assert MOVE_OFFSET + $99a4 == &f0a4
    assert MOVE_OFFSET + $99c1 == &f0c1
    assert MOVE_OFFSET + $99c4 == &f0c4
    assert MOVE_OFFSET + $99cd == &f0cd
    assert MOVE_OFFSET + $99ea == &f0ea
    assert MOVE_OFFSET + $99f4 == &f0f4
    assert MOVE_OFFSET + $9a06 == &f106
    assert MOVE_OFFSET + $9a09 == &f109
    assert MOVE_OFFSET + $9a2c == &f12c
    assert MOVE_OFFSET + $9a45 == &f145
    assert MOVE_OFFSET + $9a48 == &f148
    assert MOVE_OFFSET + $9a4b == &f14b
    assert MOVE_OFFSET + $9a5d == &f15d
    assert MOVE_OFFSET + $9a60 == &f160
    assert MOVE_OFFSET + $9a63 == &f163
    assert MOVE_OFFSET + $9a8d == &f18d
    assert MOVE_OFFSET + $9a90 == &f190
    assert MOVE_OFFSET + $9a93 == &f193
    assert MOVE_OFFSET + $9abd == &f1bd
    assert MOVE_OFFSET + $9aca == &f1ca
    assert MOVE_OFFSET + $9ad5 == &f1d5
    assert MOVE_OFFSET + $9b3a == &f23a
    assert MOVE_OFFSET + $9b45 == &f245
    assert MOVE_OFFSET + $9b53 == &f253
    assert MOVE_OFFSET + $9b71 == &f271
    assert MOVE_OFFSET + $9b7c == &f27c
    assert MOVE_OFFSET + $9b82 == &f282
    assert MOVE_OFFSET + $9b97 == &f297
    assert MOVE_OFFSET + $9bac == &f2ac
    assert MOVE_OFFSET + $9bc2 == &f2c2
    assert MOVE_OFFSET + $9bd5 == &f2d5
    assert MOVE_OFFSET + $9bda == &f2da
    assert MOVE_OFFSET + $9bf0 == &f2f0
    assert MOVE_OFFSET + $9bf3 == &f2f3
    assert MOVE_OFFSET + $9c05 == &f305
    assert MOVE_OFFSET + $9c15 == &f315
    assert MOVE_OFFSET + $9c19 == &f319
    assert MOVE_OFFSET + $9c21 == &f321
    assert MOVE_OFFSET + $9c2f == &f32f
    assert MOVE_OFFSET + $9c34 == &f334
    assert MOVE_OFFSET + $9c52 == &f352
    assert MOVE_OFFSET + $9ccb == &f3cb
    assert MOVE_OFFSET + $9ceb == &f3eb
    assert MOVE_OFFSET + $9cf4 == &f3f4
    assert MOVE_OFFSET + $9cf7 == &f3f7
    assert MOVE_OFFSET + $9d0f == &f40f
    assert MOVE_OFFSET + $9d12 == &f412
    assert MOVE_OFFSET + $9d1f == &f41f
    assert MOVE_OFFSET + $9d31 == &f431
    assert MOVE_OFFSET + $9d74 == &f474
    assert MOVE_OFFSET + $9d81 == &f481
    assert MOVE_OFFSET + $9da4 == &f4a4
    assert MOVE_OFFSET + $9da9 == &f4a9
    assert MOVE_OFFSET + $9dac == &f4ac
    assert MOVE_OFFSET + $9db1 == &f4b1
    assert MOVE_OFFSET + $9dc3 == &f4c3
    assert MOVE_OFFSET + $9dc8 == &f4c8
    assert MOVE_OFFSET + $9e81 == &f581
    assert MOVE_OFFSET + $9eaa == &f5aa
    assert MOVE_OFFSET + $9ed2 == &f5d2
    assert MOVE_OFFSET + $9ed9 == &f5d9
    assert MOVE_OFFSET + $9ee2 == &f5e2
    assert MOVE_OFFSET + $9eeb == &f5eb
    assert MOVE_OFFSET + $9efd == &f5fd
    assert MOVE_OFFSET + $9f1b == &f61b
    assert MOVE_OFFSET + $9f3b == &f63b
    assert MOVE_OFFSET + $9f42 == &f642
    assert MOVE_OFFSET + $9f66 == &f666
    assert MOVE_OFFSET + $9f69 == &f669
    assert MOVE_OFFSET + $9f6e == &f66e
    assert MOVE_OFFSET + $9f73 == &f673
    assert MOVE_OFFSET + $9f82 == &f682
    assert MOVE_OFFSET + $9f91 == &f691
    assert MOVE_OFFSET + $9f94 == &f694
    assert MOVE_OFFSET + $9fa4 == &f6a4
    assert MOVE_OFFSET + $9fab == &f6ab
    assert MOVE_OFFSET + $9fb5 == &f6b5
    assert MOVE_OFFSET + $9fbf == &f6bf
    assert MOVE_OFFSET + $9fc2 == &f6c2
    assert MOVE_OFFSET + $9fc6 == &f6c6
    assert MOVE_OFFSET + $9fc9 == &f6c9
    assert MOVE_OFFSET + $9fd7 == &f6d7
    assert MOVE_OFFSET + $9fda == &f6da
    assert MOVE_OFFSET + $9fdd == &f6dd
    assert MOVE_OFFSET + $9fe0 == &f6e0
    assert MOVE_OFFSET + $9fe3 == &f6e3
    assert MOVE_OFFSET + $a004 == &f704
    assert MOVE_OFFSET + $a02d == &f72d
    assert NILOFF == &19
    assert NUMF == &04
    assert RAMBS == &7c
    assert RAMBS+1 == &7d
    assert RELBS == &7a
    assert RELBS+1 == &7b
    assert RELOC == &7c
    assert RELOC+1 == &7d
    assert ROMBS == &7a
    assert ROMBS+1 == &7b
    assert SUBOFF == &58
    assert SUBRF == &08
    assert TVS == &30
    assert TVS+1 == &31
    assert TVS+10 == &3a
    assert TVS-1 == &2f
    assert VALOFF == &0d
    assert WARMST == &2a
    assert WRMOFF == &72
    assert WSBOT == &02
    assert XTNDL == &74
    assert osbyte_acknowledge_escape == &7e
    assert osbyte_enter_language == &8e
    assert osbyte_inkey == &81
    assert osbyte_read_adc_or_get_buffer_status == &80
    assert osbyte_read_high_order_address == &82
    assert osbyte_read_himem == &84
    assert osbyte_read_himem_for_mode == &85
    assert osbyte_read_oshwm == &83
    assert osbyte_read_tube_presence == &ea
    assert osfile_load == &ff
    assert osfile_save == &00
    assert osfind_close == &00
    assert osfind_open_output == &80
    assert osword_read_clock == &01
    assert osword_read_interval_timer == &03
    assert osword_read_io_memory == &05
    assert osword_read_line == &00
    assert osword_read_pixel == &09
    assert osword_sound == &07
    assert osword_write_clock == &02
    assert service_star_help_command == &09
    assert service_unrecognised_star_command == &04

save pydis_start, pydis_end
