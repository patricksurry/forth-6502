; ---------------------------------------------------------------------
;
; define a bunch of macros to mimic simple 16 bit opcodes.
; included by forth.asm to pretend we're using a 16-bit CPU
;
; Each opcode indicates its arguments like W - .word, IW - indirect .word, C - constant
;
;   SET[W|IW]C - set .word to 16-bit constant
;   CPY[W|IW][W|IW] - copy [indirect] .word to [indirect] .word
;   CMP[W|IW][C|W|IW] - compare two .words
;   [INC|DEC]W, DECW - increment or decrement a .word (no indirect form)
;   [ADD|SUB][W|IW][C|W|IW][W|IW] - add or subtract two .words to form a third
;   PUSH[C|W|IW], POP[W|IW] - push or pop from a stack
;
; for standalone unit tests, assemble like
;
;   bin/dasm forth.asm -oword16.bin -lword16.lst -f3 -DTESTS
;
; ---------------------------------------------------------------------

    .setcpu "6502"
    .feature c_comments

; Define zero page "registers" in an uninitialised segment
    .zeropage

        .res $80

PC:     .res 2    ; program counter
SP:     .res 2    ; data stack pointer
RP:     .res 2    ; return stack pointer

AW:     .res 2    ; some public registers
BW:     .res 2
CW:     .res 2
DW:     .res 2
EW:     .res 2

_TW:    .res 2   ; internal registers used by macros

; ---------------------------------------------------------------------
; set SET

    .macro _SETWC target, value, mode
        ; set .word with given mode to a constant
        ; _SETWC AW, #$1234, 0=abs/1=ind
        lda #<(value)
    .if mode
        ldy #0
        sta (target),y
    .else
        sta target
    .endif
    .if ! (.const(value) && <(value) = >(value))
        lda #>(value)
    .endif
    .if mode
        iny
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro SETWC target, value
        _SETWC target, value, 0
    .endmac

    .macro SETIWC target, value
        _SETWC target, value, 1
    .endmac

    .macro _SETWA target, mode
        ; set .word with given mode to accumulator
        ; _SETWA AW, 0=abs/1=ind
        ; stomps A, and Y .if ind
    .if mode
        ldy #0
        sta (target),y
    .else
        sta target
    .endif
    lda #0
    .if mode
        iny
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro SETWA target
        _SETWA target, 0
    .endmac

    .macro SETIWA target
        _SETWA target, 1
    .endmac

; ---------------------------------------------------------------------
; copy CPY

    .macro _CPYWW source, target, source_mode, target_mode
        ; copy a .word from one address to another with given modes
        ; mode 0=abs/1=ind
    .if source_mode | target_mode
        ldy #0
    .endif
    .if source_mode
        lda (source),y
    .else
        lda source
    .endif
    .if target_mode
        sta (target),y
    .else
        sta target
    .endif
    .if source_mode | target_mode
        iny
    .endif
    .if source_mode
        lda (source),y
    .else
        lda source+1
    .endif
    .if target_mode
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro CPYWW source, target
        _CPYWW source, target, 0, 0
    .endmac

    .macro CPYWIW source, target
        _CPYWW source, target, 0, 1
    .endmac

    .macro CPYIWW source, target
        _CPYWW source, target, 1, 0
    .endmac

    .macro CPYIWIW source, target
        _CPYWW source, target, 1, 1
    .endmac

; ---------------------------------------------------------------------
; compare CMP

    .macro _CMPWC target, value, mode
        ; compare word to const; sets zero flag based on W == C
    .local done
    .if mode
        ldy #1
        lda (target),y
    .else
        lda target+1
    .endif
    .if >(value)
        cmp #>(value)
    .endif
        bne done
    .if mode
        dey
        lda (target),y
    .else
        lda target
    .endif
    .if <(value)
        cmp #<(value)
    .endif
done:
    .endmac

    .macro CMPWC source, value
        _CMPWC source, value, 0
    .endmac

    .macro CMPIWC source, value
        _CMPWC source, value, 1
    .endmac

    .macro _CMPWW left, right, left_mode, right_mode
        ; modes 0=abs/1=ind
    .local done
    .if left_mode | right_mode
        ldy #1
    .endif
    .if left_mode
        lda (left),y
    .else
        lda left+1
    .endif
    .if right_mode
        cmp (right),y
    .else
        cmp right+1
    .endif
        bne done
    .if left_mode | right_mode
        dey
    .endif
    .if left_mode
        lda (left),y
    .else
        lda left
    .endif
    .if right_mode
        cmp (right),y
    .else
        cmp right
    .endif
done:
    .endmac

    .macro CMPWW left, right
        _CMPWW left, right, 0, 0
    .endmac

    .macro CMPWIW left, right
        _CMPWW left, right, 0, 1
    .endmac

    .macro CMPIWW left, right
        _CMPWW left, right, 1, 0
    .endmac

    .macro CMPIWIW left, right
        _CMPWW left, right, 1, 1
    .endmac

; ---------------------------------------------------------------------
; AND/OR/XOR

; TODO

; ---------------------------------------------------------------------
; LSR/LSL/ROR/ROL

; TODO

; ---------------------------------------------------------------------
; increment / decrement INC/DEC - note no indirect version

    .macro INCW target
        ; increment a two-byte value
        ; INCW adr
    .local done
        inc target
        bne done
        inc target+1
done:
    .endmac

    .macro DECW target
    ; decrement a two-byte value
    .local done
        lda target
        bne done   ; dec high byte .if low is zero
        dec target+1
done:
        dec target
    .endmac

; ---------------------------------------------------------------------
; add/subtract ADD/SUB

    .macro _PMWCW op, source, value, target, source_mode, target_mode
        ; op 0=adc/1=sbc, mode 0=abs/1=ind
    .local done
    .if source_mode | target_mode
        ldy #0
    .endif
    .if op
        sec
    .else
        clc
    .endif
    .if source_mode
        lda (source),y
    .else
        lda source
    .endif
    .if op
        sbc #<(value)
    .else
        adc #<(value)
    .endif
    .if target_mode
        sta (target),y
    .else
        sta target
    .endif
    .if >(value) = 0 && .xmatch(source, target)
        ; adding single byte constant to self
        .if op
        bcs done
        dec source+1
        .else
        bcc done
        inc source+1
        .endif
done:
    .else
        .if source_mode | target_mode
        iny
        .endif
        .if source_mode
        lda (source),y
        .else
        lda source+1
        .endif
        .if op
        sbc #>(value)
        .else
        adc #>(value)
        .endif
        .if target_mode
        sta (target)),y
        .else
        sta target+1
        .endif
    .endif
    .endmac

    .macro ADDWCW source, value, target
        _PMWCW 0, source, value, target, 0, 0
    .endmac

    .macro ADDWCIW source, value, target
        _PMWCW 0, source, value, target, 0, 1
    .endmac

    .macro ADDIWCW source, value, target
        _PMWCW 0, source, value, target, 1, 0
    .endmac

    .macro ADDIWCIW source, value, target
        _PMWCW 0, source, value, target, 1, 1
    .endmac


    .macro SUBWCW source, value, target
        _PMWCW 1, source, value, target, 0, 0
    .endmac

    .macro SUBWCIW source, value, target
        _PMWCW 1, source, value, target, 0, 1
    .endmac

    .macro SUBIWCW source, value, target
        _PMWCW 1, source, value, target, 1, 0
    .endmac

    .macro SUBIWCIW source, value, target
        _PMWCW 1, source, value, target, 1, 1
    .endmac


    .macro _PMWWW op, left, right, target, left_mode, right_mode, target_mode
        ; op 0=adc/1=sbc, mode 0=abs/1=ind
    .if left_mode | right_mode | target_mode
        ldy #0
    .endif
    .if op
        sec
    .else
        clc
    .endif
    .if left_mode
        lda (left),y
    .else
        lda left
    .endif
    .if right_mode
        .if op
        sbc (right),y
        .else
        adc (right),y
        .endif
    .else
        .if op
        sbc right
        .else
        adc right
        .endif
    .endif

    .if target_mode
        sta (target),y
    .else
        sta target
    .endif
    .if left_mode | right_mode | target_mode
        iny
    .endif
    .if left_mode
        lda (left),y
    .else
        lda left+1
    .endif
    .if right_mode
        .if op
        sbc (right),y
        .else
        adc (right),y
        .endif
    .else
        .if op
        sbc right+1
        .else
        adc right+1
        .endif
    .endif

    .if target_mode
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro ADDWWW left, right, target
        _PMWWW 0, left, right, target, 0, 0, 0
    .endmac

    .macro ADDWWIW left, right, target
        _PMWWW 0, left, right, target, 0, 0, 1
    .endmac

    .macro ADDWIWW left, right, target
        _PMWWW 0, left, right, target, 0, 1, 0
    .endmac

    .macro ADDWIWIW left, right, target
        _PMWWW 0, left, right, target, 0, 1, 1
    .endmac

    .macro ADDIWWW left, right, target
        _PMWWW 0, left, right, target, 1, 0, 0
    .endmac

    .macro ADDIWWIW left, right, target
        _PMWWW 0, left, right, target, 1, 0, 1
    .endmac

    .macro ADDIWIWW left, right, target
        _PMWWW 0, left, right, target, 1, 1, 0
    .endmac

    .macro ADDIWIWIW left, right, target
        _PMWWW 0, left, right, target, 1, 1, 1
    .endmac


    .macro SUBWWW left, right, target
        _PMWWW 1, left, right, target, 0, 0, 0
    .endmac

    .macro SUBWWIW left, right, target
        _PMWWW 1, left, right, target, 0, 0, 1
    .endmac

    .macro SUBWIWW left, right, target
        _PMWWW 1, left, right, target, 0, 1, 0
    .endmac

    .macro SUBWIWIW left, right, target
        _PMWWW 1, left, right, target, 0, 1, 1
    .endmac

    .macro SUBIWWW left, right, target
        _PMWWW 1, left, right, target, 1, 0, 0
    .endmac

    .macro SUBIWWIW left, right, target
        _PMWWW 1, left, right, target, 1, 0, 1
    .endmac

    .macro SUBIWIWW left, right, target
        _PMWWW 1, left, right, target, 1, 1, 0
    .endmac

    .macro SUBIWIWIW left, right, target
        _PMWWW 1, left, right, target, 1, 1, 1
    .endmac


    .macro ADDWAW source, target
        ; target := source + A  ##  A
        .local nocarry
        clc
        adc source
        sta target
    .if ! .xmatch(source, target)
        lda source+1
        sta target+1
    .endif
        bcc nocarry
        inc target+1
nocarry:
    .endmac

; ---------------------------------------------------------------------
; MUL, DIV

;TODO finish me

    .macro MULWAW source, target
        ; target := source * target  ## X, Y
    .local next, loop
        tay
    .if .xmatch(source, target)
        CPYWW source, _TW
    .endif
        SETWC target, 0
        ldx #7
        tya
loop:
        ASLW target
        asl
        bcc next
        tay
    .if .xmatch(source, target)
        ADDWWW target,_TW,target
    .else
        ADDWWW target,source,target
    .endif
        tya
next:
        dex
        bpl loop
    .endmac

    .macro DIVWWWW num, denom, quo, rem
        ; num, denom -> quo, rem ## A, X, num
    .local loop, skip
        SETWC quo, 0
        SETWC rem, 0
        ldx #15
loop:
        ASLW num
        ROLW rem
        SUBWWW rem, denom, _TW
        bcc skip
        CPYWW _TW, rem
skip:
        ROLW quo
        dex
        bpl loop
    .endmac

; ---------------------------------------------------------------------
; LSR, ASL, ROL, ROR

    .macro ASLW target
        asl target
        rol target+1
    .endmac

    .macro ROLW target
        rol target
        rol target+1
    .endmac

    .macro LSRW target
        lsr target+1
        ror target
    .endmac

    .macro RORW target
        ror target+1
        ror target
    .endmac

; ---------------------------------------------------------------------
; NOT (logical not)

    .macro NOTWW source, target
        lda source
        eor #$ff
        sta target
        lda source+1
        eor #$ff
        sta target+1
    .endmac

    .macro NEGWW source, target
        NOTWW source, target
        INCW target
    .endmac


; ---------------------------------------------------------------------
; PUSH, POP
;
; currently implemented with stack growing downward,
; and SP pointing to the latest valid element

    .macro GROW stack, count
        ; GROW SP, {2} :: SP - 2*{2} => SP ## A
        lda #256-count*2
        dec stack+1         ; subtract 256 then add acc as unsigned
        ADDWAW stack, stack
    .endmac

    .macro SHRINK stack, count
        ; SHRINK SP, 1
        lda #count*2
        ADDWAW stack, stack
    .endmac

    .macro _PUSHW stack, source, mode
        ; mode 0=abs/1=ind/-1=imm
        GROW stack, 1
    .if mode = -1
        _SETWC stack, source, 1
    .else
        _CPYWW source, stack, mode, 1
    .endif
    .endmac

    .macro PUSHW stack, source
        _PUSHW stack, source, 0
    .endmac

    .macro PUSHIW stack, source
        _PUSHW stack, source, 1
    .endmac

    .macro PUSHC stack, value
        _PUSHW stack, value, -1
    .endmac


    .macro PUSHB stack, source
        GROW stack, 1
        lda source
        SETIWA stack
    .endmac

    .macro _POPW stack, target, mode
        ; 0=abs/1=ind
        _CPYWW stack, target, 1, mode
        SHRINK stack, 1
    .endmac

    .macro POPW stack, target
        _POPW stack, target, 0
    .endmac

    .macro POPIW stack, target
        _POPW stack, target, 1
    .endmac

    .macro POPB stack, target
        ; POPB SP, B :: (SP) => B ## Y
        ldy #0
        lda (stack),y
        sta target
        SHRINK stack, 1
    .endmac

    .macro PEEK stack, index, target
        ; PEEKW SP, i, AW :: (SP), i => AW ## Y
        ldy #index*2
        lda (stack),y
        sta target
        iny
        lda (stack),y
        sta target+1
    .endmac

    .macro POKE stack, index, source
        ; POKEW SP, i, AW :: AW => (SP), i ## Y
        ldy #index*2
        lda source
        sta (stack),y
        iny
        lda source+1
        sta (stack),y
    .endmac

    .macro DUPE stack, from_index, to_index
        ; DUPE SP, i, j :: (SP), i => (SP), j ## A, X, Y
        ldy #from_index*2+1
        lda (stack),y
        tax
        dey
        lda (stack),y

        ldy #to_index*2
        sta (stack),y
        txa
        iny
        sta (stack),y
    .endmac

.ifdef TESTS
        .INCLUDE "unittest.asm"

    .segment "TEST"

test_word16:
        SETWC SP, test_word16
        SETWC SP, $400
        EXPECTWC SP, $1234, "FAIL"  ; an expected failure

        SETIWC SP, $123
        EXPECTWC $400, $123, "SETIWC"
        EXPECTIWC SP, $123, "SETIWCI"

        DECW SP
        EXPECTWC SP, $3FF, "DECW"
        INCW SP
        INCW SP
        EXPECTWC SP, $401, "INCW"
        DECW SP

        SETWC AW, $1ff
        SETWC BW, $102
        ADDWWW AW, BW, CW
        EXPECTWC CW, $301, "ADDWWW"

        SUBWCW CW, $1ff, AW
        EXPECTWC AW, $102, "SUBWCW"

        NEGWW AW, BW
        ADDWWW AW, BW, CW
        EXPECTWC CW, 0, "NEGWW"

        GROW SP, 1
        EXPECTWC SP, ($400-2), "ADDWCW"
        SHRINK SP, 1

        SETWC AW, 234
        ASLW AW
        EXPECTWC AW, 468, "ASLW"

        SETWC AW, 123
        lda #35
        ADDWAW AW, BW
        EXPECTWC BW, 158, "ADDWAW"

        lda #35
        MULWAW AW, AW
        EXPECTWC AW, 4305, "MULWAW"

        SETWC AW, 123
        SETWC BW, 42
        DIVWWWW AW, BW, CW, DW
        EXPECTWC CW, 2, "DIVWWWW_Q"
        EXPECTWC DW, 39, "DIVWWWW_R"

        PUSHC SP, 3
        PUSHC SP, $104
        EXPECTWC SP, ($400-4), "PUSHC"
        POPW SP, AW
        EXPECTIWC SP, 3, "POP1"
        EXPECTWC AW, $104, "POP2"
        PUSHW SP, AW
        EXPECTWC SP, ($400-4), "PUSHW"
        POPW SP, AW
        EXPECTWC AW, $104, "POP3"
        POPW SP, BW

        EXPECTWC SP, $400, "PUSHPOP1"
        EXPECTWC AW, $104, "PUSHPOP2"
        EXPECTWC BW, 3, "PUSHPOP3"

        ; end of tests

.endif