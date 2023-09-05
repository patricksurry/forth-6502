; ---------------------------------------------------------------------
;
; define a bunch of macros to mimic simple 16 bit opcodes.
; included by forth.asm to pretend we're using a 16-bit CPU
;
; Each opcode indicates its arguments like W - .word, IW - indirect .word, C - constant
;
;   SET[W|IW]C - set .word to 16-bit constant
;   CPY[W|IW][W|IW] - copy [indirect] .word to [indirect] .word
;   EQU[W|IW][C|W|IW] - check equality for two .words (sets Z flag)
;   SGN[w|IW] - sets Z, N, V flag
;
;   [INC|DEC]W, DECW - increment or decrement a .word (no indirect form)
;   [ADD|SUB][W|IW][C|W|IW][W|IW] - add or subtract two .words to form a third
;   PUSH[C|W|IW], POP[W|IW] - push or pop from a stack
;   SHRINK, GROW - adjust stack pointer
;
; for standalone unit tests, assemble like
;
;   bin/dasm forth.asm -oword16.bin -lword16.lst -f3 -DTESTS
;
; ---------------------------------------------------------------------

.ifndef __WORD16__
__WORD16__ = 1

    .setcpu "65C02"
    .feature c_comments

; Define zero page "registers" in an uninitialised segment
    .zeropage

PC:     .res 2    ; program counter
SP:     .res 2    ; data stack pointer
RP:     .res 2    ; return stack pointer

AW:     .res 2    ; some public registers (not used internally)
BW:     .res 2
CW:     .res 2
DW:     .res 2
EW:     .res 2

_TW:    .res 2   ; internal registers used by macros
_F:     .res 1
        .res 1

; status register flags
SR_N = %10000000
SR_V = %01000000
SR_D = %00001000
SR_I = %00000100
SR_Z = %00000010
SR_C = %00000001
SR_ZVN = SR_Z | SR_V | SR_N

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
    .if mode
        iny
        lda #0
        sta (target),y
    .else
        stz target+1
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

    unsafe .set 0
    .if .xmatch(source, target)
        .if source_mode = target_mode
            .error .sprintf ("CPY[I]W[I]W %s %s: self copy is a no-op", .string(source), .string(target))
        .elseif target_mode = 0
            ; unsafe to copy (AW) => AW
            unsafe .set 1
        .endif
    .endif
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
        .if unsafe
            pha
        .else
            sta target
        .endif
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
    .if unsafe
        pla
        sta target
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
; equality EQU

    .macro _EQUWC target, value, mode, hi2f
        ; compare word to const; sets zero flag based on W == C
    .local done
    .if mode
        ldy #1
        lda (target),y
    .else
        lda target+1
    .endif
    .ifnblank hi2f
        sta _F
    .endif
    .if >(value)        ; optimize for compare to 0
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

    .macro EQUWC source, value
        _EQUWC source, value, 0
    .endmac

    .macro EQUIWC source, value
        _EQUWC source, value, 1
    .endmac

    .macro _EQUWW left, right, left_mode, right_mode
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

    .macro EQUWW left, right
        _EQUWW left, right, 0, 0
    .endmac

    .macro EQUWIW left, right
        _EQUWW left, right, 0, 1
    .endmac

    .macro EQUIWW left, right
        _EQUWW left, right, 1, 0
    .endmac

    .macro EQUIWIW left, right
        _EQUWW left, right, 1, 1
    .endmac

; ---------------------------------------------------------------------
; SGN - set status bits for word, Z = zero?, V = bit 14, N = bit 15

    .macro _SGNW target, mode
    .local setflags
        _EQUWC target, 0, mode, 1   ; hi2f stores hi byte in _F
        beq setflags    ; Z=1 => _F = 0
        lda _F          ; hi byte, which might still be zero
        bne setflags
        inc _F          ; _F => 1 to clear Z flag
setflags:
        lda #$ff
        bit _F          ; N = bit7, V = bit6, Z = #$ff & _F, i.e. _F == 0
    .endmac

    .macro SGNW target
        _SGNW target, 0
    .endmac

    .macro SGNIW target
        _SGNW target, 1
    .endmac

; ---------------------------------------------------------------------
; increment / decrement INC/DEC - note no indirect addressing for INC/DEC

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
        bne done   ; dec high byte only if low is zero
        dec target+1
done:
        dec target
    .endmac

; ---------------------------------------------------------------------
; add/subtract ADD/SUB

    .macro _PMWCW op, source, value, target, source_mode, target_mode
        ; op adc/sbc, mode 0=abs/1=ind
    .local done
    unsafe .set 0
    .if .xmatch(source, target) && source_mode = 1 && target_mode = 0
        ; unsafe to read from (AW) while writing to AW
        unsafe .set 1
    .endif

    .if source_mode | target_mode
        ldy #0
    .endif
    .if .xmatch(op, sbc)
        sec
    .else
        clc
    .endif
    .if source_mode
        lda (source),y
    .else
        lda source
    .endif
    op #<(value)
    .if target_mode
        sta (target),y
    .else
        .if unsafe
            pha
        .else
            sta target
        .endif
    .endif
    .if .const(value) && >(value) = 0 && .xmatch(source, target) && (!source_mode) && (!target_mode)
        ; adc/sbc single byte constant to self in immediate mode
        .if .xmatch(op, sbc)
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
        op #>(value)
        .if target_mode
            sta (target),y
        .else
            sta target+1
            .if unsafe
                pla
                sta target
            .endif
        .endif
    .endif
    .endmac

    .macro ADDWCW source, value, target
        _PMWCW adc, source, value, target, 0, 0
    .endmac

    .macro ADDWCIW source, value, target
        _PMWCW adc, source, value, target, 0, 1
    .endmac

    .macro ADDIWCW source, value, target
        _PMWCW adc, source, value, target, 1, 0
    .endmac

    .macro ADDIWCIW source, value, target
        _PMWCW adc, source, value, target, 1, 1
    .endmac


    .macro SUBWCW source, value, target
        _PMWCW sbc, source, value, target, 0, 0
    .endmac

    .macro SUBWCIW source, value, target
        _PMWCW sbc, source, value, target, 0, 1
    .endmac

    .macro SUBIWCW source, value, target
        _PMWCW sbc, source, value, target, 1, 0
    .endmac

    .macro SUBIWCIW source, value, target
        _PMWCW sbc, source, value, target, 1, 1
    .endmac


    .macro _PMWWW op, left, right, target, left_mode, right_mode, target_mode
        ; op adc/sbc, mode 0=abs/1=ind
        ; stomps y; C=16bit carry

    .if .xmatch(left, right) && left_mode = right_mode
        .if .xmatch(op, sbc)
            .warning .sprintf ("SUB %s %s ... is redundant, use SETWC ..., 0 instead?", .string(left), .string(right))
        .else
            .warning .sprintf ("ADD %s %s ... is redundant, use ASL ... instead?", .string(left), .string(right))
        .endif
    .endif
    unsafe .set 0
    .if target_mode = 0
        ;TODO could generate safe code instead of error
        .if .xmatch(left, target) && left_mode = 1
            unsafe .set 1
            .error .sprintf ("ADD/SUB: (%s) ... %s: unsafe indirect source with direct write", .string(left), .string(target))
        .elseif .xmatch(right, target) && right_mode = 1
            unsafe .set 2
            .error .sprintf ("ADD/SUB: ... (%s) %s: unsafe indirect source with direct write", .string(right), .string(target))
        .endif
    .endif

    .if left_mode | right_mode | target_mode
        ldy #0
    .endif
    .if .xmatch(op, sbc)
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
        op (right),y
    .else
        op right
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
        op (right),y
    .else
        op right+1
    .endif

    .if target_mode
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro ADDWWW left, right, target
        _PMWWW adc, left, right, target, 0, 0, 0
    .endmac

    .macro ADDWWIW left, right, target
        _PMWWW adc, left, right, target, 0, 0, 1
    .endmac

    .macro ADDWIWW left, right, target
        _PMWWW adc, left, right, target, 0, 1, 0
    .endmac

    .macro ADDWIWIW left, right, target
        _PMWWW adc, left, right, target, 0, 1, 1
    .endmac

    .macro ADDIWWW left, right, target
        _PMWWW adc, left, right, target, 1, 0, 0
    .endmac

    .macro ADDIWWIW left, right, target
        _PMWWW adc, left, right, target, 1, 0, 1
    .endmac

    .macro ADDIWIWW left, right, target
        _PMWWW adc, left, right, target, 1, 1, 0
    .endmac

    .macro ADDIWIWIW left, right, target
        _PMWWW adc, left, right, target, 1, 1, 1
    .endmac


    .macro SUBWWW left, right, target
        _PMWWW sbc, left, right, target, 0, 0, 0
    .endmac

    .macro SUBWWIW left, right, target
        _PMWWW sbc, left, right, target, 0, 0, 1
    .endmac

    .macro SUBWIWW left, right, target
        _PMWWW sbc, left, right, target, 0, 1, 0
    .endmac

    .macro SUBWIWIW left, right, target
        _PMWWW sbc, left, right, target, 0, 1, 1
    .endmac

    .macro SUBIWWW left, right, target
        _PMWWW sbc, left, right, target, 1, 0, 0
    .endmac

    .macro SUBIWWIW left, right, target
        _PMWWW sbc, left, right, target, 1, 0, 1
    .endmac

    .macro SUBIWIWW left, right, target
        _PMWWW sbc, left, right, target, 1, 1, 0
    .endmac

    .macro SUBIWIWIW left, right, target
        _PMWWW sbc, left, right, target, 1, 1, 1
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

    .macro SUBWAW source, target
        ; target := source - A  ##  A
        .local noborrow
        clc
        sbc source
        eor #$ff            ; ~(a - s - 1) == s - a mod 256
        sta target
        ; a > s => no borrow, a <= s, borrow (carry clear)
    .if ! .xmatch(source, target)
        lda source+1
        sta target+1
    .endif
        bcc noborrow
        dec target+1
noborrow:
    .endmac

; ---------------------------------------------------------------------
; MUL, DIV

    .macro MULWWW left, right, target
    .local pre, loop, next, safe, done
    .if .xmatch(left, target) || .xmatch(right, target)
        .error .sprintf ("MUL: write to input is unsafe")
    .endif
        SETWC target, 0
        sta _F  ; overflow = 0
        ldx #15
pre:    ASLW right
        bcs loop
        dex
        bpl pre
        bmi done
loop:   ADDWWW target, left, target
        bcc next
        inc _F
next:   dex
        bmi done
        ASLW target
        bcc safe
        inc _F
safe:   ASLW right
        bcc next
        bra loop
done:   lda _F
        cmp #1
    .endmac

    .macro MULWAW source, target
        ; target := source * A  ## A, X, Y, _F
        ; unsigned multiply with C=1 indicating unsigned overflow
    .local loop, safe, next
        tay
    .if .xmatch(source, target)
        CPYWW source, _TW
    .endif
        SETWC target, 0
        sta _F     ; carry = 0
        ldx #7
        tya
loop:   ASLW target
        bcc safe
        inc _F
safe:   asl
        bcc next
        tay
    .if .xmatch(source, target)
        ADDWWW target,_TW,target
    .else
        ADDWWW target,source,target
    .endif
        tya
        bcc next
        inc _F     ; unsigned overflow
next:   dex
        bpl loop
        lda _F
        cmp #1      ; C=1 if _F >= 0
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
; NOT and NEG (logical not and two complement negation)

    .macro _NOTWW source, target, source_mode, target_mode
    .if source_mode | target_mode
        ldy #0
    .endif
    .if source_mode
        lda (source),y
    .else
        lda source
    .endif
        eor #$ff
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
        eor #$ff
    .if target_mode
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro NOTWW source, target
        _NOTWW source, target, 0, 0
    .endmac
    .macro NOTWIW source, target
        _NOTWW source, target, 0, 1
    .endmac
    .macro NOTIWW source, target
        _NOTWW source, target, 1, 0
    .endmac
    .macro NOTIWIW source, target
        _NOTWW source, target, 1, 1
    .endmac

    .macro _NEGWW source, target, source_mode, target_mode
        _NOTWW source, target, source_mode, target_mode
    .if target_mode
        ADDIWCIW target, 1, target
    .else
        INCW target
    .endif
    .endmac

    .macro NEGWW source, target
        _NEGWW source, target, 0, 0
    .endmac
    .macro NEGWIW source, target
        _NEGWW source, target, 0, 1
    .endmac
    .macro NEGIWW source, target
        _NEGWW source, target, 1, 0
    .endmac
    .macro NEGIWIW source, target
        _NEGWW source, target, 1, 1
    .endmac

; ---------------------------------------------------------------------
; logical and, or, xor

    .macro _LGCWW op, left, right, target, left_mode, right_mode, target_mode
    .if left_mode | right_mode | target_mode
        ldy #0
    .endif
    .if left_mode
        lda (left),y
    .else
        lda left
    .endif
    .if right_mode
        op (right),y
    .else
        op right
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
        op (right),y
    .else
        op right+1
    .endif
    .if target_mode
        sta (target),y
    .else
        sta target+1
    .endif
    .endmac

    .macro ANDWWW left, right, target
        _LGCWW and, left, right, target, 0, 0, 0
    .endmac
    .macro ORWWW left, right, target
        _LGCWW ora, left, right, target, 0, 0, 0
    .endmac
    .macro XORWWW left, right, target
        _LGCWW eor, left, right, target, 0, 0, 0
    .endmac

    .macro ANDWWIW left, right, target
        _LGCWW and, left, right, target, 0, 0, 1
    .endmac
    .macro ORWWIW left, right, target
        _LGCWW ora, left, right, target, 0, 0, 1
    .endmac
    .macro XORWWIW left, right, target
        _LGCWW eor, left, right, target, 0, 0, 1
    .endmac

    .macro ANDWIWW left, right, target
        _LGCWW and, left, right, target, 0, 1, 0
    .endmac
    .macro ORWIWW left, right, target
        _LGCWW ora, left, right, target, 0, 1, 0
    .endmac
    .macro XORWIWW left, right, target
        _LGCWW eor, left, right, target, 0, 1, 0
    .endmac

    .macro ANDWIWIW left, right, target
        _LGCWW and, left, right, target, 0, 1, 1
    .endmac
    .macro ORWIWIW left, right, target
        _LGCWW ora, left, right, target, 0, 1, 1
    .endmac
    .macro XORWIWIW left, right, target
        _LGCWW eor, left, right, target, 0, 1, 1
    .endmac

    .macro ANDIWWW left, right, target
        _LGCWW and, left, right, target, 1, 0, 0
    .endmac
    .macro ORIWWW left, right, target
        _LGCWW ora, left, right, target, 1, 0, 0
    .endmac
    .macro XORIWWW left, right, target
        _LGCWW eor, left, right, target, 1, 0, 0
    .endmac

    .macro ANDIWWIW left, right, target
        _LGCWW and, left, right, target, 1, 0, 1
    .endmac
    .macro ORIWWIW left, right, target
        _LGCWW ora, left, right, target, 1, 0, 1
    .endmac
    .macro XORIWWIW left, right, target
        _LGCWW eor, left, right, target, 1, 0, 1
    .endmac

    .macro ANDIWIWW left, right, target
        _LGCWW and, left, right, target, 1, 1, 0
    .endmac
    .macro ORIWIWW left, right, target
        _LGCWW ora, left, right, target, 1, 1, 0
    .endmac
    .macro XORIWIWW left, right, target
        _LGCWW eor, left, right, target, 1, 1, 0
    .endmac

    .macro ANDIWIWIW left, right, target
        _LGCWW and, left, right, target, 1, 1, 1
    .endmac
    .macro ORIWIWIW left, right, target
        _LGCWW ora, left, right, target, 1, 1, 1
    .endmac
    .macro XORIWIWIW left, right, target
        _LGCWW eor, left, right, target, 1, 1, 1
    .endmac

; ---------------------------------------------------------------------
; PUSH, POP
;
; currently implemented with stack growing downward,
; and SP pointing to the latest valid element

; 6168 core bytes

    .macro GROWA stack
        SUBWAW stack, stack
    .endmac

    .macro GROW stack, count
        ; GROW SP, {2} :: SP - 2*{2} => SP ## A
        .ifnblank count
            lda #(count*2)
        .else
            lda #2
        .endif
        GROWA stack
    .endmac

    .macro SHRINKA stack
        ADDWAW stack, stack
    .endmac

    .macro SHRINK stack, count
        ; SHRINK SP[, 1]
        .ifnblank count
            lda #(count*2)
        .else
            lda #2
        .endif
        SHRINKA stack
    .endmac

    .macro _PUSHW stack, source, mode
        ; mode 0=abs/1=ind/-1=imm
        GROW stack
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
        GROW stack
        lda source
        SETIWA stack
    .endmac

    .macro _POPW stack, target, mode
        ; 0=abs/1=ind
        _CPYWW stack, target, 1, mode
        SHRINK stack
    .endmac

    .macro POPW stack, target
        _POPW stack, target, 0
    .endmac

    .macro POPIW stack, target
        _POPW stack, target, 1
    .endmac

    .macro POPB stack, target
        ; POPB SP, B :: (SP) => B ## Y
        lda (stack)
        sta target
        SHRINK stack
    .endmac


    .ifdef TESTS
        .include "unittest.asm"

    .segment "TEST"

test_word16:
        SETWC SP, test_word16
        SETWC SP, $400
        EXPECTWC SP, $1234, "should fail"

        SETIWC SP, $123
        EXPECTWC $400, $123, "SETIWC"
        EXPECTIWC SP, $123, "SETIWCI"

        DECW SP
        EXPECTWC SP, $3FF, "DECW"
        INCW SP
        INCW SP
        EXPECTWC SP, $401, "INCW"
        DECW SP

        SETWC $200, $123
        SETWC AW, $200
        CPYIWW AW, AW
        EXPECTWC AW, $123, "CPYIWW"

        SETWC AW, $ffff
        EQUWC AW, $ffff
        php
        pla
        and #SR_Z
        EXPECTAC SR_Z, "EQUWC"

        SGNW AW
        php
        pla
        and #SR_ZVN
        EXPECTAC SR_V | SR_N, "SGNW -1"

        INCW AW
        SGNW AW
        php
        pla
        and #SR_ZVN
        EXPECTAC SR_Z, "SGNW 0"

        SETWC AW, $1ff
        SETWC BW, $102
        ADDWWW AW, BW, CW
        EXPECTWC CW, $301, "ADDWWW"

        SUBWCW CW, $1ff, AW
        EXPECTWC AW, $102, "SUBWCW"

        NEGWW AW, BW
        ADDWWW AW, BW, CW
        EXPECTWC CW, 0, "NEGWW"

        GROW SP
        EXPECTWC SP, ($400-2), "ADDWCW"
        SHRINK SP

        SETWC AW, 234
        ASLW AW
        EXPECTWC AW, 468, "ASLW"

        SETWC AW, %0101010101010101
        SETWC BW, %1010101010101010
        ANDWWW AW, BW, CW
        EXPECTWC CW, 0, "ANDWWW"

        XORWWW AW, BW, CW
        EXPECTWC CW, $10000-1, "XORWWW"

        SETWC AW, 123
        lda #35
        ADDWAW AW, BW
        EXPECTWC BW, 158, "ADDWAW"

        SETWC AW, 338
        lda #101
        SUBWAW AW, AW
        EXPECTWC AW, 237, "SUBWAW brw"

        SETWC AW, 123
        lda #35
        SUBWAW AW, BW
        EXPECTWC BW, 88, "SUBWAW"

        lda #35
        MULWAW AW, AW
        EXPECTWC AW, 4305, "MULWAW"

        SETWC AW, 345
        SETWC BW, 678
        MULWWW AW, BW, CW
        EXPECTWC CW, 233910, "MULWWW"

        SETWC AW, 123
        SETWC BW, 42
        DIVWWWW AW, BW, CW, DW
        EXPECTWC CW, 2, "DIVWWWW quo"
        EXPECTWC DW, 39, "DIVWWWW rem"

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
.endif