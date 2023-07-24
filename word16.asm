; ---------------------------------------------------------------------
;
; define a bunch of macros to mimic simple 16 bit opcodes.
; included by forth.asm to pretend we're using a 16-bit CPU
;
; Each opcode indicates its arguments like W - word, IW - indirect word, C - constant
;
;   SET[W|IW]C - set word to 16-bit constant
;   CPY[W|IW][W|IW] - copy [indirect] word to [indirect] word
;   CMP[W|IW][C|W|IW] - compare two words
;   [INC|DEC]W, DECW - increment or decrement a word (no indirect form)
;   [ADD|SUB][W|IW][C|W|IW][W|IW] - add or subtract two words to form a third
;   PUSH[C|W|IW], POP[W|IW] - push or pop from a stack
;
; for standalone unit tests, assemble like
;
;   bin/dasm forth.asm -oword16.bin -lword16.lst -f3 -DTESTS
;
; ---------------------------------------------------------------------

    processor 6502

    ; Define zero page "registers" in an uninitialised segment
    seg.U variables
    org $80

PC word     ; program counter
SP word     ; data stack pointer
RP word     ; return stack pointer

AX word     ; some temp registers
BX word
CX word

; ---------------------------------------------------------------------
; set SET

  MAC _SETWC
    ; set word to a constant with given mode
    ; _SETC AX, $1234, 0=abs/1=ind
    lda <{2}
  IF {3}
    ldy #0
    sta ({1}),y
  ELSE
    sta {1}
  EIF
    lda >{2}
  IF {3}
    iny
    sta ({1}),y
  ELSE
    sta {1}+1
  EIF
  ENDM

  MAC SETWC
    _SETWC {1}, {2}, 0
  ENDM
  MAC SETIWC
    _SETWC {1}, {2}, 1
  ENDM

; ---------------------------------------------------------------------
; copy CPY

  MAC _CPYWW
    ; copy a word from one address to another with given modes
    ; _CPY__ AX, BX, 0=abs/1=ind, 0=abs/1=ind
  IF {3} | {4}
    ldy #0
  EIF
  IF {3}
    lda ({1}),y
  ELSE
    lda {1}
  EIF
  IF {4}
    sta ({2}),y
  ELSE
    sta {2}
  EIF
  IF {3} | {4}
    iny
  EIF
  IF {3}
    lda ({1}),y
  ELSE
    lda {1}+1
  EIF
  IF {4}
    sta ({2}),y
  ELSE
    sta {2}+1
  EIF
  ENDM

  MAC CPYWW
    _CPYWW {1}, {2}, 0, 0
  ENDM
  MAC CPYWIW
    _CPYWW {1}, {2}, 0, 1
  ENDM
  MAC CPYIWW
    _CPYWW {1}, {2}, 1, 0
  ENDM
  MAC CPYIWIW
    _CPYWW {1}, {2}, 1, 1
  ENDM

; ---------------------------------------------------------------------
; compare CMP

  MAC _CMPWC
    ; _CMPWC AX, #$1234, 0=abs/1=ind
  IF {3}
    ldy #1
    lda ({1}),y
  ELSE
    lda {1}+1
  EIF
    cmp >{2}
    bne .done
  IF {3}
    dey
    lda ({1}),y
  ELSE
    lda {1}
  EIF
    cmp <{2}
.done:
  ENDM

  MAC CMPWC
    _CMPWC {1}, {2}, 0
  ENDM
  MAC CMPIWC
    _CMPWC {1}, {2}, 1
  ENDM

  MAC _CMPWW
    ; _CMPWW AX, BX, 0=abs/1=ind, 0=abs/1=ind
  IF {3} | {4}
    ldy #1
  EIF
  IF {3}
    lda ({1}),y
  ELSE
    lda {1}+1
  EIF
  IF {4}
    cmp ({2}),y
  ELSE
    cmp {2}+1
  EIF
    bne done
  IF {3} | {4}
    dey
  EIF
  IF {3}
    lda ({1}),y
  ELSE
    lda {1}
  EIF
  IF {4}
    cmp ({2}),y
  ELSE
    cmp {2}
  EIF
done:
  ENDM

  MAC CMPWW
    _CMPWW {1}, {2}, 0, 0
  ENDM
  MAC CMPWIW
    _CMPWW {1}, {2}, 0, 1
  ENDM
  MAC CMPIWW
    _CMPWW {1}, {2}, 1, 0
  ENDM
  MAC CMPIWIW
    _CMPWW {1}, {2}, 1, 1
  ENDM

; ---------------------------------------------------------------------
; AND/OR/XOR

; TODO

; ---------------------------------------------------------------------
; LSR/LSL/ROR/ROL

; TODO

; ---------------------------------------------------------------------
; increment / decrement INC/DEC - note no indirect version

  MAC INCW
    ; increment a two-byte value
    ; INCW adr
    inc {1}
    bne .done
    inc {1}+1
.done:
  ENDM

  MAC DECW
    ; decrement a two-byte value
    lda {1}
    bne .done
    dec {1}+1
.done:
    dec {1}
  ENDM

; ---------------------------------------------------------------------
; add/subtract ADD/SUB

  MAC _PMWCW
    ; _PMWCW 0=adc/1=sbc, AX, #$1234, CX, 0=abs/1=ind, 0=abs/1=ind
  IF {5} | {6}
    ldy #0
  EIF
  IF {0}
    sec
  ELSE
    clc
  EIF
  IF {5}
    lda ({2}),y
  ELSE
    lda {2}
  EIF
  IF {0}
    sbc <{3}
  ELSE
    adc <{3}
  EIF
  IF {6}
    sta ({4}),y
  ELSE
    sta {4}
  EIF
  IF {5} | {6}
    iny
  EIF
  IF {5}
    lda ({2}),y
  ELSE
    lda {2}+1
  EIF
  IF {0}
    sbc >{3}
  ELSE
    adc >{3}
  EIF
  IF {6}
    sta ({4}),y
  ELSE
    sta {4}+1
  EIF
  ENDM

  MAC ADDWCW
    _PMWCW 0, {1}, {2}, {3}, 0, 0
  ENDM
  MAC ADDWCIW
    _PMWCW 0, {1}, {2}, {3}, 0, 1
  ENDM
  MAC ADDIWCW
    _PMWCW 0, {1}, {2}, {3}, 1, 0
  ENDM
  MAC ADDIWCIW
    _PMWCW 0, {1}, {2}, {3}, 1, 1
  ENDM

  MAC SUBWCW
    _PMWCW 1, {1}, {2}, {3}, 0, 0
  ENDM
  MAC SUBWCIW
    _PMWCW 1, {1}, {2}, {3}, 0, 1
  ENDM
  MAC SUBIWCW
    _PMWCW 1, {1}, {2}, {3}, 1, 0
  ENDM
  MAC SUBIWCIW
    _PMWCW 1, {1}, {2}, {3}, 1, 1
  ENDM

  MAC _PMWWW
    ; _PMWWW 0=adc/1=sbc, AX, BX, CX, 0=abs/1=ind, 0=abs/1=ind, 0=abs/1=ind
  IF {5} | {6} | {7}
    ldy #0
  EIF
  IF {0}
    sec
  ELSE
    clc
  EIF
  IF {5}
    lda ({2}),y
  ELSE
    lda {2}
  EIF
  IF {6}
    IF {0}
      sbc ({3}),y
    ELSE
      adc ({3}),y
    EIF
  ELSE
    IF {0}
      sbc {3}
    ELSE
      adc {3}
    EIF
  EIF

  IF {7}
    sta ({4}),y
  ELSE
    sta {4}
  EIF
  IF {5} | {6} | {7}
    iny
  EIF
  IF {5}
    lda ({2}),y
  ELSE
    lda {2}+1
  EIF
  IF {6}
    IF {0}
      sbc ({3}),y
    ELSE
      adc ({3}),y
    EIF
  ELSE
    IF {0}
      sbc {3}+1
    ELSE
      adc {3}+1
    EIF
  EIF

  IF {7}
    sta ({4}),y
  ELSE
    sta {4}+1
  EIF
  ENDM

  MAC ADDWWW
    _PMWWW 0, {1}, {2}, {3}, 0, 0, 0
  ENDM
  MAC ADDWWIW
    _PMWWW 0, {1}, {2}, {3}, 0, 0, 1
  ENDM
  MAC ADDWIWW
    _PMWWW 0, {1}, {2}, {3}, 0, 1, 0
  ENDM
  MAC ADDWIWIW
    _PMWWW 0, {1}, {2}, {3}, 0, 1, 1
  ENDM
  MAC ADDIWWW
    _PMWWW 0, {1}, {2}, {3}, 1, 0, 0
  ENDM
  MAC ADDIWWIW
    _PMWWW 0, {1}, {2}, {3}, 1, 0, 1
  ENDM
  MAC ADDIWIWW
    _PMWWW 0, {1}, {2}, {3}, 1, 1, 0
  ENDM
  MAC ADDIWIWIW
    _PMWWW 0, {1}, {2}, {3}, 1, 1, 1
  ENDM

  MAC SUBWWW
    _PMWWW 1, {1}, {2}, {3}, 0, 0, 0
  ENDM
  MAC SUBWWIW
    _PMWWW 1, {1}, {2}, {3}, 0, 0, 1
  ENDM
  MAC SUBWIWW
    _PMWWW 1, {1}, {2}, {3}, 0, 1, 0
  ENDM
  MAC SUBWIWIW
    _PMWWW 1, {1}, {2}, {3}, 0, 1, 1
  ENDM
  MAC SUBIWWW
    _PMWWW 1, {1}, {2}, {3}, 1, 0, 0
  ENDM
  MAC SUBIWWIW
    _PMWWW 1, {1}, {2}, {3}, 1, 0, 1
  ENDM
  MAC SUBIWIWW
    _PMWWW 1, {1}, {2}, {3}, 1, 1, 0
  ENDM
  MAC SUBIWIWIW
    _PMWWW 1, {1}, {2}, {3}, 1, 1, 1
  ENDM

; ---------------------------------------------------------------------
; PUSH, POP

  MAC _PUSHW
    ; _PUSHW SP, AX, 0=abs/1=ind/-1=imm
  IF {3} == -1
    _SETWC {1}, {2}, 1
  ELSE
    _CPYWW {2}, {1}, {3}, 1
  EIF
    ADDWCW {1}, #2, {1}
  ENDM

  MAC PUSHW
    _PUSHW {1}, {2}, 0
  ENDM
  MAC PUSHIW
    _PUSHW {1}, {2}, 1
  ENDM
  MAC PUSHC
    _PUSHW {1}, {2}, -1
  ENDM

  MAC _POPW
    ; _POPW SP, AX, 0=abs/1=ind
    SUBWCW {1}, #2, {1}
    _CPYWW {1}, {2}, 1, {3}
  ENDM

  MAC POPW
    _POPW {1}, {2}, 0
  ENDM
  MAC POPIW
    _POPW {1}, {2}, 1
  ENDM

; ---------------------------------------------------------------------
; Unit tests
;
; assemble with -DTESTS to generate test code @ $1000
; with test outputs aligned at next $1000, e.g. $2000
;
; Result shows total failure count in first byte
; Each test result is displayed in 16 bytes with first byte 0=ok/1=fail
; followed by the test name:
;
; 2000: 01 00 66 61 69 6c 75 72 65 73 00 00 00 00 00 00  ..failures......
; 2010: 01 00 46 41 49 4c 00 00 00 00 00 00 00 00 00 00  ..FAIL..........
; 2020: 00 00 53 45 54 49 57 43 00 00 00 00 00 00 00 00  ..SETIWC........
; 2030: 00 00 53 45 54 49 57 43 49 00 00 00 00 00 00 00  ..SETIWCI.......
; 2040: 00 00 44 45 43 57 00 00 00 00 00 00 00 00 00 00  ..DECW..........
; 2050: 00 00 49 4e 43 57 00 00 00 00 00 00 00 00 00 00  ..INCW..........
; 2060: 00 00 41 44 44 57 57 57 00 00 00 00 00 00 00 00  ..ADDWWW........
; 2070: 00 00 41 44 44 57 43 57 00 00 00 00 00 00 00 00  ..ADDWCW........


  IFCONST TESTS

test_index set #1

  MAC _EXPECTWC
    ; _EXPECTWC AX, #$1234, "test name", 0=abs/1=ind
    clc
    bcc .test
    align 16
.result:
    dc.w 0
    dc.b {3}
    align 16
.test:
    _CMPWC {1}, {2}, {4}
    beq .ok
    ; mark this test as failed and increment overall count
    inc .result
    INCW output_base
.ok:
    ldx #$0f
.copy:
    lda .result,x
    sta [output_base+test_index*16],x
    dex
    bpl .copy
test_index set test_index + 1
  ENDM

  MAC EXPECTWC
    _EXPECTWC {1}, {2}, {3}, 0
  ENDM
  MAC EXPECTIWC
    _EXPECTWC {1}, {2}, {3}, 1
  ENDM

    ; define tests themselves
    seg code
    org $1000

    SETWC SP, #$200
    EXPECTWC SP, #$1234, "FAIL"  ; an expected failure

    SETIWC SP, #$123
    EXPECTWC $200, #$123, "SETIWC"
    EXPECTIWC SP, #$123, "SETIWCI"

    DECW SP
    EXPECTWC SP, #$1FF, "DECW"
    INCW SP
    INCW SP
    EXPECTWC SP, #$201, "INCW"
    DECW SP

    SETWC AX, #$1ff
    SETWC BX, #$102
    ADDWWW AX, BX, CX
    EXPECTWC CX, #$301, "ADDWWW"

    SUBWCW CX, #$1ff, BX
    EXPECTWC BX, #$102, "SUBWCW"

    ADDWCW SP, #2, SP
    EXPECTWC SP, #$202, "ADDWCW"

    PUSHC SP, #3
    PUSHC SP, #$104
    EXPECTWC SP, #$206, "PUSHC"
    POPW SP, AX
    EXPECTIWC SP, #$104, "POP1"
    EXPECTWC AX, #$104, "POP2"
    PUSHW SP, AX
    EXPECTWC SP, #$206, "PUSHW"
    POPW SP, AX
    EXPECTWC AX, #$104, "POP3"
    POPW SP, BX

    EXPECTWC SP, #$202, "PUSHPOP1"
    EXPECTWC AX, #$104, "PUSHPOP2"
    EXPECTWC BX, #3, "PUSHPOP3"

    ; end of tests
    brk

    ; start of test report
    align #$1000
output_base equ .
    dc.w 0  ; count of failures
    dc.b "failures"
    align 16
    ; individual tests results will be appended here

  EIF