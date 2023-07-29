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

    seg.U vars  ; Define zero page "registers" in an uninitialised segment
    org $80

PC word     ; program counter
SP word     ; data stack pointer
RP word     ; return stack pointer

AW word     ; some public registers
BW word
CW word

_TW word    ; internal registers used by macros

; hints for the disassembler
PCH = PC+1
SPH = SP+1
RPH = RP+1
AWH = AW+1
BWH = BW+1
CWH = CW+1
_TWH = _TW+1

; ---------------------------------------------------------------------
; set SET

  MAC _SETWC
    ; set word with given mode to a constant
    ; _SETWC AW, #$1234, 0=abs/1=ind
    lda #<{2}
  IF {3}
    ldy #0
    sta ({1}),y
  ELSE
    sta {1}
  EIF
  IF <{2} != >{2}
    lda #>{2}
  EIF
  IF {3}
    iny
    sta ({1}),y
  ELSE
    sta {1}+1
  EIF
  ENDM

  MAC SETWC
.SETWC:
    _SETWC {1}, {2}, 0
  ENDM
  MAC SETIWC
.SETIWC:
    _SETWC {1}, {2}, 1
  ENDM

  MAC _SETWA
    ; set word with given mode to accumulator
    ; _SETWC AW, 0=abs/1=ind
    ; stomps A, and Y if ind
  IF {2}
    ldy #0
    sta ({1}),y
  ELSE
    sta {1}
  EIF
  lda #0
  IF {2}
    iny
    sta ({1}),y
  ELSE
    sta {1}+1
  EIF
  ENDM

  MAC SETWA
.SETWA:
    _SETWA {1}, 0
  ENDM
  MAC SETIWA
.SETIWA:
    _SETWA {1}, 1
  ENDM

; ---------------------------------------------------------------------
; copy CPY

  MAC _CPYWW
    ; copy a word from one address to another with given modes
    ; _CPY__ AW, BW, 0=abs/1=ind, 0=abs/1=ind
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
.CPYWW
    _CPYWW {1}, {2}, 0, 0
  ENDM
  MAC CPYWIW
.CPYWIW
    _CPYWW {1}, {2}, 0, 1
  ENDM
  MAC CPYIWW
.CPYIWW
    _CPYWW {1}, {2}, 1, 0
  ENDM
  MAC CPYIWIW
.CPYIWIW
    _CPYWW {1}, {2}, 1, 1
  ENDM

; ---------------------------------------------------------------------
; compare CMP

  MAC _CMPWC
    ; _CMPWC AW, #$1234, 0=abs/1=ind
    ; sets zero flag based on W == C
  IF {3}
    ldy #1
    lda ({1}),y
  ELSE
    lda {1}+1
  EIF
  IF >{2}
    cmp #>{2}
  EIF
    bne .done
  IF {3}
    dey
    lda ({1}),y
  ELSE
    lda {1}
  EIF
  IF <{2}
    cmp #<{2}
  EIF
.done:
  ENDM

  MAC CMPWC
.CMPWC
    _CMPWC {1}, {2}, 0
  ENDM
  MAC CMPIWC
.CMPIWC
    _CMPWC {1}, {2}, 1
  ENDM

  MAC _CMPWW
    ; _CMPWW AW, BW, 0=abs/1=ind, 0=abs/1=ind
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
.CMPWW
    _CMPWW {1}, {2}, 0, 0
  ENDM
  MAC CMPWIW
.CMPWIW
    _CMPWW {1}, {2}, 0, 1
  ENDM
  MAC CMPIWW
.CMPIWW
    _CMPWW {1}, {2}, 1, 0
  ENDM
  MAC CMPIWIW
.CMPIWIW
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
.INCW
    ; increment a two-byte value
    ; INCW adr
    inc {1}
    bne .done
    inc {1}+1
.done:
  ENDM

  MAC DECW
.DECW
    ; decrement a two-byte value
    lda {1}
    bne .done   ; dec high byte if low is zero
    dec {1}+1
.done:
    dec {1}
  ENDM

; ---------------------------------------------------------------------
; add/subtract ADD/SUB

  MAC _PMWCW
    ; _PMWCW 0=adc/1=sbc, AW, #$1234, CW, 0=abs/1=ind, 0=abs/1=ind
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
    sbc #<{3}
  ELSE
    adc #<{3}
  EIF
  IF {6}
    sta ({4}),y
  ELSE
    sta {4}
  EIF
  IF >{3} == 0 && {2} == {4}
    ; adding single byte constant to self
    IF {0}
      bcs .done
      dec {2}+1
    ELSE
      bcc .done
      inc {2}+1
    EIF
.done
  ELSE
    IF {5} | {6}
      iny
    EIF
    IF {5}
      lda ({2}),y
    ELSE
      lda {2}+1
    EIF
    IF {0}
      sbc #>{3}
    ELSE
      adc #>{3}
    EIF
    IF {6}
      sta ({4}),y
    ELSE
      sta {4}+1
    EIF
  EIF
  ENDM

  MAC ADDWCW
.ADDWCW
    _PMWCW 0, {1}, {2}, {3}, 0, 0
  ENDM
  MAC ADDWCIW
.ADDWCIW
    _PMWCW 0, {1}, {2}, {3}, 0, 1
  ENDM
  MAC ADDIWCW
.ADDIWCW
    _PMWCW 0, {1}, {2}, {3}, 1, 0
  ENDM
  MAC ADDIWCIW
.ADDIWCIW
    _PMWCW 0, {1}, {2}, {3}, 1, 1
  ENDM

  MAC SUBWCW
.SUBWCW
    _PMWCW 1, {1}, {2}, {3}, 0, 0
  ENDM
  MAC SUBWCIW
.SUBWCIW
    _PMWCW 1, {1}, {2}, {3}, 0, 1
  ENDM
  MAC SUBIWCW
.SUBIWCW
    _PMWCW 1, {1}, {2}, {3}, 1, 0
  ENDM
  MAC SUBIWCIW
.SUBIWCIW
    _PMWCW 1, {1}, {2}, {3}, 1, 1
  ENDM

  MAC _PMWWW
    ; _PMWWW 0=adc/1=sbc, AW, BW, CW, 0=abs/1=ind, 0=abs/1=ind, 0=abs/1=ind
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
.ADDWWW
    _PMWWW 0, {1}, {2}, {3}, 0, 0, 0
  ENDM
  MAC ADDWWIW
.ADDWWIW
    _PMWWW 0, {1}, {2}, {3}, 0, 0, 1
  ENDM
  MAC ADDWIWW
.ADDWIWW
    _PMWWW 0, {1}, {2}, {3}, 0, 1, 0
  ENDM
  MAC ADDWIWIW
.ADDWIWIW
    _PMWWW 0, {1}, {2}, {3}, 0, 1, 1
  ENDM
  MAC ADDIWWW
.ADDIWWW
    _PMWWW 0, {1}, {2}, {3}, 1, 0, 0
  ENDM
  MAC ADDIWWIW
.ADDIWWIW
    _PMWWW 0, {1}, {2}, {3}, 1, 0, 1
  ENDM
  MAC ADDIWIWW
.ADDIWIWW
    _PMWWW 0, {1}, {2}, {3}, 1, 1, 0
  ENDM
  MAC ADDIWIWIW
.ADDIWIWIW
    _PMWWW 0, {1}, {2}, {3}, 1, 1, 1
  ENDM

  MAC SUBWWW
.SUBWWW
    _PMWWW 1, {1}, {2}, {3}, 0, 0, 0
  ENDM
  MAC SUBWWIW
.SUBWWIW
    _PMWWW 1, {1}, {2}, {3}, 0, 0, 1
  ENDM
  MAC SUBWIWW
.SUBWIWW
    _PMWWW 1, {1}, {2}, {3}, 0, 1, 0
  ENDM
  MAC SUBWIWIW
.SUBWIWIW
    _PMWWW 1, {1}, {2}, {3}, 0, 1, 1
  ENDM
  MAC SUBIWWW
.SUBIWWW
    _PMWWW 1, {1}, {2}, {3}, 1, 0, 0
  ENDM
  MAC SUBIWWIW
.SUBIWWIW
    _PMWWW 1, {1}, {2}, {3}, 1, 0, 1
  ENDM
  MAC SUBIWIWW
.SUBIWIWW
    _PMWWW 1, {1}, {2}, {3}, 1, 1, 0
  ENDM
  MAC SUBIWIWIW
.SUBIWIWIW
    _PMWWW 1, {1}, {2}, {3}, 1, 1, 1
  ENDM

  MAC ADDWAW
.ADDWAW
    clc
    adc {1}
    sta {2}
  IF {1} != {2}
    lda {1}+1
    sta {2}+1
  EIF
    bcc .nocarry
    inc {2}+1
.nocarry
  ENDM

  MAC MULWAW
.MULWAW
    tay
  IF {1} == {2}
    CPYWW {1}, _TW
  EIF
    SETWC {2}, #0
    ldx #7
    tya
.loop:
    ASLWW {2},{2}
    asl
    bcc .next
    tay
  IF {1} == {2}
    ADDWWW {2},_TW,{2}
  ELSE
    ADDWWW {2},{1},{2}
  EIF
    tya
.next:
    dex
    bpl .loop
  ENDM

; ---------------------------------------------------------------------
; LSR, ASL

  MAC _SHFTWW
    ; _SHFTWW {1}, {2}, -1=left,1=right
  IF {1} != {2}
    CPYWW {1}, {2}
  EIF
  IF {3} < 0
    asl {2}
    rol {2}+1
  ELSE
    lsr {2}+1
    ror {2}
  EIF
  ENDM

  MAC ASLWW
.ASLWW
    _SHFTWW {1}, {2}, -1
  ENDM

  MAC LSRWW
.LSRWW
    _SHFTWW {1}, {2}, 1
  ENDM

;TODO ROL/ROR

; ---------------------------------------------------------------------
; NOT (logical not)

  MAC NOTWW
.NOTWW
    ; NOTWW AW, BW
    lda {1}
    eor #$ff
    sta {2}
    lda {1}+1
    eor #$ff
    sta {2}+1
  ENDM

  MAC NEGWW
.NEGWW
    ; NEGWW AW, BW
    NOTWW {1}, {2}
    ADDWCW {2}, #1, {2}
  ENDM

; ---------------------------------------------------------------------
; PUSH, POP
;
; currently implemented with stack growing downward,
; and SP pointing to the latest valid element

  MAC GROW
.GROW
    ; GROW SP, 1
    lda #-[{2} * 2]
    dec {1}+1         ; subtract 256 then add acc as unsigned
    ADDWAW {1}, {1}
  ENDM

  MAC SHRINK
.SHRINK
    ; SHRINK SP, 1
    lda #[{2} * 2]
    ADDWAW {1}, {1}
  ENDM

  MAC _PUSHW
    ; _PUSHW SP, AW, 0=abs/1=ind/-1=imm
    GROW {1}, 1
  IF {3} == -1
    _SETWC {1}, {2}, 1
  ELSE
    _CPYWW {2}, {1}, {3}, 1
  EIF
  ENDM

  MAC PUSHW
.PUSHW
    _PUSHW {1}, {2}, 0
  ENDM
  MAC PUSHIW
.PUSHIW
    _PUSHW {1}, {2}, 1
  ENDM
  MAC PUSHC
.PUSHC
    _PUSHW {1}, {2}, -1
  ENDM

  MAC PUSHA
.PUSHA
    tax
    GROW {1}, 1
    txa
    SETIWA {1}
  ENDM

  MAC _POPW
    ; _POPW SP, AW, 0=abs/1=ind
    _CPYWW {1}, {2}, 1, {3}
    SHRINK {1}, 1
  ENDM

  MAC POPW
.POPW
    _POPW {1}, {2}, 0
  ENDM
  MAC POPIW
.POPIW
    _POPW {1}, {2}, 1
  ENDM

  MAC POPA
    ; pop stack item with low byte => A
    ; stomps Y
.POPA
    ldy #0
    lda ({1}),y
    SHRINK {1}, 1
  ENDM

  IFCONST TESTS
    INCLUDE "unittest.asm"

    align #$1000
    SUBROUTINE
test_word16:
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

    SETWC AW, #$1ff
    SETWC BW, #$102
    ADDWWW AW, BW, CW
    EXPECTWC CW, #$301, "ADDWWW"

    SUBWCW CW, #$1ff, AW
    EXPECTWC AW, #$102, "SUBWCW"

    NEGWW AW, BW
    ADDWWW AW, BW, CW
    EXPECTWC CW, #0, "NEGWW"

    GROW SP, 1
    EXPECTWC SP, [$200-2], "ADDWCW"
    SHRINK SP, 1

    SETWC AW, #234
    ASLWW AW, BW
    EXPECTWC BW, #468, "ASLWW"

    SETwC AW, #123
    lda #35
    ADDWAW AW, BW
    EXPECTWC BW, #158, "ADDWAW"

    lda #35
    MULWAW AW, AW
    EXPECTWC AW, #4305, "MULWAW"

    PUSHC SP, 3
    PUSHC SP, $104
    EXPECTWC SP, [$200-4], "PUSHC"
    POPW SP, AW
    EXPECTIWC SP, 3, "POP1"
    EXPECTWC AW, $104, "POP2"
    PUSHW SP, AW
    EXPECTWC SP, [$200-4], "PUSHW"
    POPW SP, AW
    EXPECTWC AW, $104, "POP3"
    POPW SP, BW

    EXPECTWC SP, $200, "PUSHPOP1"
    EXPECTWC AW, #$104, "PUSHPOP2"
    EXPECTWC BW, #3, "PUSHPOP3"

    ; end of tests
    brk
.fill.d:
  EIF