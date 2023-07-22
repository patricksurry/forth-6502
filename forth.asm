    processor 6502

    MACRO INCWP
    ; INCWP sp
    clc
    lda {1}
    adc #2
    sta {1}
    bcc .done
    ldx #1
    inc {1},x
.done:
    ENDM

    MACRO DECWP
    ; DECWP sp
    sec
    lda {1}
    sbc #2
    bcc .done
    ldx #1
    dec {1},x
.done:
    ENDM

    MACRO PUSHW
    ; PUSHW sp srcp
    ; push word from scrp to stack and increment sp
    ldy #1
.next:
    lda {2},y
    sta ({1}),y
    dey
    bpl .next
    INCWP {1}
    ENDM

    MACRO POPW
    ; POPW sp
    ; decrement sp and pop word to ACC
    DECWP {1}
    ldy #1
.next:
    lda ({1}),y
    sta ACC,y
    dey
    bpl .next
    ENDM

; Zero page variables in an uninitialised segment
    seg.U variables
    org $80

PC word
PCI word
SP word
RSP word

    MACRO NEXT
    ; jump to the address which the address in PC points to
    ; while incrementing PC
    ldy #2
.next:
    dey
    lda (PC),y
    sta PCI,y       ; copy (PC) => PCI
    bne .next
    INCWP PC         ; inc PC
    JMP (PCI)       ; jmp (PCI)
    ENDM

    seg code
    org $2000

MAIN:
    lda #0
    sta SP
    lda #$10
    sta SP+1

    ldy #0
    ldx #3
    txa
    sta (SP),y
    inx
    txa
    iny
    sta (SP),y

    INCWP SP
    PUSHW SP, $1000

    brk

    SUBROUTINE
ONE:
    WORD ONE
    ldy #0
    lda #1
    sta (ACC),y
    lda #0
    iny
    sta (ACC),y
    PUSHW SP, ACC
    NEXT


    SUBROUTINE
DUP:
    POPW SP
    PUSHW SP, ACC
    PUSHW SP, ACC
    NEXT

