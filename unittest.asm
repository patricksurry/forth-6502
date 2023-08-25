; ---------------------------------------------------------------------
; Simple unit test framework
;
; assemble with -DTESTS to generate test code
;
; Result shows total failure count in first byte
; Each test result is displayed in 16 bytes with first byte 0=ok/1=fail
; followed by the test name:
;
; 2000: 01 00 66 61 69 6c 75 72 65 73 00 00 00 00 00 00  ....fail / total
; 2010: 01 00 46 41 49 4c 00 00 00 00 00 00 00 00 00 00  ..FAIL..........
; 2020: 00 00 53 45 54 49 57 43 00 00 00 00 00 00 00 00  ..SETIWC........
; 2030: 00 00 53 45 54 49 57 43 49 00 00 00 00 00 00 00  ..SETIWCI.......
; 2040: 00 00 44 45 43 57 00 00 00 00 00 00 00 00 00 00  ..DECW..........
; 2050: 00 00 49 4e 43 57 00 00 00 00 00 00 00 00 00 00  ..INCW..........
; 2060: 00 00 41 44 44 57 57 57 00 00 00 00 00 00 00 00  ..ADDWWW........
; 2070: 00 00 41 44 44 57 43 57 00 00 00 00 00 00 00 00  ..ADDWCW........

.ifndef __UNITTEST__
__UNITTEST__ = 1

    .setcpu "65C02"
    .feature c_comments

    .include "word16.asm"

    .zeropage

TST:     .res 2

test_report := $c000
        ; this will contain a 16 byte summary
        ; plus a 16 byte result for each test

test_index .set 1

    .macro _EXPECTPRE desc
    .local next, sptr, test_body
    .if test_index
.ident (.sprintf("test_%04X", test_report + test_index*16)):
    .endif
        ldx #0
next:
        lda sptr,x
        beq test_body
        sta test_report + test_index*16 + 2,x
        inx
        bne next   ; always (string must be <256)
sptr:   .byte desc
        .byte 0
test_body:
    .endmac

    ; test body should store result to _TW, with 0 = success, non-zero = failure

    .macro _EXPECTPOST
    .local ok
        ; store test result
        CPYWW TST, test_report + test_index*16
test_index .set test_index + 1
        EQUWC TST, 0
        beq ok
        ; test failed, increment overall count
        INCW test_report
ok:     INCW test_report+2
    .endmac

    .macro EXPECTWC actual, expected, label
        _EXPECTPRE label
        SUBWCW actual, expected, TST
        _EXPECTPOST
    .endmac

    .macro EXPECTIWC actual, expected, label
        _EXPECTPRE label
        SUBIWCW actual, expected, TST
        _EXPECTPOST
    .endmac

    .macro EXPECTAC expected, label
        sta TST
        lda #$0
        sta TST+1
        EXPECTWC TST, expected, label
    .endmac

    .macro _EXPECTSTR actual, expected, label, mode
    .local strdat, next, done
        _EXPECTPRE label
        stz TST
        stz TST+1
        ldy #$ff
        bne next
strdat:
        .byte expected, 0
next:   iny
        lda strdat,y
        beq done
    .if mode = 1
        cmp (actual),y
    .else
        cmp actual,y
    .endif
        beq next
        iny
        sty TST
done:
        _EXPECTPOST
    .endmac

    .macro EXPECTSTR actual, expected, label
        _EXPECTSTR actual, expected, label, 0
    .endmac

    .macro EXPECTISTR actual, expected, label
        _EXPECTSTR actual, expected, label, 0
    .endmac

    .segment "TEST"
test_main:
        cld
        ldx #$0f
@next:  lda @header,x
        sta test_report,x
        dex
        bpl @next
        bmi @end
@header:
        .byte 0,0,0,0,"fail / total"
@end:
        ; modules will write further tests to this segment

.endif
