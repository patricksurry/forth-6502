; ---------------------------------------------------------------------
; Simple unit test framework
;
; assemble with -DTESTS to generate test code
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

test_report := $c000
        ; this will contain a 16 byte summary
        ; plus a 16 byte result for each test

test_index .set 1

    .macro _EXPECTPRE label
    .local next, lbl, test
.ident (.sprintf("test_%03d_%s", test_index, label)):
        ldx #$ff
next:
        inx
        lda lbl,x
        beq test
        sta test_report + test_index*16 + 2,x
        bne next   ; always
lbl:    .byte label
        .byte 0
test:
    .endmac

    ; test body should store result to _TW, with 0 = success, non-zero = failure

    .macro _EXPECTPOST
    .local ok
        ; store test result
        CPYWW _TW, test_report + test_index*16
test_index .set test_index + 1
        CMPWC _TW, 0
        beq ok
        ; test failed, increment overall count
        INCW test_report
ok:
    .endmac

    .macro EXPECTWC actual, expected, label
        _EXPECTPRE label
        SUBWCW actual, expected, _TW
        _EXPECTPOST
    .endmac

    .macro EXPECTIWC actual, expected, label
        _EXPECTPRE label
        SUBIWCW actual, expected, _TW
        _EXPECTPOST
    .endmac

    .macro EXPECTAC expected, label
        sta _TW
        lda #$0
        sta _TW+1
        EXPECTWC _TW, expected, label
    .endmac

    .macro EXPECTSTR actual, expected, label
    .local strdat, next, done
        _EXPECTPRE label
        lda #0
        sta _TW
        sta _TW+1
        ldy #$ff
        bne next
strdat:
        .byte expected, 0
next:  iny
        lda strdat,y
        beq done
        cmp (actual),y
        beq next
        iny
        sty _TW
done:
        _EXPECTPOST
    .endmac

    .ifdef TESTS

    .data
test_header:
        .byte 0, 0, "failures"
        .align 16

    .segment "TEST"
test_main:
        ldx #15
@copy:
        lda test_header,x
        sta test_report,x
        dex
        bpl @copy
        ; test suites will follow

    .endif