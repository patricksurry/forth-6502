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

test_index set #1

    seg code
    org $1000
test_report.d equ .
    dc.w 0  ; count of failures
    dc.b "failures"
    align 16
    ; individual tests results will be appended here


  MAC _EXPECTPRE
    ; _EXPECTPRE "test_label"
test_,test_index
    ldx #$ff
.next
    inx
    lda .label.d,x
    beq .run_test
    sta [test_report.d + test_index*16 + 2],x
    bne .next   ; always
.label.d:
    byte {1}
    byte 0
.run_test:
  ENDM

    ; test body should store result to _TW, with 0 = success, non-zero = failure

  MAC _EXPECTPOST
    ; store test result
    CPYWW _TW, [test_report.d + test_index*16]
    CMPWC _TW, #0
    beq .ok
    ; test failed, increment overall count
    INCW test_report.d
.ok:
test_index set test_index + 1
  ENDM

  MAC EXPECTWC
    ; EXPECTWC AW, #$1234, "test_label"
    _EXPECTPRE {3}
    SUBWCW {1}, {2}, _TW
    _EXPECTPOST
  ENDM

  MAC EXPECTIWC
    ; EXPECTIWC AW, #$1234, "test_label"
    _EXPECTPRE {3}
    SUBIWCW {1}, {2}, _TW
    _EXPECTPOST
  ENDM

  MAC EXPECTAC
    ; EXPECTAC #42, "test_label"
    _EXPECTPRE {2}
    sta _TW
    lda #$0
    sta _TW+1
    _EXPECTPOST
  ENDM

