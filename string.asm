    .setcpu "65C02"
    .feature c_comments

    .include "word16.asm"

    .macro STRN s, pad
        ; write s as <length word>, s[, padding]
        ; where padding aligns total length to multiple of pad
        .word .strlen(s)
        .byte s
    .ifnblank pad
    .if .strlen(s) .mod pad
        .res pad - (.strlen(s) .mod pad)
    .endif
    .endif
    .endmac

    .zeropage

FLG:    .res 1
IDX:    .res 1
ERR:    .res 1
LEN:    .res 1
TMP:    .res 1
        .res 1

FMTBUF: .res 32

    .code

; ---------------------------------------------------------------------
; simple hash

hashxrl3:
        ; compute a simple hash of up to 256 bytes
        ; set AW H/L to start address, Y as number of bytes; on return A contains hash
        ; hash is calculated by setting A = 0 and reducing A = (A ^ x) <o 3
        ; for each byte x in the reversed range, starting from the last byte
        ; where ^ means "exclusive or" and <o 3 means "circular rotate 3 bits left"

        ; python version, strhash('banana') => 51 == 0x33
        ;
        ; def rol(x):   # a circulate rotate not through carry
        ;     x <<= 1
        ;     return (x & 0xff) | (x >> 8)
        ;
        ; def hash(xs):
        ;     h = 0
        ;     for x in xs:
        ;         h = rol(rol(rol(h ^ x)))
        ;     return h
        ;
        ; def strhash(s):
        ;     return hash(map(ord, reversed(s)))

        lda #0
@loop:
        dey
        bpl @chr    ; while y >= 0
        rts
@chr:
        eor (AW),y  ; xor acc with next char
        ; rotate acc left 3 bits (despite 6502's usual roll-thru-carry semantics)
        ; the letters a-h show the current MSB -> LSB bits in the acc, followed by the carry flag
                    ; abcd efgh  ?   (carry is initially unknown)
        asl         ; bcde fgh0  a   (the MSB 'a' -> carry)
        adc #$80    ; Bcde fgha  b   (adc trick puts 'a' -> LSB and b -> carry; leaves MSB as "not b")
        rol         ; cdef ghab  B   (rol cycles in 'b' and drops the unneeded B -> carry)
        asl         ; defg hab0  c   (discard B and gets c -> carry)
        adc #0      ; defg habc  0   (another adc to set the LSB, and always leave 0 -> carry)
        ; (note for rotl4 we could instad repeat the adc #$80 / rol trick)
        bcc @loop   ; unconditional since final carry is always 0

; ---------------------------------------------------------------------
; parseint, fmtint

upcase_mask = %1101_1111    ; clears l/c bit

    .data

radixlist:
        ; lookup tables for radix prefixes
        ; coincidentally(!?) if we look at the five lowest bits
        ; the radix character's leading 1-bit corresponds to the base
        ; B = 00010 => 2, [F = 000110 => 3], O = 01111 => 4, X = 11000 => 5
        .byte "BFOX"     ; binary / 'forth' / octal / hex
radixmax:
        .byte 2, 4, 8, 16
digitlist:
        .byte "0123456789ABCDEF"

    .code

fmtint:
    .proc _fmtint
        ; fmtint(AW: number, A=radix), sets LEN, clobbers Y, AW
        sta TMP         ; radix
        SETWA CW        ; radix
        stz IDX
        bit AW+1
        bpl nosign
        lda #'-'        ; negative?
        sta FMTBUF
        inc IDX
        NEGWW AW, AW
nosign:
        ldx #4
        lda TMP
findpfx:
        dex
        bmi calclen
        cmp radixmax,x
        bne findpfx
        ; with known radix r, display 0r prefix
        ldy IDX
        lda #'0'
        sta FMTBUF,y
        iny
        lda radixlist,x
        ora #($ff ^ upcase_mask) ; use lower case 0x, 0b, 0o etc
        sta FMTBUF,y
        iny
        sty IDX
calclen:
        SUBWWW AW,CW,DW
        bit DW+1
        bmi setlen      ; num < radix^k ?
        inc IDX
        lda TMP
        MULWAW CW,CW    ; CW := CW * radix
        bcc calclen     ; no overflow, keep looking
setlen:
        lda IDX
        sta LEN
        inc LEN
        lda TMP
        SETWA DW        ; radix
next:
        DIVWWWW AW,DW, CW,EW    ; AW/DW => quo=CW, rem=EW
        ldx EW
        lda digitlist,x
        ldy IDX
        sta FMTBUF,y
        EQUWC CW, 0
        beq done
        CPYWW CW, AW
        dec IDX
        bpl next
done:   rts
    .endproc

;TODO forth allows prefix #, $, and % for base 10, 16, 2, e.g. #-123
parseint:
    .proc _parseint
        ; parseint :: AX, LEN => BX, ERR ## X, Y; FLG, IDX, TMP
        ; parse a number from a string with length N < 256
        ;
        ;   123, -123, 0b0101010, -0o754, 0xbead
        ;
        ; specifically we look for these two patterns
        ;
        ;   [-][1-9][0-9]+
        ;   [-]0[bBoOxX][0-9a-fA-F]+
        ;
        ; AW points to string
        ; LEN contains length
        ; on return ERR=0 indicates success, with parsed value in BW
        ; on failure ERR != 0 as # unconverted chars (-1 if fail by exhausting string)
        SETWC BW, 0     ; initialize result
        ldy #0          ; character index
        cpy LEN
        beq invalid
        sty FLG         ; sign=0
        sty ERR         ; err=0
;TODO shouldn't ref this here, pass as ACC?
        lda BASE_value  ; forth variable value
        sta TMP         ; default radix
        lda (AW),y
        cmp #'-'
        bne nosign
        iny
        cpy LEN
        beq invalid
        sty FLG         ; sign=1
        lda (AW),y
nosign:
        cmp #'0'        ; leading 0?
        bne digits
        iny             ; look for radix
        cpy LEN
        beq done        ; 0 or -0 are valid
        lda (AW),y
        and #upcase_mask
        ldx #4
radixloop:
        dex
        bmi invalid
        cmp radixlist,x
        bne radixloop
        lda radixmax,x
        sta TMP     ; radix
        iny         ; grab initial digit
        cpy LEN
        beq invalid
        lda (AW),y
        jmp digits
invalid:
        ; return # of unprocessed chars
        ; failed on the y-th char, so N-Y unprocessed
        ; if Y == N (string exhausted) return -1
        cpy LEN
        bne notlast
        iny
notlast:
        sty TMP
        lda LEN
        sec
        sbc TMP
        sta ERR
        rts
done:
        lda FLG
        beq positive
        NEGWW BW, BW        ; negate value
positive:
        rts
digits:
        ; check digit is valid 0 <= d < radix
        sec
        sbc #'0'
        bmi invalid
        cmp #10             ; 0-9 ?
        bmi checkmax
        and #upcase_mask    ; clear lower case flag
        cmp #('A' - '0')    ; between 9 and A?
        bmi invalid
        sbc #('A' - '0' - 10)   ; 'A' => 'A' - '0' - ('A' - '0' - 10) => 10
checkmax:
        cmp TMP
        bpl invalid
        ; result := result * radix + digit
        sty IDX
        pha         ; stash digit
        lda TMP     ; radix
        MULWAW BW,BW
        pla
        ADDWAW BW,BW    ; add digit
        ldy IDX
        ; next digit
        iny
        cpy LEN
        beq done
        lda (AW),y
        jmp digits
    .endproc


; ---------------------------------------------------------------------
; unit tests

.ifdef TESTS

    .include "unittest.asm"

    .segment "TEST"

        jmp test_string

banana:         .byte "banana"
strint0:        STRN "0"
strint1:        STRN "1234"
strint2:        STRN "-456"
strint3:        STRN "0b00101010"
strint4:        STRN "-0o754"
strint5:        STRN "0xbead"

test_string:
        lda #<banana
        sta AW
        lda #>banana
        sta AW+1
        ldy #$6
        jsr hashxrl3
        EXPECTAC $33, "hashxrl3"

        SETWC AW, 32767
        lda #10
        jsr fmtint
        EXPECTSTR FMTBUF, "32767", "fmtint MAXINT"
        lda LEN
        EXPECTAC 5, "fmtint LEN"

        SETWC AW, -32768
        lda #10
        jsr fmtint
        EXPECTSTR FMTBUF, "-32768", "fmtint MININT"

        SETWC AW, 23290
        lda #$10
        jsr fmtint
        EXPECTSTR FMTBUF, "0x5AFA", "fmtint hex"

        SETWC AW, -1234
        lda #$10
        jsr fmtint
        EXPECTSTR FMTBUF, "-0x4D2", "fmtint -hex"

        SETWC AW, 1234
        lda #2
        jsr fmtint
        EXPECTSTR FMTBUF, "0b10011010010", "fmtint bin"

        lda strint0
        sta LEN
        SETWC AW, strint0+2
        jsr parseint
        lda ERR
        EXPECTAC 0, "parseint 0"

        lda strint1
        sta LEN
        SETWC AW, strint1+2
        jsr parseint
        EXPECTWC BW, 1234, "parseint 1"

        lda strint2
        sta LEN
        SETWC AW, strint2+2
        jsr parseint
        EXPECTWC BW, -456, "parseint 2"

        lda strint3
        sta LEN
        SETWC AW, strint3+2
        jsr parseint
        EXPECTWC BW, 42, "parseint 3"

        lda strint4
        sta LEN
        SETWC AW, strint4+2
        jsr parseint
        EXPECTWC BW, -492, "parseint 4"

        lda strint5
        sta LEN
        SETWC AW, strint5+2
        jsr parseint
        EXPECTWC BW, 48813, "parseint 5"

.endif