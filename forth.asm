; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    INCLUDE "word16.asm"

    seg.U vars

CH byte
N byte

    seg code

  IFCONST TESTS
    align #$1000
  ELSE
    org $1000
  ENDIF

  MAC GETC
.wait:
    lda $f004
    beq .wait
    PUTC
  ENDM

  MAC PUTC
    sta $f001
  ENDM

  MAC INCPC
.INCPC
    ADDWCW PC, #2, PC
  ENDM

  MAC NEXT
.NEXT
    JMP _NEXT
  ENDM

DOCOL:
    ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
    ; AW contains the address of the codeword, so we just need to push the current PC
    ; which tells us what to do after we're finished this word,
    ; and continue executing words at AW+2
    PUSHW RP, PC
    ADDWCW AW, #2, PC
    ; fall through to NEXT
_NEXT:
    ; indirect threading: PC has the address of the next codeword's address
    ; the codeword contains the address of adapter/interpreter used to execute the word
    ; we jump to the target codeword's code after incrementing PC
    ; leaving AW containing the address of the codeword
    CPYIWW PC, AW       ; now AW contains the codeword's address
    INCPC               ; next codeword to execute
    ; we need to jump to the address stored at the address stored in AW
    CPYIWW AW, BW       ; now BW has the interpreter address
    JMP (BW)            ; run the word's interpreter code

MAIN:
    ; initial entry point
    SETWC SP, #$400
    SETWC RP, #$600
    SETWC PC, #cold_start.d
    NEXT

cold_start.d:
;    WORD KEY.d
;    WORD EMIT.d
    WORD WORD.d
    WORD NUMBER.d
    WORD DROP.d  ; drop the err count
    WORD LIT.d
    WORD #1
    WORD PLUS.d
    WORD QUADRUPLE.d
    WORD QUIT.d

; ---------------------------------------------------------------------
; macros to define words with appropriate header

; flag defintions
F_IMMED = 0x80
F_HIDDEN = 0x20
F_LENMASK = 0x1f	; length mask

link set 0

  MAC _DEFWORD
    ; DEFWORD name, label, flags, 0=native, 1=forth
    ; define a forth word
.here:
{2},"_header.d"
    WORD link
link set .here
    BYTE [.end.d - .name.d] | {3}
{2},"_name.d":
.name.d:
    BYTE {1}
.end.d:
{2},".d"
  ENDM

  MAC DEFCODE
    ; define a native word
    _DEFWORD {1}, {2}, {3}
    WORD .code
.code
{2},"_code"
    ; native assembly should follow, ended by NEXT
  ENDM

  MAC DEFWORD
    ; define a forth word
    _DEFWORD {1}, {2}, {3}
    WORD DOCOL  ; Forth words use codeword DOCOL
    ; sequence of word pointers should follow, ended by EXIT
{2},"_words.d"
  ENDM

; ---------------------------------------------------------------------
; native word definitions

    DEFCODE "QUIT", QUIT, 0
    ; TODO fix me
    brk

    ; when we've finished a Forth word, we just recover the old PC and proceed
    DEFCODE "EXIT", EXIT, 0
    POPW RP, PC
    NEXT

    ; drop top element of stack
	DEFCODE "DROP",DROP,0
    SHRINK SP, 1
	NEXT

	DEFCODE "SWAP",SWAP,0
    POPW SP, AW
    POPW SP, BW
    PUSHW SP, AW
    PUSHW SP, BW
	NEXT

    DEFCODE "DUP", DUP, 0
    POPW SP, AW
    PUSHW SP, AW
    PUSHW SP, AW
    NEXT

    ; push the next word as a constant and skip it
    DEFCODE "LIT", LIT, 0
    PUSHIW SP, PC   ; copy literal to stack
    INCPC           ; and skip it
    NEXT

    DEFCODE "KEY", KEY, 0
    GETC
    PUSHA SP
    NEXT

    DEFCODE "EMIT", EMIT, 0
    POPA SP
    PUTC
    NEXT

    DEFCODE "WORD", WORD, 0
    ; -- adr len
    PUSHC SP, #strbuf.d
    ldy #0
.skip
    GETC
    cmp #$5c    ; backslash
    bne .nocomment
.skipcomment
    GETC
    cmp #$0a    ; newline
    bne .skipcomment
.nocomment
    cmp #$21    ; space + 1
    bmi .skip
.store
    sta strbuf.d,y
    iny
    GETC
    cmp #$21
    bpl .store
    tya
    PUSHA SP
    NEXT

    DEFCODE "NUMBER", NUMBER, 0
    ; adr len -- value err  where err is # of unconverted chars
    jsr _number
    NEXT

    ;TODO this would be a lot more efficient
    ; as SP -= 4 ;ADDIW (SP), (SP)+2, (SP)
    DEFCODE "+", PLUS, 0
    POPW SP, AW
    POPW SP, BW
    ADDWWW AW, BW, CW
    PUSHW SP, CW
    NEXT

; ---------------------------------------------------------------------
; forth word definitions

    DEFWORD "DOUBLE", DOUBLE, 0
    WORD DUP.d
    WORD PLUS.d
    WORD EXIT.d

    DEFWORD "QUADRUPLE", QUADRUPLE, 0
    WORD DOUBLE.d
    WORD DOUBLE.d
    WORD EXIT.d

strbuf.d byte 32

; ---------------------------------------------------------------------
; helpers

    SUBROUTINE
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

    lda #$0
.loop:
    dey
    bpl .chr    ; while y >= 0
    rts
.chr:
    eor (AW),y  ; xor acc with next char
.rotl3:         ; rotate acc left 3 bits (despite 6502's usual roll-thru-carry semantics)
    ; the letters a-h show the current MSB -> LSB bits in the acc, followed by the carry flag
                ; abcd efgh  ?   (carry is initially unknown)
    asl         ; bcde fgh0  a   (the MSB 'a' -> carry)
    adc #$80    ; Bcde fgha  b   (adc trick puts 'a' -> LSB and b -> carry; leaves MSB as "not b")
    rol         ; cdef ghab  B   (rol cycles in 'b' and drops the unneeded B -> carry)
    asl         ; defg hab0  c   (discard B and gets c -> carry)
    adc #$0     ; defg habc  0   (another adc to set the LSB, and always leave 0 -> carry)
    ; (note for rotl4 we could instad repeat the adc #$80 / rol trick)
    bcc .loop   ; unconditional since final carry is always 0


    SUBROUTINE
upcase_mask = #%11011111    ; clears l/c bit

.radixlist.d:
    byte 0, "BFOX"     ; binary / 'forth' / octal / hex
.radixmax.d:
    byte 10, 2, 4, 8, 16
    ; coincidentally(!?) if we look at the five lowest bits
    ; the radix character's leading 1-bit corresponds to the base
    ; B = 00010 => 2, [F = 000110 => 3], O = 01111 => 4, X = 11000 => 5
_number:
    ; parse a string
    ; char* length -- value err
    POPW SP, BW ; length
    POPW SP, AW ; char*
    ldy BW
    sty N
    lda (AW),y
    sta CH
    lda #0
    sta (AW),y  ; temporarily zero-terminate the string
    jsr parseint ; (AW, y) => (BW, A)
    beq .ok
    ; set <CW to # of unconverted chars = N - K where K is # converted
    clc     ; force borrow since A = K+1
    sbc N   ; A = K+1 - N - 1 = K - N
    eor #$ff
    adc #1     ; A = N - K
.ok:
    SETWA CW
    lda CH  ; replace temp zero terminator
    ldy N
    sta (AW),y
    PUSHW SP, BW    ; parsed value
    PUSHW SP, CW    ; number of unconverted chars
    rts

parseint:
    ; parse a number from a zero-terminated string of up to 256 bytes like
    ;
    ;   123, -123, 0b0101010, -0o754, 0xbead
    ;
    ; specifically we look for these two patterns
    ;
    ;   [-][1-9][0-9]+
    ;   [-]0[bBoOxX][0-9a-fA-F]+
    ;
    ; AW points to zero-terminated string to parse
    ; on return A=0 indicates success, with parsed value in BW
    ; or A > 0 on failure as # converted chars + 1
    ldx #0      ; default radix index (decimal)
    ldy #0      ; character index
    SETWC BW, #0    ; initialize result
    lda (AW),y
    beq .invalid2
    cmp #'-
    php         ; stash status with Z => negative value
    bne .nosign
    iny
    lda (AW),y
    beq .invalid
.nosign:
    cmp #'0    ; leading 0?
    bne .digits
    iny         ; look for radix
    lda (AW),y
    beq .invalid
    and #upcase_mask
    ldx #5
.radixloop:
    dex
    beq .invalid
    cmp .radixlist.d,x
    bne .radixloop
    iny         ; grab initial digit
    lda (AW),y
    bne .digits
.invalid:
    plp     ; discard negative flag
.invalid2:
    iny     ; A = Y+1 != 0
    tya
    rts
.digits:
    ; check digit is valid 0 <= d < radix
    sec
    sbc #'0
    bmi .invalid
    cmp #10
    bmi .checkmax
    and #upcase_mask
    sbc #['A - '0 - 10]
.checkmax:
    cmp .radixmax.d,x
    bpl .invalid
    ; result <- result * radix + digit
    ; stash x=radix index, y=index, a=digit
    stx CW
    sty CW+1
    pha
    lda .radixmax.d,x
    MULWAW BW,BW
    pla
    ADDWAW BW,BW
    ldx CW
    ldy CW+1
    ; next digit
    iny
    lda (AW),y
    bne .digits
.done:
    plp
    bne .positive
    ; negate value
    NEGWW BW, BW
.positive:
    lda #0
    rts

.fill.d:


; ---------------------------------------------------------------------
; unit tests

  IFCONST TESTS
    align #$1000

test_forth:

    ; test hashxrl3
    lda #<banana.d
    sta AW
    lda #>banana.d
    sta AW+1
    ldy #$6
    jsr hashxrl3
    EXPECTAC #$33, "hashxrl3"

    SETWC AW, strint1.d
    jsr parseint
    EXPECTWC BW, #1234, "parseint1"

    SETWC AW, #strint2.d
    jsr parseint
    EXPECTWC BW, #-456, "parseint2"

    SETWC AW, #strint3.d
    jsr parseint
    EXPECTWC BW, #42, "parseint3"

    SETWC AW, #strint4.d
    jsr parseint
    EXPECTWC BW, #-492, "parseint4"

    SETWC AW, #strint5.d
    jsr parseint
    EXPECTWC BW, #48813, "parseint5"

    PUSHW SP, AW
    PUSHC SP, #6
    jsr _number
    POPW SP, BW
    POPW SP, AW
    EXPECTWC AW, #48813, "good_number_bw"
    EXPECTWC BW, #0, "good_number_cw"

    PUSHC SP, #banana.d
    PUSHC SP, #6
    jsr _number
    POPW SP, BW
    POPW SP, AW
    EXPECTWC AW, #0, "bad_number_bw"
    EXPECTWC BW, #6, "bad_number_cw"

    brk

banana.d:
    byte "banana"
strint1.d:
    byte "1234",0
strint2.d:
    byte "-456",0
strint3.d:
    byte "0b00101010",0
strint4.d:
    byte "-0o754",0
strint5.d:
    byte "0xbead",0

  ENDIF

