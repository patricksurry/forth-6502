; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

__VERSION__ = 1

    INCLUDE "word16.asm"

    seg.U vars

F byte
I byte
M byte
N byte

strbuf.d byte 32

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
;    word KEY.d
;    word EMIT.d
    word LIT.d
    word #7
    word LIT.d
    word #42
    word SWAP.d
    word DUP.d
    word WORD.d
    word NUMBER.d
    word DROP.d  ; drop the err count
    word PLUS.d
    word QUADRUPLE.d
    word QUIT.d

; ---------------------------------------------------------------------
; macros to define words with appropriate header

; flag defintions
F_IMMED = 0x80
F_HIDDEN = 0x20
F_LENMASK = 0x1f	; length mask

link set 0

  MAC _DEFWORD
    ; _DEFWORD name, label, flags
    ; define a forth word
.here:
{2},"_header.d"
    word link
link set .here
    byte [.end.d - .name.d] | {3}
{2},"_name.d":
.name.d:
    byte {1}
.end.d:
{2},".d"
  ENDM

  MAC DEFCODE ; name, label, flags
    ; define a native word
    _DEFWORD {1}, {2}, {3}
    word .code
.code
{2},"_code"
    ; native assembly should follow, ended by NEXT
  ENDM

  MAC DEFCONST ; name, label, flags, value
    ; define a constant
    DEFCODE {1}, {2}, {3}
    PUSHW SP, {4}
    NEXT
  ENDM

  MAC DEFVAR ; name, label, flags, value
    ; define a variable
    DEFCODE {1}, {2}, {3}
vptr set .vptr
    PUSHW SP, vptr
    NEXT
{2},"_value.d":
.vptr
    word {4}
  ENDM

  MAC DEFWORD
    ; define a forth word
    _DEFWORD {1}, {2}, {3}
    word DOCOL  ; Forth words use codeword DOCOL
    ; sequence of word pointers should follow, ended by EXIT
{2},"_words.d"
  ENDM

; ---------------------------------------------------------------------
; core constants

	DEFCONST "VERSION", VERSION, 0, __VERSION__     ; current version of this
;	DEFCONST "R0", R0, 0, 0		    ; The address of the top of the return stack.
	DEFCONST "DOCOL", _DOCOL, 0, DOCOL              ; Pointer to DOCOL.
	DEFCONST "F_IMMED", _F_IMMED, 0, F_IMMED		; Flag values
	DEFCONST "F_HIDDEN", _F_HIDDEN, 0, F_HIDDEN
    DEFCONST "F_LENMASK", _F_LENMASK, 0, F_LENMASK

; ---------------------------------------------------------------------
; core variables

    DEFVAR "STATE", STATE, 0, 0     ; executing = 0 or compiling != 0
    DEFVAR "LATEST", LATEST, 0, 0   ;TODO latest word in the dictionary
    DEFVAR "HERE", HERE, 0, 0       ;TODO next free byte of memory
    DEFVAR "BASE", BASE, 0, 10      ; radix for printing and reading numbers

; ---------------------------------------------------------------------
; native word definitions

    DEFCODE "_RTS", _RTS, 0
    rts

    DEFCODE "QUIT", QUIT, 0
    ; TODO fix me
    brk

    ; when we've finished a Forth word, we just recover the old PC and proceed
    DEFCODE "EXIT", EXIT, 0
    POPW RP, PC
    NEXT

    ; DROP :: x --
	DEFCODE "DROP",DROP,0
    SHRINK SP, 1
	NEXT

    ; SWAP :: x y -- y x
	DEFCODE "SWAP",SWAP,0
    PEEK SP, 0, AW
    DUPE SP, 1, 0
    POKE SP, 1, AW
	NEXT

    ; DUP :: x -- x x
    DEFCODE "DUP", DUP, 0
    GROW SP, 1
    DUPE SP, 1, 0
    NEXT

    ; OVER :: x y -- x y x
    DEFCODE "OVER", OVER, 0
    GROW SP, 1
    DUPE SP, 2, 0
    NEXT

    ; ROT :: x y z -- y z x
    DEFCODE "ROT", ROT, 0
    PEEK SP, 2, AW
    DUPE SP, 1, 2
    DUPE SP, 0, 1
    POKE SP, 0, AW
    NEXT

    ; -ROT :: x y z -- z x y
    DEFCODE "-ROT", NROT, 0
    PEEK SP, 0, AW
    DUPE SP, 1, 0
    DUPE SP, 2, 1
    POKE SP, 2, AW
    NEXT

    ; 2DROP :: x y --
	DEFCODE "2DROP",2DROP,0
    SHRINK SP, 2
	NEXT

    ; 2DUP :: x y -- x y x y
    DEFCODE "2DUP", 2DUP, 0
    GROW SP, 2
    DUPE SP, 3, 1
    DUPE SP, 2, 0
    NEXT

    ; 2SWAP :: x y z w -- z w x y
    DEFCODE "2SWAP", 2SWAP, 0
    PEEK SP, 0, AW
    PEEK SP, 1, BW
    DUPE SP, 2, 0
    DUPE SP, 3, 1
    POKE SP, 2, AW
    POKE SP, 3, BW
    NEXT

    ; ?DUP :: x -- 0 | x x
    DEFCODE "?DUP", QDUP, 0
    CMPIWC SP, #0
    bne .done
    GROW SP, 1
    DUPE SP, 1, 0
.done:
    NEXT

    ; + :: x y -- x+y
    DEFCODE "+", PLUS, 0
    POPW SP, AW
    ADDIWWIW SP, AW, SP
    NEXT

    ; - :: x y -- x-y
    DEFCODE "-", MINUS, 0
    POPW SP, AW
    SUBIWWIW SP, AW, SP
    NEXT

    ; * :: x y -- x*y
    DEFCODE "*", MULTIPLY, 0
    POPW SP, AW
;TODO
    ;MULIWWIW SP, AW, SP
    NEXT

    ; /MOD :: x y -- r q ; with q = x//y, r = remainder
    DEFCODE "/MOD", DIVMOD, 0
    POPW SP, AW
    POPW SP, BW
;TODO DIVIW ....
    DIVWWWW AW, BW, CW, DW ; CW is quotient
    PUSHW SP, DW
    PUSHW SP, CW
    NEXT


;TODO stack words
    ; SP@ - return SP
    ; S0 return SP-2 (next free location)

    ; push the next word as a constant and skip it
    DEFCODE "LIT", LIT, 0
    PUSHIW SP, PC   ; copy literal to stack
    INCPC           ; and skip it
    NEXT

    DEFCODE "KEY", KEY, 0
    GETC
    sta I
    PUSHB SP, I
    NEXT

    DEFCODE "EMIT", EMIT, 0
    POPB SP, I
    lda I
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
    sty N
    PUSHB SP, N
    NEXT

    ; parse a number from a string
    ; char* len -- value err  where err is # of unconverted chars
    DEFCODE "NUMBER", NUMBER, 0
    POPB SP, N      ; length
    POPW SP, AW     ; char*
    jsr parseint    ; (AW, y) => (BW, A)
    sta N
    PUSHW SP, BW    ; parsed value
    PUSHB SP, N     ; number of unconverted chars (0 => success)
    NEXT

; ---------------------------------------------------------------------
; forth word definitions

    DEFWORD "DOUBLE", DOUBLE, 0
    word DUP.d
    word PLUS.d
    word EXIT.d

    DEFWORD "QUADRUPLE", QUADRUPLE, 0
    word DOUBLE.d
    word DOUBLE.d
    word EXIT.d


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
    byte "BFOX"     ; binary / 'forth' / octal / hex
.radixmax.d:
    byte 2, 4, 8, 16
    ; coincidentally(!?) if we look at the five lowest bits
    ; the radix character's leading 1-bit corresponds to the base
    ; B = 00010 => 2, [F = 000110 => 3], O = 01111 => 4, X = 11000 => 5

parseint:
    ; parseint :: AX, N => BX, N ## X, Y; F, I, M
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
    ; N contains length
    ; on return A=0 indicates success, with parsed value in BW
    ; on failure A != 0 as # unconverted chars (-1 if fail by exhausting string)
    SETWC BW, #0    ; initialize result
    ldy #0          ; character index
    cpy N
    beq .invalid
    sty F           ; sign=0
    lda BASE_value.d
    sta M           ; default radix
    lda (AW),y
    cmp #'-
    bne .nosign
    iny
    cpy N
    beq .invalid
    sty F           ; sign=1
    lda (AW),y
.nosign:
    cmp #'0         ; leading 0?
    bne .digits
    iny             ; look for radix
    cpy N
    beq .invalid
    lda (AW),y
    and #upcase_mask
    ldx #4
.radixloop:
    dex
    bmi .invalid
    cmp .radixlist.d,x
    bne .radixloop
    lda .radixmax.d,x
    sta M
    iny         ; grab initial digit
    cpy N
    beq .invalid
    lda (AW),y
    jmp .digits
.invalid:
    ; return # of unprocessed chars
    ; failed on the y-th char, so N-Y unprocessed
    ; if Y == N (string exhausted) return -1
    cpy N
    bne .notlast
    iny
.notlast:
    sty M
    lda N
    sec
    sbc M
    rts
.digits:
    ; check digit is valid 0 <= d < radix
    sec
    sbc #'0
    bmi .invalid
    cmp #10             ; 0-9 ?
    bmi .checkmax
    and #upcase_mask    ; A-... ?
    sbc #['A - '0 - 10]
.checkmax:
    cmp M
    bpl .invalid
    ; result := result * radix + digit
    sty I
    pha
    lda M
    MULWAW BW,BW
    pla
    ADDWAW BW,BW
    ldy I
    ; next digit
    iny
    cpy N
    beq .done
    lda (AW),y
    jmp .digits
.done:
    lda F
    beq .positive
    NEGWW BW, BW        ; negate value
.positive:
    lda #0              ; success
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

    LDA strint1.d
    STA N
    SETWC AW, strint1.d+1
    jsr parseint
    EXPECTWC BW, #1234, "parseint1"

    LDA strint2.d
    STA N
    SETWC AW, strint2.d+1
    jsr parseint
    EXPECTWC BW, #-456, "parseint2"

    LDA strint3.d
    STA N
    SETWC AW, strint3.d+1
    jsr parseint
    EXPECTWC BW, #42, "parseint3"

    LDA strint4.d
    STA N
    SETWC AW, strint4.d+1
    jsr parseint
    EXPECTWC BW, #-492, "parseint4"

    LDA strint5.d
    STA N
    SETWC AW, strint5.d+1
    jsr parseint
    EXPECTWC BW, #48813, "parseint5"

    SETWC SP, #$400

    SETWC test_word.d, NUMBER.d

    PUSHC SP, strint5.d+1
    PUSHB SP, strint5.d
    jsr test_word
    POPW SP, BW
    POPW SP, AW
    EXPECTWC AW, #48813, "ok_NUMBER_val"
    EXPECTWC BW, #0, "ok_NUMBER_err"

    PUSHC SP, banana.d
    PUSHC SP, 6
    jsr test_word
    POPW SP, BW
    POPW SP, AW
    EXPECTWC AW, #0, "bad_NUMBER_val"
    EXPECTWC BW, #6, "bad_NUMBER_err"

    brk

test_word:
    ; set test_word.d to word under test,
    ; set up stack appropriately, jsr here
    SETWC RP, #$600
    SETWC PC, #test_word.d
    NEXT

test_word.d:
    word
    word _RTS.d

banana.d:
    byte "banana"
strint1.d:
    byte 4, "1234"
strint2.d:
    byte 4, "-456"
strint3.d:
    byte 10, "0b00101010"
strint4.d:
    byte 6, "-0o754"
strint5.d:
    byte 6, "0xbead"

  ENDIF

