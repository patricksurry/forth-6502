; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    .setcpu "6502"
    .feature c_comments
    .feature underline_in_numbers

__VERSION__ = 1

    .include "word16.asm"
    .zeropage

FLG:    .res 1
IDX:    .res 1
ERR:    .res 1
LEN:    .res 1
TMP:    .res 1

strbuf: .res 32

    .macro GETC
    .local wait
wait:
        lda $f004
        beq wait
        PUTC
    .endmac

    .macro PUTC
        sta $f001
    .endmac

    .macro INCPC
        ADDWCW PC, 2, PC
    .endmac

    .macro NEXT
        jmp _NEXT
    .endmac

    .code

DOCOL:
        ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
        ; AW contains the address of the codeword, so we just need to push the current PC
        ; which tells us what to do after we're finished this word,
        ; and continue executing words at AW+2
        PUSHW RP, PC
        ADDWCW AW, 2, PC
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

    .data

cold_start:
        ; .word KEY
        ; .word EMIT
        .word LIT
        .word 7
        .word LIT
        .word 42
        .word SWAP
        .word DUP
        .word WORD
        .word NUMBER
        .word DROP  ; drop the err count
        .word PLUS
        .word QUADRUPLE
        .word QUIT

    .code

main:
        ; initial entry point
        SETWC SP, $400
        SETWC RP, $600
        SETWC PC, cold_start
        NEXT

; ---------------------------------------------------------------------
; macros to define words with appropriate header

; flag defintions
F_IMMED     = %1000_0000
F_HIDDEN    = %0010_0000
F_LENMASK   = %0001_1111

link .set 0

    .macro _DEFWORD name, label, flags
        ; define a forth word
; symbol for linked list
.ident (.sprintf("_link_%04d", link+1)):
; sybmol for debugging
    .ifnblank label
.ident (.sprintf("%s_header", label)):
    .else
.ident (.sprintf("%s_header", name)):
    .endif
    .if link = 0
        .word 0
    .else
        ; point to previous word
        .word .ident (.sprintf("_link_%04d", link))
    .endif
link .set link + 1
    .ifnblank flags
        .byte .strlen(name) | flags
    .else
        .byte .strlen(name)
    .endif
        .byte name
; main entrypoint for word pointing at codeword
    .ifnblank label
.ident (label):
    .else
.ident (name):
    .endif
    .endmac

    .macro DEFCODE name, label, flags
    .local code
        ; define a native word
        _DEFWORD name, label, flags
        .word code
code:
        ; followed by native assembly, ended by NEXT
    .endmac

    .macro DEFWORD name, label, flags
        _DEFWORD name, label, flags
        .word DOCOL     ; Forth words use codeword DOCOL
        ; followed by list of word pointers, ended by EXIT
    .endmac

    .macro DEFCONST name, label, flags, value
        ; define a constant
        DEFCODE name, label, flags
        PUSHW SP, value
        NEXT
    .endmac

    .macro DEFVAR name, label, flags, value
    .local vptr
        ; define a variable
        DEFCODE name, label, flags
        PUSHW SP, vptr
        NEXT
vptr:
    .ifnblank label
.ident(.sprintf("%s_value", label)):
    .else
.ident(.sprintf("%s_value", name)):
    .endif
    .ifnblank value
        .word value
    .else
        .word 0
    .endif
    .endmac

; ---------------------------------------------------------------------
; core constants

        DEFCONST "VERSION",,,   __VERSION__ ; current version of this forth
        ; DEFCONST "R0", R0, 0, 0		    ; The address of the top of the return stack.
        DEFCONST "DOCOL", "_DOCOL",, DOCOL   ; Pointer to DOCOL.
        DEFCONST "F_IMMED", "_F_IMMED",, F_IMMED		; Flag values
        DEFCONST "F_HIDDEN", "_F_HIDDEN",,  F_HIDDEN
        DEFCONST "F_LENMASK", "_F_LENMASK",, F_LENMASK

; ---------------------------------------------------------------------
; core variables

        DEFVAR "STATE"          ; executing = 0 or compiling != 0
        DEFVAR "LATEST"         ;TODO latest word in the dictionary
        DEFVAR "HERE"           ;TODO next free byte of memory
        DEFVAR "BASE",,, 10     ; radix for printing and reading numbers

; ---------------------------------------------------------------------
; native word definitions

        DEFCODE "_RTS"
        rts

        DEFCODE "QUIT"
        ; TODO fix me
        brk

        ; when we've finished a Forth word, we just recover the old PC and proceed
        DEFCODE "EXIT"
        POPW RP, PC
        NEXT

        ; DROP :: x --
        DEFCODE "DROP"
        SHRINK SP, 1
        NEXT

        ; SWAP :: x y -- y x
        DEFCODE "SWAP"
        PEEK SP, 0, AW
        DUPE SP, 1, 0
        POKE SP, 1, AW
        NEXT

        ; DUP :: x -- x x
        DEFCODE "DUP"
        GROW SP, 1
        DUPE SP, 1, 0
        NEXT

        ; OVER :: x y -- x y x
        DEFCODE "OVER"
        GROW SP, 1
        DUPE SP, 2, 0
        NEXT

        ; ROT :: x y z -- y z x
        DEFCODE "ROT"
        PEEK SP, 2, AW
        DUPE SP, 1, 2
        DUPE SP, 0, 1
        POKE SP, 0, AW
        NEXT

        ; -ROT :: x y z -- z x y
        DEFCODE "-ROT", "NROT"
        PEEK SP, 0, AW
        DUPE SP, 1, 0
        DUPE SP, 2, 1
        POKE SP, 2, AW
        NEXT

        ; 2DROP :: x y --
        DEFCODE "2DROP", "_2DROP"
        SHRINK SP, 2
        NEXT

        ; 2DUP :: x y -- x y x y
        DEFCODE "2DUP", "_2DUP"
        GROW SP, 2
        DUPE SP, 3, 1
        DUPE SP, 2, 0
        NEXT

        ; 2SWAP :: x y z w -- z w x y
        DEFCODE "2SWAP", "_2SWAP"
        PEEK SP, 0, AW
        PEEK SP, 1, BW
        DUPE SP, 2, 0
        DUPE SP, 3, 1
        POKE SP, 2, AW
        POKE SP, 3, BW
        NEXT

        ; ?DUP :: x -- 0 | x x
        DEFCODE "?DUP", "QDUP"
        CMPIWC SP, 0
        bne qdup_done   ; local labels don't work across macros containing labels
        GROW SP, 1
        DUPE SP, 1, 0
qdup_done:
        NEXT

        ; + :: x y -- x+y
        DEFCODE "+", "PLUS"
        POPW SP, AW
        ADDIWWIW SP, AW, SP
        NEXT

        ; - :: x y -- x-y
        DEFCODE "-", "MINUS"
        POPW SP, AW
        SUBIWWIW SP, AW, SP
        NEXT

        ; * :: x y -- x*y
        DEFCODE "*", "MULTIPLY"
        POPW SP, AW
;TODO
        ;MULIWWIW SP, AW, SP
        NEXT

        ; /MOD :: num den -- rem quo
        DEFCODE "/MOD", "DIVMOD"
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
        DEFCODE "LIT"
        PUSHIW SP, PC   ; copy literal to stack
        INCPC           ; and skip it
        NEXT

        DEFCODE "KEY"
        GETC
        sta TMP
        PUSHB SP, TMP
        NEXT

        DEFCODE "EMIT"
        POPB SP, TMP
        lda TMP
        PUTC
        NEXT

        DEFCODE "WORD"
        ; -- adr len
    .proc _word
        PUSHC SP, strbuf
        ldy #0
skipspace:
        GETC
        cmp #$5c    ; backslash
        bne nocomment
skipcomment:
        GETC
        cmp #$0a    ; newline
        bne skipcomment
nocomment:
        cmp #$21    ; space + 1
        bmi skipspace
store:
        sta strbuf,y
        iny
        GETC
        cmp #$21
        bpl store
        sty LEN
        PUSHB SP, LEN
        NEXT
    .endproc

        ; parse a number from a string
        ; char* len -- value err  where err is # of unconverted chars
        DEFCODE "NUMBER"
        POPB SP, LEN
        POPW SP, AW     ; char*
        jsr parseint    ; (AW, y) => (BW, A)
        PUSHW SP, BW    ; parsed value
        PUSHB SP, ERR   ; number of unconverted chars (0 => success)
        NEXT

; ---------------------------------------------------------------------
; forth word definitions

        DEFWORD "DOUBLE"
        .word DUP
        .word PLUS
        .word EXIT

        DEFWORD "QUADRUPLE"
        .word DOUBLE
        .word DOUBLE
        .word EXIT

; ---------------------------------------------------------------------
; helpers

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
        ; fmtint :: AW, BW, A=radix => string *BW, LEN ## Y, AW
        sta TMP
        SETWA CW        ; radix
        ldy #0
        sty IDX
        bit AW+1
        bpl nosign
        lda #'-'        ; negative?
        sta (BW),y
        inc IDX
        NEGWW AW,AW
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
        sta (BW),y
        iny
        lda radixlist,x
        ora #($ff ^ upcase_mask) ; use lower case 0x, 0b, 0o etc
        sta (BW),y
        iny
        sty IDX
calclen:
        SUBWWW AW,CW,DW
        bit DW+1
        bmi setlen      ; num < radix^k ?
        inc IDX
        lda TMP
        MULWAW CW,CW    ; CW := CW * radix
        jmp calclen
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
        sta (BW),y
        CMPWC CW, 0
        beq done
        CPYWW CW, AW
        dec IDX
        bpl next
done:   rts
    .endproc

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
        beq invalid
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
digits:
        ; check digit is valid 0 <= d < radix
        sec
        sbc #'0'
        bmi invalid
        cmp #10             ; 0-9 ?
        bmi checkmax
        ;TODO need further check or : to @ behave like digits before A!
        and #upcase_mask    ; A-... ?
        sbc #('A' - '0' - 10)
checkmax:
        cmp TMP
        bpl invalid
        ; result := result * radix + digit
        sty IDX
        pha
        lda TMP     ; radix
        MULWAW BW,BW
        pla
        ADDWAW BW,BW
        ldy IDX
        ; next digit
        iny
        cpy LEN
        beq done
        lda (AW),y
        jmp digits
done:
        lda FLG
        beq positive
        NEGWW BW, BW        ; negate value
positive:
        rts
    .endproc

; ---------------------------------------------------------------------
; unit tests

.ifdef TESTS

    .segment "TEST"
        jmp test_forth

test_word:
        ; set test_word.d to word under test,
        ; set up stack appropriately, jsr here
        SETWC RP, $600
        SETWC PC, ::test_wordlist
        NEXT

test_wordlist:
        .word 0
        .word _RTS

banana:
        .byte "banana"
strint1:
        .byte 4, "1234"
strint2:
        .byte 4, "-456"
strint3:
        .byte 10, "0b00101010"
strint4:
        .byte 6, "-0o754"
strint5:
        .byte 6, "0xbead"

test_forth:

        lda #<banana
        sta AW
        lda #>banana
        sta AW+1
        ldy #$6
        jsr hashxrl3
        EXPECTAC $33, "hashxrl3"

        SETWC AW, 1234
        SETWC BW, strbuf
        lda #10
        jsr fmtint
        EXPECTSTR BW, "1234", "fmtint"

        SETWC AW, -1234
        lda #$10
        jsr fmtint
        EXPECTSTR BW, "-0x4D2", "fmtint_x"

        SETWC AW, 1234
        lda #2
        jsr fmtint
        EXPECTSTR BW, "0b10011010010", "fmtint_b"

        lda strint1
        sta LEN
        SETWC AW, strint1+1
        jsr parseint
        EXPECTWC BW, 1234, "parseint1"

        lda strint2
        sta LEN
        SETWC AW, strint2+1
        jsr parseint
        EXPECTWC BW, -456, "parseint2"

        lda strint3
        sta LEN
        SETWC AW, strint3+1
        jsr parseint
        EXPECTWC BW, 42, "parseint3"

        lda strint4
        sta LEN
        SETWC AW, strint4+1
        jsr parseint
        EXPECTWC BW, -492, "parseint4"

        lda strint5
        sta LEN
        SETWC AW, strint5+1
        jsr parseint
        EXPECTWC BW, 48813, "parseint5"

        SETWC SP, $400

        SETWC test_wordlist, NUMBER

        PUSHC SP, strint5+1
        PUSHB SP, strint5
        jsr test_word
        POPW SP, BW
        POPW SP, AW
        EXPECTWC AW, 48813, "ok_NUMBER_val"
        EXPECTWC BW, 0, "ok_NUMBER_err"

        PUSHC SP, banana
        PUSHC SP, 6
        jsr test_word
        POPW SP, BW
        POPW SP, AW
        EXPECTWC AW, 0, "bad_NUMBER_val"
        EXPECTWC BW, 6, "bad_NUMBER_err"

.endif

