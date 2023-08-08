; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    .setcpu "6502"
    .feature c_comments
    .feature underline_in_numbers

__FORTH_VERSION__ = 1

    .import __MAIN_LAST__
    .import __STACK_START__
    .import __STACK_SIZE__

__RETSTACK_START__ = __STACK_START__ - __STACK_SIZE__

    .include "word16.asm"

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

    .macro STRN s, pad
        .byte .strlen(s)
        .byte s
    .ifnblank pad
        .res (.strlen(s) + 1) .mod pad
    .endif
    .endmac

    .macro LSTRN s
        .word LITSTRING
        STRN s, 2
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

forth:
        ; initial entry point
        SETWC SP, __STACK_START__
        SETWC PC, cold_start
        NEXT

.include "string.asm"

    .data

cold_start:
        .word QUIT      ; start up interpreter

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
.ident (.sprintf("__link__%04d", link+1)):
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
        .word .ident (.sprintf("__link__%04d", link))
    .endif
link .set link + 1
    .ifnblank flags
        .byte .strlen(name) | flags
    .else
        .byte .strlen(name)
    .endif
        .byte name
;TODO could store strlen at end as well for backtracking?
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
        ; define word that fetches constant to stack
        DEFCODE name, label, flags
        PUSHW SP, value
        NEXT
    .endmac

    .macro DEFVAR name, label, flags, value
    .local vptr
        ; define word that returns value of a variable
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

    .code

; ---------------------------------------------------------------------
; core constants

        DEFCONST "VERSION",,, __FORTH_VERSION__ ; current version of this forth
        DEFCONST "R0",,, __RETSTACK_START__ ; top of return stack
        DEFCONST "DOCOL", "_DOCOL",, DOCOL   ; Pointer to DOCOL.
        DEFCONST "F_IMMED", "_F_IMMED",, F_IMMED		; Flag values
        DEFCONST "F_HIDDEN", "_F_HIDDEN",,  F_HIDDEN
        DEFCONST "F_LENMASK", "_F_LENMASK",, F_LENMASK

; ---------------------------------------------------------------------
; core variables

        DEFVAR "STATE"                          ; immediate = 0 / compile != 0
        DEFVAR "LATEST", , , LASTWORD_header    ; head of our linked word list
        DEFVAR "HERE", , , __MAIN_LAST__ + 1    ; next free byte of memory
        DEFVAR "BASE", , , 10   ; radix for printing and reading numbers

; ---------------------------------------------------------------------
; native word definitions

    .ifdef TESTS
    .include "unittest.asm"

        ; use as last word in a DEFWORD phrase to make a testable subroutine
        DEFCODE "_RTS"
        rts

        ; use as last word in a DEFWORD phrase to terminate, e.g. in cold_start test
        DEFCODE "_BRK"
        brk

    .endif

        ; when we've finished a Forth word, we just recover the old PC and proceed
        DEFCODE "EXIT"
        POPW RP, PC
        NEXT

        ; DROP :: x --
        DEFCODE "DROP"
        SHRINK SP
        NEXT

        ; SWAP :: x y -- y x
        DEFCODE "SWAP"
        PEEK SP, 0, AW
        DUPE SP, 1, 0
        POKE SP, 1, AW
        NEXT

        ; DUP :: x -- x x
        DEFCODE "DUP"
        GROW SP
        DUPE SP, 1, 0
        NEXT

        ; OVER :: x y -- x y x
        DEFCODE "OVER"
        GROW SP
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
    .proc _qdup
        EQUIWC SP, 0
        bne done
        GROW SP
        DUPE SP, 1, 0
done:
        NEXT
    .endproc

; ---------------------------------------------------------------------
; comparisons


        ; = :: x y -- 0 | 1
        DEFCODE "=", "EQU"
    .proc _equ
        POPW SP, AW
        ldx #0
        EQUIWW SP, AW
        bne result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; <> :: x y -- 0 | 1
        DEFCODE "<>", "NEQU"
    .proc _nequ
        POPW SP, AW
        ldx #0
        EQUIWW SP, AW
        beq result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; < :: x y -- 0 | 1
        DEFCODE "<", "LT"
    .proc _lt
        POPW SP, AW             ; y
        SUBIWWW SP, AW, AW      ; x < y <=> x - y < 0
        ldx #0
        SGNW AW
        bpl result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; > :: x y -- 0 | 1
        DEFCODE ">", "GT"
    .proc _gt
        POPW SP, AW             ; y
        SUBWIWW AW, SP, AW      ; x > y <=> y - x < 0
        ldx #0
        SGNW AW
        bpl result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; <= :: x y -- 0 | 1
        DEFCODE "<=", "LE"
    .proc _le
        POPW SP, AW             ; y
        SUBWIWW AW, SP, AW      ; x <= y <=> y - x >= 0
        ldx #0
        SGNW AW
        bmi result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; >= :: x y -- 0 | 1
        DEFCODE ">=", "GE"
    .proc _ge
        POPW SP, AW             ; y
        SUBIWWW SP, AW, AW      ; x >= y <=> x - y >= 0
        ldx #0
        SGNW AW
        bmi result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0= :: x -- 0 | 1
        DEFCODE "0=", "ZEQU"
    .proc _zequ
        ldx #0
        EQUIWC SP, 0
        bne result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0<> :: x -- 0 | 1
        DEFCODE "0<>", "ZNEQU"
    .proc _znequ
        ldx #0
        EQUIWC SP, 0
        beq result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0< :: x -- 0 | 1
        DEFCODE "0<", "ZLT"
    .proc _zlt
        ldx #0
        SGNIW SP
        bpl result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0> :: x -- 0 | 1
        DEFCODE "0>", "ZGT"
    .proc _zgt
        ldx #0
        SGNIW SP
        bpl result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0<= :: x -- 0 | 1
        DEFCODE "0<=", "ZLE"
    .proc _zle
        ldx #0
        SGNIW SP
        bmi result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

        ; 0>= :: x -- 0 | 1
        DEFCODE "0>=", "ZGE"
    .proc _zge
        ldx #0
        SGNIW SP
        bmi result
        inx
result: txa
        SETIWA SP
        NEXT
    .endproc

;TODO
        ; AND
        ; OR
        ; XOR
        ; INVERT (bitwise complement)

; ---------------------------------------------------------------------
; arithmetic

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
;TODO  this should be a signed multiply (xor sign flags and negate then reapply)
; set overflow if sign flag gets overwritten, carry if exceed 16bits
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

        ; 1+ :: num -- num
        DEFCODE "1+", "INC1"
        POPW SP, AW
        INCW AW
        PUSHW SP, AW
        NEXT

        ; 2+ :: num -- num
        DEFCODE "2+", "INC2"
        POPW SP, AW
        ADDWCW AW, 2, AW
        PUSHW SP, AW
        NEXT

        ; LIT :: -- x
        ; push the next word as a constant and skip it
        DEFCODE "LIT"
        PUSHIW SP, PC   ; copy literal to stack
        INCPC           ; and skip it
        NEXT

        ; LIT :: -- sptr len
        ; push the next word(s) as a length delim string the stack
        DEFCODE "LITSTRING"
        CPYWW PC, AW
        INCW AW
        PUSHW SP, AW
        ldy #0
        lda (PC),y
        sta LEN
        PUSHB SP, LEN
        clc
        lda LEN
        adc #2      ; add two bytes (length byte + padding) and
        and #$fe    ; ... clear last bit to get number of full words
        ADDWAW PC, PC   ; bump PC to word boundary past string
        NEXT

; ---------------------------------------------------------------------
; STACK

        ; DSP@ :: -- sp
        ; push the current stack pointer on the stack
        DEFCODE "DSP@", "DSPFETCH"
        PUSHW SP, SP
        NEXT

        ; DSP! :: sp --
        ; set the stack pointer from the top of the stack
        DEFCODE "DSP!", "DSPSTORE"
        POPW SP, SP
        NEXT

        ; RSP@ ::  -R- sp
        ; push the current return stack pointer on the stack
        DEFCODE "RSP@", "RSPFETCH"
        PUSHW RP, RP
        NEXT

        ; RSP! :: sp -R-
        ; set the return stack pointer from the top of the return stack
        DEFCODE "RSP!", "RSPSTORE"
        POPW RP, RP
        NEXT

        ; RDROP :: x -R-
        ; drop top of return stack
        DEFCODE "RDROP"
        SHRINK RP
        NEXT

        ; >R :: x --  ;  -R- x
        ; move top stack elt to return stack
        DEFCODE ">R", "TORS"
        POPW SP, AW
        PUSHW RP, AW
        NEXT

        ; R> :: -- x  ;  x -R-
        ; move top of return stack to stack
        DEFCODE "R>", "FROMRS"
        POPW RP, AW
        PUSHW SP, AW
        NEXT

; ---------------------------------------------------------------------
; Memory

        ; STORE :: x adr --
        ; store x @ adr
        DEFCODE "!", "STORE"
        POPW SP, AW
        POPW SP, BW
        CPYWIW BW, AW
        NEXT

        ; FETCH :: adr -- x
        DEFCODE "@", "FETCH"
        POPW SP, AW
        CPYIWW AW, BW
        PUSHW SP, BW
        NEXT

        ; ADDSTORE :: x adr --
        ; (adr) += x
        DEFCODE "+!", "ADDSTORE"
        POPW SP, AW
        POPW SP, BW
        ADDIWWIW AW, BW, AW
        NEXT

        ; SUBSTORE :: x adr --
        ; (adr) -= x
        DEFCODE "-!", "SUBSTORE"
        POPW SP, AW
        POPW SP, BW
        SUBIWWIW AW, BW, AW
        NEXT

        ; C! :: x adr --
        ; store x => adr
        DEFCODE "C!", "STOREBYTE"
        POPW SP, AW
        POPB SP, TMP
        ldy #0
        lda TMP
        sta (AW),y
        NEXT

        ; C@ :: adr -- x
        ; fetch adr => x
        DEFCODE "C@", "FETCHBYTE"
        POPW SP, AW
        ldy #0
        lda (AW),y
        sta TMP
        PUSHB SP, TMP
        NEXT

;TODO  C@C! (CCOPY), CMOVE

; ---------------------------------------------------------------------
; I/O
        ; KEY :: -- c
        DEFCODE "KEY"
        GETC
        sta TMP
        PUSHB SP, TMP
        NEXT

        ; EMIT :: c --
        DEFCODE "EMIT"
        POPB SP, TMP
        lda TMP
        PUTC
        NEXT

        ; TELL :: sptr len --
        DEFCODE "TELL"
    .proc _tell
        POPB SP, LEN
        POPW SP, AW
        ldy #0
        cpy LEN
loop:   beq done
        lda (AW),y
        PUTC
        iny
        bne loop
done:   NEXT
    .endproc

        ; WORD :: -- sptr len
        ; read a word from input
        DEFCODE "WORD"
    .proc _word
        PUSHC SP, STRBUF
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
        sta STRBUF,y
        iny
        GETC
        cmp #$21
        bpl store
        sty LEN
        PUSHB SP, LEN
        NEXT
    .endproc

        ; NUMBER :: sptr len -- value err
        ; parse a number from a string
        ; where err is 0 on success else # of unconverted chars
        DEFCODE "NUMBER"
        POPB SP, LEN
        POPW SP, AW     ; char*
        jsr parseint    ; (AW, y) => (BW, A)
        PUSHW SP, BW    ; parsed value
        PUSHB SP, ERR   ; number of unconverted chars (0 => success)
        NEXT

        ; FIND :: sptr len -- link | nul
        DEFCODE "FIND"
    .proc _find
        POPB SP, LEN
        POPW SP, AW
        CPYWW LATEST_value, BW
nextword:
        EQUWC BW, 0         ; BW is curr ptr in linked lst
        beq done
        ldy #2
        lda (BW),y
        and #(F_HIDDEN | F_LENMASK)  ; get length, let hidden flag fail match
        cmp LEN
        bne nomatch
        lda #3
        ADDWAW BW, CW       ; CW is start of name
        ldy #0
loop:                       ; compare name
        lda (CW),y
        cmp (AW),y
        bne nomatch
        iny
        cpy LEN
        bne loop
done:
        PUSHW SP, BW        ; found match
        NEXT
nomatch:
        CPYIWW BW, BW       ; check prev word
        jmp nextword
    .endproc

        ; >CFA :: link -- cptr
        DEFCODE ">CFA", "TCFA"
        POPW SP, AW
        ldy #2
        lda (AW),y
        and #F_LENMASK
        clc
        adc #3
        ADDWAW AW, AW
        PUSHW SP, AW
        NEXT

        ; >DFA :: link -- dptr
        DEFWORD ">DFA", "TDFA"
        .word TCFA
        .word INC2
        .word EXIT

        ; CREATE :: sptr len --
        ; create the header for new word defintion, updates HERE, LATEST
        DEFCODE "CREATE"
    .proc _create
        POPB SP, LEN
        POPW SP, AW
        ; write the link to point back to latest
        CPYWW HERE_value, BW
        CPYWIW LATEST_value, BW
        ; and update latest to point here
        CPYWW HERE_value, LATEST_value
        ; write the length byte followed by the string
        ldy #2
        lda LEN
        sta (BW),y
        lda #3
        ADDWAW BW,BW        ; here += 3
        ldy LEN
copy:
        lda (AW),y
        sta (BW),y
        dey
        bne copy
        lda LEN
        ADDWAW BW,BW        ; here += LEN
        CPYWW BW,HERE_value
        NEXT
    .endproc

        ; , :: ptr --
        ; append a codeword to HERE, updates HERE
        DEFCODE ",", "COMMA"
        POPW SP, AW
        CPYWW HERE_value, BW
        CPYWIW AW, BW
        ADDWCW HERE_value, 2, HERE_value
        NEXT

        ; [ :: --
        ; set STATE = 0 (immediate mode)
        DEFCODE "[", "LBRAC"
        lda #0
        sta STATE_value
        NEXT

        ; ] :: --
        ; set STATE = 1 (compile mode)
        DEFCODE "]", "RBRAC"
        lda #1
        sta STATE_value
        NEXT

        ; : :: --
        ; start definition of anew word
        DEFWORD ":", "COLON"
	    .word WORD		            ; Get the name of the new word
	    .word CREATE		        ; CREATE the dictionary entry / header
	    .word LIT, DOCOL, COMMA     ; Append DOCOL  (the codeword).
	    .word LATEST, FETCH, HIDDEN ; Hide the word (toggle)
	    .word RBRAC		            ; Set compile mode.
	    .word EXIT

        ; ; :: --
        ; end current definition
	    DEFWORD ";", "SEMICOLON", F_IMMED
        .word LIT, EXIT, COMMA	    ; Append EXIT
        .word LATEST, FETCH, HIDDEN ; Unhide the word
        .word LBRAC		            ; Go back to IMMEDIATE mode.
        .word EXIT		            ; Return from the function.

        ; IMMEDIATE :: --
        ; toggle F_IMMED on latest word
        DEFCODE "IMMEDIATE", , F_IMMED
        ldy #2
        CPYWW LATEST_value, AW
        lda (AW),y
        eor #F_IMMED
        sta (AW),y
        NEXT

        ; HIDDEN :: addr --
        ; toggle F_HIDDEN on word at addr
        DEFCODE "HIDDEN"
        POPW SP, AW
        ldy #2
        lda (AW),y
        eor #F_HIDDEN
        sta (AW),y
        NEXT

        ; HIDE :: --
        ; make next word hidden
        DEFWORD "HIDE"
        .word WORD
        .word FIND
        .word HIDDEN
        .word EXIT

        ; ' :: adr --
        ; in compiled mode, append next word to stack and skip it
        ;TODO identical to LIT ? in jonesforth it's just push v pushl
        DEFCODE "'", "TICK"
        PUSHIW SP, PC   ; copy next word to stack
        INCPC           ; and skip it
        NEXT

        ; BRANCH :: --
        ; increment PC by the word after BRANCH which should be even
        ; BRANCH 2 is a no-op (just skipping the literal)
        ; BRANCH -2 is an infinite loop to self
        DEFCODE "BRANCH"
        ADDWIWW PC, PC, AW  ;TODO unsafe to write direct back to PC
        CPYWW AW, PC
        NEXT

        ; 0BRANCH :: flag --
        ; branch by offset in next word if flag is 0, else continue
        DEFCODE "0BRANCH", "ZBRANCH"
        POPW SP, AW
        EQUWC AW, 0
        beq BRANCH+2     ; jump to unconditional branch above
        INCPC
        NEXT

        DEFWORD "QUIT"
        .word R0, RSPSTORE  ; set up return stack
        .word INTERPRET
        .word BRANCH, $10000 - 2*2  ; repeat INTERPRET forever

        DEFWORD "INTERPRET"
        .word WORD      ; -- sptr len
        .word _2DUP
        .word FIND      ; sptr len -- link | nul
        .word DUP       ; sptr len link link
check_found:
        .word ZBRANCH, not_found - check_found - 2
        .word NROT, _2DROP ; drop copy of word keeping link
        .word STATE
check_mode:
        .word ZBRANCH, execute - check_mode - 2
        .word COMMA, EXIT ; compiled
execute:
        ; jmp to it like next but without affecting PC
        ; so NEXT eventually carries on with BRANCH in QUIT
;TODO        .word JUMP

not_found:
        .word NUMBER    ; sptr len -- value err
        .word ZEQU
        .word ZBRANCH, error - not_found - 2

        .word STATE
lit_mode:
        .word ZBRANCH, pushlit - lit_mode - 2
        .word LIT, COMMA, COMMA
pushlit:
        .word EXIT
error:
        LSTRN "? parse error"
        .word TELL, EXIT

; ---------------------------------------------------------------------
; a few exploratory forth words for testing

        ; DOUBLE :: x -- 2*x
        DEFWORD "DOUBLE"
        .word DUP
        .word PLUS
        .word EXIT

        ; QUADRUPLE :: x -- 2*2*x
        DEFWORD "QUADRUPLE"
        .word DOUBLE
        .word DOUBLE
        .word EXIT

        ; FIB :: n -- fn
        DEFWORD "FIB"
; : FIB 0 1 ROT 1 BEGIN 2DUP > WHILE 1+ 2SWAP DUP ROT + 2SWAP REPEAT 2DROP NIP ;
; : FIB 0 1 ROT 0 ?DO OVER + SWAP LOOP DROP ;
        .word EXIT

        ; this is the head of our linked list of words
        DEFWORD "LASTWORD"
        .word EXIT

; ---------------------------------------------------------------------
; unit tests

.ifdef TESTS

    .macro EXPECTPOP expected, label
        POPW SP, AW
        EXPECTWC AW, expected, label
    .endmac

    .macro TESTPHRASE phrase
        SETWC PC, phrase
        jsr _NEXT
    .endmac

    .segment "TEST"
        jmp test_forth

test_numberok:
        LSTRN "48813"  ; adds LITSTRING, leading length and pad to stack word width
        .word NUMBER, _RTS

test_numberbad:
        LSTRN "banana"
        .word NUMBER, _RTS

test_find:
        LSTRN "-ROT"
        .word FIND, _RTS

test_ge:    .word LIT, 3, LIT, 2, GE, _RTS
test_zge0:  .word LIT, 0, ZGE, _RTS
test_zge1:  .word LIT, 1, ZGE, _RTS
test_zge_1: .word LIT, $10000 - 1, ZGE, _RTS

test_forth:
        SETWC SP, __STACK_START__
        SETWC RP, __RETSTACK_START__

        TESTPHRASE test_numberok
        EXPECTPOP 0, "NUMBER ok flg"
        EXPECTPOP 48813, "NUMBER ok val"

        TESTPHRASE test_numberbad
        EXPECTPOP 6, "NUMBER bad flg"
        EXPECTPOP 0, "NUMBER bad val"

        TESTPHRASE test_find
        EXPECTPOP NROT_header, "FIND -ROT"

        TESTPHRASE test_ge
        EXPECTPOP 1, "3 >= 2"

        TESTPHRASE test_zge0
        EXPECTPOP 1, "0 0>="

        TESTPHRASE test_zge1
        EXPECTPOP 1, "1 0>="

        TESTPHRASE test_zge_1
        EXPECTPOP 0, "-1 0>="
.endif

