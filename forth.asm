; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    .setcpu "65C02"
    .feature c_comments
    .feature underline_in_numbers
    .feature string_escapes

__FORTH_VERSION__ = 1

    .import __STACK_START__
    .import __STACK_SIZE__

__RETSTACK_START__ = __STACK_START__ - __STACK_SIZE__

    .include "word16.asm"

    .macro INCPC
        ADDWCW PC, 2, PC
    .endmac

    .macro NEXT
        jmp _NEXT
    .endmac

    .macro LSTRN s
        .word LITSTRING
        STRN s, 2
    .endmac

; minimal character I/O which assumes we have magic getc/putc memory locations

WORDBUF = $100      ; $100 - 120
parsebuf = $200     ; $200 -$3FF
nparsebuf = $200

    .code

DOCOL:
        ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
        ; AW contains the address of the codeword, so we just need to push the current PC
        ; which tells us where we are after we're finished this word,
        ; and then continue executing words at AW+2
        PUSHW RP, PC
        ADDWCW AW, 2, PC
        ; fall through to NEXT, and eventually matching EXIT pops the old PC
_NEXT:
        ; indirect threading: PC has the address of the next codeword's address
        ; the codeword contains the address of adapter/interpreter used to execute the word
        ; we jump to the target codeword's code after incrementing PC
        ; leaving AW containing the address of the codeword
        CPYIWW PC, AW       ; now AW contains the codeword's address
        INCPC               ; codeword to execute after we finish this one
        ; we need to jump to the address stored at the address stored in AW
        CPYIWW AW, BW       ; now BW has the interpreter address
nextword:                   ; useful breakpoint to step through words
        jmp (BW)            ; run the word's interpreter code

forth:
        ; initial entry point
        cld
        SETWC SP, __STACK_START__
        SETWC PC, cold_start
        NEXT

cold_start:
        .word QUIT          ; start up interpreter

.include "string.asm"

; ---------------------------------------------------------------------
; macros to define words with appropriate header

; flag defintions
F_IMMED     = %1000_0000
F_HIDDEN    = %0010_0000
F_LENMASK   = %0001_1111

/*
    Each Forth word has a layout like this (adapted from Jonesforth):

	  ptr to prev word          address of code interpreter (DOCOL or @code)
       ^                           ^
	   |         name of word      |
	   |            ^              |
	+--|------+---+-|-+---+---+----|-----+------------------+-----------+
	| link    | 3 | D | U | P | codeword |  ... code ...    | NEXT/EXIT |
	+|--------+-|-+---+---+---+|---------+|-----------------+-----------+
     |          v              |          v
     |     length + flags      |          @code (>DFA)
     v                         v
     DUP_header sym (FIND)     DUP sym (>CFA)

    Native words have a codeword which is just @code, so jmp (codeword)
    simply executes the native code, which ends with NEXT.

    Compiled words have code which is a list of word pointers and codeword DOCOL.
    This executes each word in turn ending with EXIT which pops the DOCOL ret stack
    and falls through to NEXT.

    We set up a number of macros (like Jonesforth) so that we can define
    variations on this structure.  All are based on _DEFWORD which creates
    the first part of the header (link, nstr).  The derived macros are:

        DEFCODE - define a native word (assembly code ending with NEXT)
        DEFWORD - define a compiled word (list of forth words ending with EXIT)
        DEFVAR - define a word that pushes its address to the stack (see @, !)
        DEFCONST - define a word that pushes a constant to the stack

    Useful Forth words to manipulate the word structure:

        FIND - searches the linked list for a words LINK ptr
        >CFA - advances from link pointer to codeword ptr
        >DFA - advances from link pointer to code (data) ptr
        .NAME - converts a link pointer to name sptr len

*/

link .set 0         ; this counts word defs so we can generate linked list of symbols

    .macro _DEFWORD name, label, flags

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
        ; define word that pushes constant to stack
        DEFCODE name, label, flags
        PUSHC SP, value
        NEXT
    .endmac

    .macro DEFVAR name, label, flags, value
    .local vptr
        ; define word that returns address of a variable
        DEFCODE name, label, flags
        PUSHC SP, vptr
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
        DEFCONST "S0",,, __STACK_START__        ; top of param stack
        DEFCONST "R0",,, __RETSTACK_START__     ; top of return stack
        DEFCONST "DOCOL", "_DOCOL",, DOCOL      ; pointer to DOCOL
        DEFCONST "F_IMMED", "_F_IMMED",, F_IMMED		; Flag values
        DEFCONST "F_HIDDEN", "_F_HIDDEN",,  F_HIDDEN
        DEFCONST "F_LENMASK", "_F_LENMASK",, F_LENMASK

; ---------------------------------------------------------------------
; core variables

        DEFVAR "STATE"                          ; immediate = 0 / compile != 0
        DEFVAR "LATEST", , , NOOP_header        ; head of our linked word list
        DEFVAR "HERE", , , bootstrap            ; overwrite bootstrap text
        DEFVAR "BASE", , , 10   ; radix for printing and reading numbers


        DEFCONST "parsebuf", "_parsebuf", , parsebuf
        DEFCONST "#parsebuf", "_nparsebuf", , nparsebuf

srcids:     .byte 0, $ff   ; user input, bootstrap
            .res 14

        DEFVAR "'srcid", "_srcid", , srcids+1
        DEFVAR "'srcbuf", "_srcbuf", , bootstrap
        DEFVAR "#srcbuf", "nsrcbuf", , bootstrap_end - bootstrap
        DEFVAR ">IN", "_IN", , 0

; ---------------------------------------------------------------------
; native word definitions

        ; EXIT :: adr -R-
        ; when we've finished a Forth word, we just recover the old PC and proceed
        DEFCODE "EXIT"
        POPW RP, PC
        NEXT

        ; exit a word we've called as an assembly subroutine
        DEFCODE "EXIT_RTS"
        POPW RP, PC
        rts

        ; EXECUTE :: xt --
        ; execute the execution token (codeword) on the stack, cf NEXT
        ; we don't increment PC since target word's NEXT will do it
        DEFCODE "EXECUTE"
        POPW SP, AW     ; AW contains the address of the codeword (required for DOCOL)
        CPYIWW AW, BW   ; Indirect copy so BW contains the codeword itself
        jmp (BW)        ; jump to the codeword (address of interpreter)

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
        beq done
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
        ; this tests if x < 0, ie. x 0< is the same as x 0 <
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
        beq result
        bmi result
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

; ---------------------------------------------------------------------
; bitwise ops

;TODO IW versions

        ; AND :: x y -- x & y
        DEFCODE "AND", "_AND"
        POPW SP, AW
        ANDIWWIW SP, AW, SP
        NEXT

        ; OR :: x y -- x | y
        DEFCODE "OR"
        POPW SP, AW
        ORIWWIW SP, AW, SP
        NEXT

        ; XOR :: x y -- x ^ y
        DEFCODE "XOR"
        POPW SP, AW
        XORIWWIW SP, AW, SP
        NEXT

        ; INVERT :: x -- ~x
        DEFCODE "INVERT"        ; bitwise complement
        NOTIWIW SP, SP
        NEXT

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

;TODO  this should prob be a signed multiply (xor sign flags and negate then reapply)
; set overflow if sign flag gets overwritten, carry if exceed 16bits
; but this works ok with small numbers since two complement behaves ok
; e.g. ($10000 - x) * ($10000 - y) mod $10000 => -x * -y

        ; * :: x y -- x*y
        DEFCODE "*", "MULTIPLY"
        POPW SP, AW
        PEEK SP, 0, BW
        MULWWW AW, BW, CW
        POKE SP, 0, CW
        NEXT

        ; /MOD :: num den -- rem quo
        DEFCODE "/MOD", "DIVMOD"
        PEEK SP, 0, BW
        PEEK SP, 1, AW
;TODO DIVIW ....
        DIVWWWW AW, BW, CW, DW ; CW is quotient
        POKE SP, 0, CW
        POKE SP, 1, DW
        NEXT

        ; 1+ :: num -- num
        DEFCODE "1+", "INC1"
        ADDIWCIW SP, 1, SP      ; note INC has no indirect addressing mode
        NEXT

        ; 2+ :: num -- num
        DEFCODE "2+", "INC2"
        ADDIWCIW SP, 2, SP
        NEXT

        ; 1- :: num -- num
        DEFCODE "1-", "DEC1"
        SUBIWCIW SP, 1, SP
        NEXT

        ; 2+ :: num -- num
        DEFCODE "2-", "DEC2"
        SUBIWCIW SP, 2, SP
        NEXT

        ; LIT :: -- x
        ; push the next word as a constant and skip it
        DEFCODE "LIT"
        PUSHIW SP, PC   ; copy literal to stack
        INCPC           ; and skip it
        NEXT

        ; LITSTRING :: -- sptr len
        ; push subsequent bytes to the stack as a length delim string
        ; padded to a word boundary
        DEFCODE "LITSTRING"
        CPYWW PC, AW
        INCW AW
        PUSHW SP, AW
        lda (PC)
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
        PUSHW SP, RP
        NEXT

        ; RSP! :: sp -R-
        ; set the return stack pointer from the top of the return stack
        DEFCODE "RSP!", "RSPSTORE"
        POPW SP, RP
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

        ; 2>R :: x y --  ;  -R- x y
        ; move top two stack elts to return stack preserving order
        DEFCODE "2>R", "_2TORS"
        POPW SP, AW
        POPW SP, BW
        PUSHW RP, BW
        PUSHW RP, AW
        NEXT

        ; R> :: -- x  ;  x -R-
        ; move top of return stack to stack
        DEFCODE "R>", "FROMRS"
        POPW RP, AW
        PUSHW SP, AW
        NEXT

        ; 2R> :: -- x y ; x y -R-
        ; move top two elts of return stack to stack preserving order
        DEFCODE "2R>", "_2FROMRS"
        POPW RP, AW
        POPW RP, BW
        PUSHW SP, BW
        PUSHW SP, AW
        NEXT

        ; R@ ::  -- x ;  x -R- x )
        ; copy top of return stack to data stack
        DEFCODE "R@", "DUPRS"
        PEEK RP, 0, AW
        PUSHW SP, AW
        NEXT

        ; 2R@ ::  -- x y ;  x y -R- x y)
        ; copy top of return stack to data stack keeping order
        DEFCODE "2R@", "_2DUPRS"
        PEEK RP, 0, AW
        PEEK RP, 1, BW
        PUSHW SP, BW
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
        lda TMP
        sta (AW)
        NEXT

        ; C@ :: adr -- x
        ; fetch adr => x
        DEFCODE "C@", "FETCHBYTE"
        PEEK SP, 0, AW
        lda (AW)
        sta AW
        stz AW+1
        POKE SP, 0, AW
        NEXT

;TODO  C@C! (CCOPY), CMOVE

; ---------------------------------------------------------------------
; I/O

        ; KEY :: -- c
        DEFCODE "KEY"
        jsr key_
        sta TMP
        PUSHB SP, TMP
        NEXT
key_:       ; must preserve y register
    .proc _key
        phy
        EQUWW _IN_value, nsrcbuf_value
        bne getcbuf
        SETWC AW, _refill-2  ; DOCOL is implied
        jsr DOCOL
        POPW SP, AW         ; ignore error for now
getcbuf:
        ADDWWW _srcbuf_value, _IN_value, AW
        lda (AW)
        INCW _IN_value
        ply
        rts
_refill:  .word REFILL, EXIT_RTS
    .endproc

        ; DEFER'd definition of REFILL that will patch in bootstrap
        ; -- success
        DEFWORD "REFILL"
        .word NOOP, EXIT

        ; EMIT :: c --
        DEFCODE "EMIT"
        POPB SP, TMP
        lda TMP
        jsr emit_
        NEXT
emit_:  sta $f001   ; magic monitored write for PUTC
        rts

        ; TELL :: sptr len --
        DEFCODE "TELL"
    .proc _tell
        POPW SP, BW
        POPW SP, AW
        ADDWWW AW, BW, BW
loop:   EQUWW BW, AW
        beq done
        lda (AW)
        jsr emit_
        INCW AW
        bra loop
done:   NEXT
    .endproc

        ; WORD :: -- sptr len
        ; read a word from input
        DEFCODE "WORD"
    .proc _word
        PUSHC SP, WORDBUF
        ldy #0
skipspace:
        jsr key_
        cmp #$5c    ; backslash
        bne nocomment
skipcomment:        ; eat characters until end of line
        jsr key_
        cmp #$0a    ; LF
        bne skipcomment
nocomment:
        cmp #$21    ; space + 1
        bmi skipspace
store:
        sta WORDBUF,y
        iny
        jsr key_
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
        PEEKB SP, 0, LEN
        PEEK  SP, 1, AW     ; char*
        jsr parseint        ; (AW, LEN) => (BW, ERR)
        POKE  SP, 1, BW     ; parsed value
        POKEB SP, 0, ERR    ; number of unconverted chars (0 => success)
        NEXT

        ; N>$ :: x -- sptr len
        DEFCODE "N>$", "N2STRN"
        POPW SP, AW
        lda BASE_value
        jsr fmtint
        PUSHC SP, FMTBUF
        PUSHB SP, LEN
        NEXT

        ; NAME :: link -- sptr len
        ; put the name of a word on the stack
        DEFCODE ".NAME", "_NAME"
        POPW SP, AW
        ldy #2
        lda (AW),y
        and #F_LENMASK
        sta LEN
        ADDWCW AW, 3, AW
        PUSHW SP, AW
        PUSHB SP, LEN
        NEXT

        ; FIND :: sptr len -- hfa | nul
        DEFCODE "FIND"
    .proc _find
        POPB SP, LEN
        POPW SP, AW
        CPYWW LATEST_value, BW
nextlink:
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
        jmp nextlink
    .endproc

        ; >CFA :: link -- cptr
        ; convert a header field address, e.g. a link from find,
        ; to the corresponding codeword field address
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
        ; convert a header field address to the word's data field address (code)
        ; our words all have a two-byte codeword so >DFA is just >CFA + 2
        DEFWORD ">DFA", "TDFA"
        .word TCFA, INC2, EXIT

        ; >HFA :: adr -- link | 0
        DEFCODE ">HFA", "THFA"
        ; find the first word whose header field is at or before adr
; TODO must be within 32+2+1+2 assuming adr is a cfa or dfa
        ; the second call is a no-op: someadr >HFA >HFA
        ; this is also a no-op: link >CFA >HFA
    .proc _thfa
        POPW SP, AW
        CPYWW LATEST_value, BW  ; latest (highest) HFA
next:
        SUBWWW AW, BW, CW   ; CW = AW - BW
        bcs done            ; no borrow, so AW >= BW
        CPYIWW BW, BW       ; follow linked list to prev HFA
        jmp next            ; eventually BW = 0 which will break the loop
done:
        PUSHW SP, BW        ; push match or 0
        NEXT
    .endproc

        ; CREATE :: sptr len --
        ; create the header for new word defintion, updates HERE, LATEST
        DEFCODE "CREATE"
    .proc _create
        POPB SP, LEN
        POPW SP, AW
        ; stash the heap ptr in zp
        CPYWW HERE_value, BW
        ; write pointer back to prev word
        CPYWIW LATEST_value, BW
        ; update latest pointer
        CPYWW BW, LATEST_value
        ; skip link word
        lda #2
        ADDWAW BW, BW
        lda LEN
        sta (BW)                ; write the length
        INCW BW
        ldy #0
copy:   cpy LEN                 ; write the name
        beq done
        lda (AW),y
        sta (BW),y
        iny
        bne copy
done:   tya                     ; Y is LEN after loop
        ADDWAW BW,BW
        CPYWW BW, HERE_value    ; update the heap pointer
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
        ; set STATE = 0 (immediate mode) - note it's an immediate word itself so it works in compile mode
        DEFCODE "[", "LBRAC", F_IMMED
        stz STATE_value
        NEXT

        ; ] :: --
        ; set STATE = 1 (compile mode)
        DEFCODE "]", "RBRAC"
        lda #1
        sta STATE_value
        NEXT

        ; : :: --
        ; start definition of a new word
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
        CPYWW LATEST_value, AW
        ldy #2
        lda (AW),y
        eor #F_IMMED
        sta (AW),y
        NEXT

        ; ?IMMEDIATE link :: 0 | 1
        ; TODO this should get split into proper ?IMMEDIATE plus separate state check
        ; return 1 if word has F_IMMED set or state is 0 (imm mode)
        DEFCODE "?IMMEDIATE", "QIMMEDIATE"
    .proc _qimmediate
        POPW SP, AW
        ldy #2
        lda (AW),y
        dey             ; y = 1
        and #F_IMMED
        bne immset      ; !=0 means F_IMMED is set
        lda STATE_value
        beq immset      ; or state = 0 (imm mode)
        dey             ; otherwise y = 0
immset: sty TMP
        PUSHB SP, TMP
        NEXT
    .endproc

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
        .word WORD, FIND, HIDDEN, EXIT

        ; ' :: -- adr
        ; in compile mode, append next word to stack and skip it
        ;TODO identical to LIT ? in jonesforth it's just push v pushl
;        DEFCODE "'", "TICK"
;        PUSHIW SP, PC   ; copy next word to stack
;        INCPC           ; and skip it
;        NEXT
        DEFWORD "'", "TICK", F_IMMED
    .proc _tick
        .word STATE, FETCH, ZBRANCH
        .word immed - *
        .word LIT, LIT, COMMA, EXIT
immed:  .word WORD, FIND, TCFA, EXIT
    .endproc

        ; BRANCH :: --
        ; increment PC by the following word (which should be even)
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
        INCPC            ; skip the offset
        NEXT

        DEFWORD "QUIT"
    .proc _quit
        .word R0, RSPSTORE  ; set up return stack
loop:   .word INTERPRET
        .word BRANCH
        .word loop - * + $10000      ; repeat INTERPRET forever
    .endproc

        ; create a word which acts like another word
        DEFWORD "DEFER"
	    .word COLON		            ; create a new word
        .word LIT, NOOP, COMMA      ; which does nothing for now
        .word SEMICOLON
	    .word EXIT

        ; DEFER :: xt defer' --
        ; store xt as word to wrap, e.g. ' + ' defname DEFER!
        DEFWORD "DEFER!", "DEFERSTORE"
        .word INC2, STORE       ; store xt @ defer+2
        .word EXIT

        DEFWORD "INTERPRET"
        .word WORD      ; -- sptr len
        .word _2DUP     ; sptr len -- sptr len sptr len
        .word FIND      ; sptr len sptr len -- sptr len link | nul
        .word DUP       ; copy link to test
        .word ZBRANCH
        .word maybe_number - *
        .word NROT, _2DROP ; drop copy of word keeping link
        .word DUP, TCFA, SWAP, QIMMEDIATE   ; codeword 1=immed|0=compile
        .word ZBRANCH
        .word compile - *
        .word EXECUTE, EXIT ; execute word right away
compile:
        .word COMMA, EXIT ; add word to compiled code
maybe_number:
        .word DROP      ; drop the nul link
        .word NUMBER    ; sptr len -- value err
        .word ZBRANCH
        .word is_number - *
        .word DROP      ; drop the ptr
        LSTRN " ? parse error\n"
        .word TELL, EXIT
is_number:
        .word STATE, FETCH
        .word ZBRANCH
        .word lit_immed - *
        .word LIT, LIT, COMMA, COMMA     ; compile LIT, <value>
lit_immed:
        .word EXIT      ; just leave it on the stack
error:

; ---------------------------------------------------------------------
; a dummy word that marks the end of our linked list of words

        DEFWORD "NOOP"
        .word EXIT

    .data

; we'll set HERE to bootstrap and compile over the included text

bootstrap:
    .incbin "bootstrap.f"
bootstrap_end:

; ---------------------------------------------------------------------
; unit tests

.ifdef TESTS
    .include "unittest.asm"

    .macro EXPECTPOP expected, label
        POPW SP, AW
        EXPECTWC AW, expected, label
    .endmac

    ; call a DOCOL-style word ending with EXIT_RTS as an assembly subroutine
    .macro TESTPHRASE phrase
        SETWC AW, phrase
        jsr DOCOL
    .endmac

    .segment "TEST"
        jmp test_forth

phrase_ver:   .word DOCOL, VERSION, EXIT_RTS
phrase_ge:    .word DOCOL, LIT, 3, LIT, 2, GE, EXIT_RTS
phrase_zge0:  .word DOCOL, LIT, 0, ZGE, EXIT_RTS
phrase_zge1:  .word DOCOL, LIT, 1, ZGE, EXIT_RTS
phrase_zge_1: .word DOCOL, LIT, $10000 - 1, ZGE, EXIT_RTS
phrase_numberok:
        .word DOCOL
        LSTRN "48813"  ; adds LITSTRING, leading length and pad to stack word width
        .word NUMBER, EXIT_RTS
phrase_numberbad:
        .word DOCOL
        LSTRN "banana"
        .word  NUMBER, EXIT_RTS
phrase_find:
        .word DOCOL
        LSTRN "-ROT"
        .word FIND, EXIT_RTS
phrase_thfa: .word DOCOL, LIT, SWAP, THFA, THFA, DUP, TCFA, DUP, THFA, LIT, $ff, THFA, EXIT_RTS
; expect: SWAP_header, SWAP, SWAP_header, 0

test_forth:
        SETWC SP, __STACK_START__
        SETWC RP, __RETSTACK_START__

        TESTPHRASE phrase_ver
        EXPECTPOP __FORTH_VERSION__, "VERSION"

        TESTPHRASE phrase_ge
        EXPECTPOP 1, "3 >= 2"

        TESTPHRASE phrase_zge0
        EXPECTPOP 1, "0 0>="

        TESTPHRASE phrase_zge1
        EXPECTPOP 1, "1 0>="

        TESTPHRASE phrase_zge_1
        EXPECTPOP 0, "-1 0>="

        TESTPHRASE phrase_numberok
        EXPECTPOP 0, "NUMBER ok flg"
        EXPECTPOP 48813, "NUMBER ok val"

        TESTPHRASE phrase_numberbad
        EXPECTPOP 6, "NUMBER bad flg"
        EXPECTPOP 0, "NUMBER bad val"

        TESTPHRASE phrase_find
        EXPECTPOP NROT_header, "FIND -ROT"

        TESTPHRASE phrase_thfa
        EXPECTPOP 0, "0 >HFA"
        EXPECTPOP SWAP_header, ">CFA >HFA"
        EXPECTPOP SWAP, ">HFA >CFA"
        EXPECTPOP SWAP_header, ">HFA >HFA"

.endif

