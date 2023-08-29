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

    .macro MEMCOPY source, target, length
    .local loop
        ldx #(length-1)
loop:   lda source, x
        sta target, x
        dex
        bpl loop
    .endmac

REG_NONE = 0
REG_A = 1
REG_AB = 2
REG_ABC = 3
REG_ABCD = 4

pushreg .set REG_NONE

; minimal character I/O which assumes we have magic getc/putc memory locations

WORDBUF = $100      ; $100 - 120
parsebuf = $200     ; $200 -$3FF
nparsebuf = $200

    .zeropage

NPUSH:  .res 1
CFA:    .res 2
IA:     .res 2
UW:     .res 2
VW:     .res 2

    .code

DOCOL:
        ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
        ; CFA contains the address of the codeword (via prev NEXT)
        ; so push the current PC which is where to resume after finishing this word,
        ; and then continue executing words at AW+2
        PUSHW RP, PC
        ADDWCW CFA, 2, PC
        ; fall through to NEXT, and eventually matching EXIT pops the old PC
_NEXT:
        ; indirect threading: PC has the address of the next codeword's address
        ; the codeword contains the address of adapter/interpreter used to execute the word
        ; we jump to the target codeword's code after incrementing PC
        ; leaving AW containing the address of the codeword
        CPYIWW PC, CFA       ; now CFA contains the codeword's address (cf DOCOL)
        INCPC               ; codeword to execute after we finish this one
        ; we need to jump to the address stored at the address stored in AW
        CPYIWW CFA, IA       ; now IA has the interpreter address
nextword:                   ; useful breakpoint to step through words
        jmp (IA)            ; run the word's interpreter code

    .macro NEXT
        jmp .ident(.sprintf("next%d", ::pushreg))
    .endmac

forth:
        ; initial entry point
        cld
        SETWC SP, __STACK_START__
        SETWC PC, cold_start
        CPYWW _srcbuf_value, SRCBUFP    ; init our zp buf ptr for KEY
        NEXT

POP_NONE = REG_NONE
POP_A = REG_A
POP_AB = REG_AB
POP_ABC = REG_ABC
POP_ABCD = REG_ABCD

    .macro PUSH_A
::pushreg .set ::REG_A
    .endmac
    .macro PUSH_AB
::pushreg .set ::REG_AB
    .endmac
    .macro PUSH_ABC
::pushreg .set ::REG_ABC
    .endmac
    .macro PUSH_ABCD
::pushreg .set ::REG_ABCD
    .endmac

next0:  ldx #REG_NONE
        bra nextp
next1:  ldx #REG_A * 2
        bra nextp
next2:  ldx #REG_AB * 2
        bra nextp
next3:  ldx #REG_ABC * 2
        bra nextp
next4:  ldx #REG_ABCD * 2
        bra nextp
nextp:  stx NPUSH
        bra _NEXT

cold_start:
        .word QUIT          ; start up interpreter

syncstack0:
    ldx #REG_NONE
    bra syncstack
syncstack1:
    ldx #REG_A * 2
    bra syncstack
syncstack2:
    ldx #REG_AB * 2
    bra syncstack
syncstack3:
    ldx #REG_ABC * 2
    bra syncstack
syncstack4:
    ldx #REG_ABCD * 2
    bra syncstack

syncstack:
    .proc _syncstack
        ; syncstack is called to setup for each DEFCODE word
        ; these are the only words that directly manipulate the data stack
        ; so we can maintain a mapping between the registers AW, BW, ... DW
        ; and the head of the stack (AW at the head, BW next etc)
        ; an operation declares it wants to push registers back to the stack
        ; before calling NEXT, e.g. PUSH_AB.
        ; This sets NPUSH as the # of register bytes to push
        ; When the next native (DEFCODE) word is called, it declares the
        ; number of stack bytes to pop as an optional argument to DEFCODE,
        ; e.g. DEFCODE ... POP_A.   This sets the X register as NPOP
        ; and we do the appropriate juggling to match registers and stack
        ; which often avoids us having to write registers back to the stack.
        txa
        sec
        sbc NPUSH   ; NPOP - NPUSH
        beq done    ; equal?  nothing to do
        bpl morepop ; NPOP > NPUSH
        eor #$ff    ; NPUSH > NPOP so invert diff to NPUSH - NPOP
        inc         ; e.g. push c,b,a; pop a
        GROWA SP    ; grow stack by difference in bytes
        ldy #0
tostk:  lda AW,x    ; cp reg [x=NPOP, NPUSH) => stack [0, diff)
        sta (SP),y
        inx
        cpx NPUSH
        beq done
        iny
        bra tostk
morepop:            ; e.g. push a;  pop a, b, c
        pha         ; save difference for shrink
        tay         ; and for countdown
toreg:  dey         ; cp stack [0, diff) => reg [NPUSH, NPOP)
        bmi shrnk
        dex
        lda (SP),y
        sta AW,x
        bra toreg
shrnk:  pla
        SHRINKA SP  ; shrink stack by difference in bytes (not words)
done:   stz NPUSH   ; default next op to no unpushed registers
        rts
    .endproc

    .macro SETAWNEXT mode
        ; we store forth const and vars in the word following a JSR
        ; ie. at the return address on the stack
        pla
        sta UW
        pla
        sta UW+1
        INCW UW         ; JSR puts curr address + 2 (not 3) on the stack
    .if mode = 0
        CPYIWW UW, AW   ; for a constant we set the value
    .else
        CPYWW UW, AW    ; for a variable we set the address
    .endif
        INCW UW         ; INC twice so we return past the value
        lda UW+1
        pha
        lda UW
        pha
        rts
    .endmac

setawconst:
        SETAWNEXT 0

setawvar:
        SETAWNEXT 1

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

    .ifnblank flags
        .if flags & (~(F_IMMED|F_HIDDEN))
            .error .sprintf ("_DEFWORD %s: invalid flags", name)
        .endif
    .endif

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

    .macro DEFCODE name, label, flags, popreg
    .local code
        ; define a native word
        _DEFWORD name, label, flags
pushreg .set ::REG_NONE
        .word code
code:
    .ifblank popreg
        jsr syncstack0
    .else
        jsr .ident(.sprintf("syncstack%d", popreg))
    .endif
        ; followed by native assembly, ended by NEXT
    .endmac

    .macro DEFWORD name, label, flags
        _DEFWORD name, label, flags
        .word DOCOL     ; Forth words use codeword DOCOL
        ; followed by list of word pointers, ended by EXIT
    .endmac

    .macro _DEFCV name, label, flags, value, mode
        ; push a constant (value; mode=0) or variable (address; mode=1) to stack
        DEFCODE name, label, flags
    .if mode = 0
        jsr setawconst
    .else
        jsr setawvar
    .endif
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
        PUSH_A
        NEXT
    .endmac

    .macro DEFCONST name, label, flags, value
        ; define word that pushes a constant to stack
        _DEFCV name, label, flags, value, 0
    .endmac

    .macro DEFVAR name, label, flags, value
        ; define word that pushes address of a variable to stack
        _DEFCV name, label, flags, value, 1
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

    .zeropage

SRCBUFP:    .res 2          ; current character in srcbuf, see coldstart, KEY

    .code

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
        DEFCODE "EXECUTE", , , POP_A
        CPYWW AW, CFA   ; CFA contains the address of the codeword (required for DOCOL)
        CPYIWW CFA, IA  ; Indirect copy so IA contains the codeword itself
        jmp (IA)        ; jump to the codeword (address of interpreter)

        ; DROP :: x --
        DEFCODE "DROP", , , POP_A
        NEXT

        ; SWAP :: x y -- y x
        DEFCODE "SWAP", , , POP_AB
        CPYWW AW, UW
        CPYWW BW, AW
        CPYWW UW, BW
        PUSH_AB
        NEXT

        ; DUP :: x -- x x
        DEFCODE "DUP", , , POP_A
        CPYWW AW, BW
        PUSH_AB
        NEXT

        ; OVER :: x y -- x y x
        DEFCODE "OVER", , , POP_AB
        CPYWW BW, CW
        CPYWW AW, BW
        CPYWW CW, AW
        PUSH_ABC
        NEXT

        ; ROT :: x y z -- y z x
        DEFCODE "ROT", , , POP_ABC
        CPYWW AW, UW
        CPYWW CW, AW
        CPYWW BW, CW
        CPYWW UW, BW
        PUSH_ABC
        NEXT

        ; -ROT :: x y z -- z x y
        DEFCODE "-ROT", "NROT", , POP_ABC
        CPYWW AW, UW
        CPYWW BW, AW
        CPYWW CW, BW
        CPYWW UW, CW
        PUSH_ABC
        NEXT

        ; 2DROP :: x y --
        DEFCODE "2DROP", "_2DROP", , POP_AB
        NEXT

        ; 2DUP :: x y -- x y x y
        DEFCODE "2DUP", "_2DUP", , POP_AB
        MEMCOPY AW, CW, 2*2
        PUSH_ABCD
        NEXT

        ; 2SWAP :: x y z w -- z w x y
        DEFCODE "2SWAP", "_2SWAP",  , POP_ABCD
        MEMCOPY AW, UW, 4
        MEMCOPY CW, AW, 4
        MEMCOPY UW, CW, 4
        PUSH_ABCD
        NEXT

        ; ?DUP :: x -- 0 | x x
        DEFCODE "?DUP", "QDUP", , POP_A
    .proc _qdup
        EQUWC AW, 0
        beq done
        PUSHW SP, AW    ; push an extra copy
done:   PUSH_A
        NEXT
    .endproc

; ---------------------------------------------------------------------
; comparisons

        ; = :: x y -- 0 | 1
        DEFCODE "=", "EQU", , POP_AB
    .proc _equ
        ldx #0
        EQUWW AW, BW
        bne isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; <> :: x y -- 0 | 1
        DEFCODE "<>", "NEQU", , POP_AB
    .proc _nequ
        ldx #0
        EQUWW AW, BW
        beq isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; < :: x y -- 0 | 1
        DEFCODE "<", "LT", , POP_AB
    .proc _lt
        SUBWWW BW, AW, AW       ; x < y <=> x - y < 0
        ldx #0
        SGNW AW
        bpl isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; > :: x y -- 0 | 1
        DEFCODE ">", "GT", , POP_AB
    .proc _gt
        SUBWWW AW, BW, AW       ; x > y <=> y - x < 0
        ldx #0
        SGNW AW
        bpl isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; <= :: x y -- 0 | 1
        DEFCODE "<=", "LE", , POP_AB
    .proc _le
        SUBWWW AW, BW, AW       ; x <= y <=> y - x >= 0
        ldx #0
        SGNW AW
        bmi isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; >= :: x y -- 0 | 1
        DEFCODE ">=", "GE", , POP_AB
    .proc _ge
        SUBWWW BW, AW, AW      ; x >= y <=> x - y >= 0
        ldx #0
        SGNW AW
        bmi isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0= :: x -- 0 | 1
        DEFCODE "0=", "ZEQU", , POP_A
    .proc _zequ
        ldx #0
        EQUWC AW, 0
        bne isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0<> :: x -- 0 | 1
        DEFCODE "0<>", "ZNEQU", , POP_A
    .proc _znequ
        ldx #0
        EQUWC AW, 0
        beq isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0< :: x -- 0 | 1
        ; this tests if x < 0, ie. x 0< is the same as x 0 <
        DEFCODE "0<", "ZLT", , POP_A
    .proc _zlt
        ldx #0
        SGNW AW
        bpl isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0> :: x -- 0 | 1
        DEFCODE "0>", "ZGT", , POP_A
    .proc _zgt
        ldx #0
        SGNW AW
        beq isnt
        bmi isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0<= :: x -- 0 | 1
        DEFCODE "0<=", "ZLE", , POP_A
    .proc _zle
        ldx #1
        SGNW AW
        bmi is
        beq is
        dex
is:     txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0>= :: x -- 0 | 1
        DEFCODE "0>=", "ZGE", , POP_A
    .proc _zge
        ldx #0
        SGNW AW
        bmi isnt
        inx
isnt:   txa
        SETWA AW
        PUSH_A
        NEXT
    .endproc

; ---------------------------------------------------------------------
; bitwise ops

        ; AND :: x y -- x & y
        DEFCODE "AND", "_AND", , POP_AB
        ANDWWW AW, BW, AW
        PUSH_A
        NEXT

        ; OR :: x y -- x | y
        DEFCODE "OR", , , POP_AB
        ORWWW AW, BW, AW
        PUSH_A
        NEXT

        ; XOR :: x y -- x ^ y
        DEFCODE "XOR", , , POP_AB
        XORWWW AW, BW, AW
        PUSH_A
        NEXT

        ; INVERT :: x -- ~x
        DEFCODE "INVERT", , , POP_A     ; bitwise complement
        NOTWW AW, AW
        PUSH_A
        NEXT

; ---------------------------------------------------------------------
; arithmetic

        ; + :: x y -- x+y
        DEFCODE "+", "PLUS", , POP_AB
        ADDWWW BW, AW, AW
        PUSH_A
        NEXT

        ; - :: x y -- x-y
        DEFCODE "-", "MINUS", , POP_AB
        SUBWWW BW, AW, AW
        PUSH_A
        NEXT

;TODO  this should prob be a signed multiply (xor sign flags and negate then reapply)
; set overflow if sign flag gets overwritten, carry if exceed 16bits
; but this works ok with small numbers since two complement behaves ok
; e.g. ($10000 - x) * ($10000 - y) mod $10000 => -x * -y

        ; * :: x y -- x*y
        DEFCODE "*", "MULTIPLY", , POP_AB
        MULWWW BW, AW, UW
        CPYWW UW, AW
        PUSH_A
        NEXT

        ; /MOD :: num den -- rem quo
        DEFCODE "/MOD", "DIVMOD", , POP_AB
        DIVWWWW BW, AW, UW, VW ; num denom -- quo rem
        MEMCOPY UW, AW, 4
        PUSH_AB
        NEXT

        ; 1+ :: num -- num
        DEFCODE "1+", "INC1", , POP_A
        INCW AW
        PUSH_A
        NEXT

        ; 2+ :: num -- num
        DEFCODE "2+", "INC2", , POP_A
        ADDWCW AW, 2, AW
        PUSH_A
        NEXT

        ; 1- :: num -- num
        DEFCODE "1-", "DEC1", , POP_A
        DECW AW
        PUSH_A
        NEXT

        ; 2+ :: num -- num
        DEFCODE "2-", "DEC2", , POP_A
        SUBWCW AW, 2, AW
        PUSH_A
        NEXT

        ; LIT :: -- x
        ; push the next word as a constant and skip it
        DEFCODE "LIT"
        CPYIWW PC, AW   ; fetch the literal
        INCPC           ; and skip it
        PUSH_A
        NEXT

        ; LITSTRING :: -- sptr len
        ; push subsequent bytes to the stack as a length delim string
        ; padded to a word boundary
        DEFCODE "LITSTRING"
        ADDWCW PC, 2, BW    ; string data starts after length word
        CPYIWW PC, AW       ; get string length word @ PC
        ADDWCW AW, 3, TMP    ; skip data via strlen + 3  bytes (length word + 1 byte padding)
        lda TMP
        and #$fe            ; and clear last bit to get number of full words
        sta TMP
        ADDWWW PC, TMP, PC   ; bump PC to word boundary past string data
        PUSH_AB
        NEXT

        ; helper to generate compiled LITSTRING
    .macro LSTRN s
        .word LITSTRING
        STRN s, 2
    .endmac

; ---------------------------------------------------------------------
; STACK

        ; DSP@ :: -- sp
        ; push the current stack pointer on the stack
        DEFCODE "DSP@", "DSPFETCH"
        CPYWW SP, AW
        PUSH_A
        NEXT

        ; DSP! :: sp --
        ; set the stack pointer from the top of the stack
        DEFCODE "DSP!", "DSPSTORE", , POP_A
        CPYWW AW, SP
        NEXT

        ; RSP@ ::  -- sp
        ; push the current return stack pointer on the stack
        DEFCODE "RSP@", "RSPFETCH"
        CPYWW RP, AW
        PUSH_A
        NEXT

        ; RSP! :: sp --
        ; set the return stack pointer from the top of the return stack
        DEFCODE "RSP!", "RSPSTORE", , POP_A
        CPYWW AW, RP
        NEXT

        ; RDROP :: x -R-
        ; drop top of return stack
        DEFCODE "RDROP"
        SHRINK RP
        NEXT

        ; >R :: x --  ;  -R- x
        ; move top stack elt to return stack
        DEFCODE ">R", "TORS", , POP_A
        PUSHW RP, AW
        NEXT

        ; 2>R :: x y --  ;  -R- x y
        ; move top two stack elts to return stack preserving order
        DEFCODE "2>R", "_2TORS", , POP_AB
        PUSHW RP, BW
        PUSHW RP, AW
        NEXT

        ; R> :: -- x  ;  x -R-
        ; move top of return stack to stack
        DEFCODE "R>", "FROMRS"
        POPW RP, AW
        PUSH_A
        NEXT

        ; 2R> :: -- x y ; x y -R-
        ; move top two elts of return stack to stack preserving order
        DEFCODE "2R>", "_2FROMRS"
        MEMCOPY RP, AW, 4
        SHRINK RP, 2
        PUSH_AB
        NEXT

        ; R@ ::  -- x ;  x -R- x )
        ; copy top of return stack to data stack
        DEFCODE "R@", "DUPRS"
        MEMCOPY RP, AW, 2
        PUSH_A
        NEXT

        ; 2R@ ::  -- x y ;  x y -R- x y)
        ; copy top of return stack to data stack keeping order
        DEFCODE "2R@", "_2DUPRS"
        MEMCOPY RP, AW, 4
        PUSH_AB
        NEXT

; ---------------------------------------------------------------------
; Memory

        ; STORE :: x adr --
        ; store x @ adr
        DEFCODE "!", "STORE", , POP_AB
        CPYWIW BW, AW
        NEXT

        ; FETCH :: adr -- x
        DEFCODE "@", "FETCH", , POP_A
        CPYIWW AW, AW
        PUSH_A
        NEXT

        ; ADDSTORE :: x adr --
        ; (adr) += x
        DEFCODE "+!", "ADDSTORE", , POP_AB
        ADDIWWIW AW, BW, AW
        NEXT

        ; SUBSTORE :: x adr --
        ; (adr) -= x
        DEFCODE "-!", "SUBSTORE", , POP_AB
        SUBIWWIW AW, BW, AW
        NEXT

        ; C! :: x adr --
        ; store x => adr
        DEFCODE "C!", "STOREBYTE", , POP_AB
        lda BW
        sta (AW)
        NEXT

        ; C@ :: adr -- x
        ; fetch adr => x
        DEFCODE "C@", "FETCHBYTE", , POP_A
        lda (AW)
        SETWA AW
        PUSH_A
        NEXT

;TODO  C@C! (CCOPY), CMOVE

; ---------------------------------------------------------------------
; I/O

        ; KEY :: -- c
        DEFCODE "KEY"
        jsr key_
        SETWA AW
        PUSH_A
        NEXT

key_:
    .proc _key
        EQUWC nsrcbuf_value, 0
        bne nxtbuf
        SETWC CFA, _refill-2    ; set CFA pointing to implied DOCOL
        jsr DOCOL               ; execute as forth word
        SHRINK SP               ;TODO ignore any error for now
        CPYWW _srcbuf_value, SRCBUFP
nxtbuf: INCW _IN_value
        DECW nsrcbuf_value      ; stomps acc
        lda (SRCBUFP)
        INCW SRCBUFP
        rts
_refill:  .word REFILL, EXIT_RTS
    .endproc

        ; DEFER'd definition of REFILL that we'll patch in bootstrap
        ; -- success
        DEFWORD "REFILL"
        .word NOOP, EXIT

        ; EMIT :: c --
        DEFCODE "EMIT", , , POP_A
        lda AW
        jsr emit_
        NEXT
emit_:  sta $f001   ; magic monitored write for PUTC
        rts

        ; TELL :: sptr len --
        DEFCODE "TELL", , , POP_AB
    .proc _tell
loop:   EQUWC AW, 0
        beq done
        lda (BW)
        jsr emit_
        INCW BW
        DECW AW
        bra loop
done:   NEXT
    .endproc

        ; WORD :: -- sptr len
        ; read a word from input
        DEFCODE "WORD"
    .proc _word
skipws: jsr key_
        cmp #$5c    ; backslash
        bne nocomment
skipcomment:        ; eat characters until end of line
        jsr key_
        cmp #$0a    ; LF
        bne skipcomment
nocomment:
        cmp #$21    ; space + 1
        bmi skipws
        ldy #0
store:  sta WORDBUF,y
        iny
        phy
        jsr key_    ; this can stomp lots of stuff during refill
        ply
        cmp #$21
        bpl store
        tya
        SETWA AW
        SETWC BW, WORDBUF
        PUSH_AB
        NEXT
    .endproc

        ; NUMBER :: sptr len -- value err
        ; parse a number from a string
        ; where err is 0 on success else # of unconverted chars
        DEFCODE "NUMBER", , , POP_AB
        lda AW              ; single byte length
        sta LEN
        CPYWW BW, AW
        jsr parseint        ; (AW, LEN) => (BW = parsed value, ERR)
        lda ERR
        SETWA AW            ; number of unconverted chars (0 => success)
        PUSH_AB
        NEXT

        ; N>$ :: x -- sptr len
        DEFCODE "N>$", "N2STRN", , POP_A
        lda BASE_value
        jsr fmtint
        SETWC BW, FMTBUF
        lda LEN
        SETWA AW
        PUSH_AB
        NEXT

        ; FIND :: sptr len -- hfa | nul
        DEFCODE "FIND", , , POP_AB
    .proc _find
        lda AW
        sta LEN             ; single byte length
        CPYWW LATEST_value, AW
next:   ldy #2              ; AW is word to check against BW/LEN
        lda (AW),y
        and #(F_HIDDEN | F_LENMASK)  ; get length, let hidden flag fail match
        cmp LEN
        bne differ
        lda #3              ; length ok, check actual string
        ADDWAW AW, UW       ; UW is start of name
        ldy #0
loop:   lda (UW),y          ; strncmp
        cmp (BW),y
        bne differ
        iny
        cpy LEN
        beq done            ; found!
        bra loop
differ: CPYIWW AW, AW       ; check prev word
        EQUWC AW, 0         ; AW is curr ptr in linked lst
        bne next
done:   PUSH_A
        NEXT
    .endproc

        ; >CFA :: link -- cptr
        ; convert a header field address, e.g. a link from find,
        ; to the corresponding codeword field address
        DEFCODE ">CFA", "TCFA", , POP_A
        ldy #2
        lda (AW),y
        and #F_LENMASK
        clc
        adc #3
        ADDWAW AW, AW
        PUSH_A
        NEXT

        ; >DFA :: link -- dptr
        ; convert a header field address to the word's data field address (code)
        ; our words all have a two-byte codeword so >DFA is just >CFA + 2
        DEFWORD ">DFA", "TDFA"
        .word TCFA, INC2, EXIT

        ; >HFA :: adr -- link | 0
        DEFCODE ">HFA", "THFA", , POP_A
        ; find the first word whose header field is at or before adr
; TODO must be within 32+2+1+2 assuming adr is a cfa or dfa
        ; the second call is a no-op: someadr >HFA >HFA
        ; this is also a no-op: link >CFA >HFA
    .proc _thfa
        CPYWW LATEST_value, UW  ; latest (highest) HFA
loop:   SUBWWW AW, UW, VW   ; VW = AW - UW
        bcs done            ; no borrow, so AW >= UW
        CPYIWW UW, UW       ; follow linked list to prev HFA
        bra loop            ; eventually UW = 0 which will break the loop
done:
        CPYWW UW, AW        ; push match or 0
        PUSH_A
        NEXT
    .endproc

        ; CREATE :: sptr len --
        ; create the header for new word defintion, updates HERE, LATEST
        DEFCODE "CREATE", , , POP_AB
    .proc _create
        lda AW
        sta LEN
        ; stash the heap ptr in zp
        CPYWW HERE_value, AW
        ; write pointer back to prev word
        CPYWIW LATEST_value, AW
        ; update latest pointer
        CPYWW AW, LATEST_value
        ; skip link word
        lda #2
        ADDWAW AW, AW
        lda LEN
        sta (AW)                ; write the length
        INCW AW
        ldy #0
copy:   cpy LEN                 ; write the name
        beq done
        lda (BW),y
        sta (AW),y
        iny
        bne copy
done:   tya                     ; Y is LEN after loop
        ADDWAW AW,AW
        CPYWW AW, HERE_value    ; update the heap pointer
        NEXT
    .endproc

        ; , :: ptr --
        ; append a codeword to HERE, updates HERE
        DEFCODE ",", "COMMA", , POP_A
        CPYWW HERE_value, UW
        CPYWIW AW, UW
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
        CPYWW LATEST_value, UW
        ldy #2
        lda (UW),y
        eor #F_IMMED
        sta (UW),y
        NEXT

        ; ?IMMEDIATE link :: 0 | 1
        ; TODO this should get split into proper ?IMMEDIATE plus separate state check
        ; return 1 if word has F_IMMED set or state is 0 (imm mode)
        DEFCODE "?IMMEDIATE", "QIMMEDIATE", , POP_A
    .proc _qimmediate
        ldy #2
        lda (AW),y
        dey             ; y = 1
        and #F_IMMED
        bne immset      ; !=0 means F_IMMED is set
        lda STATE_value
        beq immset      ; or state = 0 (imm mode)
        dey             ; otherwise y = 0
immset: sty AW
        stz AW+1
        PUSH_A
        NEXT
    .endproc

        ; HIDDEN :: addr --
        ; toggle F_HIDDEN on word at addr
        DEFCODE "HIDDEN", , , POP_A
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
;TODO write commment => for compile-only identical to LIT ? in jonesforth it's just push v pushl
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
        ADDWIWW PC, PC, UW  ;TODO unsafe to write direct back to PC
        CPYWW UW, PC
        NEXT

        ; 0BRANCH :: flag --
        ; branch by offset in next word if flag is 0, else continue
        DEFCODE "0BRANCH", "ZBRANCH", , POP_A
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
        .word DUP, ZBRANCH      ; copy and test link
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
        .word _2DUP, NUMBER    ; sptr len -- sptr len value err
        .word ZBRANCH
        .word is_number - *
        .word DROP, TELL      ; drop the value and show the unknown string
        LSTRN " ?\n"
        .word TELL, EXIT
is_number:
        .word NROT, _2DROP   ; drop the extra sptr, len leaving -- value
        .word STATE, FETCH
        .word ZBRANCH
        .word lit_immed - *
        .word LIT, LIT, COMMA, COMMA     ; compile LIT, <value>
lit_immed:
        .word EXIT      ; just leave value on the stack
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
        SETWC CFA, phrase
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

        SETWC AW, 1
        SETWC BW, 2
        SETWC CW, 3

        lda #6
        sta NPUSH
        ldx #2
        jsr syncstack

        EXPECTPOP 2, "push>pop"
        EXPECTPOP 3, "push>pop2"

        SETWC AW, 99
        PUSHC SP, 7
        PUSHC SP, 42            ; stack is ... 7 42
        lda #2
        sta NPUSH
        ldx #4
        jsr syncstack        ; push 2 pop 4 should set BW to 42
        EXPECTWC AW, 99, "pop>push"
        EXPECTWC BW, 42, "pop>push1"
        EXPECTPOP 7, "pop>push2"

        EXPECTWC SP, __STACK_START__, "stk safe 1"

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

        EXPECTWC SP, __STACK_START__, "stk safe 2"
.endif

