; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    .setcpu "65C02"
    .feature c_comments
    .feature underline_in_numbers
    .feature string_escapes

__FORTH_VERSION__ = 2

    .import __STACK_START__
    .import __STACK_SIZE__

__RETSTACK_START__ = __STACK_START__ - __STACK_SIZE__

    .include "word16.asm"

    .macro INCPC
        ADDWCW PC, 2, PC
    .endmac

    ; copy <256 bytes from const src to tgt address
    ; will fail if ranges overlap with src < tgt; cf CMOVEUP/DN
    .macro MEMCOPY src, tgt, length
    .local loop
        ldy #(length-1)
loop:   lda src, y
        sta tgt, y
        dey
        bpl loop
    .endmac

    .macro W2XY src
        ldx src
        ldy src+1
    .endmac
    .macro XY2W tgt
        stx tgt
        sty tgt+1
    .endmac

REG_NONE = 0
REG_A = 1
REG_AB = 2
REG_ABC = 3
REG_ABCD = 4

pushreg .set REG_NONE

/*
    Memory map looks something like this (mostly defined in ld65.cfg):

       0 -  $ff    zeropage
    $100 - $1ff    6502 stack
    $200 - $400    input buffer (512 bytes), defined in bootstrap.f
    $400 - $420    WORDBUF - 32 char buffer for "WORD"
     ... - $7FF    return stack (RP), max 496 cells (~1K bytes)
     ... - $FFF    data stack (SP), max 1024 cells (2K bytes)
    $1000 - ...    code, (test), data
    $C000 - ...    (optional unit test report)
*/

WORDBUF = $400

    .zeropage

CFA:    .res 2      ; address of current word's codeword field
NPUSH:  .res 1      ; number of register bytes pushed by native word (see syncstack)
MUTFLG: .res 1      ; see VALUE and VALUE' - return address or value?
UW:     .res 2      ; tmp registers not mapped to stack head
VW:     .res 2
SRCSZ:  .res 2      ; size of current source buffer (nb SRCSZ, SRCBUF must be contig 4 bytessrcs)
SRCBUF: .res 2      ; pointer to current source buffer
SRCP:   .res 2      ; tmp pointer in source buffer (SRCBUF + >IN) used by KEY

    .code

DOCOL:
        ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
        ; CFA contains the address of the codeword (via prev NEXT)
        ; so push the current PC which is where to resume after finishing this word,
        ; and then continue executing words at CFA+2
        PUSHW RP, PC
        ADDWCW CFA, 2, PC
        ; fall through to NEXT, and eventually matching EXIT pops the old PC
_NEXT:
        ; indirect threading: PC has the address of the next codeword's address
        ; the codeword contains the address of adapter/interpreter used to execute the word
        ; we jump to the target codeword's code after incrementing PC
        ; leaving CFA containing the address of the codeword
        CPYIWW PC, CFA      ; now CFA contains the codeword's address (cf DOCOL)
        INCPC               ; codeword to execute after we finish this one
        ; we need to jump to the address stored at the address stored in AW
        CPYIWW CFA, UW      ; copy interpeter address to zeropage in UW so we can indirect
nextword:                   ; useful breakpoint to step through words
        jmp (UW)            ; run the word's interpreter code

    .macro NEXT
        jmp .ident(.sprintf("next%d", ::pushreg))
::pushreg .set ::REG_NONE
    .endmac

; helper to call forth words from native code as subroutines
; usage: set tgtword to CFA of target word, then jsr callword
tgtword:
        .word NOOP              ; will contain the word to execute
        .word * + 2             ; points to the following anonymous native word

        .word syncstack0        ; native word interpreter to finalize stack and continue
        POPW RP, PC             ; recover the PC
        rts                     ; return to callword's caller

callword:
        PUSHW RP, PC            ; stash the current PC
        SETWC PC, tgtword       ; point at the word to execute
        NEXT                    ; go

/*
lazy stack macros

We can take advantage of the fact that nly native words manipulate the stack directly.
Most work by popping from the stack into fixed zeropage registers (AW, BW, ...),
manipulating those registers, and then pushing back to the stack.
We can optimize for space (and sometimes speed) by lazily synchronizing
between the stack and registers.  We map registers to the head
of the stack as shown.  The stack grows downward from S0 (typically #$1000)
with SP pointing at the latest valid cell on the stack.  Thus (SP, SP+1)
are the low and high bytes of that cell and SP=S0 represents an empty stack.

	+------+------+------+-....-+------+------+------+------+------+
    |      |      |      |      |      |  DW  |  CW  |  BW  |  AW  |
	+------+------+------+-....-+------+------+------+------+------+
    ^                                                              ^
    S0                                                             SP

On exit each native word notes how many registers to push to the stack,
in reverse order so that AW ends up as the head.  For example PUSH_ABCD implies
a stack like that shown above.  However, we don't actually push anything, and
simply leave the results in the registers.
Eventually we encounter another native word, which requests
the registers it wants to pop (in order AW, BW, ...) as part of the DEFCODE header.
This generates a codeword interpreter pointing to syncstack<#reg>
which synchronizes between registers and stack.
For example, if we've previously pushed AW and now want to pop AW, BW then
AW is unaffected and we need to pop a single element from the stack into BW.
Or vice versa if we've previously pushed AW and BW and now want to pop AW,
we only need to push BW onto the current stack, potentially avoiding a bunch of stack juggling.
Within the native word the stack and registers are always consistent with what it requested
regardless of which native or compiled words went before.

Note that the push/pop behavior of native words is declared statically,
so any dynamic stack behavior must be implemented explicitly in the word.
See for example ?DUP ( x -- 0 | x x ) which is declared to push a single register
with the conditional push written explicitly.

Further note that we mustn't use the native word registers AW ... EW outside of native words.
It's also bad form to use registers that we aren't touching, e.g. a word with POP_A ... PUSH_AB
shouldn't use CW, DW, etc.   (This might not actually fail now but would be problematic if
we extended the lazy sync across multiple words.)

TODO - check for underflow/overflow ifdef TESTS

*/

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

nextp:  stx NPUSH
        bra _NEXT

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


syncstack4:
        ldx #REG_ABCD * 2
        bra _syncstack
syncstack3:
        ldx #REG_ABC * 2
        bra _syncstack
syncstack2:
        ldx #REG_AB * 2
        bra _syncstack
syncstack1:
        ldx #REG_A * 2
        bra _syncstack
syncstack0:
        ldx #0
    .proc _syncstack
        ; syncstack is used as an interpreter for native (DEFCODE) words
        ; These are the only words that directly manipulate the data stack
        ; so we can maintain a mapping between the registers AW, BW, ... DW
        ; and the head of the stack (AW at the head, BW next etc)
        ; an operation declares it wants to push registers back to the stack
        ; before calling NEXT, e.g. PUSH_AB.
        ; This sets NPUSH as the # of register bytes to push
        ; When the next native (DEFCODE) word is called, it declares the
        ; number of stack bytes to pop as an optional argument to DEFCODE,
        ; e.g. DEFCODE ... POP_A.   This jumps to syncstack0-4 which sets X = NPOP
        ; and we do the appropriate juggling to match registers and stack
        ; which often avoids us having to write registers back to the stack.
        txa         ; A = X = NPOP
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
        ADDWCW CFA, 2, CFA
        jmp (CFA)   ; continue with native code at (CFA),2
    .endproc

; we store forth CONSTANT, VARIABLE and VALUE in the word following a JSR setaw(const|var|val)
; which sets the carry bit to indicate whether to return the value (clc) or value pointer (sec)
; normally a VALUE behaves like a constant but TO uses the hlper (toggle-mutate)
; to temporarily swap the clc <-> sec in setawval so that we can set it like a variable

bymut:  lda MUTFLG          ; set by VALUE' and reset below
        beq byval           ; VALUE normally behaves like CONSTANT
        stz MUTFLG          ; behave like a VARIABLE once and reset flag
byref:  ADDWCW CFA, 2, UW   ; copy CFA+2 by reference
        bra bysync
byval:  ldy #2              ; copy CFA+2 by value
        lda (CFA),y
        sta UW
        iny
        lda (CFA),y
        sta UW+1
bysync: SETWC CFA, byend-2
        bra syncstack0      ; POP_NONE and resume at CFA+2

byend:  CPYWW UW, AW
        PUSH_A
        NEXT

; ---------------------------------------------------------------------
; main entry point to run forth
forth:  cld
        stz MUTFLG
        SETWC SP, __STACK_START__   ; return stack cleared in quit
        SETWC PC, cold_start
        NEXT

cold_start:
        .word LIT, bootstrap
        .word LIT, bootstrap_end - bootstrap
        .word SOURCESET
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
    variations on this structure.  All are based on _DEFHEADER which creates
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

    .macro _DEFHEADER name, label, flags

    .ifnblank flags
        .if flags & (~(F_IMMED|F_HIDDEN))
            .error .sprintf ("_DEFHEADER %s: invalid flags", name)
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
        _DEFHEADER name, label, flags
    .ifblank popreg
        .word syncstack0
    .else
        .word .ident(.sprintf("syncstack%d", popreg))
    .endif
        ; followed by native assembly, ended by NEXT
    .endmac

    .macro DEFWORD name, label, flags
        _DEFHEADER name, label, flags
        .word DOCOL     ; Forth words use codeword DOCOL
        ; followed by list of word pointers, ended by EXIT
    .endmac

    .macro _DEFCV name, label, value, mode
        ; define a constant (mode=0), variable (mode=1) or value (mode=2)
        _DEFHEADER name, label
    .if mode = 0
        .word byval
    .elseif mode = 1
        .word byref
    .else
        .word bymut
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
    .endmac

    .macro DEFCONST name, label, value
        ; define word that pushes a constant to stack
        _DEFCV name, label, value, 0
    .endmac

    .macro DEFVAR name, label, value
        ; define word that pushes address of a variable to stack
        _DEFCV name, label, value, 1
    .endmac

    .macro DEFVALUE name, label, value
        ; define a mutatable constant
        _DEFCV name, label, value, 2
    .endmac

    .code

; ---------------------------------------------------------------------
; core constants exposing asm symbols

        DEFCONST "VERSION",, __FORTH_VERSION__ ; current version of this forth
        DEFCONST "S0",, __STACK_START__        ; top of param stack
        DEFCONST "R0",, __RETSTACK_START__     ; top of return stack
        DEFCONST "DOCOL", "_DOCOL", DOCOL      ; pointer to DOCOL
        DEFCONST "F_IMMED", "_F_IMMED", F_IMMED		; Flag values
        DEFCONST "F_HIDDEN", "_F_HIDDEN", F_HIDDEN
        DEFCONST "F_LENMASK", "_F_LENMASK", F_LENMASK

; ---------------------------------------------------------------------
; core variables

        DEFVAR "STATE"                          ; immediate = 0 / compile != 0
        DEFVAR "LATEST",, NOOP_header        ; head of our linked word list
        DEFVAR "HERE",, bootstrap            ; overwrite bootstrap text
        DEFVAR "BASE",, 10                   ; radix for printing and reading numbers
        DEFVAR ">IN", "_IN", 0                ; current position in SOURCE

        DEFVALUE "SOURCE-ID", "SOURCE_ID"

    .code

; ---------------------------------------------------------------------
; native word definitions

        ; EXIT :: adr -R-
        ; when we've finished a Forth word, we just recover the old PC and proceed
        DEFCODE "EXIT"
        POPW RP, PC
        NEXT

        ; EXECUTE :: xt --
        ; execute the execution token (codeword) on the stack, cf NEXT
        ; we don't increment PC since target word's NEXT will do it
        DEFCODE "EXECUTE", , , POP_A
        CPYWW AW, CFA   ; CFA contains the address of the codeword (required for DOCOL)
        CPYIWW CFA, UW  ; Copy interpreter address to zeropage so we can indirect
        jmp (UW)        ; jump to the interpreter

        ; DROP :: x --
        DEFCODE "DROP", , , POP_A
        NEXT

        ; SWAP :: x y -- y x
        DEFCODE "SWAP", , , POP_AB
        W2XY AW
        CPYWW BW, AW
        XY2W BW
        PUSH_AB
        NEXT

        ; DUP :: x -- x x
        DEFCODE "DUP", , , POP_A
        CPYWW AW, BW
        PUSH_AB
        NEXT

        ; OVER :: x y -- x y x
        DEFCODE "OVER", , , POP_AB
        W2XY BW
        XY2W CW
        CPYWW AW, BW
        XY2W AW
        PUSH_ABC
        NEXT

        ; ROT :: x y z -- y z x
        DEFCODE "ROT", , , POP_ABC
        W2XY AW
        CPYWW CW, AW
        CPYWW BW, CW
        XY2W BW
        PUSH_ABC
        NEXT

        ; -ROT :: x y z -- z x y
        DEFCODE "-ROT", "NROT", , POP_ABC
        W2XY AW
        CPYWW BW, AW
        CPYWW CW, BW
        XY2W CW
        PUSH_ABC
        NEXT

        ; 2DROP :: x y --
        DEFCODE "2DROP", "_2DROP", , POP_AB
        NEXT

        ; 2DUP :: x y -- x y x y
        DEFCODE "2DUP", "_2DUP", , POP_AB
        MEMCOPY AW, CW, 4
        PUSH_ABCD
        NEXT

        ; 2SWAP :: x y z w -- z w x y
        DEFCODE "2SWAP", "_2SWAP",  , POP_ABCD
        MEMCOPY AW, UW, 4
        MEMCOPY CW, AW, 4
        MEMCOPY UW, CW, 4
        PUSH_ABCD

        ; ?DUP :: x -- 0 | x x
        DEFCODE "?DUP", "QDUP", , POP_A
    .proc _qdup
        EQUWC AW, 0
        beq done
        PUSHW SP, AW    ; conditionally push an extra copy
done:   PUSH_A
        NEXT
    .endproc

; ---------------------------------------------------------------------
; comparisons

        ; 0= :: x -- 0 | 1
        DEFCODE "0=", "ZEQU", , POP_A
        lda #0          ; no invert

    .proc _zequ
        sta UW
        ldx #0
        EQUWC AW, 0
test:   bne not0
        inx
not0:   lsr UW          ; invert => carry flag
        txa             ; result => A
        bcc done        ; invert A => A'?
        eor #1
done:   SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0<> :: x -- 0 | 1
        DEFCODE "0<>", "ZNEQU", , POP_A
        lda #1          ; invert
        bra _zequ

/*
    compare AW op0, pushing a result 0 or 1
    we test the sign bit (N) of AW or -AW
    allowing us to implement all of <, >=, >, <=

    op  calc    invert  negate
    <0   AW -> N     0       0
    >   -AW -> N     0       1
    >=   AW -> N'    1       0
    <=  -AW -> N'    1       1
*/

_cmp_lt = %00       ; bit 1 is invert, bit 0 is negate
_cmp_gt = %01
_cmp_ge = %10
_cmp_le = %11

        ; 0< :: x -- 0 | 1
        ; this tests if x < 0, ie. x 0< is the same as x 0 <
        DEFCODE "0<", "ZLT", , POP_A
        lda #_cmp_lt

    .proc _cmp0
        lsr                     ; bit 0 (negate) => carry, bit 1 (invert) => 0 or 1
        sta UW                  ; invert flag
        lda AW+1
        tay
        ora AW
        bne not0
        clc                     ; don't negate if zero
not0:   ldx #0
        tya                     ; A has hi byte with sign bit
        bcc chksgn              ; negate?
        eor #%10000000          ; flip the sign bit
chksgn: bpl ispos               ; branch on N clear
        inx                     ; X=N
ispos:  txa
        eor UW                  ; invert?
        SETWA AW
        PUSH_A
        NEXT
    .endproc

        ; 0> :: x -- 0 | 1
        DEFCODE "0>", "ZGT", , POP_A
        lda #_cmp_gt
        bra _cmp0

        ; 0<= :: x -- 0 | 1
        DEFCODE "0<=", "ZLE", , POP_A
        lda #_cmp_le
        bra _cmp0

        ; 0>= :: x -- 0 | 1
        DEFCODE "0>=", "ZGE", , POP_A
        lda #_cmp_ge
        bra _cmp0

        ; = :: x y -- 0 | 1
        DEFCODE "=", "EQU", , POP_AB
        lda #0         ; no invert

    .proc _equ
        sta UW
        ldx #0
        EQUWW AW, BW
        bra _zequ::test
    .endproc

        ; <> :: x y -- 0 | 1
        DEFCODE "<>", "NEQU", , POP_AB
        lda #1         ; invert
        bra _equ

; B op A is equivalent to B-A op0

        ; < :: x y -- 0 | 1
        DEFCODE "<", "LT", , POP_AB

        lda #_cmp_lt

    .proc _cmp
        tax
        SUBWWW BW, AW, AW
        txa
        bra _cmp0
    .endproc

        ; > :: x y -- 0 | 1
        DEFCODE ">", "GT", , POP_AB
        lda #_cmp_gt
        bra _cmp

        ; <= :: x y -- 0 | 1
        DEFCODE "<=", "LE", , POP_AB
        lda #_cmp_le
        bra _cmp

        ; >= :: x y -- 0 | 1
        DEFCODE ">=", "GE", , POP_AB
        lda #_cmp_ge
        bra _cmp

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

;TODO this should be a proper signed multiply where we xor sign bits, clear them,
; then multiply unsigned values and set overflow if sign bit is set, carry if exceed 16bits
; and apply sign bit.   This simple implementation works OK with small signed numbers
; since twos complement behaves e.g. ($10000 - x) * ($10000 - y) mod $10000 == -x * -y

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
        ADDWCW AW, 3, UW    ; skip data via strlen + 3  bytes (length word + 1 byte padding)
        lda UW
        and #$fe            ; and clear last bit to get number of full words
        sta UW
        ADDWWW PC, UW, PC   ; bump PC to word boundary past string data
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
        GROW RP, 2
        MEMCOPY AW, (RP), 4
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
        ldx #1
    .proc _2FROMRS_
        MEMCOPY (RP), AW, 4
        txa
        beq skip
        SHRINK RP, 2
skip:   PUSH_AB
        NEXT
    .endproc

        ; R@ ::  -- x ;  x -R- x )
        ; copy top of return stack to data stack
        DEFCODE "R@", "DUPRS"
        CPYIWW RP, AW
        PUSH_A
        NEXT

        ; 2R@ ::  -- x y ;  x y -R- x y)
        ; copy top of return stack to data stack keeping order
        DEFCODE "2R@", "_2DUPRS"
        ldx #0
        bra _2FROMRS_

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

        ; CMOVE :: src dst len --
        ; copy len chars from src to dst, from bottom upwards
        ; if src < dst and regions overlap, use CMOVE> instead
        ; see http://www.6502.org/source/general/memory_move.html
        DEFCODE "CMOVE", , , POP_ABC
    .proc _cmovedn
        ldy #0
        ldx AW+1
        beq rmdn
pgdn:   lda (CW),y ; move a page at a time
        sta (BW),y
        iny
        bne pgdn
        inc CW+1
        inc BW+1
        dex
        bne pgdn
rmdn:   ldx AW
        beq done
rmloop: lda (CW),y ; move the remaining bytes
        sta (BW),y
        iny
        dex
        bne rmloop
done:   NEXT
    .endproc

        ; CMOVE> :: src dst len --
        ; copy len chars from src to dst
        ; if src > dst and regions overlap, use CMOVE> instead
        DEFCODE "CMOVE>", "CMOVEUP", , POP_ABC
    .proc _cmoveup
        ldx AW+1    ; the last byte must be moved first
        clc         ; start at the final pages of src and dst
        txa
        adc CW+1
        sta CW+1
        clc
        txa
        adc BW+1
        sta BW+1
        inx          ; allows the use of BNE after the DEX below
        ldy AW
        beq nxtpg
        dey          ; move bytes on the last page first
        beq pgup
rmup:   lda (CW),y
        sta (BW),y
        dey
        bne rmup
pgup:   lda (CW),y ; handle Y = 0 separately
        sta (BW),y
nxtpg:  dey
        dec CW+1   ; move the next page (if any)
        dec BW+1
        dex
        bne rmup
        NEXT
    .endproc

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
        EQUWW SRCSZ, _IN_value
        bne nxtbuf
        SETWC tgtword, REFILL
        jsr callword            ; REFILL updates SOURCE (SRCBUF, SRCSZ) and IN>
        SHRINK SP               ;TODO we currently ignore the status flag (0 == success)
nxtbuf: ADDWWW SRCBUF, _IN_value, SRCP    ; form zeropage pointer to next char
        INCW _IN_value          ; advance >IN
        lda (SRCP)
        rts
    .endproc

        ; REFILL :: -- success
        ; DEFER'd definition of REFILL that we'll patch in bootstrap with (REFILL)
        ; sets 'srcbuf and #srcbuf, IN>
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
        jsr _word           ; returns length in AW, ptr in BW
        PUSH_AB
        NEXT

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
        phy         ; stash y since refill can stomp things
        jsr key_
        ply
        cmp #$21
        bpl store
        tya
        SETWA AW
        SETWC BW, WORDBUF
        rts
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
        jsr _find
        PUSH_A
        NEXT

    .proc _find
        lda AW
        sta LEN             ; single byte length
        CPYWW LATEST_value, AW
nxt:    ldy #2              ; AW is word to check against BW/LEN
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
        bne nxt
done:   rts
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
        ; find header field for first word at or before adr
        ; this means that the second call here is a no-op: someadr >HFA >HFA
        ; as is the call here: link >CFA >HFA
; TODO should check result is within 32+2+1+2, i.e. assuming adr is a cfa or dfa
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
        jsr _create
        NEXT

    .proc _create   ; length in AW, ptr in BW
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
        ADDWAW AW, HERE_value    ; update the heap pointer
        rts
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
        ;TODO this should get split into proper ?IMMEDIATE plus separate state check
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
        ; in jonesforth.S, this is equivalent to LIT (push(l) and skip the next word)
        ; but that means it only works in compile mode: it essentially just
        ; leaves itself as LIT which when later executed will push the next word
        ; Here we've rewritten so that it also works in immediate mode
        DEFWORD "'", "TICK", F_IMMED
    .proc _tick
        .word STATE, FETCH, ZBRANCH
        .word immed - *
        .word LIT, LIT, COMMA, EXIT
immed:  .word WORD, FIND, TCFA, EXIT        ;todo cf VALUE' ?
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


; ---------------------------------------------------------------------
; native constant, variable and value

        ; CONSTANT :: val --
        ; define a constant using next word, set to val
        DEFCODE "CONSTANT", , , POP_A
        CPYWW AW, UW    ; stash the value
        SETWC CFA, byval

_cvv:   jsr _word
        jsr _create     ; write the header
        CPYWW HERE_value, VW
        CPYWIW CFA, VW
        ADDWCW VW, 2, VW
        CPYWIW UW, VW
        ADDWCW HERE_value, 4, HERE_value
        NEXT

        ; VALUE :: val --
        ; define a value (mutable constant) using next word, set to val
        DEFCODE "VALUE", , , POP_A
        CPYWW AW, UW
        SETWC CFA, bymut
        bra _cvv

        ; VARIABLE :: --
        ; create a value using next word with default value of zero
        DEFCODE "VARIABLE"
        SETWC UW, 0             ; variables don't pull default from stack
        SETWC CFA, byref        ; copy the template
        bra _cvv

        ; VALUE' :: --
        ; make the next reference to a value behave like variable rather than constant
        DEFCODE "VALUE'", "VALUE_"
        lda #1
        sta MUTFLG
        NEXT

        ; SOURCE :: -- addr n
        DEFCODE "SOURCE"
        MEMCOPY SRCSZ, AW, 4
        PUSH_AB
        NEXT

        ; SOURCE! :: addr n --
        DEFCODE "SOURCE!", "SOURCESET", , POP_AB
        MEMCOPY AW, SRCSZ, 4
        SETWC _IN_value, 0
        NEXT

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

    .macro TESTWORD word
        SETWC tgtword, word
        jsr callword
    .endmac

    .segment "TEST"
        jmp test_forth

dummy_rts = * - 2   ; a dummy word that just returns
        rts

test_ge:        .word DOCOL, LIT, 3, LIT, 2, GE, EXIT
test_zge0:      .word DOCOL, LIT, 0, ZGE, EXIT
test_zge1:      .word DOCOL, LIT, 1, ZGE, EXIT
test_zge_1:     .word DOCOL, LIT, $10000 - 1, ZGE, EXIT
test_numberok:
        .word DOCOL
        LSTRN "48813"  ; adds LITSTRING, leading length and pad to stack word width
        .word NUMBER, EXIT
test_numberbad:
        .word DOCOL
        LSTRN "banana"
        .word NUMBER, EXIT
test_find:
        .word DOCOL
        LSTRN "-ROT"
        .word FIND, EXIT
test_thfa: .word DOCOL, LIT, SWAP, THFA, THFA, DUP, TCFA, DUP, THFA, LIT, $ff, THFA, EXIT
; expect: SWAP_header, SWAP, SWAP_header, 0

test_forth:
        SETWC SP, __STACK_START__
        SETWC RP, __RETSTACK_START__

        SETWC AW, 1
        SETWC BW, 2
        SETWC CW, 3

        lda #3 * 2
        sta NPUSH
        SETWC CFA, dummy_rts
        jsr syncstack1

        EXPECTPOP 2, "push>pop"
        EXPECTPOP 3, "push>pop2"

        SETWC AW, 99
        PUSHC SP, 7
        PUSHC SP, 42            ; stack is ... 7 42
        lda #1 * 2
        sta NPUSH
        SETWC CFA, dummy_rts
        jsr syncstack2          ; push 2 pop 4 should set BW to 42
        EXPECTWC AW, 99, "pop>push"
        EXPECTWC BW, 42, "pop>push1"
        EXPECTPOP 7, "pop>push2"

        EXPECTWC SP, __STACK_START__, "stk safe 1"

        TESTWORD VERSION
        EXPECTPOP __FORTH_VERSION__, "VERSION"

        TESTWORD test_ge
        EXPECTPOP 1, "3 >= 2"

        TESTWORD test_zge0
        EXPECTPOP 1, "0 0>="

        TESTWORD test_zge1
        EXPECTPOP 1, "1 0>="

        TESTWORD test_zge_1
        EXPECTPOP 0, "-1 0>="

        TESTWORD test_numberok
        EXPECTPOP 0, "NUMBER ok flg"
        EXPECTPOP 48813, "NUMBER ok val"

        TESTWORD test_numberbad
        EXPECTPOP 6, "NUMBER bad flg"
        EXPECTPOP 0, "NUMBER bad val"

        TESTWORD test_find
        EXPECTPOP NROT_header, "FIND -ROT"

        TESTWORD test_thfa
        EXPECTPOP 0, "0 >HFA"
        EXPECTPOP SWAP_header, ">CFA >HFA"
        EXPECTPOP SWAP, ">HFA >CFA"
        EXPECTPOP SWAP_header, ">HFA >HFA"

        EXPECTWC SP, __STACK_START__, "stk safe 2"
.endif

