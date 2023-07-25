; ---------------------------------------------------------------------
; bootstrap a simple 16-bit Forth for 6502 based on jonesforth
; see https://github.com/nornagon/jonesforth/blob/master/jonesforth.S

    INCLUDE "word16.asm"

  MAC INCPC
    ADDWCW PC, #2, PC
  ENDM

  MAC NEXT
    JMP _NEXT
  ENDM

    seg code
    org $2000

DOCOL:
    ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
    ; AX contains the address of the codeword, so we just need to push the current PC
    ; which tells us what to do after we're finished this word,
    ; and continue executing words at AX+2
    PUSHW RP, PC
    ADDWCW AX, #2, PC
    ; fall through to NEXT
_NEXT:
    ; indirect threading: PC has the address of the next codeword's address
    ; the codeword contains the address of adapter/interpreter used to execute the word
    ; we jump to the target codeword's code after incrementing PC
    ; leaving AX containing the address of the codeword
    CPYIWW PC, AX       ; now AX contains the codeword's address
    INCPC               ; next codeword to execute
    ; we need to jump to the address stored at the address stored in AX
    CPYIWW AX, BX       ; now BX has the interpreter address
    JMP (BX)            ; run the word's interpreter code


    align $100
MAIN:
    ; initial entry point
    SETWC SP, #$400
    SETWC RP, #$600
    SETWC PC, #cold_start
    NEXT

QUIT:
    brk

cold_start:
    WORD LIT
    WORD #3
    WORD QUADRUPLE
    WORD QUIT

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
{2},"_name"
    WORD link
link set .here
    BYTE [.name_end - .name_start] | {3}
.name_start:
    BYTE {1}
.name_end:
{2}
  ENDM

  MAC DEFWORD
    ; define a forth word
    _DEFWORD {1}, {2}, {3}
    WORD DOCOL  ; Forth words use codeword DOCOL
    ; sequence of word pointers should follow, ended by EXIT
  ENDM

  MAC DEFCODE
    ; define a native word
    _DEFWORD {1}, {2}, {3}
    WORD .code
.code:
    ; native assembly should follow, ended by NEXT
  ENDM

; ---------------------------------------------------------------------
; native word definitions

    ; when we've finished a Forth word, we just recover the old PC and proceed
    DEFCODE "EXIT", EXIT, 0
    POPW RP, PC
    NEXT

    ; drop top element of stack
	DEFCODE "DROP",DROP,0
    SUBWCW SP, #2, SP
	NEXT

	defcode "SWAP",SWAP,0
    POPW SP, AX
    POPW SP, BX
    PUSHW SP, AX
    PUSHW SP, BX
	NEXT

    DEFCODE "DUP", DUP, 0
    POPW SP, AX
    PUSHW SP, AX
    PUSHW SP, AX
    NEXT


    ; push the next word as a constant and skip it
    DEFCODE "LIT", LIT, 0
    PUSHIW SP, PC   ; copy literal to stack
    INCPC           ; and skip it
    NEXT

    DEFCODE "+", PLUS, 0
    POPW SP, AX
    POPW SP, BX
    ADDWWW AX, BX, CX
    PUSHW SP, CX
    NEXT

; ---------------------------------------------------------------------
; forth word definitions

    DEFWORD "DOUBLE", DOUBLE, 0
    WORD DUP
    WORD PLUS
    WORD EXIT

    DEFWORD "QUADRUPLE", QUADRUPLE, 0
    WORD DOUBLE
    WORD DOUBLE
    WORD EXIT