; bootstrap a simple 16-bit Forth

    INCLUDE "word16.asm"

  MAC INCPC
    ADDWCW PC, #2, PC
  ENDM

  MAC NEXT
    ; indirect threading: PC has the address of the next codeword's address
    ; the codeword contains the address of adapter/interpreter used to execute the word
    ; we jump to the target codeword's code after incrementing PC
    ; leaving AX containing the address of the codeword
    CPYIWW PC, AX       ; now AX contains the codeword's address
    INCPC               ; next codeword to execute
    ; we need to jump to the address stored at the address stored in AX
    CPYIWW AX, BX       ; now BX has the interpreter address
    JMP (BX)            ; run the word's interpreter code
    word #$0000  ; TODO DEBUG
  ENDM


    seg code
    org $2000

MAIN:
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

DOCOL:
    ; interpreter for a Forth word, which has a codeword followed by a list of codeword addresses
    ; AX contains the address of the codeword, so we just need to push the current PC
    ; which tells us what to do after we're finished this word,
    ; and continue executing words at AX+2
    PUSHW RP, PC
    ADDWCW AX, #2, PC
    NEXT

; native subroutines
    SUBROUTINE
; when we've finished a Forth word, we just recover the old PC and proceed
ECOL:
    WORD code_ECOL
code_ECOL:
    POPW RP, PC
    NEXT

    SUBROUTINE
; push the next word as a constant and skip it
LIT:
    WORD code_LIT
code_LIT:
    PUSHIW SP, PC   ; copy literal to stack
    INCPC           ; and skip it
    NEXT

    SUBROUTINE
DUP:
    WORD code_DUP
code_DUP:
    POPW SP, AX
    PUSHW SP, AX
    PUSHW SP, AX
    NEXT

    SUBROUTINE
PLUS:
    WORD code_PLUS
code_PLUS:
    POPW SP, AX
    POPW SP, BX
    ADDWWW AX, BX, CX
    PUSHW SP, CX
    NEXT

; forth dict entries

    SUBROUTINE
DOUBLE:
    WORD DOCOL
    WORD DUP
    WORD PLUS
    WORD ECOL

    SUBROUTINE
QUADRUPLE:
    WORD DOCOL
    WORD DOUBLE
    WORD DOUBLE
    WORD ECOL