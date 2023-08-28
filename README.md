TODO

- signed multiply

https://eater.net/6502
https://www.reddit.com/r/beneater/comments/15k19xp/be6502_in_an_altoid_tin/

http://rubbermallet.org/fake6502.c

signed v unsigned comparisons http://6502.org/tutorials/compare_beyond.html

test suite: https://forth-standard.org/standard/testsuite



Setup
    conda create -n forth python=3.9
    pip install -e ../py65/


    conda activate forth

    brew install cl65

    cl65 --verbose --config ld65.cfg -l foo.lst -m foo.map -o foo.bin foo.asm

`--target none` avoids .byte string mangling, -Ln produces symbols with -g

    cl65 --verbose --target none  --config ld65.cfg -g --asm-define TESTS -l word16.lst -m word16.map -Ln word16.sym -o word16.bin word16.asm

    cl65 --verbose --target none --config ld65.cfg -g --asm-define TESTS -l forth.lst -m forth.map -Ln forth.sym -o forth.bin forth.asm


    ../dasm/bin/dasm word16.asm -oword16.bin -R -lword16.lst -sword16.sym -T1 -f3 -DTESTS
    py65mon --batch test_word16.mon

    python dasmsym2json.py < word16.sym > word16.sym.json
    python ../pydisass6502/disass.py -i word16.bin -o word16.disasm -e word16.sym.json -s 0x1000

    ../dasm/bin/dasm forth.asm -oforth.bin -R -lforth.lst -sforth.sym -T1 -f3 -DTESTS
    py65mon --batch test_forth.mon

To get a clean disassembly:

    python dasmsym2json.py < forth.sym > forth.sym.json
    python ../pydisass6502/disass.py -i forth.bin -o forth.disasm -e forth.sym.json -s 0x1000


Forth resources

https://github.com/nornagon/jonesforth/blob/master/jonesforth.S
https://forth-standard.org/standard/core
https://skilldrick.github.io/easyforth/


6502 resources

https://atariwiki.org/wiki/Wiki.jsp?page=6502%20Coding%20Algorithms%20Macro%20Library
https://www.masswerk.at/6502/6502_instruction_set.html
https://www.nesdev.org/wiki/Synthetic_instructions
https://www.nesdev.org/wiki/6502_assembly_optimisations



Optimizing for speed
---

About 8s for startup in py65mon

presumably quicker in C e.g. https://github.com/omarandlorraine/fake6502/

Optimizing for size
---


/*
        .if .blank(count) && .xmatch(stack, SP)
            jsr SHRINK_SP
        .else
            ...
        .endif

SHRINK_SP:                  ; 5656 v 6168 (save 8%)
        lda #2
        ADDWAW SP, SP
        rts
*/

/*
_POPW_SP_AW:
        POPW SP, AW
        rts

.macro POPW_SP_AW
;        POPW SP, AW
        jsr _POPW_SP_AAW    ; 5400 v 6168 (save 12%, 24bytes x 32 = 768)
.endmac
*/
