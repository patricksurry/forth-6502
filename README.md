Down a deep rabbithole via
https://ratfactor.com/forth/the_programming_language_that_writes_itself.html

Mostly this is a port of JonesForth (https://github.com/nornagon/jonesforth/blob/master/jonesforth.S)
which implements a small 16-bit Forth for the 65c02.
This is only a toy, if you want a real Forth for 65c02 try https://github.com/SamCoVT/TaliForth2

There are a couple of novelties as I learned and explored:

- lazy push/pop via zero page registers; reduces size significantly, likely helps with performance in some cases, also isolates almost all stack interaction in one subroutine which makes it relatively easy to add stack under/overflow checking if desired

- compact constant/value/variable words rather than separate native and forth implementations like Jonesforth

- integrated to mfio in py65 monitor to get readline and external file IO; could optionally swap back to the magic single-byte read byte if desired

- word16 macros are reasonably efficient, don't deal with large signed arith perfectly, cf sweet16


TODO:
clean up mfio plugin as separate py65 PR

Debugging:

look at SP, PC, HERE, LATEST
look at SRCBUF, SRCP to see what was being read last

tricky: stack underflow - syncstack could check for many cases in test mode?
non-matching immediate conditionas, e.g. missing IF for THEN


Interesting links:

ben eater breadboard 6502 https://eater.net/6502

interesting open-hw 65c02 computer    https://planck6502.com/

thinking forth https://www.forth.com/wp-content/uploads/2018/11/thinking-forth-color.pdf

naming conventions: https://github.com/ForthHub/discussion/issues/73

forth core: https://forth-standard.org/standard/core

fake 65c02 https://github.com/omarandlorraine/fake6502/

TODO
---

[x] run on C simulator - how fast?
[ ] track heatmap of code via memory accesses
[ ] IO block $f000-f100 (move mfio; implement C version)
[ ] relocatable / monitor?   could even watch for write to read-only memory tho self-modifying code and data/code intermingling makes it hard


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


make forth

2048 top of return stack
4096 top of data stack
4096 core begins
3997 bytes core
1923 bytes bootstrap


6523 align + test
24499 unused 16 bit words

123 align + test
27699 unused 16 bit words



look for repeated assembly lines (excl comments)

sed -E -e 's/[ \t]*;.*//' -e 's/^[a-z_0-9]+://' -e 's/^[ \t]+//' forth.asm | sort | uniq -c | sort -rn | head -20

look for largest words (ex: write in forth)


94638 - thru tests; go forth: 13981611 - to ready; approx 12.5s
13.8 million cycles in 12.5s  ~1.1MHz

fakeforth.c 14M cycles in ~0.11s > 125MHz