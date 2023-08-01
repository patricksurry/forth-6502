
Setup
    conda create -n forth python=3.9
    pip install -e ../py65/


    conda activate forth

    brew install cl65

    cl65 --verbose --config ld65.cfg -l foo.lst -m foo.map -o foo.bin foo.asm

`--target none` avoids string mangling, -Ln produces symbols with -g

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