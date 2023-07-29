
Setup
    conda create -n forth python=3.9
    pip install -e ../py65/


    conda activate forth

    ../dasm/bin/dasm word16.asm -oword16.bin -R -lword16.lst -sword16.sym -T1 -f3 -DTESTS
    py65mon --batch test_word16.mon

    python dasmsym2json.py < word16.sym > word16.sym.json
    python ../pydisass6502/disass.py -i word16.bin -o word16.disasm -e word16.sym.json -s 0x1000

    ../dasm/bin/dasm forth.asm -oforth.bin -R -lforth.lst -sforth.sym -T1 -f3 -DTESTS
    py65mon --batch test_forth.mon

To get a clean disassembly:

    python dasmsym2json.py < forth.sym > forth.sym.json
    python ../pydisass6502/disass.py -i forth.bin -o forth.disasm -e forth.sym.json -s 0x1000

