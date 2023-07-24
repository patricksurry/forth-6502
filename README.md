
Setup
    conda create -n forth python=3.9
    pip install -e ../py65/


    conda activate forth

    ../dasm/bin/dasm word16.asm -oword16.bin -lword16.lst -sword16.sym -f3 -DTESTS
    py65mon --batch test_word16.mon


    ../dasm/bin/dasm forth.asm -oforth.bin -lforth.lst -sforth.sym -f3

https://en.wikipedia.org/wiki/Pearson_hashing