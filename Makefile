all: clean tests

SOURCES = forth.asm bootstrap.f word16.asm string.asm ld65.cfg sym2py65.py

forth-test.bin: $(SOURCES) unittest.asm
	cl65 -g --verbose --target none --config ld65.cfg --asm-define TESTS -l forth-test.lst -m forth-test.map -Ln forth-test.sym -o forth-test.bin forth.asm

forth.bin: $(SOURCES)
	cl65 -g --verbose --target none --config ld65.cfg -l forth.lst -m forth.map -Ln forth.sym -o forth.bin forth.asm

forth-test.mon: forth-test.bin
	python sym2py65.py < forth-test.sym | sort | egrep -v 'MACRO_SYMBOL|__link__' > forth-test.mon
	echo "width 72" >> forth-test.mon
	echo "goto test_main" >> forth-test.mon
	echo "m test_report/500 ascii" >> forth-test.mon
	echo "m test_report/4" >> forth-test.mon

forth.mon: forth.bin
	python sym2py65.py < forth.sym | sort | egrep -v 'MACRO_SYMBOL|__link__' > forth.mon
	echo "width 72" >> forth.mon
	echo "goto forth" >> forth.mon

fakeforth: forth.mon fakeforth.c
	set -e ;\
	ENTRY=`egrep '^al.*\sforth$$' forth.mon | cut -f2 -d\ ` ;\
	OFFSET=`grep __MAIN_START__ forth.mon | cut -f2 -d\ ` ;\
	LAST=`grep __MAIN_LAST__ forth.mon | cut -f2 -d\ ` ;\
	gcc -DOFFSET=0x$$OFFSET -DENTRY=0x$$ENTRY -DLAST=0x$$LAST -DDECIMALMODE -DCMOS6502 -I../fake6502 -o fakeforth fakeforth.c ../fake6502/fake6502.c

tests: forth-test.bin forth-test.mon forth-test.sym
	py65mon -m 65C02 -l forth-test.bin -b forth-test.mon -a `grep __MAIN_START__ forth-test.sym | cut -d' '  -f2`

forth: forth.mon
	py65mon -m 65C02 -l forth.bin -b forth.mon -a `grep __MAIN_START__ forth.sym | cut -d' '  -f2`

clean:
	rm -f *.bin *.lst *.map *.o *.mon
