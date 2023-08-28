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
	echo "m test_report/400 ascii" >> forth-test.mon
	echo "m test_report/4" >> forth-test.mon

forth.mon: forth.bin
	python sym2py65.py < forth.sym | sort | egrep -v 'MACRO_SYMBOL|__link__' > forth.mon
	echo "width 72" >> forth.mon
	echo "goto forth" >> forth.mon

tests: forth-test.bin forth-test.mon forth-test.sym
	py65mon -m 65C02 -l forth-test.bin -b forth-test.mon -a `grep __MAIN_START__ forth-test.sym | cut -d' '  -f2`

forth: forth.mon
	py65mon -m 65C02 -l forth.bin -b forth.mon -a `grep __MAIN_START__ forth.sym | cut -d' '  -f2`

clean:
	rm -f *.bin *.lst *.map *.o *.mon
