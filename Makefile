all: clean tests

forth: forth.asm word16.asm unittest.asm ld65.cfg sym2py65.py
	cl65 -g --verbose --target none --config ld65.cfg --asm-define TESTS -l forth.lst -m forth.map -Ln forth.sym -o forth.bin forth.asm
	python sym2py65.py < forth.sym | sort | egrep -v 'MACRO_SYMBOL|__link__' > forth.mon
	echo "width 72" >> forth.mon
	echo "goto test_main" >> forth.mon
	echo "m test_report/400 ascii" >> forth.mon
	echo "m test_report/1" >> forth.mon

tests: forth
	py65mon -l forth.bin -b forth.mon -a `grep __MAIN_START__ forth.sym | cut -d' '  -f2`

clean:
	rm -f *.bin *.lst *.map *.o *.mon
