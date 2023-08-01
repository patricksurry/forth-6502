all: clean tests

forth: forth.asm word16.asm unittest.asm ld65.cfg
	cl65 -g --verbose --target none --config ld65.cfg --asm-define TESTS -l forth.lst -m forth.map -Ln forth.sym -o forth.bin forth.asm

tests: forth sym2py65.py
	python sym2py65.py < forth.sym | sort | egrep -v 'MACRO_SYMBOL|__link__' > forth.mon
	echo "width 72" >> forth.mon
	echo "goto test_main" >> forth.mon
	echo "m test_report/300 ascii" >> forth.mon
	echo "m test_report/1" >> forth.mon
	py65mon -l forth.bin -a 800 -b forth.mon

clean:
	rm *.bin *.lst *.map *.o *.mon
