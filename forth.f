\ scratch

42 VALUE FOO

FIND - could make it not case-sensitive?

SOURCE -- addr n   - start and length of buffer
SOURCE-ID -1 string (EVALUATE), 0 user, [+# fileno]
REFILL -- 0|1 - true if buffer was refilled, 0 if source-id is -1

\ https://forth-standard.org/standard/core/EVALUATE
: EVALUATE ( addr n -- )
	\ push -1 to SOURCE-IDS stack, rewind open file if possible
	\ set source to addr/n
	#srcbuf ! 'srcbuf ! 0 >IN !
	'source-id @ 1+ dup 'source-id ! -1 swap C!
;

INCLUDE  \ should rewind current file (if any) to cursor, then push another fileno on the list and start reading from that
INCLUDED

DEFER/DEFER@/DEFER!/IS

: FIB 0 1 ROT 1 BEGIN 2DUP > WHILE 1+ 2SWAP DUP ROT + 2SWAP REPEAT 2DROP NIP ;

: FIB
    DUP 0=
    IF
        EXIT
    THEN
        0 1 ROT 1 ( n 0 1 -- 0 1 n 1)
        BEGIN
            2DUP >
        WHILE
            1+ 2SWAP DUP ROT + 2SWAP
        REPEAT
        2DROP NIP
;

: FIB 0 1 ROT 0 ?DO OVER + SWAP LOOP DROP ;


: nextword ( hfa -- hfa )
	LATEST @
	BEGIN 2DUP @ < WHILE @ REPEAT NIP
;

: dumpword
	WORD FIND 					\ get word
	CR DUP . DUP ID TELL CR  	\ show hfa and name
	DUP nextword SWAP 			\ find next word ( nxthfa hfa -- )
	SPACE >CFA DUP @ . CR		\ show codeword adr
	2+
	BEGIN 2DUP >				\ show words until next word
	WHILE
		DUP @					( nxt cfa )
		SPACE DUP . >HFA
		?DUP IF ID TELL THEN CR
		2+
	REPEAT
	2DROP CR
	;


: BIG BEGIN 17 + DUP 1000 > UNTIL ;

: P2P DUP 0> IF 2 + THEN . ;


\ RECURSE makes a recursive call to word being compiled (which is hidden to FIND)
: RECURSE IMMEDIATE
	LATEST @	\ LATEST points to the word being compiled at the moment
	>CFA		\ get the codeword
	,		\ compile it
;

: :NONAME
	0 0 CREATE	( create a word with no name - we need a dictionary header because ; expects it )
	HERE @		( HERE is the address of the codeword, ie. the xt, so push it as ref to function )
	DOCOL ,     ( compile DOCOL (the codeword), note DOCOL is a constant xt for DOCOL )
	]		    ( go into compile mode )
;
