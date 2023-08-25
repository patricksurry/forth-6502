WORDBUF - for parse
PARSEBUF - for user and file input

: source 'srcbuf ! #srcbuf @ ;

: evaluate ( addr n -- )
	#srcbuf ! 'srcbuf ! 0 >IN !
	'source-id @ 1+ dup 'source-id ! -1 swap C!
;

: refill-fileno ( fileno -- ior )
	0 >IN !
	parsebuf #parsebuf rot read-file  ( -- n ior )
	swap #srcbuf !
;

: refill ( -- flag )
	begin
		'source-id @ C@ dup dup
		-1 =
			if parsebuf 'srcbuf !  	\ evaluate? fail back to parsebuf, leave -1 as err
			else refill-fileno 		\ else try fileno => err
		then
	( source-id err )
	swap over and while 			\ err and not stdin?
		'source-id @ 1- 'source-id !	\ switch to prev source
	repeat
	not
;

FIND - not case-sensitive
KEY - could essentially readline to fill buffer and allow editing

SOURCE -- addr n   - start and length of buffer
SOURCE-ID -1 string (EVALUATE), 0 user, [+# fileno]
REFILL -- 0|1 - true if buffer was refilled, 0 if source-id is -1

INCLUDE
INCLUDED
>IN - variable with offset from start of buffer

DEFER/DEFER@/DEFER!/IS


; : FIB 0 1 ROT 1 BEGIN 2DUP > WHILE 1+ 2SWAP DUP ROT + 2SWAP REPEAT 2DROP NIP ;

: RANGE 0 ?DO I LOOP ;

: FIB 0 1 ROT 0 ?DO OVER + SWAP LOOP DROP ;


S" forth.f" FC_BUFSIZ ! FC_BUFPTR !
F_OPEN O_READ OR FC_ACTION C!
	\ trying open("forth.f", "rb") as fileno 0
FC_STATUS C@ . \ success = 0
FC_RESULT @ . \ fileno

HERE @ FC_BUFPTR ! 256 FC_BUFSIZ !
FC_RESULT @ F_READ OR FC_ACTION C!
FC_BUFPTR @ FC_RESULT @ TELL


: nextword ( hfa -- hfa )
	LATEST @
	BEGIN 2DUP @ < WHILE @ REPEAT NIP
;

: dumpword
	WORD FIND 					\ get word
	CR DUP . DUP .NAME TELL CR  \ show hfa and name
	DUP nextword SWAP 			\ find next word ( nxthfa hfa -- )
	SPACE >CFA DUP @ . CR		\ show codeword adr
	2+
	BEGIN 2DUP >				\ show words until next word
	WHILE
		DUP @					( nxt cfa )
		SPACE DUP . >HFA
		?DUP IF .NAME TELL THEN CR
		2+
	REPEAT
	2DROP CR
	;


5a49:
	1000	DOCOL
	1be7	LIT
	0000	0
	160c	2DUP
	170b	<>
	24ed	0BRANCH
	000e	+7 words
	567c	2>R
	56d0  	I
	566c	2R>
	1b7e	1+
	24c6	BRANCH
	ffee	-0x12 (-9 words)
	15f5	2DROP
	1461	EXIT
5a67:   here
	0000



0 1 5 0 ?DO


0 1 OVER => 0 1 0 + => 0 1

0 1 => 1 1 => 1 2 => 2 3

OVER + SWAP
2 1 => 3 2 => 5 3


\ comment
: SPACE 32 EMIT ;
: . N>$ TELL SPACE ;

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

\ NEGATE leaves the negative of a number on the stack.
: NEGATE 0 SWAP - ;

\ Standard words for booleans.
: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;

\ RECURSE makes a recursive call to word being compiled (which is hidden to FIND)
: RECURSE IMMEDIATE
	LATEST @	\ LATEST points to the word being compiled at the moment
	>CFA		\ get the codeword
	,		\ compile it
;

\ Control structures from jones.f for use in compiled words.
\ Jones "Making these work in immediate mode is left as an exercise for the reader."  :-)
: IF IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -	\ calculate the offset from the address saved on the stack
	SWAP !		\ store the offset in the back-filled location
;

: ELSE IMMEDIATE
	' BRANCH ,	\ definite branch to just over the false-part
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
	SWAP		\ now back-fill the original (IF) offset
	DUP		\ same as for THEN word above
	HERE @ SWAP -
	SWAP !
;


\ BEGIN loop-part condition UNTIL
\	-- compiles to: --> loop-part condition 0BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
	HERE @		\ save location on the stack
;

: UNTIL IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @ -	\ calculate the offset from the address saved on the stack
	,		\ compile the offset here
;

\ BEGIN loop-part AGAIN
\	-- compiles to: --> loop-part BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
	' BRANCH ,	\ compile BRANCH
	HERE @ -	\ calculate the offset back
	,		\ compile the offset here
;

\ BEGIN condition WHILE loop-part REPEAT
\	-- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
: WHILE IMMEDIATE
	' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;

: REPEAT IMMEDIATE
	' BRANCH ,	\ compile BRANCH
	SWAP		\ get the original offset (from BEGIN)
	HERE @ - ,	\ and compile it after BRANCH
	DUP
	HERE @ SWAP -	\ calculate the offset2
	SWAP !		\ and back-fill it in the original location
;


\ x y -- y
: NIP SWAP DROP ;

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

: BIG BEGIN 17 + DUP 1000 > UNTIL ;

: P2P DUP 0> IF 2 + THEN . ;



\ now some actual forth!
\ this text is assembled with the binary and compiled on top of itself
\ most of this is borrowed directly from jonesforth.F

\ support ( block comments )

: ( IMMEDIATE
	1		\ allowed nested parens by keeping track of depth
	BEGIN
		KEY		\ read next character
		DUP '(' = IF	\ open paren?
			DROP		\ drop the open paren
			1+		\ depth increases
		ELSE
			')' = IF	\ close paren?
				1-		\ depth decreases
			THEN
		THEN
	DUP 0= UNTIL		\ continue until we reach matching close paren, depth 0
	DROP		\ drop the depth counter
;

: :NONAME
	0 0 CREATE	( create a word with no name - we need a dictionary header because ; expects it )
	HERE @		( HERE is the address of the codeword, ie. the xt, so push it as ref to function )
	DOCOL ,     ( compile DOCOL (the codeword), note DOCOL is a constant xt for DOCOL )
	]		    ( go into compile mode )
;

: CONSTANT
	WORD		( get the name (the name follows CONSTANT) )
	CREATE		( make the dictionary entry )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		    ( append the value on the top of the stack )
	' EXIT ,	( append the codeword EXIT )
;

\ reserve n bytes of memory, n should be a multiple of 2 (cell size) at least after padding
: ALLOT		( n -- addr )
	HERE @ SWAP	( here n )
	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;

\ calculate size of n cells in bytes, e.g. 2 CELLS ALLOT
: CELLS ( n -- n ) 2 * ;

\ define a variable like  VARIABLE FOO
: VARIABLE ( -- )
	1 CELLS ALLOT	( allocate 1 cell of memory, push the pointer to this memory )
	WORD CREATE	( make the dictionary entry (the name follows VARIABLE) )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		    ( append the pointer to the new memory )
	' EXIT ,	( append the codeword EXIT )
;



; was bootstrap

\ push nonzero status if buffer is ready for read
\ later we'll override this to read another chunk from file if empty
: READY-BUF? BUFLEN @ ;

\ get a character from buffer if any, else from user
: GETC-BUF
    READY-BUF?
    IF
        BUFPTR @ 1 BUFPTR +! 1 BUFLEN -!
    ELSE
        KEY
        DUP
        EMIT    \ echo user input
    THEN
;

\ WORD can read from a buffer and try BUFFER-INPUT to refill it, else call GETCH
\ should only echo if not reading from buffer


0xF0 CONSTANT _MFCNTL           \ base address for MFCTNL
_MFCNTL 0 + CONSTANT FC_ACTION  \ I(1): request an action (write after setting other params)
_MFCNTL 1 + CONSTANT FC_STATUS  \ O(1): action status
_MFCNTL 2 + CONSTANT FC_BUFPTR  \ I(2): pointer to data buffer
_MFCNTL 4 + CONSTANT FC_BUFSIZ  \ I(2): max size of data buffer
_MFCNTL 6 + CONSTANT FC_RESULT  \ O(2): actual read/write size or open'd fileno
_MFCNTL 8 + CONSTANT FC_OFFSET  \ IO(4): 32-bit offset for seek/tell (read/write)

0x00 CONSTANT F_OPEN \ mode in low bits
0x10 CONSTANT F_READ \ fileno in low bits
0x20 CONSTANT F_WRITE
0x30 CONSTANT F_TELL
0x40 CONSTANT F_SEEK \ OR seek flags
0x80 CONSTANT F_CLOSE
0xf0 CONSTANT F_UNLINK

\ modes for open
0b00000000 CONSTANT O_READ
0b00000001 CONSTANT O_WTRUNC
0b00000010 CONSTANT O_WAPPND
0b00000011 CONSTANT O_WCREAT

\ optionally specify r/w; see python open for r+ vs w+
0b00000100 CONSTANT O_UPDATE

: OPEN-FILE \ sptr len mode -- fileno status
    -ROT FC_BUFSIZ ! FC_BUFPTR ! FC_ACTION !
    FC_RESULT @ FC_STATUS @
;

: READ-FILE \ bufptr bufsiz fileno -- rwsiz status
    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_READ OR FC_ACTION !
    FC_RESULT @ FC_STATUS 0
;

: WRITE-FILE \ bufptr bufsiz fileno -- ior )

    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_WRITE OR FC_ACTION !
    FC_STATUS 0
;

; CLOSE-FILE \ fileno -- status
    F_CLOSE OR FC_ACTION !
    FC_STATUS @
;

\ INCLUDE should rewind current file (if any) to cursor, then push another fileno on the list and start reading from that
\ KEY would then fgetc from current file until complete (reading new chunks as needed) then pop a fileno