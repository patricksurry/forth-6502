\ comment

HERE @

: LITERAL IMMEDIATE
	' LIT ,		\ compile LIT
	,		\ compile the literal itself (from the stack)
	;

\ Define some character constants
: '\n' 10 ;
: BL   32 ; \ BL (BLank) is a standard FORTH word for space.

\ -- c  where c is the first character of the following word
: CHAR WORD DROP C@ ;

: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: CR '\n' EMIT ;
: SPACE BL EMIT ;

: . N>$ TELL SPACE ;

: NOT   0= ;

\ While compiling, '[COMPILE] word' compiles 'word' if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
	WORD		\ get the next word
	FIND		\ find it in the dictionary
	>CFA		\ get its codeword
	,		\ and compile that
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

\ limit start ?DO ... LOOP
: ?DO IMMEDIATE
	HERE @					\ save adr to loop back to ( C: -- repeatadr )
	' 2DUP , ' <> ,
	' 0BRANCH , HERE @ 0 ,	\ save loc for done offset and write placeholder value
	' 2>R ,   				\ ( C: -- repeatadr donebr )  ( I: -R- limit start )
;

: I 2R@ DROP ;				\ top elt is our own return address
\ TODO J, K

: LOOP IMMEDIATE
	' 2R> , ' 1+ , 			\ ( I: -- limit start++ )
	' BRANCH ,
	SWAP HERE @ - ,  		\ branch [repeatadr - repeatbr] ( C: -- donebr )
	HERE @ OVER - SWAP !	\ write [doneadr - donebr] => donebr
	' 2DROP ,
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

: UNLESS IMMEDIATE
	' NOT ,		\ compile NOT (to reverse the test)
	[COMPILE] IF	\ continue by calling the normal IF
;

\ support for parenthesized comments
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

: / ( x y -- q ) /MOD SWAP DROP ;
: MOD ( x y -- r ) /MOD DROP ;

: DEPTH ( -- n ) S0 DSP@ - 2 / 2- ;

\ display current stack
: .S		( -- )
	'(' EMIT DEPTH N>$ TELL ')' EMIT SPACE SPACE
    DSP@
    S0 2-
	BEGIN
		2DUP <      ( loop while current is above DSP@)
	WHILE
		DUP @ .	    ( print the stack element )
		SPACE
		2-		    ( move up )
	REPEAT
	2DROP
    CR
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

\ align to the next 2 byte boundary
: ALIGNED	( addr -- addr )
	1 + 1 INVERT AND	( (addr+3) & ~3 )
;

\ align the HERE pointer
: ALIGN HERE @ ALIGNED HERE ! ;

( C, appends a byte to the current compiled word. )
: C,
	HERE @ C!	( store the character in the compiled image )
	1 HERE +!	( increment HERE pointer by 1 byte )
;

: S" IMMEDIATE		( -- addr len )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '"' <>
		WHILE
			C,		( copy character )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		2-			( subtract 2 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		ALIGN		( round up to next multiple of 4 bytes for the remaining code )
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		SWAP 		( addr len )
	THEN
;

\ file handling

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

: READ-FILE ( bufptr bufsiz fileno -- rwsiz status )
    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_READ OR FC_ACTION C!
    FC_RESULT @ FC_STATUS C@
;

: REFILL-FILENO ( fileno -- ior )
	0 >IN !
	parsebuf #parsebuf ROT READ-FILE  ( -- n ior )
	SWAP #srcbuf !
;

: (REFILL) ( -- success )
	BEGIN
		'srcid @ C@ DUP DUP			( -- srcid srcid srcid )
		255 =						\ char -1 is 255
		IF 							\ from evaluate?
			parsebuf 'srcbuf !  	\ switch back to parsebuf, leave srcid = -1 as err
		ELSE
			REFILL-FILENO 			( srcid srcid -- srcid err )
		THEN  						\ leaving ( -- srcid err )
		SWAP OVER AND 				( -- err (srcid && err) )
	WHILE 							\ error and not stdin?
		'srcid @ 1- 'srcid !		\ switch to prev source
		DROP
	REPEAT
	NOT								\ returns success = not error
;

' (REFILL) ' REFILL DEFER!			\ inject our REFILL before the bootstrap text runs out!

: NEGATE ( x -- -x ) 0 SWAP - ;
: NIP ( x y -- y ) SWAP DROP ;

: TRUE  1 ;
: FALSE 0 ;

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

: UNUSED	( -- n )
	HERE @		( get current position in data segment )
	NEGATE		( subtract from $10000 )
	2 /			( returns number of cells )
;

S" version " TELL VERSION . CR
R0 . S" top of return stack" TELL CR
S0 . S" top of data stack" TELL CR
DOCOL . S" core begins" TELL CR
DUP DOCOL - . S" core bytes" TELL CR
HERE @ SWAP - . S" bytes compiled ok" TELL CR
UNUSED . S" unused 2-byte cells" TELL CR