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

: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;

: CR '\n' EMIT ;
: SPACE BL EMIT ;

: . N>$ TELL SPACE ;

: NOT   0= ;

\ While compiling, '[COMPILE] word' compiles 'word' if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
	WORD		\ get the next word
	FIND		\ find it in the dictionary
	>CFA		\ get its codeword
	,			\ and compile that
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

: I 2R@ DROP ;				\ current loop index; nb. top of RS is own return address
\TODO also support J, K for nested loops

: LOOP IMMEDIATE
	' 2R> , ' 1+ , 			\ ( I: -- limit start++ )
	' BRANCH ,
	SWAP HERE @ - ,  		\ branch [repeatadr - repeatbr] ( C: -- donebr )
	HERE @ OVER - SWAP !	\ write [doneadr - donebr] => donebr
	' 2DROP ,
;

: RANGE 0 ?DO I LOOP ;

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
	DROP		\ drop m SP/the depth counter
;

: / ( x y -- q ) /MOD SWAP DROP ;
: MOD ( x y -- r ) /MOD DROP ;

: DEPTH ( -- n ) S0 DSP@ - 2 / 1- ;

\ display current stack
: .S		( -- )
	'(' EMIT DEPTH N>$ TELL ')' EMIT SPACE SPACE
    DSP@
    S0 2-
	BEGIN
		2DUP <=     ( loop while current is above DSP@)
	WHILE
		DUP @ .	    ( print the stack element )
		SPACE
		2-		    ( move up )
	REPEAT
	2DROP
    CR
;

\ reserve n bytes of memory, n should be a multiple of 2 (cell size) at least after padding
: ALLOT		( n -- addr )
	HERE @ SWAP	( here n )
	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;

\ calculate size of n cells in bytes, e.g. 2 CELLS ALLOT
: CELLS ( n -- n ) 2 * ;

\ align to the next 2 byte boundary
: ALIGNED	( addr -- addr )
	1 + 1 INVERT AND	( (addr+1) & ~1 )
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
		DUP			( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		2-			( subtract 2 since we measured from the start of the length word )
		DUP DUP ALIGNED SWAP -
		HERE @ + HERE !  ( pad length to multiple of 2 bytes; but HERE could still be odd )
		SWAP !		( and back-fill the length location )
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

: ." IMMEDIATE		( -- )
	STATE @ IF	( compiling? )
		[COMPILE] S"	( read the string, and compile LITSTRING, etc. )
		' TELL ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			KEY
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;

: VALUE! ( n xt -- ) VALUE' EXECUTE ! ;

\ in immediate mode the phrase: n TO v is equivalent to: n VALUE' v !
: TO IMMEDIATE	( n -- )
	WORD FIND >CFA
	STATE @ IF		\ compiling?
		' LIT , ,		\ compile LIT and xt from stack
		' VALUE! ,		\ update its value
	ELSE
		VALUE!			\ just do it
	THEN
;

\ file handling

0xF0 CONSTANT _MFIO           \ base address for MFIO
_MFIO 0 + CONSTANT FC_ACTION  \ I(1): request an action (write after setting other params)
_MFIO 1 + CONSTANT FC_STATUS  \ O(1): action status
_MFIO 2 + CONSTANT FC_BUFPTR  \ I(2): pointer to data buffer
_MFIO 4 + CONSTANT FC_BUFSIZ  \ I(2): max size of data buffer
_MFIO 6 + CONSTANT FC_RESULT  \ O(2): actual read/write size or open'd fileno
_MFIO 8 + CONSTANT FC_OFFSET  \ IO(4): 32-bit offset for seek/tell (read/write)

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

: OPEN-FILE ( sptr len mode -- fileno status )
    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_OPEN OR FC_ACTION C!
    FC_RESULT @ FC_STATUS C@
;

: READ-FILE ( bufptr bufsiz fileno -- rwsiz status )
    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_READ OR FC_ACTION C!
    FC_RESULT @ FC_STATUS C@
;

: WRITE-FILE ( bufptr bufsiz fileno -- status )
    -ROT FC_BUFSIZ ! FC_BUFPTR ! F_WRITE OR FC_ACTION C!
    FC_STATUS C@
;

: CLOSE-FILE ( fileno -- status )
    F_CLOSE OR FC_ACTION C!
    FC_STATUS C@
;

(
	\ TODO

	FILE-POSITION ( fileno -- lo hi status )
	REPOSITION-FILE ( lo hi fileno -- status )
	DELETE-FILE ( addr n -- ior )
)

: (input-buffer) 0x200 0x200 ;		\ addr and length of fixed input buffer

8 CELLS ALLOT CONSTANT (SOURCE-IDS) 		\ stack of pending source ids
0 (SOURCE-IDS) !					\ user input is next up
-1 TO SOURCE-ID						\ current input is from eval buffer (this file in memory)

: (NEXT-SOURCE) ( -- )				\ shrink the list
	(SOURCE-IDS) DUP @ TO SOURCE-ID
	DUP 2+ SWAP 8 CELLS CMOVE
;

: REFILL-FILENO ( fileno -- ior )
	(input-buffer) ROT READ-FILE  	( -- n ior )
	(input-buffer) DROP ROT SOURCE! \ set source to bufadr n leaving -- ior
;

: (REFILL) ( -- success )
	BEGIN
		SOURCE-ID DUP -1 <>
		IF							\ try refill if not reading from eval block
			REFILL-FILENO
		THEN  						\ refill err or -1 source-id
		DUP 						\ save error flag
		SOURCE-ID AND 				\ error and not stdin? (source 0 is the last resort)
	WHILE
		DROP (NEXT-SOURCE)			\ drop the error flag, try next
	REPEAT
	NOT								\ returns success = not error
;

' (REFILL) ' REFILL DEFER!			\ inject our REFILL before this bootstrap text runs out!

\ ------------------------------------------------------

: TRUE  1 ;
: FALSE 0 ;

: NEGATE ( x -- -x ) 0 SWAP - ;
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+			( add one because of 'u' on the stack )
	2 *			( multiply by the word size )
	DSP@ +		( add to the stack pointer )
	@    		( and fetch )
;

: SPACES	( n -- )
	BEGIN
		DUP 0>		( while n > 0 )
	WHILE
		SPACE		( print a space )
		1-		( until we count down to 0 )
	REPEAT
	DROP
;

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

( c a b WITHIN returns true if a <= c and c < b )
(  or define without ifs: OVER - >R - R>  U<  )
: WITHIN
	-ROT		( b c a )
	OVER		( b c a c )
	<= -ROT 	( f b c )
	> AND
;

: MAX ( a b -- a | b )
	2DUP < IF SWAP THEN DROP
;

: MIN ( a b -- a | b )
	2DUP > IF SWAP THEN DROP
;

: UNUSED	( -- n )
	0x8000
	HERE @		( get current position in data segment )
	2 /	-		( calc mumber of words)
;

\ convert a header-field address to strn name of the word
: ID		( link -- sptr len )
	2+ DUP 1+ SWAP C@ F_LENMASK AND
;

\ show all the words in the dictionary
: WORDS
	0 LATEST @		( start at LATEST dictionary entry )
	BEGIN
		?DUP		( while link pointer is not null )
	WHILE
		\ TODO could flag or skip hidden words
		DUP ID TELL SPACE
		SWAP 1+ SWAP @		( inc count and follow link to prev word )
	REPEAT
	'(' EMIT . S" words)" TELL CR
;

." version " VERSION . CR
R0 . ." top of return stack" CR
S0 . ." top of data stack" CR
DOCOL . ." core begins" CR
' NOOP DOCOL - . ." bytes core" CR
DUP ' NOOP - . ." align & test" CR
HERE @ SWAP - . ." bytes bootstrap" CR
UNUSED . ." unused 16 bit words" CR
WORDS

\ test IO by dumping compiled words
S" dump.bin" O_WTRUNC OPEN-FILE DROP 		\ drop ior
DUP S0 HERE @ OVER - ROT WRITE-FILE DROP	\ drop ior
CLOSE-FILE DROP

." stack " .S

." ready" CR

(
	version 2
	2048 top of return stack
	4096 top of data stack
	4096 core begins
	4257 bytes core
	119 align & test
	2018 bytes bootstrap
	24323 unused 16 bit words
)