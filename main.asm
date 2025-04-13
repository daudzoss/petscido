	torch = $51
	
.if 0
;;;    2^1
;;; 2^2   2^0
;;;    2^3
;;; upper 4 msb are masked off once that entry/exit connects to adjacent symbol
symchar	.text	$20		; 0: space, unoccupied spot in play area
	.text	torch		; 1: circle dead end, closes off escape
	.text	torch 		; 2: circle dead end, closes off escape
	.text	$4a		; 3: right bending upward
	.text	torch		; 4: circle dead end, closes off escape
	.text	$40		; 5: right straight leftward
	.text	$4b		; 6: up bending leftward
	.text	$71		; 7: right straight leftward teed upward
	.text	torch		; 8: circle dead end, closes off escape
	.text	$55		; 9: right bending downward
	.text	$5d		; a=10: up straight downward
	.text	$6b		; b=11: up straight downward teed rightward
	.text	$49		; c=12: left bending downward
	.text	$72		; d=13: right straight leftward teed downward
	.text	$73		; e=14: up straight downward teed leftward
	.text	$5b		; f=15: all directions inward

rot90cc	.macro			;
	and	#$0f		;inline rot90cc(uint4_t a) {
	clc			;
	adc	#$f8		; return ((a & 7) << 1) | ((a & 8) ? 1 : 0);
	rol			;
	and	#0f		;} // rot90cc()
	.endm			;
.else
;;;    2^3
;;; 2^2   2^0
;;;    2^1
;;; upper 4 msb are masked off once that entry/exit connects to adjacent symbol
symchar	.text	$20		; 0: space, unoccupied spot in play area
	.text	torch		; 1: circle dead end, closes off escape
	.text	torch 		; 2: circle dead end, closes off escape (n/a)
	.text	$4a		; 3: right bending downward
	.text	torch		; 4: circle dead end, closes off escape (n/a)
	.text	$40		; 5: right straight leftward
	.text	$49		; 6: down bending leftward
	.text	$72		; 7: right straight leftward teed downward
	.text	torch		; 8: circle dead end, closes off escape (n/a)
	.text	$4a		; 9: right bending upward
	.text	$5d		; a=10: down straight upward
	.text	$6b		; b=11: down straight upward teed rightward
	.text	$4b		; c=12: up bending leftward
	.text	$71		; d=13: right straight leftward teed upward
	.text	$73		; e=14: up straight downward teed leftward
	.text	$5b		; f=15: all directions inward

rot90cw	.macro			;
	and	#$0f		;inline rot90cw(uint4_t a) { // rotate nyb left
	clc			;
	adc	#$f8		; return ((a & 7) << 1) | ((a & 8) ? 1 : 0);
	rol			;
	and	#0f		;} // rot90cw()
	.endm			;
.endif
	
;;;    2^1 2^3
;;; 2^0       2^5
;;;    2^2 2^4
;;; upper 2 msb are used for (clockwise!) rotation angle
innsyma	.text	$1		; 0: 
	.text	$5		; 1: enters from left, closed off
	.text	$9		; 2: enters from top left, closed off
	.text	$d		; 3: enters from left, deflected up, closed off
	.text	$3		; 4: enters from bottom left, close off
	.text	$7		; 5: enters from left, deflected down, closed off
	.text	$b		; 6: enters from top left, exits bottom left
	.text	$f		; 7: enters all left sides, closed off
.if 0
	.text	$1		; 8: enters from top right, closed off
	.text	$5		; 9: enters from left, exits top right
	.text	$9		; 10: enters from top left, exits top right
	.text	$d		; 11: enters from left, exits both tops
	.text	$3		; 12: enters from bottom left, exits top right
	.text	$7		; 13
	.text	$b		; 14
	.text	$f		; 15
	.text			; 16
	.text			; 17 AD NAUSEUM; these are just the same 8 
	.text			; 18  odd values repeated 8 times! so it can be
	.text			; 19  done with an array of just 8 values and
	.text			; 20  a mask:
.endif
	
innsym	.macro			;
	and	#$07		;
	tay			;
	lda	innsyma,y	;
	.endm			;

outsyma	.text	1;,1,1,1,1,1,1,1; 000000-000111: no entries/exits in right half
	.text	$c		; 001000-001111: upper right to left half
	.text	$6		; 010000-010111: lower right to left half
	.text	$e		; 011000-011111: upper+lower right to left half
	.text	$5		; 100000-100111: right to left half
	.text	$d		; 101000-101000: right+upper right to left half
	.text	$7		; 110000-110111: right+lower right to left half
	.text	$f		; 111000-111111: all right entries to left half

outsym	.macro			;
	lsr			;
	lsr			;
	lsr			;
	tay			;
	lda	outsyma,y	;
	.endm
	
