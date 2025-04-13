	torch = $51
	
;;;    2^3
;;; 2^2   2^0
;;;    2^1
;;; upper 4 msb are masked off once that entry/exit connects to adjacent symbol
symchar	.text	$80|$20		; 0: space, unoccupied spot in play area
	.text	$80|torch	; 1: circle dead end, closes off escape
	.text	$80|torch	; 2: circle dead end, closes off escape (n/a)
	.text	$80|$4a		; 3: right bending downward
	.text	$80|torch	; 4: circle dead end, closes off escape (n/a)
	.text	$80|$40		; 5: right straight leftward
	.text	$80|$49		; 6: down bending leftward
	.text	$80|$72		; 7: right straight leftward teed downward
	.text	$80|torch	; 8: circle dead end, closes off escape (n/a)
	.text	$80|$4a		; 9: right bending upward
	.text	$80|$5d		; a=10: down straight upward
	.text	$80|$6b		; b=11: down straight upward teed rightward
	.text	$80|$4b		; c=12: up bending leftward
	.text	$80|$71		; d=13: right straight leftward teed upward
	.text	$80|$73		; e=14: up straight downward teed leftward
	.text	$80|$5b		; f=15: all directions inward

rot90cw	.macro			;
	and	#$0f		;inline uint8_t rot90cw(uint4_t a) { // nyb rot
	clc			;
	adc	#$f8		; return ((a & 7) << 1) | ((a & 8) ? 1 : 0);
	rol			;
	and	#$0f		;} // rot90cw()
	.endm			;
	
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
	
innsym	.macro			;static uint4_t innsyma = {1,5,9,13,3,7,11,15};
	and	#$07		;inline uint4_t innsym(uint6_t a) { // on 3 lsb
	tay			; return innsyma[a & 7];
	lda	innsyma,y	;} // innsym()
	.endm			;

outsyma	.text	1;,1,1,1,1,1,1,1; 000000-000111: no entries/exits in right half
	.text	$c		; 001000-001111: upper right to left half
	.text	$6		; 010000-010111: lower right to left half
	.text	$e		; 011000-011111: upper+lower right to left half
	.text	$5		; 100000-100111: right to left half
	.text	$d		; 101000-101000: right+upper right to left half
	.text	$7		; 110000-110111: right+lower right to left half
	.text	$f		; 111000-111111: all right entries to left half

outsym	.macro			;static uint4_t outsyma = {1,12,6,14,5,13,7,15};
	lsr			;inline uint4_t outsym(uint6_t a) {
	lsr			;
	lsr			;
	tay			; return outsym[a >> 3];
	lda	outsyma,y	;}
	.endm

*	= $1001
	.word	(+), 2055
	.null	$9e, format("%4d", start)
+	.word 0
start	lda	#$93
	jsr	$ffd2
	lda	#$0		; black
	sta	$0800		; 16's digit
	sta	$0801		; 1's digit
	sta	$0802		; outer if rot=11
	sta	$0829		; outer if rot=10
	sta	$082a		; inner (pivot point)
	sta	$082b		; outer if rot=00
	sta	$0852		; outer if rot=01
	lda	#$01
	
loop	sta	$76
	lsr
	lsr
	lsr
	lsr
	ora	#$30
	cmp	#$3a
	bcc	loop2
	clc
	adc	#$06

loop2	sta	$0c00
	lda	$76
	and	#$0f
	ora	#$30
	cmp	#$3a
	bcc	loop3
	clc
	adc	#$06
	
loop3	sta	$0c01
	lda	$76
	innsym
	bit	$76
	bpl	loop5		; rot=00 or 01
	bit	$76
	bvc	loop4		; rot=10
	
	rot90cw
	rot90cw
	rot90cw
	tay
	lda	symchar,y
	sta	$0c2a		; pivot point
	lda	$76
	outsym
	rot90cw
	rot90cw
	rot90cw
	tay
	lda	symchar,y
	sta	$0c02		; rot=11 is above
	jmp	loop7
		
loop4	rot90cw
	rot90cw
	tay
	lda	symchar,y
	sta	$0c2a		; pivot point
	lda	$76
	outsym
	rot90cw
	rot90cw
	tay
	lda	symchar,y
	sta	$0c29		; rot=10 is left
	jmp	loop7
	
loop5	bit	$76
	bvc	loop6		; rot=00
	rot90cw
	tay
	lda	symchar,y
	sta	$0c2a		; pivot point
	lda	$76
	outsym
	rot90cw
	tay
	lda	symchar,y
	sta	$0c52		; rot=01 is below
	jmp	loop7

loop6	tay
	lda	symchar,y
	sta	$0c2a		; pivot point
	lda	$76
	outsym
	tay
	lda	symchar,y
	sta	$0c2b		; rot=00 is right

loop7	jsr	$ffe4
	beq	loop7
	cmp	#$91
	beq	loop8		; crsr up = increase tile# (wrap at 64)
	cmp	#$11
	beq	loop9		; crsr dn = decrease tile# (wrap at 0)

	ldy	#$20
	sty	$0c02		; outer if rot=11
	sty	$0c29		; outer if rot=10
	sty	$0c2a		; inner (pivot point)
	sty	$0c2b		; outer if rot=00
	sty	$0c52		; outer if rot=01

	cmp	#$9d
	beq	loopa		; crsr left = rotate ccw
	cmp	#$1d
	beq	loopb		; crsr right = rotate cw
	and	#$6f
	cmp	#$51		; q = done
	bne	loop7
	rts

loop8	lda	$76
	clc
	adc	#$1
	and	#$3f
	beq	+
	jmp	loop
+	lda	#$01
	jmp	loop

loop9	lda	$76
	sec
	sbc	#$1
	and	#$3f
	beq	+
	jmp	loop
	lda	#$3f
	jmp	loop

loopa	lda	$76
	sec
	sbc	#$40
	jmp	loop
	
loopb	lda	$76
	clc
	adc	#$40
	jmp	loop

