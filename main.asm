	
*	= $1001
	.word	(+), 2055
	.null	$9e, format("%4d", start)
+	.word 0
start	jmp	main
	
FIELDSZ	= 1<<(2*FIELDPW)	;
FIELDMX = field+FIELDSZ-1	; last byte of FIELDSZ-aligned region 'field'
	
INITILE	= $3e			; start with five rightmost paths open
XHAIRPV	= SCREENW*SCREENH/2	; character to the right of screen center
XHAIRLT	= XHAIRPV-1		; character to the left of scren center
XHAIRRT	= XHAIRPV+1		; the initial unplaced tile position, w/ XHAIRPV
XHAIRUP = XHAIRPV-SCREENW	;
XHAIRDN	= XHAIRPV+SCREENW	;
	
XFLDOFS	= ZP			; static uint8_t XFLDOFS;
YFLDOFS	= ZP+1			; static uint8_t YFLDOFS;
POINTRA = ZP+2			; static uint8_t* POINTRA;
POINTRB = ZP+4			; static uint8_t* POINTRB;

CURTILE	.byte	$32;arbitrary	; static uint8_t CURTILE = 0x32;
PBACKUP	.byte	$20		; static uint8_t PBACKUP = ' ';
LBACKUP	.byte	$20		; static uint8_t LBACKUP = ' ';
RBACKUP .byte	$20		; static uint8_t RBACKUP = ' ';
UBACKUP	.byte	$20		; static uint8_t UBACKUP = ' ';
DBACKUP	.byte	$20		; static uint8_t DBACKUP = ' ';
	
FIELDC	.byte	$0		; black
XHAIRC	.byte	$2		; orange
	
;;;    2^3
;;; 2^2   2^0
;;;    2^1
;;; upper 4 msb are masked off once that entry/exit connects to adjacent symbol
	torch = $51
symchar	.text	$80|$20		; 0: space, unoccupied spot in play area
	.text	$80|torch	; 1: circle dead end, closes off escape
	.text	$80|torch	; 2: circle dead end, closes off escape (n/a)
	.text	$80|$55		; 3: right bending downward
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
	and	#$0f		;inline uint8_t rot90cw(uint4_t a,
-	clc			;                       uint2_t y) { // nyb rot
	adc	#$f8		; do {
	rol			;  a = ((a & 7) << 1) | ((a & 8) ? 1 : 0);
	and	#$0f		; } while (--y);
	dey			; return a;
	bne	-		;} // rot90cw()
	.endm			;
	
;;;    2^1 2^3
;;; 2^0       2^5
;;;    2^2 2^4
;;; upper 2 msb are used for (clockwise) rotation angle
	
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
	and	#$07		;
	tay			; return outsym[(a & 0x38) >> 3];
	lda	outsyma,y	;}
	.endm

.if TESTSYM	
main	lda	#$13		;void main(void) {
	jsr	$ffd2		; putchar(0x13); // go to screen home
	ldy	#SCREENW*2+2	;
	lda	#$20		; for (uint8_t y = SCREEN*2+2; y; y--)
-	jsr	$ffd2		;  putchar(' '); // for VIC20 (instead of CLR)
	dey			;
	bne	-		;
.if SCREENC
	lda	#$0		; // set black foreground for all characters:
	sta	SCREENC		; *((void*) SCREENC) = 0;// 16's digit of tile#
	sta	SCREENC+$1	; *((void*) SCREENC+1) = 0;// 1's digit of tile#
	sta	SCREENC+$2	; *((void*) SCREENC+42-40) = 0;// outer if rot=3
	sta SCREENC+SCREENW+2-1	; *((void*) SCREENC+42-1) = 0;// outer if rot=2
	sta SCREENC+SCREENW+2	; *((void*) SCREENC+42) = 0;// inner=pivot point
	sta SCREENC+SCREENW+2+1	; *((void*) SCREENC+42+1) = 0;// outer if rot=0
	sta SCREENC+2*SCREENW+2	; *((void*) SCREENC+42+40) = 0;// outer if rot=1
.endif
	lda	#$01		; uint8_t zp = 1;
	
loop	sta	ZP		; do {
	lsr			;  uint8_t a;
	lsr			;
	lsr			;
	lsr			;
	ora	#$30		;  a = '0' | (zp >> 4);
	cmp	#$3a		;
	bcc	loop2		;  if (a > '9')
	sec			;
	sbc	#$39		;   a -= '9'; // hex-digit screen codes are 1-6

loop2	sta	SCREENM		;  *((void*) (1024+2048)) = a; // 16's digit
	lda	ZP		;
	and	#$0f		;
	ora	#$30		;  a = '0' | (zp & 0x0f);
	cmp	#$3a		;
	bcc	loop3		;  if (a > '9')
	sec			;
	sbc	#$39		;   a -= '9';
	
loop3	sta	SCREENM+$1	;  *((void*) (1024+2049)) = a; // 1's digit
	lda	ZP		;
	innsym			;  a = innsym(zp);
	bit	ZP		;  switch (a >> 6) {
	bpl	loop5		;
	bvc	loop4		;
	
	ldy	#$03		;  case 3: // 270 degrees clockwise (upward)
	rot90cw			;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	ldy	#$03		;
	rot90cw			;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta	SCREENM+$2	;   *((void*) (1024+2090-40)) = a;
	jmp	loop7		;   break;
		
loop4	ldy	#$02		;  case 2: // 180 degrees clockwise (leftward)
	rot90cw			;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	ldy	#$02		;
	rot90cw			;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2-1	;   *((void*) (1024+2090-1)) = a;
	jmp	loop7		;   break;
	
loop5	bvc	loop6		;
	ldy	#$01		;  case 1: // 90 degrees clockwise (downward)
	rot90cw			;   a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	ldy	#$01		;
	rot90cw			;   a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+2*SCREENW+2	;   *((void*) (1024+2090+40)) = a;
	jmp	loop7		;   break;

loop6	and	#$0f		;  case 0: default: // unrotated (rightward)
	tay			;
	lda	symchar,y	;   a = symchar[a & 0x0f];
	sta SCREENM+SCREENW+2		;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2+1	;   *((void*) (1024+2090+1)) = a;

loop7	jsr	$ffe4		;  }
	beq	loop7		;  do {} while ((a = getchar()) == 0);
	ldy	#$20		;  // erase prior to a possible rotation/change
	sty	SCREENM+2	;  *((void*) (1024+2090-40)) = ' ';
	sty SCREENM+SCREENW+2-1	;  *((void*) (1024+2090-1)) = ' ';
	sty SCREENM+SCREENW+2	;  *((void*) (1024+2090)) = ' ';
	sty SCREENM+SCREENW+2+1	;  *((void*) (1024+2090+1)) = ' ';
	sty SCREENM+2*SCREENW+2	;  *((void*) (1024+2090+40)) = ' ';

	cmp	#$91		;  // crsr up $91 = increase tile# (wrap at 64)
	beq	loop8		;
	cmp	#$11		;  // crsr dn $11 = decrease tile# (wrap at 0)
	beq	loop9		; 
	cmp	#$9d		;  // crsr left $9d = rotate ccw
	beq	loopa		;
	cmp	#$1d		;  // crsr right $1d = rotate cw
	beq	loopb		;
	and	#$5f		;  switch (a) {
	cmp	#$51		;  case 'q':
	bne	loop7		;  case 'Q':
	rts			;   return;

loop8	lda	ZP		;  case 0x91:
	clc			;
	adc	#$1		;
	and	#$3f		;   a = (a + 1) & 0x3f; // next highest tile#
	beq	+		;
	jmp	loop		;   if (a == 0)
+	lda	#$01		;    a = 1; // 0 is a non-tile
	jmp	loop		;   break;

loop9	lda	ZP		;  case 0x11:
	sec			;
	sbc	#$1		;
	and	#$3f		;   a = (a - 1) & 0x3f; // next highest tile#
	beq	+		;
	jmp	loop		;   if (a == 0)
+	lda	#$3f		;    a = 63; // 63 is 6-path tile (not in game)
	jmp	loop		;   break;

loopa	lda	ZP		;  case 0x9d:
	sec			;
	sbc	#$40		;   a -= 0x40; // cycle msb through 00,11,10,01
	jmp	loop		;   break;
	
loopb	lda	ZP		;  case 0x1d:
	clc			;   a += 0x40; // cycle msb through 00,01,10,11
	adc	#$40		;   break;
	jmp	loop		; } while (1);
.endif				;} // main(TESTSYM)

.if TESTFLD
main	lda	#$93		;
	jsr	$ffd2		;

	lda	# <FIELDMX	;
	sta	selfmod+1	;
	lda	# >FIELDMX	;
	sta	selfmod+2	;
	lda	#$20		;
selfmod	sta	FIELDMX		;
	dec	selfmod+1	;
	bne	selfmod		;
	dec	selfmod+2	;
	ldy	selfmod+2	;
	cpy	# >field	;
	bcs	selfmod		;

	lda	#1<<(FIELDPW-1)	;
	sta	XFLDOFS		;
	sta	YFLDOFS		;
	
loop1	lda	SCREENM+XHAIRPV	;
	sta	PBACKUP		;
	lda	SCREENM+XHAIRLT	;
	sta	LBACKUP		;
	lda	SCREENM+XHAIRRT	;
	sta	RBACKUP		;
	lda	SCREENM+XHAIRUP	;
	sta	UBACKUP		;
	lda	SCREENM+XHAIRDN	;
	sta	DBACKUP		;

	lda	XHAIRC		;
	sta	SCREENC+XHAIRPV	;
	lda	CURTILE		;
	
loop2	bit	CURTILE		; // perform the rotation
	bpl	loopb		;
	bvc	loopa		;
	lda	UBACKUP		;
	sta	SCREENM+XHAIRUP	;
	lda	FIELDC		;
	sta	SCREENC+XHAIRUP	;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRRT	;
	jmp	loopd		;
loopa	lda	LBACKUP		;
	sta	SCREENM+XHAIRLT	;
	lda	FIELDC		;
	sta	SCREENC+XHAIRLT	;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRDN	;
	jmp	loopd		;
loopb	bvc	loopc		;
	lda	DBACKUP		;
	sta	SCREENM+XHAIRDN	;
	lda	FIELDC		;
	sta	SCREENC+XHAIRDN	;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRLT	;
	jmp	loopd		;
loopc	lda	RBACKUP		;
	sta	SCREENM+XHAIRRT	;
	lda	FIELDC		;
	sta	SCREENC+XHAIRRT	;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRDN	;
loopd	lda	CURTILE		;
	clc			;
	adc	#$40		;
	sta	CURTILE		;

loop3	lda	CURTILE		;  // depict the rotation
	innsym			;  a = innsym(zp);
	bit	CURTILE		;  switch (a >> 6) {
	bpl	loop5		;
	bvc	loop4		;
	
	ldy	#$03		;  case 3: // 270 degrees clockwise (upward)
	rot90cw			;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta 	SCREENM+XHAIRPV	;   *((void*) 1024+2090 = a; // pivot point
	lda	CURTILE		;
	outsym			;   a = outsym(zp);
	ldy	#$03		;
	rot90cw			;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta	SCREENM+XHAIRUP	;   *((void*) (1024+2090-40)) = a;
	jmp	loop7		;   break;
		
loop4	ldy	#$02		;  case 2: // 180 degrees clockwise (leftward)
	rot90cw			;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta 	SCREENM+XHAIRPV	;   *((void*) 1024+2090 = a; // pivot point
	lda	CURTILE		;
	outsym			;   a = outsym(zp);
	ldy	#$02		;
	rot90cw			;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta 	SCREENM+XHAIRLT	;   *((void*) (1024+2090-1)) = a;
	jmp	loop7		;   break;
	
loop5	bvc	loop6		;
	ldy	#$01		;  case 1: // 90 degrees clockwise (downward)
	rot90cw			;   a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta 	SCREENM+XHAIRPV	;   *((void*) 1024+2090 = a; // pivot point
	lda	CURTILE		;
	outsym			;   a = outsym(zp);
	ldy	#$01		;
	rot90cw			;   a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta 	SCREENM+XHAIRDN	;   *((void*) (1024+2090+40)) = a;
	jmp	loop7		;   break;

loop6	and	#$0f		;  case 0: default: // unrotated (rightward)
	tay			;
	lda	symchar,y	;   a = symchar[a & 0x0f];
	sta	SCREENM+XHAIRPV	;   *((void*) 1024+2090 = a; // pivot point
	lda	CURTILE		;
	outsym			;   a = outsym(zp);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta	SCREENM+XHAIRRT	;   *((void*) (1024+2090+1)) = a;

loop7	jsr	$ffe4		;  }
	beq	loop7		;  do {} while ((a = getchar()) == 0);

	cmp	#$91		;
	beq	loop8		;
	cmp	#$11		;
	beq	loop8		;
	cmp	#$9d		;
	beq	loop8		;
	cmp	#$1d		;
	beq 	loop8		;
	
	cmp	#$20		;  if (a == ' ')
	bne	loopq		;
	jmp	loop2		;   continue;

loopq	and	#$5f		;
	cmp	#$51		;
	bne	loop7		;  else if (a == 'q' || a == 'Q')
	rts			;   return;
	
loop8	jmp	loop7 ;for now
.endif	










	.align	FIELDSZ
field
  	.fill	FIELDSZ
.if 0
	brk
.endif	

