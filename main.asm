*	= BASIC+1
	.word	(+), 2055
	.text	$99, $22, $8e, $08, $13
topline	.text	"?? left 1="
toplin1	.text	"?  2="
toplin2	.text	"?  3="
toplin3	.text	"?", $22
	.null	$3a, $9e, format("%4d", start)
+	.word 0
start	jmp	main
	
TILESAT	.byte 0 ; ignored
TILE1AT	.byte toplin1-topline
TILE2AT	.byte toplin2-topline
TILE3AT	.byte toplin3-topline
	
FDIM	= 1<<FIELDPW		;
FIELDSZ	= FDIM*FDIM		;
FIELDMX = field+FIELDSZ-1	; last byte of FIELDSZ-aligned region 'field'
	
INITILE	= $3e			; start with five rightmost paths open
XHAIRPV	= SCREENW*SCREENH/2	; character to the right of screen center
XHAIRLT	= XHAIRPV-1		; character to the left of scren center
XHAIRRT	= XHAIRPV+1		; the initial unplaced tile position, w/ XHAIRPV
XHAIRUP = XHAIRPV-SCREENW	;
XHAIRDN	= XHAIRPV+SCREENW	;
	
STL	= SCREENM+SCREENW	; top-left corner (title/tile line is abovee it)
STL1D	= SCREENM+2*SCREENW	; down 1 row from top-left corner
SBR1U	= SCREENM+SCREENW*(SCREENH-1)-1 ; up 1 row from screen bottom-right corner
SBR	= SCREENM+SCREENW*(SCREENH-0)-1 ; screen bottom-right corner
	
POINTER = ZP			; static void* POINTER;
POINTR2 = ZP+2			; static void* POINTR2;
ZP_TEMP	= ZP+4			; static uint8_t ZP_TEMP;
	
CURTILE	.byte	0		; static uint8_t CURTILE[4]; // shown and 2 more
CURTIL1	.byte	0		;
CURTIL2	.byte	0		;
CURTIL3	.byte	0		;
CURTNUM	.byte	0		; static uint2_t CURTNUM;
	
XFLDOFS	.byte	FDIM/2		; static uint8_t XFLDOFS = FDIM/2;
YFLDOFS	.byte	FDIM/2		; static uint8_t YFLDOFS;
ROTNOFS .byte	FDIM+1		; static const ROTNOFS[] = {FDIM+1,
	.byte 	FDIM*2		;                           FDIM*2,
	.byte 	FDIM-1		;                           FDIM-1,
	.byte	0		;                           0};

UNRSLVD	.byte	$05		; static uint8_t UNRSLVD = 5; // from INITILE
PBACKUP	.byte	$20		; static uint8_t PBACKUP = ' ';
LBACKUP	.byte	$20		; static uint8_t LBACKUP = ' ';
RBACKUP .byte	$20		; static uint8_t RBACKUP = ' ';
UBACKUP	.byte	$20		; static uint8_t UBACKUP = ' ';
DBACKUP	.byte	$20		; static uint8_t DBACKUP = ' ';
	
FIELDC	.byte	$0		; black
XHAIRC	.byte	$62		; orange
	
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

dx = 1
dy = 2
rot90cw	.macro	decrement=0
	and	#$0f		;inline uint8_t rot90cw(uint4_t a,
.if \decrement
-	clc			;                       uint2_t y) { // nyb rot
.else
	clc			;
.endif
	adc	#$f8		; do {
	rol			;  a = ((a & 7) << 1) | ((a & 8) ? 1 : 0);
	and	#$0f		; } while (--y);
.if \decrement
	.if \decrement == dy
	 dey
	.else
	 dex
	.endif
	bne	-		; return a;
.endif
	.endm			;} // rot90cw()
	
deck
PETSCIDA :?= 1;;; FIXME :?= 0 after default deck no longer all zeroes
.if PETSCIDA
	.text	$01,$21,$31,$33,$28,$01,$21
	.text	$29,$2d,$30,$09,$2b,$39,$31
	.text	$28,$39,$35,$39,$29,$30,$11
	.text	$22,$11,$31,$2a,$29,$24,$09
	.text	$29,$34,$31,$28,$12,$10,$08
	.text	$0a,$18,$0c,$03,$38,$1e,$1a
	.text	$12,$0f,$3a,$1e,$1c,$0c,$17
	.text	$3c,$14,$18,$13,$2e,$1a,$0b
	.text	$32,$07,$36,$1c,$15,$2c,$0d
	.text	$2e,$19,$08,$10,$19,$36,$19
.else
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
	.text	0,0,0,0,0,0,0
.endif
pstdeck
DECKSIZ	= pstdeck-deck
.if 0	
.if DECKSIZ > $80
DECKMSK	= $ff			;
.elsif DECKSIZ > $40
DECKMSK	= $7f
.elsif DECKSIZ > $20
DECKMSK	= $3f
.elsif DECKSIZ > $10
DECKMSK	= $1f
.elsif DECKSIZ > $08
DECKMSK	= $0f
.elsif DECKSIZ > $04
DECKMSK	= $07
.elsif DECKSIZ > $02
DECKMSK	= $03
.elsif DECKSIZ > $01
DECKMSK	= $01
.elsif DECKSIZ > $00
DECKMSK	= $00
.else
error "Deck has no initial cards"
.endif
.endif
DECKREM	.byte	0
	
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

copynyb	.macro
	and	#$0f		;inline uint8_t copynyb (uint4_t a) {
	pha			;
	tsx			;
	asl			;
	asl			;
	asl			;
	asl			;
	ora	$101,x		;
	inx			; return ((a & 0x0f) << 4) | (a & 0x0f);
	txs			;} // copynyb()
	.endm
	
.if TESTSYM	
nvcases	.macro	r=0,d=0,l=0,u=0	;inline uint2_t nvcases(uint2_t nv, (*r)(),
.if	\u && \d && \l && \r
	bpl	++		;                       (*d)(), (*l)(), (*u)()){
	;; N==1, so up/left
	bvc	+		; switch ((n << 1) | v) {
	;; N==1 && V==1, so up
	jsr	\u		;  case 3: (*u)();
	jmp	++++		;          break;
	;; N==1 && V==0, so left
+	jsr	\l		;  case 2: (*l)();
	jmp	+++		;          break;
	;; N==0, so right/down
+	bvc	+		;  case 1: (*d)();
	;; N==0 && V==1, so down
	jsr	\d		;          break;
	jmp	++		;  case 0: (*r)();
	;; N==0 && V==0, so right
+	jsr	\r		; }
+	
.else
	error	"must specify four subroutines"
.endif
	.endm			;}

main	lda	#$13		;void main(void) {
.if 0
	jsr	$ffd2		; putchar(0x13); // go to screen home
	ldy	#SCREENW*2+2	;
	lda	#$20		; for (uint8_t y = SCREEN*2+2; y; y--)
-	jsr	$ffd2		;  putchar(' '); // for VIC20 (instead of CLR)
	dey			;
	bne	-		;
.endif
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
	rot90cw	dy		;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	ldy	#$03		;
	rot90cw	dy		;   a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta	SCREENM+$2	;   *((void*) (1024+2090-40)) = a;
	jmp	loop7		;   break;
		
loop4	ldy	#$02		;  case 2: // 180 degrees clockwise (leftward)
	rot90cw	dy		;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
	ldy	#$02		;
	rot90cw	dy		;   a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2-1	;   *((void*) (1024+2090-1)) = a;
	jmp	loop7		;   break;
	
loop5	bvc	loop6		;  case 1: // 90 degrees clockwise (downward)
	rot90cw			;   a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;   a = symchar[a];
	sta SCREENM+SCREENW+2	;   *((void*) 1024+2090 = a; // pivot point
	lda	ZP		;
	outsym			;   a = outsym(zp);
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
chckptr	.macro	delta		;
	clc			;
	lda	POINTR2		;
	adc	#\delta		;
	sta	POINTER		;
	lda	1+POINTR2	;
	adc	#0		;
	sta	1+POINTER	; POINTER = POINTR2 + delta;
	.endm
	
	lda FIELDC		;void main(void) {
	ldy #$e2
-	sta SCREENC+SCREENW-1,y
	sta SCREENC+SCREENW-1+$e2,y ; FIXME: harmless? workaround (for vic20 screen not having color already set)
	dey
	bne -
	
SEEDVAL	:?= 0
SEEDLOC	:?= 0
	
main	lda	#SEEDVAL	;void main(void) {
.if SEEDLOC
	sta	SEEDLOC		; *SEEDLOC = SEEDVAL;
.endif	
	lda	#<SBR1U		;
	sta	POINTER		;
	lda	#>SBR1U		;
	sta	1+POINTER	; POINTER = SBR1U; // one less than BL corner
	lda	#<(SBR1U-SCREENM+SCREENC)
	sta	POINTR2		;
	lda	#>(SBR1U-SCREENM+SCREENC)
	sta	1+POINTR2	; POINTR2 = SBR1U+(SCREENC-SCREENM); // colormem
	ldx	#SCREENH-1	; for (uint8_t x = SCREENH; x; x--) {
main1	ldy	#SCREENW	;  for (uint8_t y = SCREENW; y; y--) {
main2	lda	FIELDC		;
	sta	(POINTR2),y	;   POINTR2[y] = FIELDC; // dirt color
	lda	#$1f		;
	sta	(POINTER),y	;   POINTER[y] = 0xa0; // progress arrow
	
	txa			;
	pha			;
	tya			;
	pha			;
	lda	RNDLOC1		;   for (x = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        x >= *DECKSIZ;
-	lsr			;        x >>= 1)
	cmp	#DECKSIZ	;
	bcs	-		;    ;
	tax			;
main4	lda	RNDLOC1		;   for (y = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        y >= *DECKSIZ;
-	lsr			;        y >>= 1)
	cmp	#DECKSIZ	;
	bcs	-		;    ;
	tay			;
	lda	deck,x		;
	pha			;   uint8_t temp = deck[x];
	lda	deck,y		;
	sta	deck,x		;   deck[x] = deck[y];
	pla			;
	sta	deck,y		;   deck[y] = temp;
	pla			;
	tay			;
	pla			;
	tax			;

	lda	#$20		;
	sta	(POINTER),y	;   POINTER[y] = 0xa0; // unremoved dirt
	dey			;
	bne	main2		;  }
	sec			;
	lda	POINTER		;
	sbc	#SCREENW	;
	sta	POINTER		;
	lda	1+POINTER	;
	sbc	#0		;
	sta	1+POINTER	;  POINTER -= SCREENW;
	sec			;
	lda	POINTR2		;
	sbc	#SCREENW	;
	sta	POINTR2		;
	lda	1+POINTR2	;
	sbc	#0		;
	sta	1+POINTR2	;  POINTR2 -= SCREENW;
	dex			;
	bne	main1		; }
	lda	# <FIELDMX
	sta	selfmod+1	; uint8_t a;
	lda	# >FIELDMX	;
	sta	selfmod+2	;
	lda	#$00		;
selfmod	sta	FIELDMX		;
	dec	selfmod+1	;
	bne	selfmod		;
	dec	selfmod+2	;
	ldy	selfmod+2	;
	cpy	# >field	; for (uint8_t* sm = FIELDMX; sm > field; sm--)
	bcs	selfmod		;  *sm = 0; // whole field starts blanked

	lda	#FDIM/2
	sta	XFLDOFS		; XFLDOFS = (1<<(FIELDPW-1)); // middle of field
	sta	YFLDOFS		; YFLDOFS = (1<<(FIELDPW-1)); // middle of field
	sta	POINTR2		; POINTR2 = XFLDOFS |
	lda #1<<((FIELDPW-4)*2)	;           (YFLDOFS << FIELDPW) |
	ora	#> field	;           field; // (XLFDOFS, YFLDOFS)
	sta	1+POINTR2	;
	
	sec			;
	lda	POINTR2		;
	sbc	#FDIM		;
	sta	POINTR2		;
	lda	1+POINTR2	;
	sbc	#0		;
	sta	1+POINTR2	; POINTR2 -= FDIM; // (XFLDOFS, YFLDOFS-1)
	
	lda	#INITILE	;
	innsym			;
	copynyb			; a  = copynyb(innsym(INITILE));
	ldy	#FDIM-2	;
	sta	(POINTR2),y	; POINTR2[FDIM - 2] = a; // (XFLDOFS-2, YFLDOFS)
	and	#$0f		;
	tay			;
	lda	symchar,y	; a = symchar[a & 0x0f];
	sta   SCREENM+XHAIRLT-1	; SCREENM[XHAIRLT-1] = a;
	lda	FIELDC		;
	sta   SCREENC+XHAIRLT-1	; SCREENC[XHAIRLT-1] = FIELDC; // now visible

	lda	#INITILE	;
	outsym			;
	copynyb			; a = copynyb(outsym(INITILE));
	ldy	#FDIM-1	;
	sta	(POINTR2),y	; POINTR2[FDIM - 0] = a; // (XLFDOFS-1, YFLDOFS)
	and	#$0f		;
	tay			;
	lda	symchar,y	; a = symchar[a & 0x0f];
	sta	SCREENM+XHAIRLT	; SCREENM[XHAIRLT] = a;
	lda	FIELDC		;
	sta	SCREENC+XHAIRLT	; SCREENC[XHAIRLT] = FIELDC; // now visible
	
	ldy	#DECKSIZ	;
	dey			;
	;sty	DECKREM		;
	lda	deck,y		;
	sta	CURTIL3		; CURTILE[3] = deck[--DECKREM];
	dey			;
	sty	DECKREM		;
	lda	deck,y		;
	sta	CURTIL2		; CURTILE[2] = deck[--DECKREM];
	lda	#1		;
	sta	CURTNUM		; CURTNUM = 1; // or 2 or 3

loop	ldy	DECKREM		; for (;;) { // place a new current tile
	beq	+		;  if (DECKREM != 0)
	dey			;
	sty	DECKREM		;
	lda	deck,y		;
	ldy	CURTNUM		;
	sta	CURTILE		;   CURTILE[0] =
	sta	CURTILE,y	;            CURTILE[CURTNUM] = deck[--DECKREM];

+	ldx	#3		;  for (uint8_t x = 3; x; x--) {
-	lda	CURTILE,x	;   uint8_t a;
	innsym			;
	tay			;
	lda	symchar,y	;   a = symchar[innsym(CURTILE[x])]; // left
	ldy	TILESAT,x	;
	sta	SCREENM,y	;   SCREENM[TILESAT[x]] = a; // on the screen
	lda	CURTILE,x	;
	outsym			;
	tay			;
	lda	symchar,y	;   a = symchar[outsym(CURTILE[x])]; // right
	ldy	TILESAT,x	;
	iny			;
	sta	SCREENM,y	;   SCREENM[TILESAT[x]+1] = a; // on the screen
	dex			;
	bne	-		;  }

	jsr	numleft		;  numleft();

	sec			;  for (uint1_t c = 1; ; c = 0) { // new position
loop1	lda	SCREENM+XHAIRPV	;
	sta	PBACKUP		;   PBACKUP = SCREENM[XHAIRPV];
	lda	SCREENM+XHAIRLT	;
	sta	LBACKUP		;   LBACKUP = SCREENM[XHAIRLT];
	lda	SCREENM+XHAIRRT	;
	sta	RBACKUP		;   RBACKUP = SCREENM[XHAIRRT];
	lda	SCREENM+XHAIRUP	;
	sta	UBACKUP		;   UBACKUP = SCREENM[XHAIRUP];
	lda	SCREENM+XHAIRDN	;
	sta	DBACKUP		;   DBACKUP = SCREENM[XHAIRDN];

cychair	lda	XHAIRC		;
	sta	SCREENC+XHAIRPV	;   SCREENC[XHAIRPV] = XHAIRC;
	bcs	loop2		;
	
	lda	#$c0		;   // we're in a new position and in order to
	adc	CURTILE		;   // keep the original rotation we need to
	sta	CURTILE		;   CURTILE[0] += 0xc0; // rotate ccw before cw:

loop2	bit	CURTILE		;   for (;;) { // new rot'n (repair outer char)
	bpl	loopb		;
	bvc	loopa		;    switch (CURTILE[0] >> 6) {
	lda	UBACKUP		;    case 3: // rotate from up (3) to right (0)
	sta	SCREENM+XHAIRUP	;     SCREENM[XHAIRUP] = UBACKUP;
	lda	FIELDC		;
	sta	SCREENC+XHAIRUP	;     SCREENC[XHAIRUP] = FIELDC;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRRT	;     SCREENC[XHAIRRT] = XHAIRC;
	jmp	loopd		;     break;
loopa	lda	LBACKUP		;    case 2: // rotate from left (2) to up (3)
	sta	SCREENM+XHAIRLT	;     SCREENM[XHAIRLT] = LBACKUP;
	lda	FIELDC		;
	sta	SCREENC+XHAIRLT	;     SCREENC[XHAIRLT] = FIELDC;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRUP	;     SCREENC[XHAIRUP] = XHAIRC;
	jmp	loopd		;     break;
loopb	bvc	loopc		;
	lda	DBACKUP		;    case 1: // rotate from down (1) to left (2)
	sta	SCREENM+XHAIRDN	;     SCREENM[XHAIRDN] = DBACKUP;
	lda	FIELDC		;
	sta	SCREENC+XHAIRDN	;     SCREENC[XHAIRDN] = FIELDC;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRLT	;     SCREENC[XHAIRLT] = XHAIRC;
	jmp	loopd		;     break;
loopc	lda	RBACKUP		;    case 0: // rotate from right (0) to down (1)
	sta	SCREENM+XHAIRRT	;     SCREENM[XHAIRRT] = LBACKUP;
	lda	FIELDC		;
	sta	SCREENC+XHAIRRT	;     SCREENC[XHAIRRT] = FIELDC;
	lda	XHAIRC		;
	sta	SCREENC+XHAIRDN	;     SCREENC[XHAIRDN] = XHAIRC;
loopd	lda	CURTILE		;    }
	clc			;
	adc	#$40		;
	sta	CURTILE		;    CURTILE[0] += (1 << 6);
	
	innsym			;    // depict the rotation
	bit	CURTILE		;    a = innsym(CURTILE[0]);
	bpl	loop5		;    switch (CURTILE[0] >> 6) {
	bvc	loop4		;
	
	ldy	#$03		;    case 3: // 270 degrees clockwise (upward)
	rot90cw	dy		;     a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta 	SCREENM+XHAIRPV	;     SCREENM[XHAIRPV] = a; // pivot point
	lda	CURTILE		;
	outsym			;     a = outsym(CURTILE[0]);
	ldy	#$03		;
	rot90cw	dy		;     a = rot90cw(a, 3);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta	SCREENM+XHAIRUP	;     SCREENM[XHAIRUP] = a;
	jmp	loop7		;     break;

loop4	ldy	#$02		;    case 2: // 180 degrees clockwise (leftward)
	rot90cw	dy		;     a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta 	SCREENM+XHAIRPV	;     SCREENM[XHAIRPV] = a; // pivot point
	lda	CURTILE		;
	outsym			;     a = outsym(CURTILE[0]);
	ldy	#$02		;
	rot90cw	dy		;     a = rot90cw(a, 2);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta 	SCREENM+XHAIRLT	;     SCREENM[XHAIRLT] = a;
	jmp	loop7		;     break;
	
loop5	bvc	loop6		;    case 1: // 90 degrees clockwise (downward)
	rot90cw			;     a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta 	SCREENM+XHAIRPV	;     SCREENM[XHAIRPV] = a; // pivot point
	lda	CURTILE		;
	outsym			;     a = outsym(CURTILE[0]);
	rot90cw			;     a = rot90cw(a, 1);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta 	SCREENM+XHAIRDN	;     SCREENM[XHAIRDN] = a;
	jmp	loop7		;     break;

loop6	and	#$0f		;    case 0: default: // unrotated (rightward)
	tay			;
	lda	symchar,y	;     a = symchar[a & 0x0f];
	sta	SCREENM+XHAIRPV	;     SCREENM[XHAIRPV] = a; // pivot point
	lda	CURTILE		;
	outsym			;     a = outsym(CURTILE[0]);
	tay			;
	lda	symchar,y	;     a = symchar[a];
	sta	SCREENM+XHAIRRT	;     SCREENM[XHAIRRT] = a;

loop7	jsr	$ffe4		;    }
	beq	loop7		;    while ((a = getchar()) { // keyboard input loop

	cmp	#'1'		;
	beq	+		;
	cmp	#'2'		;
	beq	+		;
	cmp	#'3'		;
	bne	++		;     if (a == 0x31 || a == 0x32 || a == 0x33) {
+	and	#$0f		;
	tay			;      a = CURTILE[a & 0x03];
	lda	CURTILE,y	;      if (a == 0) // last few tiles, some blank
	beq	+		;       continue;
	sta	CURTILE		;      CURTILE[0] = a;
	jmp	cychair		;      goto cychair;
	
+	and	#$df		;     }
	cmp	#$1d		;     if ((a &= 0xdf) == 0x1d)
	bne	+
 	jmp	inright		;      inright();
+	cmp	#$11		;     else if (a == 0x11)
	bne	+
	jmp	indown		;      indown();
+	cmp	#$9d		;     else if (a == 0x9d)
	bne	+
	jmp	inleft		;      inleft();
+	cmp	#$91		;     else if (a == 0x91)
	bne	+
	jmp	inup		;      inup();
+
WASD	:?= 0
.if WASD
	cmp	#'d'		;     else if (a == 0x44)
	bne	+
 	jmp	inright		;      inright();
+	cmp	#'s'		;     else if (a == 0x53)
	bne	+
	jmp	indown		;      indown();
+	cmp	#'a'		;     else if (a == 0x41)
	bne	+
	jmp	inleft		;      inleft();
+	cmp	#'w'		;     else if (a == 0x57)
	bne	+
	jmp	inup		;      inup();
+
.endif
IJKL	:?= 0
.if IJKL
	cmp	#'l'		;     else if (a == 0x4c)
	bne 	+
	jmp	inright		;      inright();
+	cmp	#'k'		;     else if (a == 0x4b)
	bne	+
	jmp	indown		;      indown();
+	cmp	#'j'		;     else if (a == 0x4a)
	bne	+
	jmp	inleft		;      inleft();
+	cmp	#'i'		;     else if (a == 0x49)
	bne	+
	jmp	inup		;      inup();
+
.endif
	cmp	#0		;     else if (a == ' ' & 0xdf)
	bne	+		;
	jmp	loop2		;      break; // rotate cw through all 4 options

+	cmp	#$0d		;
	bne	+		;    else if (a == '\r') {
	jsr	stampit		;     stampit(); //copies tile into (both nybbles of) corresponding two field squares, later must return signed delta conn#
	jmp	loop		;     goto loop;
	
+	cmp	#'f'		;
	bne	+		;    else if (a == 0x46) {
	lda	XHAIRC		;
	clc			;
	adc	#1		;
	and	#$0f		;     a = (XHAIRC + 1) & 0x0f; // cycle color
	pha			;
	lda	XHAIRC		;
	and	#$f0		;
	sta	XHAIRC		;     XHAIRC &= 0xf0; // keep any luminance bits
	pla			;
	ora	XHAIRC		;
	sta	XHAIRC		;     XHAIRC |= a; // write back
	jmp	cychair		;

+	cmp	#'b'		;    // switch video chip background/border colors?
	
	cmp	#'q'		;
	bne	loop7		;    } else if (a == 'q' || a == 'Q')
	rts			;     return;
				;   } // keyboard input loop
				;  } // next rotation
				; } // next tile
				;} // main()

liftile	.macro			;inline void liftile(void) {
	lda	PBACKUP		; // could reduce code and redraw just the two obscured character positionss instead of all five (faster? slower?)
	sta	SCREENM+XHAIRPV	; SCREENM[XHAIRPV] = PBACKUP;
	lda	LBACKUP		;
	sta	SCREENM+XHAIRLT	; SCREENM[XHAIRLT] = LBACKUP;
	lda	RBACKUP		;
	sta	SCREENM+XHAIRRT	; SCREENM[XHAIRRT] = RBACKUP;
	lda	UBACKUP		;
	sta	SCREENM+XHAIRUP	; SCREENM[XHAIRUP] = UBACKUP;
	lda	DBACKUP		;
	sta	SCREENM+XHAIRDN	; SCREENM[XHAIRDN] = DBACKUP;
	lda	FIELDC		; // could reduce code and redraw just the two obscured character positionss instead of all five (faster? slower?)
	sta	SCREENC+XHAIRPV	; SCREENC[XHAIRPV] = FIELDC;
	sta	SCREENC+XHAIRLT	; SCREENC[XHAIRLT] = FIELDC;
	sta	SCREENC+XHAIRRT	; SCREENC[XHAIRRT] = FIELDC;
	sta	SCREENC+XHAIRUP	; SCREENC[XHAIRUP] = FIELDC;
	sta	SCREENC+XHAIRDN	; SCREENC[XHAIRDN] = FIELDC;
	.endm			;} // liftile()

movptrs	.macro	delta		;inline uint1_t movptrs(const int8_t delta) { // FIXME: these waste an enormous moat merely by insisting no non-field squares appear onscreen
.if \delta == +1
	lda	#FDIM-SCREENW/2	; if (delta == +1) {
	cmp	XFLDOFS		;  if (XFLDOFS <= FDIM-SCREENW/2)
	bcc	+		;   return C = 0;
	inc	XFLDOFS		;  XFLDOFS += 1;
	clc			;
	lda	POINTR2		;
	adc	#1		;
	sta	POINTR2		;
	lda	1+POINTR2	;
	adc	#0		;
	sta	1+POINTR2	;  POINTR2 += 1;
	sec			;  return C = 1; // proceed to scroll screen
+
.elsif \delta == +FDIM
	lda	#FDIM-SCREENH/2	; } else if (delta == +FDIM) {
	cmp	YFLDOFS		;  if (XFLDOFS <= FDIM-SCREENH/2)
	bcc	+		;   return C = 0;
	inc	YFLDOFS		;  YFLDOFS += 1;
	clc			;
	lda	POINTR2		;
	adc	#FDIM		;
	sta	POINTR2		;
	lda	1+POINTR2	;
	adc	#0		;
	sta	1+POINTR2	;  POINTR2 += FDIM;
	sec			;  return C = 1; // proceed to scroll screen
+	
.elsif \delta == -1
	lda	XFLDOFS		; } else if (delta == -1) {
	cmp	#SCREENW/2	;  if (XFLDOFS >= SCREENW/2)
	bcc	+		;   return C = 0;
	dec	XFLDOFS		;  XFLDOFS -= 1;
	sec			;
	lda	POINTR2		;
	sbc	#1		;
	sta	POINTR2		;
	lda	1+POINTR2	;
	sbc	#0		;
	sta	1+POINTR2	;  POINTR2 -= 1;
	sec			;  return C = 1; // proceed to scroll screen
+
.elsif \delta == -FDIM
	lda	YFLDOFS		; } else if (delta == -FDIM) {
	cmp	#SCREENH/2	;  if (YFLDOFS >= SCREENW/2)
	bcc	+		;   return C = 0;
	dec	YFLDOFS		;  YFLDOFS -= 1;
	sec			;
	lda	POINTR2		;
	sbc	#FDIM		;
	sta	POINTR2		;
	lda	1+POINTR2	;
	sbc	#0		;
	sta	1+POINTR2	;  POINTR2 -= FDIM;
	sec			;  return C = 1; // proceed to scroll screen
+
.else	
.error "invalid move distance ", \delta
.endif				; }
	.endm			;}
	
blitter	.macro	src,dst,ends	;
.if \src < \dst
	lda	# <\src		;void blitter(uint8_t* src, uint8_t* dst,
	sta	(+)-2		;             uint8_t* ends) {
	lda	# >\src		;
	sta	(+)-1		;
	lda	# <\dst		;
	sta	(+)+1		;
	lda	# >\dst		;
	sta	(+)+2		;
	
-	lda	$ffff		; do *dst-- = *src; while (src-- != ends); // 4T
+	sta	$ffff		;} // blitter()                            //+4T
	lda	(-)+1		;                                          //+4T
	beq	++		;------+                          // usually +2T
	cmp	# <\ends	;      |                                   //+2T
	beq	++++++		;--------+                        // usually +2T
	dec	(-)+1		;      | |                        // usually +6T
	lda	(-)+4		;      | |                        // usually +4T
	beq	+		;--+   | |                        // usually +2T
	dec	(-)+4		;  |   | |                                 //+6T
	jmp	-		;  |   | | // +3T=39T; ~39000T=39ms@1MHz, ~25fps
	
	;; addr for sta is $__00, addr for lda already decremented (not \ends)
+	dec	(-)+4		;<-+   | |
	dec	(-)+5		;      | |
	bne	-		;      | |
	
	;; addr for lda is $__00, unknown if it is also \ends
+	cmp	# <\ends	;<-----+ |
	beq	++		;----+   |
	dec	(-)+1		;    |   |
	dec	(-)+2		;    |   |
	lda	(-)+4		;    |   |
	beq	+		;--+ |   |
	dec	(-)+4		;  | |   |
	jmp	-		;  | |   |
	
	;; addr for sta is $__00, addr for lda already decremented
+	dec	(-)+4		;<-+ |   |
	dec	(-)+5		;    |   |
	bne	-		;    |   |

	;; addr for lda is $__00 and matched low byte of \ends
+	lda	(-)+2		;<---+   |
	cmp	# >\ends	;        |
	beq	++++		;->.endm |
	dec	(-)+1		;        |
	dec	(-)+2		;
	lda	(-)+4		;        |
	beq	+		;--+     |
	dec	(-)+4		;  |     |
	jmp	-		;  |     |

	;; addr for sta is $__00, addr for lda already decremented
+	dec	(-)+4		;<-+     |
	dec	(-)+5		;        |
	bne	-		;        |

	;; addr for lda not $__00 but matched low byte of \ends
+	lda	(-)+2		;<-------+
	cmp	# >\ends	;
	beq	++		;->.endm
	dec	(-)+1		;
	lda	(-)+4		;
	beq	+		;--+
	dec	(-)+4		;  |
	jmp	-		;  |
	
	;; addr for sta is $__00, addr for lda already decremented
+	dec	(-)+4		;<-+
	dec	(-)+5		;
	bne	-		;
+
.elsif \src > \dst
	lda	# <\src		;void blitter(uint8_t* src, uint8_t* dst,
	sta	(+)-2		;             uint8_t* ends) {
	lda	# >\src		;
	sta	(+)-1		;
	lda	# <\dst		;
	sta	(+)+1		;
	lda	# >\dst		;
	sta	(+)+2		;
	
-	lda	$ffff		; do *dst++ = *src; while (src++ != ends); // 4T
+	sta	$ffff		;} // blitter()                            //+4T
	lda	(-)+1		;                                          //+4T
	cmp	# <\ends	;                                          //+2T
	beq	++		;------+                          // usually +2T
	inc	(-)+1		;    ^ |                                   //+6T
	bne	+		;--+ | |                          // usually +3T
	inc	(-)+2		;  | | |                          // usually  0T
+	inc	(-)+4		;<-+ | |                                   //+6T
	bne	-		;----+ |                          // usually +3T
	inc	(-)+5		;    | |
	bne	-		;----+ |                          // =34T
+	lda	(-)+2		;<---|--+
	cmp	# >\ends	;    |
	beq	++		;->.endm
 	inc	(-)+1		;    |
	bne	+		;--+ |
	inc	(-)+2		;  | |
+	inc	(-)+4		;<-+ |
	bne	-		;----+
	inc	(-)+5		;    |
	bne	-		;----+
+
.else
.error \src, " cannot equal ",\dst
.endif
	.endm			;
	
repaint	.macro			;
	.endm
	
inright	movptrs	+1		;void inright(void) {
	bcs	+		; if (movptrs(+1) == 0) {
	jmp	loop7		;  liftile();
+	liftile			;  blitter(STL+1,STL,SBR);
	blitter	STL+1,STL,SBR	;  repaint(-SCREENW/2,-1);
	repaint	-SCREENW/2,-1	;  goto loop1; }
	clc			;
	jmp	loop1		;} // inright()
	
indown	movptrs	+FDIM		;void indown(void) {
	bcs	+		; if (movptrs(+FDIM) == 0) {
	jmp	loop7		;  liftile();
+	liftile			;  blitter(STL1D,STL,SBR);
	blitter	STL1D,STL,SBR	;  repaint(-1,-SCREENH/2-1);
	repaint	-1,-SCREENH/2-1	;  goto loop1; }
	clc			;
	jmp	loop1		;} // indown()
	
inleft	movptrs	-1		;void inleft(void) {
	bcs	+		; if (movptrs(-1) == 0) {
	jmp	loop7		;  liftile();
+	liftile			;  blitter(SBR-1,SBR,STL)
	blitter	SBR-1,SBR,STL	;  repaint(+SCREENW/2-1,-1
	repaint	+SCREENW/2-1,-1	;  goto loop1; }
	clc			;
	jmp	loop1		;} // inleft()
	
inup	movptrs	-FDIM		;void inup(void)
	bcs	+		; if (movptrs(-FDIM) == 0) {
	jmp	loop7		;  liftile();
+	liftile			;  blitter(SBR1U,SBR,STL)
	blitter	SBR1U,SBR,STL	;  repaint(-1,SCREENH/2);
	repaint	-1,SCREENH/2	;  goto loop1; }
	clc			;
	jmp	loop1		;} // inup()
	
angle	.macro			;inline uint2_t angle(uint8_t a) {
	rol			;
	rol			;
	rol			;
	and	#$03		; return a >>= 6;
	.endm			;}

tofield	.macro			;inline uint8_t tofield(uint1_t out,
	lda	CURTILE		;                       register uint8_t* x,
	.if \1			;                       register uint8_t* y) {
	outsym			;
	.else			; uint8_t a;
	innsym			; a = out ? outsym(CURTILE[0]) : innsym(CURTILE[0]);
	.endif			;
	copynyb			; a = copynyb(a&0x0f); // un-closed-off flags
	tay			;
	lda	CURTILE		;
	angle			;
	tax			; x = CURTILE[0] >> 6; // rotation bits only
	tya			;
	.if	\1		; if (out)
	ldy	ROTNOFS,x	;  y = ROTNOFS[x]; // +1 in x, +1 in y, -1 in x, -1 in y
	.else			; else
	ldy	#FDIM		;  y = FDIM; // +0 in x and y
	.endif			;
	rot90cw	dx		; rot90cw(x);
	tax			;
	lda	(POINTR2),y	; return x /*new*/, a = POINTR2[y] /*old,=0? */;
	.endm			;}
	
stampit	lda	CURTILE		;uint8_t stampit(uint8_t a) {
	beq	nostamp		; if ((CURTILE[0] = a) != 0) // tile not blank
	tofield	0		;
	bne	nostamp		;  if (tofield(0 /*inner*/, a, &x, &y) == 0) {
	txa			;
	pha			;   uint8_t stack = x; // will stamp inner last
	tofield	1		;
	bne	+		;   if (tofield(1 /*outer*/, a, &x, &y) == 0) {
	txa			;    // no tile(s) in this one's location yet
	sta	(POINTR2),y	;    POINTR2[y] = x;
	pla			;
	ldy	#FDIM		;
	sta	(POINTR2),y	;    POINTR2[FDIM] = stack;
	lda	CURTILE		;    return CURTILE[0];
	rts			;   }
+	pla			;  }
	lda	#0		; return 0;
nostamp	rts			;}
	
.endif	

numleft	lda	DECKREM		;
	ldy	#10
	cmp	#100
	bcc	*+3
	brk

	dey
	cmp	#90
	bcc	*+7
	sec
	sbc	#90
	bcs	*+2+90

	dey
	cmp	#80
	bcc	*+7
	sec
	sbc	#80
	bcs	*+2+80

	dey
	cmp	#70
	bcc	*+7
	sec
	sbc	#70
	bcs	*+2+70

	dey
	cmp	#60
	bcc	*+7
	sec
	sbc	#60
	bcs	*+2+60

	dey
	cmp	#50
	bcc	*+7
	sec
	sbc	#50
	bcs	*+2+50

	dey
	cmp	#40
	bcc	*+7
	sec
	sbc	#40
	bcs	*+2+40

	dey
	cmp	#30
	bcc	*+7
	sec
	sbc	#30
	bcs	*+2+30

	dey
	cmp	#20
	bcc	*+7
	sec
	sbc	#20
	bcs	*+2+20

	dey
	cmp	#10
	bcc	*+7
	sec
	sbc	#10
	bcs	*+2+10

	dey
	cmp	#0
	bcc	*+7
	sec
	sbc	#0
	bcs	*+2+0

	pha
	tya
	bne	+
	lda	#' '
	bne	++
+	ora	#$30
+	sta	SCREENM
	pla
	ora	#$30
	sta	SCREENM+1
	rts








	.align	FIELDSZ
field
  	.fill	FIELDSZ
.if 0
	brk
.endif	

