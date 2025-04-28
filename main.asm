.if FIELDPW == 5
VIC20NO	= 0
.elsif FIELDPW == 6
VIC20NO	= 1			; many features won't fit in unexpanded vic20
.else
.error	"256 > 2^FIELDPW > min(SCREENH,SCREENW) violated"	
.endif
	
DIRTCLR	= 0			; black

.if BASIC
*	= BASIC+1
.else
*	= $0002+1		; P500 loads into bank 0 but must run in bank 15
COPIED2	= $0400
	.word	(+), 3
	.text	$81,$41,$b2,$30	; FOR A = 0
	.text	$a4		; TO field-start
	.text	format("%4d",field-start)
	.text	$3a,$dc,$30	; : BANK 0
	.text	$3a,$42,$b2,$c2	; : B = PEEK
	.text	$28		; ( start
	.text	format("%2d",COPIED2)
	.text	$aa,$41,$29,$3a	; + A ) :
	.text	$dc,$31,$35,$3a	; BANK 1 5 :
	.text	$97		; POKE start
	.text 	format("%2d",COPIED2)
	.text	$aa,$41,$2c,$42	; + A , B
	.text	$3a,$82,$00	; : NEXT
+
.endif
DECKSIZ	= pstdeck-deck
	.word	(+), 2055
	.text	$99,$22,$1f,$09	; PRINT " CHR(31) CHR$(9) // BLU,enable
	.text	$8e,$08,$93	; CHR$(142) CHR$(8) CHR$(19) // UPPER,disabl,clr
topline	.text	format("%2d", DECKSIZ)
	.text	" left "
toplin1	.text	"  =1 "
toplin2	.text	"  =2 "
toplin3	.text	"  =3",$99
.if VIC20NO
	.text	" daudzoss/petscido"
.endif
.if DIRTCLR
.warning "unhandled VIC/TED color code to PETSCII conversion from ",DIRTCLR
.else
	.text	$90		; BLK
.endif	
	.text	"all rights helvetiq sa"
	.text	$22,$3a,$9e	; " : SYS main
	.null	format("%4d",main)
+	.word 0
.if !BASIC
*	= COPIED2
.endif

start

FDIM	= 1<<FIELDPW		; must be either 64 (4K field) or 32 (1K field)
FIELDSZ	= FDIM*FDIM		;
FIELDMX = field+FIELDSZ-1	; last byte of FIELDSZ-aligned region 'field'

INITILE	= $3e			; start with five rightmost paths open
XHAIRPV	= SCREENW*SCREENH/2	; character to the right of screen center
XHAIRLT	= XHAIRPV-1		; character to the left of scren center
XHAIRRT	= XHAIRPV+1		; the initial unplaced tile position, w/ XHAIRPV
XHAIRUP = XHAIRPV-SCREENW	;
XHAIRDN	= XHAIRPV+SCREENW	;

FIRSTLT	= XHAIRPV+3*SCREENW-2	; initial (5-exit tile) position below crosshair
FIRSTRT	= XHAIRPV+3*SCREENW-1

STL	= SCREENM+SCREENW	; top-left corner (title/tile line is abovee it)
STL1D	= SCREENM+2*SCREENW	; down 1 row from top-left corner
SBR1U	= SCREENM+SCREENW*(SCREENH-1)-1 ; up 1 row from screen bottom-right corner
SBR	= SCREENM+SCREENW*(SCREENH-0)-1 ; screen bottom-right corner

POINTER = ZP			; static void* POINTER;
POINTR2 = ZP+2			; static void* POINTR2;

CURTILE	= vararea+$0 ;.byte ?	; static uint8_t CURTILE[4]; // shown and 2 more
CURTIL1	= vararea+$1 ;.byte ?	;
CURTIL2	= vararea+$2 ;.byte ?	;
CURTIL3	= vararea+$3 ;.byte ?	;
CURTNUM	= vararea+$4 ;.byte ?	; static uint2_t CURTNUM;
PBACKUP	= vararea+$5 ;.byte ?	; static uint8_t PBACKUP;
LBACKUP	= vararea+$6 ;.byte ?	; static uint8_t LBACKUP;
RBACKUP = vararea+$7 ;.byte ?	; static uint8_t RBACKUP;
UBACKUP	= vararea+$8 ;.byte ?	; static uint8_t UBACKUP;
DBACKUP	= vararea+$9 ;.byte ?	; static uint8_t DBACKUP;
XFLDOFS	= vararea+$a ;.byte ?	; static uint8_t XFLDOFS; // horizontal
YFLDOFS	= vararea+$b ;.byte ?	; static uint8_t YFLDOFS; // vertical
DECKREM	= vararea+$c ;.byte ?	; static uint8_t DECKREM;

ROTNOFS .byte	FDIM+1		; static const ROTNOFS[] = {FDIM+1, //R is 1D 1R
	.byte 	FDIM*2		;                           FDIM*2, //D is 2D
	.byte 	FDIM-1		;                           FDIM-1, //L is 1D 1L
TILESAT	.byte	0		;                           0};// shared with..
TILE1AT	.byte	toplin1-topline	; static const TILESAT[] = {0, topline+10,
TILE2AT	.byte	toplin2-topline	;                              topline+15,
TILE3AT	.byte	toplin3-topline	;                              topline+20};

.if VIC20NO
UNRSLVD	.byte	$05		; static uint8_t UNRSLVD = 5; // INITILE's 5 1's
.endif
	
FIELDC	.byte	DIRTCLR		; static uint8_t FIELDC = DIRTCLR; // black
XHAIRC	.byte	$62		; static uint8_t XHAIRC = 0x62; // bright orange

;;;    2^3
;;; 2^2   2^0
;;;    2^1
;;; upper 4 msb are masked off once that entry/exit connects to adjacent symbol
	torch = $51
symchar	.text	$20		; 0: space, unoccupied spot in play area
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
rot90c	.macro	decrement=0
	and	#$0f		;inline uint8_t rot90c(uint4_t a,
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
	.endm			;} // rot90c()

rot90cw	.macro	decrement=0
.if \decrement == 0
	jsr	rot90c0
.elsif \decrement == dx
	jsr	rot90cx
.elsif \decrement == dy
	jsr	rot90cy
.endif
	.endm

deck
PETSCIDA :?= 0
.if PETSCIDA
	.text	$01,$21,$31,$33,$28,$01,$21,$29,$2d,$30,$09,$2b,$39,$31
	.text	$28,$39,$35,$39,$29,$30,$11,$22,$11,$31,$2a,$29,$24,$09
	.text	$29,$34,$31,$28,$12,$10,$08,$0a,$18,$0c,$03,$38,$1e,$1a
	.text	$12,$0f,$3a,$1e,$1c,$0c,$17,$3c,$14,$18,$13,$2e,$1a,$0b
	.text	$32,$07,$36,$1c,$15,$2c,$0d,$2e,$19,$08,$10,$19,$36,$19
.else
	.text	$23,$17,$06,$36,$12,$04,$38,$2d,$1e,$08,$0f,$34,$19,$33
	.text	$23,$17,$0b,$12,$13,$02,$19,$14,$27,$06,$21,$28,$1d,$0e
	.text	$09,$05,$33,$29,$06,$0d,$07,$2a,$19,$0f,$20,$2a,$2c,$11
	.text	$01,$02,$0c,$30,$35,$1c,$09,$0a,$21,$30,$1a,$35,$05,$01
	.text	$10,$1b,$2d,$04,$03,$32,$28,$1e,$2e,$11,$39,$16,$0c,$2c
.endif
pstdeck

;;;    2^1 2^3
;;; 2^0       2^5
;;;    2^2 2^4
;;; upper 2 msb are used for (clockwise) rotation angle

.if VIC20NO
innsyma	.text	$1		; 0: 
	.text	$5		; 1: enters from left, closed off
	.text	$9		; 2: enters from top left, closed off
	.text	$d		; 3: enters from left, deflected up, closed off
	.text	$3		; 4: enters from bottom left, close off
	.text	$7		; 5: enters from left,deflected down, closed off
	.text	$b		; 6: enters from top left, exits bottom left
	.text	$f		; 7: enters all left sides, closed off

outsyma	.text	1;,1,1,1,1,1,1,1; 000000-000111: no entries/exits in right half
	.text	$c		; 001000-001111: upper right to left half
	.text	$6		; 010000-010111: lower right to left half
	.text	$e		; 011000-011111: upper+lower right to left half
	.text	$5		; 100000-100111: right to left half
	.text	$d		; 101000-101000: right+upper right to left half
	.text	$7		; 110000-110111: right+lower right to left half
	.text	$f		; 111000-111111: all right entries to left half
.endif

innsymm	.macro			;
	and	#$07		;inline uint4_t innsym(uint6_t a) { // on 3 lsb
	tay			;
	lda	innsyma,y	; return innsyma[a & 7] & 0x0f /* on-VIC RAM */;
	and	#$0f		;} // innsymm()
	.endm			;

innsym	.macro
	jsr	innsyme
	.endm

outsymm	.macro			;static uint4_t outsyma = {1,12,6,14,5,13,7,15};
	lsr			;inline uint4_t outsym(uint6_t a) {
	lsr			;
	lsr			;
	and	#$07		;
	tay			; return outsym[(a & 0x38) >> 3] & 0x0f /*VIC*/;
	lda	outsyma,y	;
	and	#$0f		;} // outsymm()
	.endm

outsym	.macro
	jsr	outsyme
	.endm

copynym	.macro
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
copynyb	.macro
	jsr	copynye
	.endm

.if 0
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
.endif

SEEDVAL	:?= 0
SEEDLOC	:?= 0

main	lda	#<SBR1U		;static int called=0;
	sta	POINTER		;
	lda	#>SBR1U		;void main(void) {
	sta	1+POINTER	; POINTER = SBR1U; // one less than BL corner
	lda	#<(SBR1U-SCREENM+SCREENC)
	sta	POINTR2		;
	lda	#>(SBR1U-SCREENM+SCREENC)
	sta	1+POINTR2	; POINTR2 = SBR1U+(SCREENC-SCREENM); // colormem
.if SEEDLOC
	lda	#SEEDVAL	; if (SEEDLOC)
	sta	SEEDLOC		;  *SEEDLOC = SEEDVAL;
.endif
calls1x
.if !VIC20NO			;
	jsr	cphimem		; if (!VIC20NO && !called) // move consts to VIC
.endif				;  cphimem(); // will get overwritten with NOP's
	ldx	#SCREENH-2	; for (uint8_t x = SCREENH-2; x; x--) {
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
	bcs	-		;    ; // x now a valid index into the deck
	tax			;

.if VIC20NO
EXTRSHF	:?= 2
	lda	#EXTRSHF	;   for (uint8_t a = EXTRSHF; a; a--) {
main4	pha			;
.endif
	lda	RNDLOC1		;    for (y = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;         y >= *DECKSIZ;
-	lsr			;         y >>= 1)
	cmp	#DECKSIZ	;
	bcs	-		;     ; // y now a valid index into the deck
	tay			;
	lda	deck,x		;
	pha			;    uint8_t temp = deck[x];
	lda	deck,y		;
	sta	deck,x		;    deck[x] = deck[y];
	pla			;
	sta	deck,y		;    deck[y] = temp;
.if VIC20NO
	pla			;
	sec			;
	sbc	#1		;
	bne	main4		;   }
.endif
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
	lda #1<<((FIELDPW-5)*2+1);           (YFLDOFS << FIELDPW) |
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
	ldy	#4*FDIM-2	;
	sta	(POINTR2),y	; POINTR2[4*FDIM-2] = a; //(XFLDOFS-2,YFLDOFS+3)

	and	#$0f		;
	tay			;
	lda	symchar,y	; a = symchar[a & 0x0f];
	sta	SCREENM+FIRSTLT	; SCREENM[FIRSTLT] = a;
	lda	FIELDC		;
	sta	SCREENC+FIRSTLT	; SCREENC[FIRSTLT] = FIELDC; // now visible

	lda	#INITILE	;
	outsym			;
	copynyb			; a = copynyb(outsym(INITILE));
	ldy	#4*FDIM-1	;
	sta	(POINTR2),y	; POINTR2[4*FDIM-1] = a; //(XLFDOFS-1,YFLDOFS+3)
	and	#$0f		;
	tay			;
	lda	symchar,y	; a = symchar[a & 0x0f];
	sta	SCREENM+FIRSTRT	; SCREENM[XHAIRLT] = a;
	lda	FIELDC		;
	sta	SCREENC+FIRSTRT	; SCREENC[XHAIRLT] = FIELDC; // now visible

	ldy	#DECKSIZ	;
	dey			;
	;sty	DECKREM		;
	lda	deck,y		;
	sta	CURTIL3		; CURTILE[3] = deck[--DECKREM];
	jsr	numleft		;   numleft(); // decr # remaining tiles
	dey			;
	sty	DECKREM		;
	lda	deck,y		;
	sta	CURTIL2		; CURTILE[2] = deck[--DECKREM];
	jsr	numleft		;   numleft(); // decr # remaining tiles
	lda	#1		;
	sta	CURTNUM		; CURTNUM = 1; // or 2 or 3

loop	ldy	DECKREM		; for (;;) { // place a new current tile
	beq	+		;  if (DECKREM != 0) {
	dey			;
	sty	DECKREM		;
	lda	deck,y		;
	ldy	CURTNUM		;
	sta	CURTILE		;   CURTILE[0] =
	sta	CURTILE,y	;            CURTILE[CURTNUM] = deck[--DECKREM];
	jsr	numleft		;   numleft(); // decr # remaining tiles
+	jsr	reveal		;   reveal();

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

cyxhair	lda	XHAIRC		;
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
	cmp	#$00		;
	beq	loop7		;    while ((a = getchar()) { // keyboard loop
	cmp	#'1'		;
	beq	+		;
	cmp	#'2'		;
	beq	+		;
	cmp	#'3'		;
	bne	+++		;     if (a == 0x31 || a == 0x32 || a == 0x33) {
+	and	#$03		;
	tay			;
	lda	CURTILE		;
	and	#$c0		;      // turn CURTILE[0] into just its rotation
	sta	CURTILE		;      CURTILE[0] &= 0xc0; // undo if not valid
	lda	CURTILE,y	;
	bne	+		;      if (CURTILE[a & 0x03]) // not endgame 0's
	ldy	CURTNUM		;
	lda	CURTILE,y	;
+	sty	CURTNUM		;       CURTNUM = a & 0x03;
	ora	CURTILE		;
	sta	CURTILE		;      CURTILE[0] |= CURTILE[CURTNUM]; // w/rotn
	clc			;
	jmp	cyxhair		;      goto cyxhair;

+	and	#$df		;      }
	cmp	#$1d		;     } else if ((a &= 0xdf) == 0x1d)
	bne	+		;
 	jsr	inright		;      inright();
	jmp	loop1		;
+	cmp	#$11		;     else if (a == 0x11)
	bne	+		;
	jsr	indown		;      indown();
	jmp	loop1		;
+	cmp	#$9d		;     else if (a == 0x9d)
	bne	+		;
	jsr	inleft		;      inleft();
	jmp	loop1		;
+	cmp	#$91		;     else if (a == 0x91)
	bne	+		;
	jsr	inup		;      inup();
	jmp	loop1		;
+
.if VIC20NO
	cmp	#'d'		;     else if (a == 0x44)
	bne	+		;
 	jsr	inright		;      inright();
+	cmp	#'s'		;     else if (a == 0x53)
	bne	+		;
	jsr	indown		;      indown();
	jmp	loop1		;
+	cmp	#'a'		;     else if (a == 0x41)
	bne	+		;
	jsr	inleft		;      inleft();
	jmp	loop1		;
+	cmp	#'w'		;     else if (a == 0x57)
	bne	+		;
	jsr	inup		;      inup();
	jmp	loop1		;
+	cmp	#'l'		;     else if (a == 0x4c)
	bne 	+		;
	jsr	inright		;      inright();
	jmp	loop1		;
+	cmp	#'k'		;     else if (a == 0x4b)
	bne	+		;
	jsr	indown		;      indown();
	jmp	loop1		;
+	cmp	#'j'		;     else if (a == 0x4a)
	bne	+		;
	jsr	inleft		;      inleft();
	jmp	loop1		;
+	cmp	#'i'		;     else if (a == 0x49)
	bne	+		;
	jsr	inup		;      inup();
	jmp	loop1		;
	
+	cmp	#'f'		;
	bne	+		;     else if (a == 0x46) {
	lda	XHAIRC		;
	clc			;
	adc	#1		;
	and	#$0f		;      a = (XHAIRC + 1) & 0x0f; // cycle color
	pha			;
	lda	XHAIRC		;
	and	#$f0		;
	sta	XHAIRC		;      XHAIRC &= 0xf0; // keep any luminance bits
	pla			;
	ora	XHAIRC		;
	sta	XHAIRC		;      XHAIRC |= a; // write back
	jmp	cyxhair		;

+	cmp	#'b'		;     } else if (a == 0x41) { // bgnd/border clr
 bne	+		;
 brk

+	cmp	#$80		;     } else if (a == $a0 & 0xdf) { // ccw rot'n

+
.endif

	cmp	#0		;     else if (a == ' ' & 0xdf)
	bne	+		;
	jmp	loop2		;      break; // rotate cw through all 4 options

+	cmp	#$0d		;
	bne	++		;     else if (a == '\r') {
	jsr	stampit		;
	bne	+		;      if (stampit() == 0) // copies tile into (both nybbles of) corresponding two field squares, later must return signed delta conn#      
	jmp	loop7		;       continue; // FIXME?: stampit() isn't returning correctly
+	jmp	loop		;      goto loop; // draw new tile and reveal



+	cmp	#$14
	bne	++		;
	ldy	DECKREM		;
	beq	++		;     } else if ((a == 0x14) && DECKREM) {
	ldx	#$03		;      for (uint8_t x = 3; x; x--) {
-	lda	CURTILE,x	;
	beq	+		;       if (CURTILE[x]) {
	pha			;
-	lda	deck-1,y	;        for (y = DECKREM; y; y--)
	sta	deck,y		;         deck[y] = deck[y-1];
	dey			;
	bne	-		;
	pla			;
	sta	deck		;        deck[0] = CURTILE[x];
	ldy	DECKREM		;
	lda	deck,y		;
	sta	CURTILE,x	;        CURTILE[x] = deck[DECKREM];
	lda	CURTILE		;
	and	#$c0		;        CURTILE[0] &= 0xc0; // grab rot'n bits
	ora	CURTILE,x	;
	sta	CURTILE		;        CURTILE[0] |= CURTILE[x];
+	dex			;       }
	bne	--		;      }
	jsr	reveal		;      reveal();
	jmp	cyxhair		;
+
.if VIC20NO
	cmp	#$13		;     } else if (a == 0x13 /*CLR/HOME*/ {
	bne	++++++		;
	lda	#+~(FDIM/2)	;
	bit	XFLDOFS		;      // works because FDIM/2 has one bit set,
	bne	+		;
	bit	YFLDOFS		;      // so any other values will pass the mask
	bne	+		;
	jmp	loop7		;      if (XFLDOFS!=FDIM/2 || YFLDOFS!=FDIM/2) {

+	lda	XFLDOFS		;       // move at least once, revisiting loop1:
-	cmp	#FDIM/2		;
	beq	++		;       while (XFLDOFS != FDIM/2)
	bcs	+		;        if (XFLDOFS < FDIM/2) // left of center
	jsr	inright		;         inright(); // so bump right
	lda	XFLDOFS		;
	bne	-		;        else // still right of center
+	jsr	inleft		;         inleft(); // so bump left
	lda	XFLDOFS		;
	bne	-		;
	
+	lda	YFLDOFS		;
-	cmp	#FDIM/2		;
	beq	++		;       while (YFLDOFS != FDIM/2)
	bcs	+		;        if (YFLDOFS < FDIM/2) // high of center
	jsr	indown		;         indown(); // so bump down
	lda	YFLDOFS		;
	bne	-		;        else // still low of center
+	jsr	inup		;         inup(); // so bump up
	lda	YFLDOFS		;
	bne	-		;

+	clc			;       goto loop1; // redraw crosshair correctly
	jmp	loop1		;      }

+
.endif	
	cmp	#'q'		;
	beq	+		;
	jmp	loop7		;     } else if (a == 0x51) {
+
	lda	SCREENM+1	;
	pha			;
	lda	SCREENM		;
	pha			;
	lda	#$11		;      printf("%cQ?", 0x13 /* HOME */);
	sta	SCREENM		;      if (getchar() & 0xdf != 'y')      
	lda	#'?'		;
	sta	SCREENM+1	;       continue;
-	jsr	$ffe4		;       else
	cmp	#$00		;
	beq	-		;       return;
	and	#$df		;     }
	tay			;
	pla			;
	sta	SCREENM		;
	pla			;
	sta	SCREENM+1	;    } // next keyboard input

	cpy	#'y' & $df	;   } // next rotation
	beq	+		;  } // next position
	jmp	loop7		; } // next tile
;+	rts			;} // main()
reveal	ldx	#3		;void reveal(void) {
-	lda	CURTILE,x	; for (uint8_t x = 3; x; x--) {  
	innsym			;
	tay			;
	lda	symchar,y	;  uint8_t a = symchar[innsym(CURTILE[x])]; // L
	ldy	TILESAT,x	;
	sta	SCREENM,y	;  SCREENM[TILESAT[x]] = a; // on the screen
	lda	CURTILE,x	;
	outsym			;
	tay			;
	lda	symchar,y	;  a = symchar[outsym(CURTILE[x])]; // R
	ldy	TILESAT,x	;
	iny			;
	sta	SCREENM,y	;  SCREENM[TILESAT[x]+1] = a; // on the screen
	dex			;
	bne	-		; }
+	rts			;} // reveal()

liftile	lda	PBACKUP		;void liftile(void) {
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
	rts			;} // liftile()

movptrs	.macro	delta		;inline uint1_t movptrs(const int8_t delta) {
.if \delta == +1
	lda	#FDIM-2		; if (delta == +1) {
	cmp	XFLDOFS		;  if (XFLDOFS >= FDIM-2)
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
	lda	#FDIM-2		; } else if (delta == +FDIM) {
	cmp	YFLDOFS		;  if (YFLDOFS >= FDIM-2)
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
	cmp	#2		;  if (XFLDOFS < 2)
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
	cmp	#2		;  if (YFLDOFS < 2)
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

blshare .macro			;void blshare(uint8_t* ax_src_lh) {
	sta	(+)+11		;
	stx	(+)+12		; // initialized self-modifying src to (x<<8)|a
+
	.endm			;} // blshare()[blconst REPLACEMENT to save RAM]
	
blconst	.macro	src
	lda	# <\src		;void blconst(uint8_t* src) {
	sta	(+)+11		;
	lda	# >\src		; // initialized self-modifying src to src
	sta	(+)+12		;} // blconst
+
	.endm

blitter	.macro	src,dst,ends	;// IMMEDIATELY precede with blconst()/blshare()
.if \src < \dst
	lda	# <\dst		;void blitter(uint8_t* src, uint8_t* dst,
	sta	(+)+1		;             uint8_t* ends) {
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
	lda	# <\dst		;void blitter(uint8_t* src, uint8_t* dst,
	sta	(+)+1		;             uint8_t* ends) {
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

wipemac	.macro	rownum		;inline void wipemac(uint8_t a, uint8_t y,
-	sta	SCREENM+(\rownum)*SCREENW-1, y;      uint8_t rownum) {
	dey			; do { SCREENM[rownum*SCREENW + y -1] = a;
	bne	-		; } while (--y);
	.endm			;} // wipemac

wiperow	ldy	#SCREENW	;void wiperow(uint8_t a, uint8_t x) {
	cpx	#SCREENH-1	; switch (x) {
	bne	+		; case SCREENH-1:
	wipemac	SCREENH-1	;  wipemac(a, SCREENW, SCREENH-1);
	rts			;  return;
+	cpx	#1		; case 1:
	bne	+		;  wipemac(a, SCREENW, 1);
	wipemac	1		; }
+	rts			;} // wiperow()

wipecol
.for r := 1, r < SCREENH, r += 1;inline void wipecol(uint8_t a, uint8_t y) {
	sta SCREENM+r*SCREENW,y ; for (uint8_t r = 1; r < SCREENH; r++)
.next				;  SCREENM[r*SCREENW + y] = a;
	rts			;} // wipecol()
 
setpntr	; lda	XFLDOFS		;void setpntr(uint8_t a) { // a = XFLDOFS
	sec			; // set POINTER to overlap the field with the
	sbc	#SCREENW/2	; // screen square below the upper-left corner
	ldx	#8-FIELDPW	; // of the screen
-	asl			;
	dex			;
	bne	-		;
	sta	POINTER		; POINTER = (void*) ((XFLDOFS - SCREENW/2)
	lda	YFLDOFS		;
	sec			;
	sbc	#SCREENH/2 - 1	;
	ldx	#8-FIELDPW	;
-	lsr			;
	ror	POINTER		;
	dex			;
	bne	-		;       | (FDIM * (YFLDOFS - SCREENH / 2 + 1))
	ora	#>field		;       | field);
	sta	1+POINTER	;
	rts			;} // setpntr()

setpntb	; lda	XFLDOFS		;void setpntb(uint8_t a) { // a = XFLDOFS
	jsr	setpntr		; setpntr(a);
	ldx	#SCREENH-1	; // set POINTER to overlap the field with the
-	clc			; // screen square at the lower-left corner
	lda	POINTER		; // of the screen
	adc	#FDIM		;
	sta	POINTER		;
	lda	1+POINTER	;
	adc	#0		;
	sta	1+POINTER	;
	dex			;
	bne	-		; POINTER += FDIM * (SCREENH - 2); // lower left
	rts			;} // setpntb()

regenlr	lda	#<STL		;void regenlr(uint8_t x, uint8_t y) {
	sta	regensm+1	; uint8_t* dest; // x is window height, y is col
	lda	#>STL		;
	sta	regensm+2	; for (dest = STL; x; x--) {
-	txa			;
	pha			;
	lda	(POINTER),y	;
	and	#$0f		;
	tax			;
	lda	symchar,x	;
regensm	sta	$ffff,y		;  dest[y] = symchar[POINTER[y] & 0x0f];
	clc			;
	lda	POINTER		;
	adc	#FDIM		;
	sta	POINTER		;
	lda	1+POINTER	;
	adc	#0		;
	sta	1+POINTER	;  POINTER += FDIM;
	clc			;
	lda	regensm+1	;
	adc	#SCREENW	;
	sta	regensm+1	;
	lda	regensm+2	;
	adc	#0		;
	sta	regensm+2	;  dest += SCREENW;
	pla			;
	tax			;
	dex			;
	bne	-		; }
	rts			;} // regenlr()

regent	lda	#<STL		;void regent(void) {
	sta	regn2sm+1	; uint8_t* dest = STL /*upper left*/;
	lda	#>STL		;
	sta	regn2sm+2	; regentb(dest);
	jmp	regentb		;}
regenb
.if VIC20NO	
	ldy	#$11		;
-	lda	newmesg,y	;
	sta	SCREENM+$16,y	;
	dey			;
	bne	-		;
	beq	+		;
newmesg	.text	$00,$12,$09,$07	;
	.text	$08,$14,$13,$20	;
	.text	$08,$05,$0c,$16	;
	.text	$05,$14,$09,$11	;
	.text	$27,$13		;
+
.endif
	lda	#<(SBR1U+1)	;void regenb(void) {
	sta	regn2sm+1	; uint8_t* dest = SBR1U + 1 /*lower left*/;
	lda	#>(SBR1U+1)	; regentb(dest);
	sta	regn2sm+2	;}
regentb	ldy	#SCREENW-1	;void regentb(uint8_t* dest) {
-	lda	(POINTER),y	; for (int8_t y = SCREENW - 1; y >= 0; y--)
	and	#$0f		;
	tax			;
	lda	symchar,x	;
regn2sm	sta	$ffff,y		;  dest[y] = symchar[POINTER[y] & 0x0f ];
	dey			;
	bpl	-		;
	rts			;} // regentb()

repaint	.macro	xlim=0, ylim=0	;inline void repaint(uint8_t xlim,uint8_t ylim){
.if \xlim			; if (xlim) {
 .if \xlim < 0			;  if (xlim < 0) {
	lda	XFLDOFS		;   // regenerate right edge from field
	cmp	#-\xlim		;
	bcs	+		;   if (XFLDOFS < abs(xlim)) {
	jsr	setpntr		;    setpntr(XFLDOFS);
	ldy	#SCREENW-1	;
	ldx	#SCREENH-1	;    regenlr(SCREENH-1/*see below*/, SCREENW-1);
	jsr	regenlr		;   }
+
 .else				;  } else {
	lda	XFLDOFS		;   // regenerate left edge from field
	cmp	#\xlim+1		;
	bcc	+		;   if (XFLDOFS > xlim) {
	jsr	setpntr		;    setpntr(XFLDOFS);
	ldy	#0		;
	ldx	#SCREENH-1	;    regenlr(SCREENH-1 /*top row skipped*/, 0);
	jsr	regenlr		;   }
+
 .endif				;  } 
.elsif \ylim			; } else if (ylim) {
 .if \ylim < 0			;  if (ylim < 0) {
	lda	YFLDOFS		;   // regenerate bottom edge from field
	cmp	#-\ylim		;
	bcs	+		;   if (YFLDOFS < abs(ylim)) {
	lda	XFLDOFS		;
	jsr	setpntb		;    setpntb(XFLDOFS); // special case
	ldy	#SCREENW	;    regenb();
	jsr	regenb		;   }
+
 .else
	lda	YFLDOFS		;   // regenerate top edge from field
	cmp	#\ylim+1		;
	bcc	+		;   if (YFLDOFS > ylim) {
	lda	XFLDOFS		;
	jsr	setpntr		;    setpntr(XFLDOFS);
	ldy	#SCREENW	;    regent();
	jsr	regent		;   }
+
 .endif				;  }
.else				; }
 .error "either xlim or ylim must be nonzero"
.endif
	.endm			;} // repaint()

blit_ul	blshare			;void blit_ul(uint8_t* ax_lh) { blshare(ax_lh);
	blitter	SBR-1,SBR,STL	; blitter(SBR-1 /* SBR1U is equiv. */,SBR,STL);
	rts			;}
	
blit_dr	blshare			;void blit_dr(uint8_t* ax_lh) { blshare(ax_lh);
	blitter	STL+1,STL,SBR	; blitter(STL+1 /* STL1D is equiv. */,STL,SBR);
	rts			;}

CATCHLT	= (SCREENW/2)		; if XFLDOFS < CATCHLT no new L col to bring in
CATCHRT	= (FDIM-(SCREENW/2)-1)	; if XFLDOFS > CATCHRT no new R col to bring in
CATCHUP	= (SCREENH/2)		; if YFLDOFS < CATCHUP no new T row to bring in
CATCHDN	= (FDIM-(SCREENH/2)-1)	; if YFLDOFS > CATCHDN no new B row to bring in

inright	movptrs	+1		;void inright(void) {
	bcs	+		; if (movptrs(+1) == 0) {
	jmp	loop7		;
+	jsr	liftile		;  liftile();
	lda	#< (STL+1)	;  uint8_t a = (STL+1) & 0x000f;
	ldx	#> (STL+1)	;  uint8_t x = (STL+1) >> 8;
	jsr	blit_dr		;  blit_dr((x<<8)|a,STL,SBR);
	lda	#$20		;//  blitter(STL+1,STL,SBR);
	ldy	#SCREENW-1	;  wipecol(0x20, SCREENW-1); // rightmost
	jsr	wipecol		;  repaint(-SCREENW/2,0);
	repaint	-CATCHRT,	;  goto loop1;
+	clc			; }
	rts			;} // inright()

indown	movptrs	+FDIM		;void indown(void) {
	bcs	+		; if (movptrs(+FDIM) == 0) {
	jmp	loop7		;
+	jsr	liftile		;  liftile();
	lda	#< (STL1D)	;  uint8_t a = (STL1D) & 0x000f;
	ldx	#> (STL1D)	;  uint8_t x = (STL1D) >> 8;
	jsr	blit_dr		;  blit_dr((x<<8)|a,STL,SBR);
	lda	#$20		;//  blitter(STL1D,STL,SBR);
	ldx	#SCREENH-1	;  wiperow(0x20, SCREENH-1); // bottommost
	jsr	wiperow		;  repaint(0,-SCREENH/2-1);
	repaint	,-CATCHDN	;  goto loop1;
+	clc			; }
	rts			;} // indown()

inleft	movptrs	-1		;void inleft(void) {
	bcs	+		; if (movptrs(-1) == 0) {
	jmp	loop7		;
+	jsr	liftile		;  liftile();
	lda	#< (SBR-1)	;  uint8_t a = (SBR-1) & 0x000f;
	ldx	#> (SBR-1)	;  uint8_t x = (SBR-1) >> 8;
	jsr	blit_ul		;  blit_ul((x<<8)|a,STL,SBR);
	lda	#$20		;//  blitter(SBR-1,SBR,STL)
	ldy	#0		;  wipecol(0x20, 0); // leftmost
	jsr	wipecol		;  repaint(+SCREENW/2-1,0);
	repaint	+CATCHLT,	;  goto loop1;
+	clc			; }
	rts			;} // inleft()

inup	movptrs	-FDIM		;void inup(void)
	bcs	+		; if (movptrs(-FDIM) == 0) {
	jmp	loop7		;
+	jsr	liftile		;  liftile();
	lda	#< (SBR1U)	;  uint8_t a = (SBR1U) & 0x000f;
	ldx	#> (SBR1U)	;  uint8_t x = (SBR1U) >> 8;
	jsr	blit_ul		;  blit_ul((x<<8)|a,STL,SBR);
	lda	#$20		;//  blitter(SBR1U,SBR,STL)
	ldx	#1		;  wiperow(0x20, 1); // topmost
	jsr	wiperow		;  repaint(0,SCREENH/2);
	repaint	,+CATCHUP	;  goto loop1;
+	clc			; }
	rts			;} // inup()

angle	.macro			;inline uint2_t angle(uint8_t a) {
	rol			;
	rol			;
	rol			;
	and	#$03		; return a >>= 6;
	.endm			;}

tofield	.macro			;inline uint8_t tofield(uint1_t out,
	lda	CURTILE		;                       register uint8_t& x,
	.if \1			;                       register uint8_t& y) {
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
nostamp	lda	#0		; return 0;
	rts			;}

numleft	dec	SCREENM+1	;void numleft(void) {
	lda	SCREENM+1	; static char remain = {'6', '7'};
	cmp	#'0'-1		; remain[1] -= 1; // decrement # remaining tiles
	bne	++		; if (remain[1] < '0') {
	lda	#'9'		;
	sta	SCREENM+1	;  remain[1] = '9'; // restore 1's digit to 9
	dec	SCREENM		;  remain[0] -= 1; // decrement 10's digit
	lda	SCREENM		;
	cmp	#'0'		;
	bne	+		;  if (remain[0] == '0')
	lda	#' '		;
	sta	SCREENM		;   remain[0] = ' '; // at 9, wipe leading zero
	rts			;
+	cmp	#' '-1		;  else if (remain[0] < ' ')
	bne	+		;   _brk();
;	brk			; }
+	rts			;} // numleft()

rot90c0	rot90c
	rts
rot90cx	rot90c	dx
	rts
rot90cy	rot90c	dy
	rts
innsyme	innsymm
	rts
outsyme outsymm
	rts
copynye	copynym
	rts

	.align	FIELDSZ
field
 .if (field <= SCREENM) && (field + FIELDSZ >= SCREENM)
 .warn "code has grown too big for unexpanded vic20"
 .endif
.if VIC20NO
  	.fill	FIELDSZ
.else
innsyma	= SCREENC-(cphimem-field)
	.text	$1		; 0: 
	.text	$5		; 1: enters from left, closed off
	.text	$9		; 2: enters from top left, closed off
	.text	$d		; 3: enters from left, deflected up, closed off
	.text	$3		; 4: enters from bottom left, close off
	.text	$7		; 5: enters from left,deflected down, closed off
	.text	$b		; 6: enters from top left, exits bottom left
	.text	$f		; 7: enters all left sides, closed off

outsyma	= innsyma + * - field
	.text	1;,1,1,1,1,1,1,1; 000000-000111: no entries/exits in right half
	.text	$c		; 001000-001111: upper right to left half
	.text	$6		; 010000-010111: lower right to left half
	.text	$e		; 011000-011111: upper+lower right to left half
	.text	$5		; 100000-100111: right to left half
	.text	$d		; 101000-101000: right+upper right to left half
	.text	$7		; 110000-110111: right+lower right to left half
	.text	$f		; 111000-111111: all right entries to left half

cphimem	ldy	#cphimem-field	;void cphimem(void) {
-	lda	field-1,y	; for (uint8_t y = 16; y; y--)
	sta	innsyma-1,y	;  innsyma[y-1] = field[y-1]; //
	dey			;
	bne	-		; // const nybble arrays have been copied from
	lda	#$ea		; // field into VIC on-chip RAM in lower 512x4b
	sta	calls1x		;
	sta	calls1x+1	; // only can/needs to be copied into VIC once
	sta	calls1x+2	; called = 1; // call to this replace with NOP
	rts			;} // cphimem()
	.fill	FIELDSZ-(*-field)
.endif
vararea
