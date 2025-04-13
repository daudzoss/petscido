	torch = $51
	
symchar	.text	$20		; 0: space, unoccupied spot in play area
	.text	torch		; 1/2/4/8: circle dead end, closes off escape
	.text	torch 		; 1/2/4/8: circle dead end, closes off escape
	.text	$4a		; 3: right bending upward
	.text	torch		; 1/2/4/8: circle dead end, closes off escape
	.text	$40		; 5: right straight leftward
	.text	$4b		; 6: up bending leftward
	.text	$71		; 7: right straight leftward teed upward
	.text	torch		; 1/2/4/8: circle dead end, closes off escape
	.text	$4a		; 9: right bending downward
	.text	$5d		; 10: up straight downward
	.text	$6b		; 11: up straight downward teed rightward
	.text	$55		; 12: left bending downward
	.text	$72		; 13: right straight leftward teed downward
	.text	$73		; 14: up straight downward teed leftward
	.text	$5b		; 15: all directions inward

innsym	.text
	.text

outsym
