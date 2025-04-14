petscido.prg : header.inc main.asm
	64tass -a header.inc main.asm -D TESTSYM=1 -L main.lst -o petscido.prg

