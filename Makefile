all : petscido_vic20.prg petscido_p500.prg petscido_c64.prg petscido_c16.prg

petscido_vic20.prg : main.asm vic20/header.inc
	64tass -a vic20/header.inc main.asm -L petscido_vic20.lst -o petscido_vic20.prg

petscido_p500.prg : main.asm p500/header.inc
	64tass -a p500/header.inc main.asm -L petscido_p500.lst -o petscido_p500.prg

petscido_c64.prg : main.asm c64/header.inc
	64tass -a c64/header.inc main.asm -L petscido_c64.lst -o petscido_c64.prg

petscido_c16.prg : main.asm c16/header.inc
	64tass -a c16/header.inc main.asm -L petscido_c16.lst -o petscido_c16.prg

clean :
	rm -f *.prg *.lst


