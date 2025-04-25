all : petscido_vic20.prg petscido_p500.prg petscido_c64_c128.prg petscido_c16.prg

petscido_vic20.prg : main.asm vic20/header.inc
	rm -f header.inc; ln -s vic20/header.inc; 64tass -a header.inc main.asm -L petscido_vic20.lst -o petscido_vic20.prg

petscido_p500.prg : main.asm p500/header.inc
	rm -f header.inc; ln -s p500/header.inc; 64tass -a header.inc main.asm -L petscido_p500.lst -o petscido_p500.prg

petscido_c64_c128.prg : main.asm c64_c128/header.inc
	rm -f header.inc; ln -s c64_c128/header.inc; 64tass -a header.inc main.asm -L petscido_c64_c128.lst -o petscido_c64_c128.prg

petscido_c16.prg : main.asm c16/header.inc
	rm -f header.inc; ln -s c16/header.inc; 64tass -a header.inc main.asm -L petscido_c16.lst -o petscido_c16.prg

clean :
	rm -f *.prg *.lst


