all : vic20.prg c64_c128.prg p500.prg c16.prg

vic20.prg : main.asm
	rm -f header.inc; ln -s vic20/header.inc; 64tass -a header.inc main.asm -L main.lst -o vic20.prg; mv vic20.prg petscido_vic20.prg

c64_c128.prg : main.asm
	rm -f header.inc; ln -s c64_c128/header.inc; 64tass -a header.inc main.asm -L main.lst -o c64_c128.prg; mv c64_c128.prg petscido_c64_c128.prg

p500.prg : main.asm
	rm -f header.inc; ln -s p500/header.inc; 64tass -a header.inc main.asm -L main.lst -o p500.prg; mv p500.prg petscido_p500.prg

c16.prg : main.asm
	rm -f header.inc; ln -s c16/header.inc; 64tass -a header.inc main.asm -L main.lst -o c16.prg; mv c16.prg petscido_c16.prg

clean :
	rm -f petscido_vic20.prg petscido_c64_c128.prg petscido_p500.prg petscido_vic20.prg
