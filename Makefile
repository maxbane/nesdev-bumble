CC65DIR=../../cc65
CC65BINDIR=${CC65DIR}/bin
CA65=${CC65BINDIR}/ca65
LD65=${CC65BINDIR}/ld65

CAOPTS=-g
LDOPTS=

.PHONY: clean all

all: bees.nes

bees.nes: locals.o bees.o ppu.o joy.o ldcfg/nrom.cfg 
	${LD65} ${LDOPTS} -o bees.nes -C ldcfg/nrom.cfg -m bees.map.txt \
		-Ln bees.labels.txt --dbgfile bees.nes.dbg \
		bees.o ppu.o joy.o locals.o

bees.o: bees.s locals.inc ppu.inc joy.inc ines.inc math_macros.inc chr/sprites.chr
	${CA65} ${CAOPTS} -o bees.o bees.s

ppu.o: ppu.s
	${CA65} ${CAOPTS} -o ppu.o ppu.s

joy.o: joy.s
	${CA65} ${CAOPTS} -o joy.o joy.s

locals.o: locals.s
	${CA65} ${CAOPTS} -o locals.o locals.s

chr/sprites.chr: chr/sprites.png bin/bmp2nes
	bin/bmp2nes -i chr/sprites.png -o chr/sprites.chr

clean:
	rm -f locals.o bees.nes bees.o ppu.o joy.o *.map.txt *.labels.txt *.nes.dbg
