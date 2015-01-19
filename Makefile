CC65DIR=../../cc65
CC65BINDIR=${CC65DIR}/bin
CA65=${CC65BINDIR}/ca65
LD65=${CC65BINDIR}/ld65

CAOPTS=-g
LDOPTS=

.PHONY: clean all

all: bees.nes

bees.nes: locals.o bees.o ppu.o joy.o actor_routines.o ai.o ldcfg/nrom.cfg 
	${LD65} ${LDOPTS} -o bees.nes -C ldcfg/nrom.cfg -m bees.map.txt \
		-Ln bees.labels.txt --dbgfile bees.nes.dbg \
		bees.o ppu.o joy.o locals.o actor_routines.o ai.o

bees.o: bees.s locals.inc ines.inc ppu.inc joy.inc constants.inc actor.inc \
		ai.inc actor_routines.inc physics.inc math_macros.inc chr/sprites.chr
	${CA65} ${CAOPTS} -o bees.o bees.s

ppu.o: ppu.s
	${CA65} ${CAOPTS} -o ppu.o ppu.s

joy.o: joy.s locals.inc
	${CA65} ${CAOPTS} -o joy.o joy.s

locals.o: locals.s
	${CA65} ${CAOPTS} -o locals.o locals.s

actor_routines.o: actor_routines.s locals.inc constants.inc actor.inc
	${CA65} ${CAOPTS} -o actor_routines.o actor_routines.s

ai.o: ai.s locals.inc joy.inc actor.inc constants.inc math_macros.inc
	${CA65} ${CAOPTS} -o ai.o ai.s

chr/sprites.chr: chr/sprites.png bin/bmp2nes
	bin/bmp2nes -i chr/sprites.png -o chr/sprites.chr

chr/sprites.png: chr/sprites.xcf bin/xcf2png
	bin/xcf2png chr/sprites.xcf chr/sprites.png

clean:
	rm -f locals.o bees.nes bees.o ppu.o joy.o actor_routines.o \
		ai.o chr/sprites.png chr/sprites.chr *.map.txt *.labels.txt *.nes.dbg
