CC65DIR=../../cc65
CC65BINDIR=${CC65DIR}/bin
CA65=${CC65BINDIR}/ca65
LD65=${CC65BINDIR}/ld65

CAOPTS=-g
LDOPTS=

NESTOPIADIR=../../emu/nestopia-1.46.2
FCEUXDIR=../../emu/fceux-2.2.2-win32
NINTENDULATORDIR=../../emu/nintendulatordx-v34

.PHONY: clean all nestopia nintendulator

all: bumble.nes

nestopia: bumble.nes
	${NESTOPIADIR}/nestopia bumble.nes 2> /dev/null > /dev/null

nintendulator: bumble.nes
	wine ${NINTENDULATORDIR}/Nintendulator.exe bumble.nes 2> /dev/null

fceux: bumble.nes
	wine ${FCEUXDIR}/fceux.exe bumble.nes 2> /dev/null

bumble.nes: locals.o main.o ppu.o joy.o actor_routines.o ai.o ldcfg/nrom.cfg 
	${LD65} ${LDOPTS} -o bumble.nes -C ldcfg/nrom.cfg -m bumble.map.txt \
		-Ln bumble.labels.txt --dbgfile bumble.nes.dbg \
		main.o ppu.o joy.o locals.o actor_routines.o ai.o

main.o: main.s locals.inc ines.inc ppu.inc joy.inc constants.inc actor.inc \
		ai.inc actor_routines.inc physics.inc math_macros.inc chr/sprites.chr
	${CA65} ${CAOPTS} -o main.o main.s

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
	rm -f locals.o bumble.nes main.o ppu.o joy.o actor_routines.o \
		ai.o chr/sprites.png chr/sprites.chr *.map.txt *.labels.txt *.nes.dbg
