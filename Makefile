CA65="../../cc65/bin/ca65"
LD65="../../cc65/bin/ld65"

CAOPTS=-g
LDOPTS=

.PHONY: clean all

all: bees.nes

bees.nes: bees.o ppu.o joy.o ../inc/nrom.cfg 
	${LD65} ${LDOPTS} -o bees.nes -C ../inc/nrom.cfg -m bees.map.txt \
		-Ln bees.labels.txt --dbgfile bees.nes.dbg \
		bees.o ppu.o joy.o

bees.o: bees.s ppu.inc joy.inc ines.inc math_macros.inc chr/sprites.chr
	${CA65} ${CAOPTS} -o bees.o bees.s

ppu.o: ppu.s
	${CA65} ${CAOPTS} -o ppu.o ppu.s

joy.o: joy.s
	${CA65} ${CAOPTS} -o joy.o joy.s

clean:
	rm -f bees.nes bees.o ppu.o joy.o *.map.txt *.labels.txt *.nes.dbg
