# GNU Makefile

# Location of the base directory of the cc65 distribution or repository; must
# contain a bin/ subdirectory where ca65 and ld65 are located.
CC65DIR = ../../cc65

# Optional assembler (ca65) flags, e.g., for debugging symbols
ASFLAGS = -g

# Optional linker (ld65) flags
LDFLAGS =

# Required linker configuration file, which controls the layout of code/data
# segments in the iNES container as well as the runtime addresses of memory
# segments (code, data, RAM).
LDCFG	= ldcfg/nrom.cfg

# Project title, basename of final iNES container file.
PROJECT = bumble

# 
# Emulator locations
#
NESTOPIADIR 	= ../../emu/nestopia-1.46.2
FCEUXDIR		= ../../emu/fceux-2.2.2-win32
NINTENDULATORDIR= ../../emu/nintendulatordx-v34

CC65BINDIR	= $(CC65DIR)/bin
AS			= $(CC65BINDIR)/ca65
LD65		= $(CC65BINDIR)/ld65

.PHONY: clean all nestopia nintendulator summary

all: $(PROJECT).nes summary

clean:
	rm -f locals.o bumble.nes main.o ppu.o joy.o actor_routines.o \
		ai.o chr/sprites.png chr/sprites.chr *.map.txt *.labels.txt *.nes.dbg

#
# Print a table of how memory segments are laid out
#
summary:
	@cat ./bumble.map.txt | grep -A 9 "^Name"

#
# Convenience targets to launch the game in emulators for testing
#
nestopia: 		bumble.nes
	$(NESTOPIADIR)/nestopia bumble.nes 2> /dev/null > /dev/null

nintendulator: 	bumble.nes
	wine $(NINTENDULATORDIR)/Nintendulator.exe bumble.nes 2> /dev/null

fceux: 			bumble.nes
	wine $(FCEUXDIR)/fceux.exe bumble.nes 2> /dev/null

#
# Which object files should get linked together into the final iNES container.
#
$(PROJECT).nes: locals.o main.o ppu.o joy.o actor_routines.o ai.o

#
# Object file dependencies, other than the implicit %.s
#
main.o: 			locals.inc ines.inc ppu.inc joy.inc constants.inc \
					actor.inc ai.inc actor_routines.inc physics.inc \
					math_macros.inc chr/sprites.chr
joy.o: 				locals.inc
actor_routines.o:	locals.inc constants.inc actor.inc
ai.o: 				locals.inc joy.inc actor.inc constants.inc math_macros.inc

#
# Implicit rules on how to build PNGs from XCFs, and CHRs from PNGs. A code
# module that .incbin's a CHR bank need only mention the CHR file in its
# prerequisites, and make will figure out how to generate it through the 
# XCF -> PNG -> CHR pipeline.
#
chr/%.chr: 	chr/%.png bin/bmp2nes
	bin/bmp2nes -i $< -o $@

chr/%.png: 	chr/%.xcf bin/xcf2png
	bin/xcf2png $< $@

# Don't delete the intermediate PNGs created by chaining the above implicit
# rules.
.PRECIOUS: chr/%.png

#
# Implicit pattern rule for linking together .nes files from their dependencies.
# Assumes dependencies are all object files that the linker can accept.
#
%.nes: $(LDCFG)
	$(LD65) $(LDFLAGS) -o $@ -C $(LDCFG) -m $*.map.txt \
		-Ln $*.labels.txt --dbgfile $@.dbg \
		$(wordlist 2, $(words $^), $^)


