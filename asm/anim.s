.include "locals.inc"
.include "math_macros.inc"
.include "ppu.inc"

.segment "ZEROPAGE"
; currently updating effect. op handlers must not clobber
current_effect_addr = addr_0
; address of current executing opcode. op handlers can clobber to read args
current_pc			= addr_1

.segment "CODE"
.macro incr_pc_by num_bytes
	; assumes PC is at offset 0 within effect structure
	clc 
	ldy #Anim::EffectOffset::PC
	lda (current_effect_addr), Y
	adc num_bytes
	sta (current_effect_addr), Y
	bne :+
		iny
		lda (current_effect_addr), Y
		adc num_bytes
		sta (current_effect_addr), Y
	:
.endmacro

.segment "CODE"
; Animation Engine
.scope Anim
	N_EFFECTS = 16 ; must be at least 1 if you plan to call do_frame
	MAX_INSTRUCTIONS_PER_EFFECT_PER_FRAME = 8

	; would overflow index
	.assert Anim::N_EFFECTS < 256, error, "N_EFFECTS too large, must be < 256"
	.assert Anim::N_EFFECTS > 0, error, "N_EFFECTS must be > 0"

	.scope Op
		; byte values of opcodes are indices into instruction_handler_table
		.export nop						= $00
		.export yield					= $01
		.export clear_active			= $02
		.export clear_active_and_yield	= $03
		.export jmp_rel					= $04
		.export ppumask_set				= $05
	.endscope

	.scope EffectOffset
		.export PC = 0		; 2-byte "program counter"

		.export STATE = 2	; 1-byte state
							; 765543210
							; |||||||||
							; ||+++++++--- oam_entry_index
							; |+---------- has_yielded
							; +----------- is_active
							
		;;;;;;;;
		.export EFFECT_SIZE = 3
	.endscope

	.scope EffectStateMask
		.export OAM_ENTRY_INDEX = %00111111
		.export HAS_YIELDED		= %01000000
		.export IS_ACTIVE		= %10000000
	.endscope

	.segment "RAM"
	; reserved array of all effect objects
	effects_array: .res Anim::N_EFFECTS * EffectOffset::EFFECT_SIZE
	.export effects_array

	.segment "CODE"
	; animation opcode ("op") handlers
	.scope AnimOpHandler
		; NB: op handlers must not clobber X, local_0, or addr_0
		; When handler is called, current_effect_addr points to current effect,
		; current_pc contains the current value of the current effect's PC, i.e.,
		; points to the current opcode. handlers may clobber current_pc.
		; Handler guaranteed to be called with Y=2
		.proc nop ; XXX would be clever to reuse end of yield
			incr_pc_by #1
			rts 
		.endproc

		; stop executing effect this frame, continue at next instruction next frame
		.proc yield
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			ora #EffectStateMask::HAS_YIELDED
			sta (current_effect_addr), Y
			incr_pc_by #1
			rts
		.endproc

		; mark the currently executing effect as no longer active.
		; NOTE -- has no effect until next frame comes around; user must follow
		; clear_active with yield to actually stop executing in this frame
		.proc clear_active
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			; XXX stupid ca65: bitwise not fails: #~(EffectStateMask::IS_ACTIVE)
			and #(EffectStateMask::IS_ACTIVE ^ $ff)
			sta (current_effect_addr), Y
			; leave pc alone
			rts
		.endproc

		; more efficient than adjacent clear_active and yield instructions
		.proc clear_active_and_yield
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			; XXX stupid ca65: bitwise not fails: #~(EffectStateMask::IS_ACTIVE)
			and #(EffectStateMask::IS_ACTIVE ^ $ff)
			ora #(EffectStateMask::HAS_YIELDED)
			sta (current_effect_addr), Y
			; leave pc alone
			rts
		.endproc

		; relative jmp. takes a one-byte arg to add to the effect PC
		.proc jmp_rel
			; current_pc points to current executing opcode
			; point it to the next byte, our arg
			mathmac_inc16 current_pc 
			; relies on fact that incr_pc_by will set Y = EffectOffset::PC = 0
			incr_pc_by {(current_pc), Y}
			rts
		.endproc

		.proc ppumask_set
			; new ppu mask is in arg
			ldy #1
			lda (current_pc), Y 
			sta PPU::mask
			incr_pc_by #2
			rts
		.endproc
	.endscope

	.segment "RODATA"
	instruction_handler_table:
														; opcode/index
		.word AnimOpHandler::nop - 1					; $00
		.word AnimOpHandler::yield - 1					; $01
		.word AnimOpHandler::clear_active - 1			; $02
		.word AnimOpHandler::clear_active_and_yield - 1	; $03
		.word AnimOpHandler::jmp_rel - 1				; $04
		.word AnimOpHandler::ppumask_set - 1			; $05

	.segment "CODE"
	; routing routine for op handlers. Call with A = index into table = opcode
	; clobbers Y :(
	.proc jump_instruction_handler
		; RTS trick
		asl ; double A cuz each table entry (address) is two bytes long
		tay
		lda instruction_handler_table + 1, Y
		pha
		lda instruction_handler_table, Y
		pha
		rts
	.endproc

	; main function
	; clobbers addr_0, addr_1, A, X, Y
	.export do_frame
	.proc do_frame
		; Animation engine: Each frame (sometime between vblanks), for each
		; active effect:
		; 	* Clear has_yielded
		; 	* While !has_yielded && n_instructions_executed < MAX
		; 		* Animation engine fetches instruction from pc. RTSes to
		;		  handler routine via jump table.
		; 		* Each instruction has a handler routine; handler routines are
		; 		  responsible for advancing/setting pc for next frame. Handler
		; 		  for yield instruction sets has_yielded.

		; reset current_effect_addr pointer to beginning of array
		; XXX stupid ca65, who the fuck knows why this doesn't work
		; mathmac_set16 #Anim::effects_array, current_effect_addr
		lda #<Anim::effects_array
		sta current_effect_addr + 0
		lda #>Anim::effects_array
		sta current_effect_addr + 1

		; throughout this loop, X will be N_EFFECTS minus the index in the
		; array of the current effect; thus we assume that the array is no more
		; than 255 effects long
		ldx #Anim::N_EFFECTS
		each_effect:
			; check if active
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			and #EffectStateMask::IS_ACTIVE
			beq next_effect

			; clear has_yielded
			lda (current_effect_addr), Y
			and #(EffectStateMask::HAS_YIELDED ^ $ff)
			sta (current_effect_addr), Y

			; stash X
			txa
			pha
			; we now use X to count how many instructions we execute
			ldx #Anim::MAX_INSTRUCTIONS_PER_EFFECT_PER_FRAME
			; do
			;  current_pc[0,1] = current_effect_addr[0,1]
			;  opcode = *current_pc
			;  handle(opcode)
			; while !has_yielded && X > 0
			each_instruction:
				ldy #EffectOffset::PC ; Y = 0
				lda (current_effect_addr), Y ; A = low byte of PC
				sta current_pc + 0
				iny ; Y = 1
				lda (current_effect_addr), Y ; A = high byte of PC
				sta current_pc + 1
				; current_pc is now a zeropage pointer to the next opcode to execute
				; opcode is a one byte offset into instruction_handler_table
				dey ; Y = 0
				lda (current_pc), Y ; A = *current_pc (i.e., A = the opcode)
				; handle opcode
				jsr jump_instruction_handler
				; check instruction count
				dex
				beq finish_effect
				; check has_yielded
				ldy #EffectOffset::STATE
				lda (current_effect_addr), Y
				and #EffectStateMask::HAS_YIELDED
				beq each_instruction
				; note that we don't check is_active. script must yield after clear_active
			finish_effect:
				; restore X
				pla
				tax
			next_effect:
				dex
				beq frame_done
				; increment pointer to current effect by size of effect
				mathmac_add16 #EffectOffset::EFFECT_SIZE, current_effect_addr, current_effect_addr
				jmp each_effect

		frame_done:
		rts
	.endproc ; do_frame

.endscope

