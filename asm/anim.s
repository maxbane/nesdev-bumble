.include "locals.inc"
.include "math_macros.inc"
.include "ppu.inc"
.include "coroutine.inc"
.linecont +

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
	adc #<num_bytes
	sta (current_effect_addr), Y
	; FIXME: broken when carrying to high byte
	bne :+
		iny
		lda (current_effect_addr), Y
		adc #>num_bytes
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

	.scope EffectOffset
		.export PC = 0		; 2-byte "program counter"

		.export STATE = 2	; 1-byte state
							; 765543210
							; |||||||||
							; ||+++++++--- oam_entry_index
							; |+---------- has_yielded
							; +----------- is_active
							
		; something like a register/accumulator per effect.
		.export REG = 3
		; todo: also, interpreter can keep a transient flags register per effect per
		; frame, reset between each effect, for holding comparison results etc
		;;;;;;;;
		.export EFFECT_SIZE = 4
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
	.out    .sprintf("INFO: Reserved %d RAM bytes for %d effects.", \
				Anim::N_EFFECTS * EffectOffset::EFFECT_SIZE, Anim::N_EFFECTS)

	.segment "CODE"
	; animation opcode ("op") handlers
	.scope AnimOpHandler
		; NB: op handlers must not clobber X, local_0, or addr_0
		; When handler is called, current_effect_addr points to current effect,
		; current_pc contains the current value of the current effect's PC, i.e.,
		; points to the current opcode. handlers may clobber current_pc.
		; Handler guaranteed to be called with Y=2

		.proc nop ; XXX would be clever to reuse end of yield
			incr_pc_by 1
			rts 
		.endproc

		; stop executing effect this frame, continue at next instruction next frame
		.proc yield
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			ora #EffectStateMask::HAS_YIELDED
			sta (current_effect_addr), Y
			incr_pc_by 1
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

		; 2-byte arg destination address little endian
		.proc jmp_abs 
			debugggg:
			mathmac_inc16 current_pc 
			ldy #0
			lda (current_pc), Y
			; assume PC is at offset zero in effect structure
			sta (current_effect_addr), Y
			iny
			lda (current_pc), Y
			sta (current_effect_addr), Y
			rts
		.endproc

		.proc ppumask_set
			; new ppu mask is in arg
			ldy #1
			lda (current_pc), Y 
			sta PPU::mask
			incr_pc_by 2
			rts
		.endproc

		.proc ppumask_or_with
			; operand is in arg
			ldy #1
			lda (current_pc), Y 
			ora PPU::mask
			sta PPU::mask
			incr_pc_by 2
			rts
		.endproc

		.proc ppumask_and_with
			; operand is in arg
			ldy #1
			lda (current_pc), Y 
			and PPU::mask
			sta PPU::mask
			incr_pc_by 2
			rts
		.endproc

		.proc set_reg
			; value is arg
			ldy #1
			lda (current_pc), Y
			ldy #EffectOffset::REG
			sta (current_effect_addr), Y
			incr_pc_by 2
			rts
		.endproc

		.proc inc_reg
			ldy #EffectOffset::REG
			lda (current_effect_addr), Y
			clc
			adc #1
			sta (current_effect_addr), Y
			incr_pc_by 1
			rts
		.endproc

		.proc jsr
			rts
		.endproc
	.endscope

	.scope Op
		; byte values of opcodes are indices into instruction_handler_table
		.export nop						= $00
		.export yield					= $01
		.export clear_active			= $02
		.export clear_active_and_yield	= $03
		.export ppumask_set				= $04
		.export ppumask_or_with			= $05
		.export ppumask_and_with		= $06
		.export jmp_abs					= $07
		.export jsr						= $08
	.endscope

	.segment "RODATA"
	instruction_handler_table:							; opcode/index
		.addr AnimOpHandler::nop - 1					; $00
		.addr AnimOpHandler::yield - 1					; $01
		.addr AnimOpHandler::clear_active - 1			; $02
		.addr AnimOpHandler::clear_active_and_yield - 1	; $03
		.addr AnimOpHandler::ppumask_set - 1			; $04
		.addr AnimOpHandler::ppumask_or_with - 1		; $05
		.addr AnimOpHandler::ppumask_and_with - 1		; $06
		.addr AnimOpHandler::jmp_abs - 1				; $07
		.addr AnimOpHandler::jsr - 1					; $08

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

	; call with:
	;	X = <script address
	;	Y = >script address
	;	A = state
	; returns with A = effect index, or 255 if no effect was available
	; clobbers addr_0, addr_1, X, Y
	.export create_effect
	.proc create_effect
		script_addr = addr_1
		pha	; stash A. we must be sure to pop it before rts!
		stx script_addr
		sty script_addr + 1
		lda #<Anim::effects_array
		sta current_effect_addr + 0
		lda #>Anim::effects_array
		sta current_effect_addr + 1

		ldx #Anim::N_EFFECTS
		each_effect:
			; check if active
			ldy #EffectOffset::STATE
			lda (current_effect_addr), Y
			and #EffectStateMask::IS_ACTIVE
			beq is_inactive
			dex
			beq no_free_effects
			; increment pointer to current effect by size of effect
			mathmac_add16 #EffectOffset::EFFECT_SIZE, current_effect_addr, current_effect_addr
			jmp each_effect

		is_inactive:
			; found a free effect, pointed to by current_effect_addr
			; set effect's PC to script addr
			lda script_addr
			ldy #EffectOffset::PC
			sta (current_effect_addr), Y
			lda script_addr + 1
			iny 
			sta (current_effect_addr), Y
			; set state
			pla 
			ldy #EffectOffset::STATE
			sta (current_effect_addr), Y
			; return effect index = N_EFFECTS - X
			lda #Anim::N_EFFECTS
			; use first byte of script_addr (since we don't need it anymore) as
			; tmp store for X
			stx script_addr
			sec
			sbc script_addr
			; ignore overflowwwwwwwwww
			; A now equals index of new effect
			rts

		no_free_effects:
			pla
			lda #255 ; error indicator
			rts
	.endproc ; create_effect

.endscope

