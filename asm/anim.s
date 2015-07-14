.include "locals.inc"
.include "math_macros.inc"

.segment "ZEROPAGE"
; currently updating effect. op handlers must not clobber
current_effect_addr = addr_0
current_pc			= addr_1

.segment "CODE"
.scope EffectOffset
	PC = 0				; 2-byte "program counter"

	STATE = 2			; 1-byte state
						; 765543210
						; |||||||||
						; ||+++++++--- oam_entry_index
						; |+---------- has_yielded
						; +----------- is_active
						
	FRAME_MODULUS = 3	; 1-byte frame divider
	;;;;;;;;
	EFFECT_SIZE = 4
.endscope

.enum EffectStateMask
	OAM_ENTRY_INDEX = %00111111
	HAS_YIELDED		= %01000000
	IS_ACTIVE		= %10000000
.endenum

.macro incr_pc_by_one num_bytes
	; assumes PC is at offset 0 within effect structure
	mathmac_inc16 current_effect_addr
.endmacro

.macro incr_pc_by num_bytes
	; assumes PC is at offset 0 within effect structure
	mathmac_add16 num_bytes, current_effect_addr, current_effect_addr
.endmacro

; animation opcode ("op") handlers
.scope AnimOp
	; NB: op handlers must not clobber X, local_0, or addr_0
	; When handler is called, current_effect_addr points to current effect,
	; current_pc contains the current value of the current effect's PC, i.e.,
	; points to the current opcode
	.segment "CODE"
	.proc nop ; XXX would be clever to reuse end of yield
		incr_pc_by_one
		rts 
	.endproc

	.proc yield
		ldy #EffectOffset::STATE
		lda (current_effect_addr), Y
		ora #EffectStateMask::HAS_YIELDED
		sta (current_effect_addr), Y
		incr_pc_by_one
		rts
	.endproc

	.proc clear_active
		ldy #EffectOffset::STATE
		lda (current_effect_addr), Y
		; XXX stupid ca65: bitwise not fails: #~(EffectStateMask::IS_ACTIVE)
		and #(EffectStateMask::IS_ACTIVE ^ $ff)
		sta (current_effect_addr), Y
		incr_pc_by_one
		rts
	.endproc
.endscope

; Animation Engine
.scope Anim
	N_EFFECTS = 16 ; must be at least 1 if you plan to call do_frame
	MAX_INSTRUCTIONS_PER_EFFECT_PER_FRAME = 8

	.segment "RAM"
	; reserved array of all effect objects
	effects_array: .res Anim::N_EFFECTS * EffectOffset::EFFECT_SIZE

	.segment "RODATA"
	instruction_handler_table:
		; opcode = index
		.word AnimOp::nop - 1			; $00
		.word AnimOp::yield - 1			; $01
		.word AnimOp::clear_active - 1	; $02

	.segment "CODE"
	; routing routine for op handlers
	.proc jump_instruction_handler
		; RTS trick
		asl
		tay
		lda instruction_handler_table + 1, Y
		pha
		lda instruction_handler_table, Y
		pha
		rts
	.endproc

	; main function
	; clobbers addr_0, addr_1, A, X, Y
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
		; than 256 effects long
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
			; do while !has_yielded && X > 0
			;  fetch pc and rts
			each_instruction:
				ldy #EffectOffset::PC
				lda (current_effect_addr), Y ; A = low byte of PC
				sta current_pc + 0
				iny
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
	.endproc

.endscope

