.ifndef Anim_included
Anim_included = 1

.scope Anim
	; routines
	.import do_frame
	.import create_effect
	.import activate_effect
	.import deactivate_effect

	; mutable data
	.import effects_array
	
	; constants
	.import N_EFFECTS

	; XXX this is retarded that we have to duplicate the list here and in the
	; module
	.scope EffectOffset
		.importzp PC	; 2-byte "program counter"
		.importzp STATE ; 1-byte state
                        ; 765543210
                        ; |||||||||
                        ; ||+++++++--- oam_entry_index
                        ; |+---------- has_yielded
                        ; +----------- is_active
		.importzp REG	; register
		.importzp EFFECT_SIZE
	.endscope

	; XXX this is retarded that we have to duplicate the list here and in the
	; module
	.scope EffectStateMask
		.importzp OAM_ENTRY_INDEX
		.importzp HAS_YIELDED
		.importzp IS_ACTIVE
	.endscope

	; effect script opcodes!
	; XXX this is retarded that we have to duplicate the list here and in the
	; module
	.scope Op
		; Flow
		.importzp nop
		.importzp yield
		.importzp clear_active
		.importzp clear_active_and_yield
		.importzp jmp_abs
		.importzp set_reg
		.importzp inc_reg
		.importzp dec_reg
		.importzp cmp_reg
		.importzp beq
		.importzp bne

		; PPU
		.importzp ppumask_set
		.importzp ppumask_or_with
		.importzp ppumask_and_with

		; Sprite manipulation
		; oam_set_x <byte>
		; oam_set_y <byte>
		; oam_set_tile_num <byte>
		; oam_set_palette_num <2 bits>
		; oam_set_orientation <2 bits>
		; oam_incr_entry_index
		; oam_decr_entry_index
		; palette_set <2 bits> <4 bytes>

		; Nametable manipulation
	.endscope
.endscope

.macro Anim_Op_jmp_abs dest_label
	.byte Anim::Op::jmp_abs, <dest_label, >dest_label
.endmacro

.macro Anim_Op_yield_n N
	.repeat N
		.byte Anim::Op::yield
	.endrepeat
.endmacro

.macro Anim_create_effect script_addr, active, oam_index 
	ldx #<script_addr
	ldy #>script_addr
	lda #((active << 7) | (%00111111 & oam_index))
	jsr Anim::create_effect
.endmacro

.endif ; .ifndef AI_included
