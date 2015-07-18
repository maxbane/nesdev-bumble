.ifndef Anim_included
Anim_included = 1

.scope Anim
	; routines
	.import do_frame

	; mutable data
	.import effects_array
	
	; constants
	.import N_EFFECTS

	; XXX this is retarded that we have to duplicate the list here and in the
	; module
	.scope EffectOffset
		.importzp PC
		.importzp STATE
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
		.importzp nop
		.importzp yield
		.importzp clear_active
		.importzp clear_active_and_yield
		.importzp jmp_rel
		.importzp ppumask_set
	.endscope
.endscope

.endif ; .ifndef AI_included
