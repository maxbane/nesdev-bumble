.ifndef AI_included
AI_included = 1

.include "actor.inc"

.scope AI
    ; routines

    ; Given A = AI routine index from AI::Routine, and X = actor::addr, call
    ; the given AI routine to update the given actor. Potentially clobbers
    ; everything.
    .import jump_ai_subroutine

    ; constants
    ; AI routine indices
    .scope Routine
		; zp so ca65 knows they have byte range
        .importzp NO_BRAIN
        .importzp PLAYER0_CONTROL
    .endscope
.endscope ; AI

.macro AI_do_ai N
    ; inlined loop over all actors
    .repeat N, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            Actor_is_active actor_i::render_flags
            beq do_ai_done
            lda actor_i::ai_routine
            ldx #actor_i::addr
            jsr AI::jump_ai_subroutine
            do_ai_done:
        .endscope
        .undefine actor_i
    .endrepeat
.endmacro

.endif ; .ifndef AI_included
