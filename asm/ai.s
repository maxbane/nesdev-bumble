.include "locals.inc"
.include "joy.inc"
.include "actor.inc"
.include "constants.inc"
.include "math_macros.inc"
.include "effects.inc"

.scope AI

.segment "RODATA"
; 11 byte lookup table for for player-controlled AI: associates direction
; button combinations with ActorFacing constants
button_dir_map:
    ; BUTTON_UP     = %0001
    ; BUTTON_DOWN   = %0010
    ; BUTTON_LEFT   = %0100
    ; BUTTON_RIGHT  = %1000
    ; -------------------------------
    ; Offset in this map = OR of above bits corresponding to depressed
    ; direction buttons. Value of byte at offset = corresponding ActorFacing
    ; constant
    ; -------------------------------
    ; Offset = 0, no direction button
    .byte ActorFacing::NO_CHANGE ; special facing: no change
    ; Offset = %0001, UP
    .byte ActorFacing::UP
    ; Offset = %0010, DOWN
    .byte ActorFacing::DOWN
    ; Offset = %0011, UP+DOWN (impossible on gamepad)
    .byte ActorFacing::NO_CHANGE
    ; Offset = %0100, LEFT
    .byte ActorFacing::LEFT
    ; Offset = %0101, UP+LEFT
    .byte ActorFacing::UP_LEFT
    ; Offset = %0110, DOWN+LEFT
    .byte ActorFacing::DOWN_LEFT
    ; Offset = %0111, DOWN+LEFT+UP (impossible)
    .byte ActorFacing::NO_CHANGE
    ; Offset = %1000, RIGHT
    .byte ActorFacing::RIGHT
    ; Offset = %1001, RIGHT+UP
    .byte ActorFacing::UP_RIGHT
    ; Offset = %1010, RIGHT+DOWN
    .byte ActorFacing::DOWN_RIGHT
    ; Remaining offsets correspond to impossible button combinations, so we
    ; don't bother recording facings for them in the table, instead relying on
    ; a check by the user of the table that he's not asking for an offset >
    ; %1010 = 10
    ;; Offset = %1011, RIGHT+DOWN+UP (impossible)
    ;.byte ActorFacing::NO_CHANGE
    ;; Offset = %1100, RIGHT+LEFT (impossible)
    ;.byte ActorFacing::NO_CHANGE
    ;; Offset = %1101, RIGHT+LEFT+UP (impossible)
    ;.byte ActorFacing::NO_CHANGE
    ;; Offset = %1110, RIGHT+LEFT+DOWN (impossible)
    ;.byte ActorFacing::NO_CHANGE
    ;; Offset = %1111, RIGHT+LEFT+DOWN+UP (impossible)
    ;.byte ActorFacing::NO_CHANGE

.segment "CODE"

; AI subroutine indices and implementations. Indices must accord with actual
; order in subroutine_table. Convention is that AI subroutines are called with
; X=actor::addr, the address of the actor the AI subroutine should operate on.
.scope Routine
    .export NO_BRAIN = 0
    .proc no_brain
        ; do nothing
        rts
    .endproc

    .export PLAYER0_CONTROL = 1
    .proc player0_control
        ; new buttons (edge)
        lda Joy::new_buttons_0
        ; A button: flap
        and #Joy::BUTTON_A
        beq :+
            jsr do_flap_player0
            ; cycle palette
            lda #3
            ldy #4
            jsr Effects::strobe_actor_palette
            rts
        :

        ; current buttons (level)
        lda Joy::pad_0
        ; Look up the ActorFacing constant that corresponds to the bitmask of
        ; currently depressed directional buttons, and set that as our actor's
        ; facing
        and #%11110000
        .repeat 4
            lsr ; huh is there a faster way to shift 4 bits right?
        .endrepeat
        tay
        lda button_dir_map, Y
        ; X is still actor::addr
        Actor_set_facing_from_A

        ;; current buttons (level)
        ;lda Joy::pad_0
        ;; A button: show flame
        ;and #Joy::BUTTON_A
        ;beq :+
        ;    jsr do_flap_player0
        ;    rts
        ;:

        rts
    .endproc
.endscope

; Main routing routine. 
.export jump_ai_subroutine
.proc jump_ai_subroutine
    ; RTS trick
    asl
    tay
    lda subroutine_table + 1, Y
    pha
    lda subroutine_table, Y
    pha
    rts
.endproc

.segment "RODATA"
subroutine_table:
    ; index 0
    .word Routine::no_brain - 1
    ; index 1
    .word Routine::player0_control - 1

.segment "CODE"
; utility routines used by the AI routines

; Given X=actor::addr, thrusts actor in direction determined by player0's
; gamepad
.proc do_flap_player0
    dx = ActorOffset::VELOCITY_X
    dy = ActorOffset::VELOCITY_Y
    lda Joy::pad_0
    and #Joy::BUTTON_LEFT
    beq :++
        ; Diagonal acceleration up/down and to the left
        mathmac_add16_xoff #-Constants::BASE_FLAP_ACCEL_DIAG, dx, dx
        lda Joy::pad_0
        and #Joy::BUTTON_DOWN
        beq :+
            ; down
            mathmac_add16_xoff #Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
            rts
        :
        ; up
        mathmac_add16_xoff #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        rts
    :
    lda Joy::pad_0
    and #Joy::BUTTON_RIGHT
    beq :++
        ; Diagonal acceleration up/down and to the right
        mathmac_add16_xoff #Constants::BASE_FLAP_ACCEL_DIAG, dx, dx
        lda Joy::pad_0
        and #Joy::BUTTON_DOWN
        beq :+
            ; down
            mathmac_add16_xoff #Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
            rts
        :
        ; up
        mathmac_add16_xoff #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        rts
    :
    lda Joy::pad_0
    and #Joy::BUTTON_DOWN
    beq :+
        ; Voitical acceleration down
        mathmac_add16_xoff #Constants::BASE_FLAP_ACCEL_VERT, dy, dy
        rts
    :
    ; Voitical acceleration up
    mathmac_add16_xoff #-Constants::BASE_FLAP_ACCEL_VERT, dy, dy
    rts
.endproc

.endscope
