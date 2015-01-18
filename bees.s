; Assembler options

.linecont +

; imports

.include "locals.inc"
.include "ines.inc"
.include "ppu.inc"
.include "joy.inc"
.include "constants.inc"
.include "actor.inc"
.include "physics.inc"
.include "actor_routines.inc"

.include "math_macros.inc"

;
; iNES header
;

.segment "HEADER"

INES_PRG_BANK_COUNT = 2 ; 16k PRG bank count
INES_CHR_BANK_COUNT = 1 ; 8k CHR bank count
INES_MAPPER         = 0 ; 0 = NROM (iNES standard mapper number)
INES_MIRROR         = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM           = 0 ; 1 = battery backed SRAM at $6000-7FFF

; INES_HEADER macro constructs the header bytes given arguments
INES_HEADER INES_PRG_BANK_COUNT, INES_CHR_BANK_COUNT, INES_MAPPER, INES_MIRROR, INES_SRAM

;
; CHR ROM
;

.segment "TILES"
.incbin "chr/sprites.chr"
.incbin "chr/sprites.chr"

;
; interrupt vectors 
;

.segment "VECTORS"
.word PPU::nmi_buffered
.word reset
.word irq

.segment "RODATA"
main_palettes:
.byte $11,$15,$26,$37 ; bg0 purple/pink
.byte $11,$09,$19,$29 ; bg1 green
.byte $11,$01,$11,$21 ; bg2 blue
.byte $11,$00,$10,$30 ; bg3 greyscale

.byte $11,$08,$37,$20 ; sp0 BEES
.byte $11,$14,$24,$34 ; sp1 purple
.byte $11,$1B,$2B,$3B ; sp2 teal
.byte $11,$12,$22,$32 ; sp3 marine

; 11 byte lookup table for fun stuff
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


.segment "ZEROPAGE"
Actor_RESERVE_ACTORS Constants::N_ACTORS
.define the_player actor_00

;
; do-nothing irq
;

.segment "CODE"
irq:
    rti

;
; reset routine
;

.segment "CODE"
.proc reset
    sei       ; mask interrupts
    cld       ; disable decimal mode

    lda #0
    sta $4015 ; disable APU sound
    sta $4010 ; disable DMC IRQ
    lda #$40
    sta $4017 ; disable APU IRQ

    ldx #$FF
    txs       ; initialize stack

    ; clear all RAM to 0 (except $100 stack area)
    lda #0
    ldx #0
    :
        sta $0000, X
        sta $0200, X
        sta $0300, X
        sta $0400, X
        sta $0500, X
        sta $0600, X
        sta $0700, X
        inx
        bne :-

    jsr PPU::reset
    jmp main
    ; no rts
.endproc

;
; main
;

.segment "CODE"
.proc main
    ; setup 
    ldx #0
    :
        lda main_palettes, X
        sta PPU::palette_buffer, X
        inx
        cpx #32
        bcc :-

    jsr PPU::clear_background
    jsr init_actors

    ; loop forever
    infiniloop:
        jsr handle_input
        Physics_do_gravity_inline Constants::N_ACTORS, Constants::GRAVITY_DDY
        Physics_move_actors_with_bounce_inline Constants::N_ACTORS, \
                                               Constants::FLOOR_Y, \
                                               Constants::CEILING_Y
        Actor_draw_actors Constants::N_ACTORS, \
                          {jsr Actor::draw_1x1_actor_sprite}, \
                          {jsr Actor::draw_2x2_actor_sprites}
        jsr PPU::update
        jmp infiniloop
    ; no rts
.endproc

; TODO : this should depend on level, etc.
.proc init_actors
    ; position player at x=64, y=64
    mathmac_set16 #$4000, \
                  the_player::position::xval, \
                  the_player::position::yval, \
                  actor_01::position::yval
    mathmac_set16 #$8000, actor_01::position::xval
    ; zero velocity
    mathmac_set16 #$0000, the_player::velocity::xval, the_player::velocity::yval
    mathmac_set16 #$0000, actor_01::velocity::xval, actor_01::velocity::yval
    lda #$10
    sta the_player::base_tile
    lda #$01
    sta actor_01::base_tile
    lda #(ActorFlagMask::is_2x2)
    sta the_player::flags
    lda #0
    sta actor_01::flags
    rts
.endproc

.proc handle_input
    ; buttons newly depressed this frame
    new_buttons = local_0
    jsr Joy::lda_new_buttons
    sta new_buttons

    ; A button: flap
    and #Joy::BUTTON_A
    beq :+
        jsr do_flap
        rts
    :

    ; Look up the ActorFacing constant that corresponds to the bitmask of
    ; currently depressed directional buttons, and set that as our actor's
    ; facing
    lda Joy::pad0
    and #%11110000
    .repeat 4
        lsr ; huh is there a faster way to shift 4 bits right?
    .endrepeat
    tay
    lda button_dir_map, Y
    ldx #actor_01::addr
    Actor_set_facing_from_A
    rts
.endproc

.proc do_flap
    ;dx = the_player::velocity::xval
    ;dy = the_player::velocity::yval
    dx = actor_01::velocity::xval
    dy = actor_01::velocity::yval
    lda Joy::pad0
    and #Joy::BUTTON_LEFT
    beq :++
        ; Diagonal acceleration up/down and to the left
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dx, dx
        lda Joy::pad0
        and #Joy::BUTTON_DOWN
        beq :+
            ; down
            mathmac_add16 #Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
            rts
        :
        ; up
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        rts
    :
    lda Joy::pad0
    and #Joy::BUTTON_RIGHT
    beq :++
        ; Diagonal acceleration up/down and to the right
        mathmac_add16 #Constants::BASE_FLAP_ACCEL_DIAG, dx, dx
        lda Joy::pad0
        and #Joy::BUTTON_DOWN
        beq :+
            ; down
            mathmac_add16 #Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
            rts
        :
        ; up
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        rts
    :
    lda Joy::pad0
    and #Joy::BUTTON_DOWN
    beq :+
        ; Voitical acceleration down
        mathmac_add16 #Constants::BASE_FLAP_ACCEL_VERT, dy, dy
        rts
    :
    ; Voitical acceleration up
    mathmac_add16 #-Constants::BASE_FLAP_ACCEL_VERT, dy, dy
    rts
.endproc

