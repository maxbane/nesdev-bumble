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


.segment "ZEROPAGE"
; Reserve space for N_ACTORS actors, with a nice scope and labels for static
; access to each.
;.repeat Constants::N_ACTORS, I
;    ;.out .sprintf ("actor_%02d", I)
;    .scope .ident (.sprintf ("actor_%02d", I))
;        actor_idx = I
;        addr:
;        .scope position
;            xval:   .res 2
;            yval:   .res 2
;        .endscope
;        .scope velocity
;            xval:   .res 2
;            yval:   .res 2
;        .endscope
;        base_tile:  .res 1
;        flags:      .res 1
;        ; flags mask:
;        ; %7654 3210
;        ;       ||||
;        ;       |||+- is_2x2: Is actor 2x2 sprites (1) or 1 sprite (0)
;        ;       +++-- actor's facing. 8 possible facings.
;        ;             Bit 3 indicates diagonal (rotate CC 45 degrees).
;        ;             000   = RIGHT
;        ;             001   = LEFT
;        ;             010   = UP
;        ;             011   = DOWN
;        ;             100   = UP-RIGHT
;        ;             101   = DOWN-LEFT
;        ;             110   = UP-LEFT
;        ;             111   = DOWN-RIGHT
;    .endscope
;.endrepeat
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
    :
    rts
.endproc

.proc do_flap
    dx = the_player::velocity::xval
    dy = the_player::velocity::yval
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

