; Assembler options

.linecont +

; imports

.include "locals.inc"
.include "ines.inc"
.include "ppu.inc"
.include "joy.inc"

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
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine


; Numeric constants
.scope Constants
    N_ACTORS                = 1
    PLAYER_TILE_BASE        = 1
    ACTOR_BASE_OAM_SPRITENO = 1
    ; gravitational acceleration 
    GRAVITY_DDY             = $0004
    ; Amount by which to accelerate player if flapping straight up
    BASE_FLAP_ACCEL_VERT    = $0080 ; -ddy
    ; Amount to accelerate if flapping up and to the left/right
    ; Idea is to keep vector magnitude the same
    BASE_FLAP_ACCEL_DIAG    = $005A ; sqrt((ddy^2)/2)

    FLOOR_Y                 = 231
    CEILING_Y               = 12
.endscope

.segment "ZEROPAGE"
; Game variable "globals"
.repeat Constants::N_ACTORS, I
    ;.out .sprintf ("actor_%02d", I)
    .scope .ident (.sprintf ("actor_%02d", I))
        actor_idx = I
        addr:
        .scope velocity
            xval:   .res 2
            yval:   .res 2
        .endscope
        .scope position
            xval:   .res 2
            yval:   .res 2
        .endscope
    .endscope
.endrepeat
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

    ; address a tile toward the middle of the screen
    ;ldy #15
    ;ldx #9
    ;jsr PPU::address_tile
    ; position player at x=64, y=64
    ;mathmac_set16 #$4000, player_x_low, player_y_low
    mathmac_set16 #$4000, the_player::position::xval, the_player::position::yval
    ; zero velocity
    mathmac_set16 #$0000, the_player::velocity::xval, the_player::velocity::yval

    ; enable nmi-handler
    ;jsr PPU::update

    ; loop forever
    @infiniloop:
        jsr handle_input
        jsr do_gravity
        jsr move_actors
        jsr draw_actors
        jsr PPU::update
        jmp @infiniloop
    ; no rts
.endproc

.proc handle_input
    ; buttons newly depressed this frame
    new_buttons = local_0
    jsr Joy::lda_new_buttons
    sta new_buttons

    ; A button: jump
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
    beq :+
        ; Diagonal acceleration up and to the left
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dx, dx
        rts
    :
    lda Joy::pad0
    and #Joy::BUTTON_RIGHT
    beq :+
        ; Diagonal acceleration up and to the right
        mathmac_add16 #-Constants::BASE_FLAP_ACCEL_DIAG, dy, dy
        mathmac_add16 #Constants::BASE_FLAP_ACCEL_DIAG,  dx, dx
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

.proc do_gravity
    .repeat Constants::N_ACTORS, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            dy = actor_i::velocity::yval
            mathmac_add16 #Constants::GRAVITY_DDY, dy, dy
        .endscope
        .undefine actor_i
    .endrepeat
    rts
.endproc

.proc move_actors
    .repeat Constants::N_ACTORS, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            px = actor_i::position::xval
            dx = actor_i::velocity::xval
            py = actor_i::position::yval
            dy = actor_i::velocity::yval
            mathmac_add16 px, dx, px
            mathmac_add16 py, dy, py

            ; floor and ceiling bounce wrt most significant byte of py
            ; Note we are inlining this for each actor!
            lda py+1
            cmp #Constants::CEILING_Y
            bcc hit_ceiling
            cmp #Constants::FLOOR_Y
            bcc collision_done
            hit_floor:
                ; clip to floor
                mathmac_set16 #((Constants::FLOOR_Y - 1) * $100), py
                jmp hit_common
            hit_ceiling:
                ; clip to ceiling
                mathmac_set16 #((Constants::CEILING_Y + 1) * $100), py
            hit_common:
                ; negate velocity y-component
                mathmac_neg16 dy, dy
                ; divide velocity y-component by two
                lda dy + 1
                asl
                ror dy + 1
                ror dy + 0
            collision_done:
        .endscope
        .undefine actor_i
    .endrepeat
    rts
.endproc

.proc draw_actors
    .repeat Constants::N_ACTORS, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            oam_entry_idx = PPU::oam_buffer + \
                (Constants::ACTOR_BASE_OAM_SPRITENO + I) * 4
            ; high bytes of actor's little endian 16-bit position coords become
            ; screen coords
            lda actor_i::position::yval + 1
            sta oam_entry_idx + 0
            ; TODO: per-actor tiles and oam attributes
            lda #Constants::PLAYER_TILE_BASE
            sta oam_entry_idx + 1
            lda #0
            sta oam_entry_idx + 2
            lda actor_i::position::xval + 1
            sta oam_entry_idx + 3
        .endscope
        .undefine actor_i
    .endrepeat
    rts
.endproc

