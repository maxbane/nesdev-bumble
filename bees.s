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
.byte $11,$15,$26,$37 ; bg0 purple/pink
.byte $11,$09,$19,$29 ; bg1 green
.byte $11,$01,$11,$21 ; bg2 blue
.byte $11,$00,$10,$30 ; bg3 greyscale

.byte $11,$08,$37,$20 ; sp0 BEES
.byte $11,$14,$24,$34 ; sp1 purple
.byte $11,$1B,$2B,$3B ; sp2 teal
.byte $11,$12,$22,$32 ; sp3 marine


; Numeric constants
.scope Constants
    N_ACTORS                = 2
    ;PLAYER_TILE_BASE        = 1
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
    SCREEN_WIDTH            = 256
.endscope

.struct Vector
    xval    .word
    yval    .word
.endstruct

; How big an actor is
.struct Actor
    position    .tag Vector
    velocity    .tag Vector
    base_tile   .byte
    flags       .byte
.endstruct

.enum ActorFlagMask
    is_2x2  = %00000001
.endenum

.segment "ZEROPAGE"
; Reserve space for N_ACTORS actors, with a nice scope and labels for static
; access to each. XXX Had better agree with the Actor struct above!!!
.repeat Constants::N_ACTORS, I
    ;.out .sprintf ("actor_%02d", I)
    .scope .ident (.sprintf ("actor_%02d", I))
        actor_idx = I
        addr:
        .scope position
            xval:   .res 2
            yval:   .res 2
        .endscope
        .scope velocity
            xval:   .res 2
            yval:   .res 2
        .endscope
        base_tile:  .res 1
        flags:      .res 1
        ; flags mask:
        ; %7654 3210
        ;       ||||
        ;       |||+- is_2x2: Is actor 2x2 sprites (1) or 1 sprite (0)
        ;       +++-- actor's facing. 8 possible facings.
        ;             Bit 3 indicates diagonal (rotate CC 45 degrees).
        ;             000   = RIGHT
        ;             001   = LEFT
        ;             010   = UP
        ;             011   = DOWN
        ;             100   = UP-RIGHT
        ;             101   = DOWN-LEFT
        ;             110   = UP-LEFT
        ;             111   = DOWN-RIGHT
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
    jsr init_actors

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

; TODO : this should depend on level, etc.
.proc init_actors
    ; position player at x=64, y=64
    mathmac_set16 #$4000, the_player::position::xval, the_player::position::yval, actor_01::position::yval
    mathmac_set16 #$8000, actor_01::position::xval
    ; zero velocity
    mathmac_set16 #$0000, the_player::velocity::xval, the_player::velocity::yval
    mathmac_set16 #$0000, actor_01::velocity::xval, actor_01::velocity::yval
    lda #2
    sta the_player::base_tile
    lda #1
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
            ; 2x2 actors effectively have their floor shifted up by 8 pixels
            lda actor_i::flags
            and #ActorFlagMask::is_2x2
            beq :+
                ; 2x2 actor
                lda py+1
                cmp #(Constants::FLOOR_Y - 8)
                bcc collision_done
                jmp hit_floor
            :
            ; 1x1 actor
            lda py+1
            cmp #Constants::FLOOR_Y
            bcc collision_done
            hit_floor:
                ; clip to floor
                lda actor_i::flags
                and #ActorFlagMask::is_2x2
                beq :+
                    mathmac_set16 #((Constants::FLOOR_Y - 9) * $100), py
                    jmp hit_common
                :
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
    ; address of current 4-byte oam buffer entry we are updating
    buffer_entry_addr = addr_0
    lda #<PPU::oam_buffer
    sta buffer_entry_addr
    lda #>PPU::oam_buffer
    sta buffer_entry_addr + 1
    ; inlined loop over all actors
    .repeat Constants::N_ACTORS, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            .if I > 0
                ; advance the buffer entry pointer
                lda buffer_entry_addr ; 3 cycles
                clc                   ; 2 cycles
                adc #4                ; 2 cycles
                sta buffer_entry_addr ; 3 cycles
            .endif
            ; draw actor's sprites with appropriate routine, depending whether
            ; it's a 1x1 or 2x2 actor. Both routines take the actor's base addr
            ; (in zeropage) from X, and the current OAM buffer entry pointer
            ; from addr_0
            ldx #actor_i::addr
            lda actor_i::flags
            and #ActorFlagMask::is_2x2
            beq draw_1x1
            draw_2x2:
                jsr draw_2x2_actor_sprites
                jmp draw_done
            draw_1x1:
                ; Could inline this
                jsr draw_1x1_actor_sprite
            draw_done:
        .endscope
        .undefine actor_i
    .endrepeat
    rts
.endproc

.proc draw_1x1_actor_sprite
    buffer_entry_ptr = addr_0
    ldy #0
    ; high bytes of actor's little endian 16-bit position coords become
    ; screen coords
    ; Y screen coord
    lda 3, X      ; A = MSB(actor::position::yval)
    sta (buffer_entry_ptr), Y
    iny
    ; Tile number
    ;lda actor_i::base_tile
    lda 8, X      ; A = actor::base_tile
    sta (buffer_entry_ptr), Y
    iny
    ; OAM flags
    ; TODO compute OAM flags from actor state
    lda #0
    sta (buffer_entry_ptr), Y
    iny
    ; X screen coord
    ;lda actor_i::position::xval + 1
    lda 1, X      ; A = MSB(actor::position::xval)
    sta (buffer_entry_ptr), Y
    rts
.endproc

; Call with X = zp addr of actor, addr_0 = address of actor's first OAM buffer
; entry
.proc draw_2x2_actor_sprites
    ; Quadrants of a 2x2 actor. MSBs of actor's position x,y coords = pixel
    ; coords of top left of quadrant 3.
    ; 3 | 0  Each quadrant corresponds an entry in the oam buffer.
    ; --+--  Quadrant index is entry offset in oam buffer.
    ; 2 | 1
    buffer_entry_ptr = addr_0
    ; if position::xval is far to the right onscreen, skip quadrants 0 and 1
    lda 1, X      ; A = MSB(actor::position::xval)
    cmp #(Constants::SCREEN_WIDTH - 8)
    bcc :+
        ; Actor's x-coord is < 8 pixels from right edge. Hide quadrants 0 and 1
        ; by setting their screen y to $FF, then jump to drawing quadrant 2
        clc
        ldy #0
        ; Quadrant 0
        lda #$ff
        sta (buffer_entry_ptr), Y

        ; Quadrant 1
        lda buffer_entry_ptr
        adc #4
        sta buffer_entry_ptr
        lda #$ff
        sta (buffer_entry_ptr), Y

        jmp quadrant_2
    :
    .repeat 4, I
        .ident (.sprintf ("quadrant_%d", I)):
        .if I = 2
            clc
        .endif
        .if I > 0
            ; advance to next buffer entry
            lda buffer_entry_ptr 
            adc #4                
            sta buffer_entry_ptr 
        .endif
        ldy #0
        lda 3, X      ; A = MSB(actor::position::yval)
        .if I = 1 || I = 2
            ; bottom two quadrants: add 8px to screen y
            adc #8
        .endif
        sta (buffer_entry_ptr), Y
        iny
        lda 8, X      ; A = actor::base_tile
        .if I = 0
            adc #1
        .elseif I = 1
            adc #17
        .elseif I = 2
            adc #16
        .endif
        sta (buffer_entry_ptr), Y
        iny
        ; TODO compute OAM flags from actor state
        lda #0
        sta (buffer_entry_ptr), Y
        iny
        lda 1, X      ; A = MSB(actor::position::xval)
        .if I < 2
            ; right two quadrants: add 8px to screen x
            adc #8
        .endif
        sta (buffer_entry_ptr), Y
    .endrepeat
    rts
.endproc
