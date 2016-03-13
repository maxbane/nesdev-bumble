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
.include "ai.inc"
.include "coroutine.inc"
.include "effects.inc"
.include "random.inc"

.include "math_macros.inc"
.include "sprites_manifest.inc"

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
.incbin "chr/background.chr"
.incbin "chr/sprites-bee.chr"

;
; interrupt vectors 
;

.segment "VECTORS"
.word PPU::nmi_buffered
.word reset
.word irq

.segment "RODATA"
.proc main_palettes
    BG = $21

    .byte BG,$09,$19,$29 ; bg0 grass tones
    .byte BG,$09,$19,$29 ; bg1 
    .byte BG,$01,$11,$21 ; bg2 
    .byte BG,$00,$10,$30 ; bg3 

    .byte BG,$1d,$38,$20 ; sp0 bee
    .byte BG,$00,$10,$21 ; sp1 jet
    .byte BG,$1B,$2B,$3B ; sp2 
    .byte BG,$12,$22,$32 ; sp3 
.endproc


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

    Random_seed_crc16 #$ff00

    jsr PPU::clear_background
    jsr draw_grass
    jsr init_actors

    lda #%00011110
    sta PPU::mask
    lda #%10001000
    sta PPU::ctrl

    jsr Effects::init
    jmp loop_gameplay
    ; no rts
.endproc

;.segment "RODATA"
;my_effect_script1:
;    ; do nothing then quit
;    .byte Anim::Op::nop
;    .byte Anim::Op::clear_active_and_yield
;
;my_effect_script2:
;    :
;    .byte Anim::Op::nop
;    .byte Anim::Op::yield
;    .byte Anim::Op::jmp_abs, <:-, >:- 
;
;my_effect_script3:
;    ; clear emphasis bits
;    .byte Anim::Op::ppumask_and_with, %00011111
;    Anim_Op_yield_n 8   ; wait 8 frames
;    ; emphasize red
;    .byte Anim::Op::ppumask_or_with, %00100000
;    Anim_Op_yield_n 8
;    ; emphasize green
;    .byte Anim::Op::ppumask_and_with, %00011111
;    .byte Anim::Op::ppumask_or_with, %01000000
;    Anim_Op_yield_n 8
;    ; emphasize blue
;    .byte Anim::Op::ppumask_and_with, %00011111
;    .byte Anim::Op::ppumask_or_with, %10000000
;    Anim_Op_yield_n 8
;    Anim_Op_jmp_abs my_effect_script3

.segment "CODE"

;.proc init_effects
;    ; no fancy "find free effect" functionality here, just take effect 0
;    lda #<my_effect_script1
;    sta Anim::effects_array + Anim::EffectOffset::PC + 0
;    lda #>my_effect_script1
;    sta Anim::effects_array + Anim::EffectOffset::PC + 1
;    lda #%10000001
;    sta Anim::effects_array + Anim::EffectOffset::STATE
;    ; ... and effect 1
;    lda #<my_effect_script2
;    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 0
;    lda #>my_effect_script2
;    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 1
;    lda #%10000010
;    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::STATE
;
;    ; now use the fanciness
;    Anim_create_effect my_effect_script3, 1, 3 ; active 1, oam_index 3
;    rts
;.endproc

; Call with A = Joy::new_buttons_?
.macro handle_input_gameplay
    jsr Joy::store_new_buttons
    and #Joy::BUTTON_START
    beq :+
        jsr loop_paused
    :
.endmacro

; main loop for core gameplay
.proc loop_gameplay
    Physics_do_gravity_inline Constants::N_ACTORS, Constants::GRAVITY_DDY
    Physics_move_actors_with_bounce_inline Constants::N_ACTORS, \
                                           Constants::FLOOR_Y, \
                                           Constants::CEILING_Y
    handle_input_gameplay
    AI_do_ai Constants::N_ACTORS
    Actor_draw_actors Constants::N_ACTORS, \
                      Constants::N_EFFECTS, \
                      {jsr Actor::draw_1x1_actor_sprite}, \
                      {jsr Actor::draw_2x2_actor_sprites}

    ; Update effects
    Coroutine_select Effects::effects
    jsr Coroutine::step_all

    ; Wait for NMI
    jsr PPU::update

    ; Loop
    jmp loop_gameplay
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

    lda #Sprites::tank
    sta the_player::base_tile
    lda #Sprites::jet
    sta actor_01::base_tile

    lda #(ActorRenderFlagMask::is_2x2 | ActorRenderFlagMask::is_active)
    sta the_player::render_flags
    lda #(ActorRenderFlagMask::is_active)
    sta actor_01::render_flags

    ldx #actor_01::addr
    Actor_set_palette 1

    lda #AI::Routine::PLAYER0_CONTROL
    sta the_player::ai_routine
    ;lda #AI::Routine::NO_BRAIN
    sta actor_01::ai_routine
    rts
.endproc

.proc loop_paused
    ; whoo do nothing
    jsr PPU::update
    jsr Joy::store_new_buttons
    and #Joy::BUTTON_START
    beq :+
        rts
    :
    jmp loop_paused
.endproc

.proc draw_grass
    ; indices into background pattern tables
    GRASS_TOPPER_OFFSET = 1
    N_GRASS_TOPPERS = 4
    GRASS_FILLER_OFFSET = 17
    N_GRASS_FILLERS = 1
    GRASS_ACCENT_OFFSET = 18
    N_GRASS_ACCENTS = 3
    ACCENT_PROB = 25

    ; nametable tile coords
    HORIZON_Y = 15

    ; topper
    ldx #0
    ldy #HORIZON_Y
    jsr PPU::address_tile
    ldy #32
    :
        ;lda #1
        jsr Random::random_crc16
        ; and #%111 ; faster if we know the smallest power of 2 greater than N
        mathmac_mod8 #N_GRASS_TOPPERS
        clc
        adc #GRASS_TOPPER_OFFSET
        sta PPU::REG_DATA
        dey
        bne :-

    ; filler
    lda #(30 - HORIZON_Y - 1) ; rows to fill
    cur_row = local_0
    sta cur_row
    :
        ldy #32 ; cols to fill
        :
            jsr Random::random_crc16
            cmp #ACCENT_PROB
            bcs filler
            ; accent
            mathmac_mod8 #N_GRASS_ACCENTS
            clc
            adc #GRASS_ACCENT_OFFSET
            sta PPU::REG_DATA
            jmp nextcol

            filler:
            mathmac_mod8 #N_GRASS_FILLERS
            clc
            adc #GRASS_FILLER_OFFSET
            sta PPU::REG_DATA

            nextcol:
            dey
            bne :-
        dec cur_row
        bne :--
    

    ; TODO attribute table
    rts
.endproc
