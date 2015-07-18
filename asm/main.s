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
.include "anim.inc"

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
.byte $1D,$15,$26,$37 ; bg0 purple/pink
.byte $1D,$09,$19,$29 ; bg1 green
.byte $1D,$01,$11,$21 ; bg2 blue
.byte $1D,$00,$10,$30 ; bg3 greyscale

.byte $1D,$10,$00,$21 ; sp0 player!
.byte $1D,$14,$24,$34 ; sp1 purple
.byte $1D,$1B,$2B,$3B ; sp2 teal
.byte $1D,$12,$22,$32 ; sp3 marine


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
    jsr init_effects

    jmp loop_gameplay
    ; no rts
.endproc

.segment "RODATA"
my_effect_script1:
    ; do nothing then quit
    .byte Anim::Op::nop
    .byte Anim::Op::clear_active_and_yield

my_effect_script2:
    ; loopity
    .byte Anim::Op::nop
    .byte Anim::Op::yield
    .byte Anim::Op::jmp_rel, 254 ; -2 two's complement

my_effect_script3:
    .byte Anim::Op::ppumask_set, %10000000
    .byte Anim::Op::clear_active_and_yield

.segment "CODE"

.proc init_effects
    ; no fancy "find free effect" functionality here, just take effect 0
    lda #<my_effect_script1
    sta Anim::effects_array + Anim::EffectOffset::PC + 0
    lda #>my_effect_script1
    sta Anim::effects_array + Anim::EffectOffset::PC + 1
    lda #%10000001
    sta Anim::effects_array + Anim::EffectOffset::STATE
    ; ... and effect 1
    lda #<my_effect_script2
    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 0
    lda #>my_effect_script2
    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 1
    lda #%10000010
    sta Anim::effects_array + Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::STATE
    ; ... and effect 2
    lda #<my_effect_script3
    sta Anim::effects_array + 2*Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 0
    lda #>my_effect_script3
    sta Anim::effects_array + 2*Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::PC + 1
    lda #%10000011
    sta Anim::effects_array + 2*Anim::EffectOffset::EFFECT_SIZE + Anim::EffectOffset::STATE
    rts
.endproc

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
    ; loop forever
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
    jsr Anim::do_frame
    jsr PPU::update
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

    ;ldx #actor_01::addr
    ;Actor_set_palette 2

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
