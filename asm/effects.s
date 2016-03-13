
.include "math_macros.inc"
.include "locals.inc"
.include "constants.inc"
.include "coroutine.inc"
.include "effects.inc"
.include "actor.inc"


.segment "RAM"
; Array of coroutine structs for effects
Effects::effects: .res .sizeof(Coroutine::State)*Constants::N_EFFECTS + 2
effects_guard = Effects::effects + .sizeof(Coroutine::State)*Constants::N_EFFECTS

; bit vector of N_EFFECTS bits, bit_i indicating the availability of OAM sprite_i
; Enough space for each effect to reserve one sprite
sprites_avail: .res Constants::N_EFFECTS/8

.segment "CODE"

; Find a free OAM sprite index (according to sprites_avail) and put it into A.
; Y,X clobbered.
.proc lda_free_sprite_index
    ; each byte of sprites_avail represents 8 sprites. conceptually, find index
    ; of first bit in sequence of bytes that is zero. We search "backward"
    ; starting from the end of the sprites_avail array, just because it's
    ; easier to have Y decrement
    ldy #Constants::N_EFFECTS/8
    ldx #Constants::N_EFFECTS ; current sprite being checked
    next_byte:
        lda sprites_avail, Y
        next_bit:
            dex
            

        dey
        bne next_byte
    rts
.endproc

Effects::init = init
.proc init
    ; set guard on effects array (end-of-list marker)
	lda #$ff
    sta effects_guard
	sta effects_guard + 1

    ; point tmp to head of effects array
    tmp = addr_0
    lda #<Effects::effects
    sta tmp
    lda #>Effects::effects
    sta tmp + 1

    ; fill PROGs with 0
    ldx #Constants::N_EFFECTS
    :
        lda #$00
        ldy #Coroutine::State::PROG
        sta (tmp), Y
        iny
        sta (tmp), Y
        mathmac_add16 #.sizeof(Coroutine::State), tmp, tmp
        dex
        bne :-
    rts
.endproc

; Create Coroutine
Effects::strobe_actor_palette = strobe_actor_palette
.proc strobe_actor_palette
    DATA0 = Coroutine::State::DATA0
    XREG = Coroutine::State::XREG
    DATA1 = Coroutine::State::DATA1
    pha ; # cycles
    txa
    pha ; Actor idx
    tya
    pha ; Frames per palette
    Coroutine_new strobe_actor_palette_main, Effects::effects, failed
    ; coroutine main starts with X = actor, DATA0 = frames per palette, DATA1 =
    ; # cycles
    pla ; frames
    Coroutine_sta_state DATA0
    pla ; actor idx
    tax
    Coroutine_sta_state XREG
    pla ; cycles
    Coroutine_sta_state DATA1
    lda #0 ; success
    rts

    failed:
        pla ; frames
        pla ; actor
        tax
        pla ; cycles
        lda #1 ; failure
        rts

    .proc strobe_actor_palette_main
        ; more cycles to do?
        Coroutine_lda_state DATA1
        bne :+
            Coroutine_halt
        :
        sec
        sbc #1
        Coroutine_sta_state DATA1

        .repeat 4, palette_idx
            Actor_set_palette ((palette_idx+1) .mod 4)
            Coroutine_lda_state DATA0 ; frames per palette
            tay
            :
                Coroutine_yield
                dey
                bne :-
        .endrepeat
        jmp strobe_actor_palette_main
        Coroutine_halt ; should never reach this
    .endproc
.endproc
