; NES Gamepad module
; Useful constants and subroutines
; TODO: support multiple gamepads, efficiently (macros?)

.scope Joy

.export PAD0_ADDR = $4016
.export PAD1_ADDR = $4017

.segment "ZEROPAGE"
pad0: .res 1
.exportzp pad0

.segment "CODE"
; Joy::poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
.export poll
poll:
    ; strobe the gamepad to latch current button state
    lda #1
    sta PAD0_ADDR
    lda #0
    sta PAD0_ADDR
    ; read 8 bytes from the interface at $4016
    ldx #8
    :
        pha
        lda PAD0_ADDR
        ; combine low two bits and store in carry bit
        and #%00000011
        cmp #%00000001
        pla
        ; rotate carry into gamepad variable
        ror
        dex
        bne :-
    sta pad0
    rts

.endscope ; Joy
