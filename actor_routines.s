.include "locals.inc"
.include "constants.inc"
.include "actor.inc"

; Non-inlined actor subroutines

.scope Actor

.export draw_1x1_actor_sprite
.proc draw_1x1_actor_sprite     ; 52 cycles
    buffer_entry_ptr = addr_0
    ldy #0                      ; 2 cycles
    ; high bytes of actor's little endian 16-bit position coords become
    ; screen coords
    ; Y screen coord
    lda ActorOffset::POSITION_Y+1, X      ; A = MSB(actor::position::yval)
                                ; 4 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles
    ; Tile number
    lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
                                ; 4 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles
    ; OAM flags
    ; TODO compute OAM flags from actor state
    lda #0                      ; 2 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles
    ; X screen coord
    ;lda actor_i::position::xval + 1
    lda ActorOffset::POSITION_X+1, X      ; A = MSB(actor::position::xval)
                                ; 4 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    rts                         ; 6 cycles
.endproc

; Call with X = zp addr of actor, addr_0 = address of actor's first OAM buffer
; entry
.export draw_2x2_actor_sprites
.proc draw_2x2_actor_sprites
    ; Quadrants of a 2x2 actor. MSBs of actor's position x,y coords = pixel
    ; coords of top left of quadrant 3.
    ; 3 | 0  Each quadrant corresponds an entry in the oam buffer.
    ; --+--  Quadrant index is entry offset in oam buffer.
    ; 2 | 1
    buffer_entry_ptr = addr_0
    ; if position::xval is far to the right onscreen, skip quadrants 0 and 1
    lda ActorOffset::POSITION_X+1, X      ; A = MSB(actor::position::xval)
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
        lda ActorOffset::POSITION_Y+1, X      ; A = MSB(actor::position::yval)
        .if I = 1 || I = 2
            ; bottom two quadrants: add 8px to screen y
            adc #8
        .endif
        sta (buffer_entry_ptr), Y
        iny
        lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
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
        lda ActorOffset::POSITION_X+1, X      ; A = MSB(actor::position::xval)
        .if I < 2
            ; right two quadrants: add 8px to screen x
            adc #8
        .endif
        sta (buffer_entry_ptr), Y
    .endrepeat
    rts
.endproc

.endscope
