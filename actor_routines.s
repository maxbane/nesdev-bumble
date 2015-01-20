.include "locals.inc"
.include "constants.inc"
.include "actor.inc"

; Non-inlined actor subroutines

.scope Actor

; call with X = actor::addr, addr_0 = pointer to oam buffer entry
.export draw_1x1_actor_sprite
.proc draw_1x1_actor_sprite     ; 88 cycles with rts (but not with inbound jsr)
    buffer_entry_ptr = addr_0
    facing_offset = local_0
    palette_idx   = local_0 ; re-use

    ldy #0                      ; 2 cycles

    ; high bytes of actor's little endian 16-bit position coords become
    ; screen coords
    ; Y screen coord (12 cycles)
    lda ActorOffset::POSITION_Y+1, X      ; A = MSB(actor::position::yval)
                                ; 4 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles

    ; Tile number (26 cycles)
    ; Get facing offset to base tile number and stash it
    lda ActorOffset::RENDER_FLAGS, X   ; 4 cycles
    and #ActorRenderFlagMask::facing_tile_offset ; 2 cycles
    lsr                         ; 2 cycles
    sta facing_offset           ; 3 cycles
    ; Get base tile number
    lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
                                ; 4 cycles
    ; Add facing offset to base_tile
    adc facing_offset           ; 3 cycles
    ; Store final effective tile number to OAM entry
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles

    ; OAM flags (32 cycles)
    lda ActorOffset::RENDER_FLAGS, X  ; 4 cycles
    and #ActorRenderFlagMask::palette ; 2 cycles
    lsr                         ; 2 cycles
    lsr                         ; 2 cycles
    lsr                         ; 2 cycles
    sta palette_idx             ; 3 cycles
    lda ActorOffset::RENDER_FLAGS, X     ; 4 cycles
    and #ActorRenderFlagMask::facing_oam ; 2 cycles
    ora palette_idx             ; 3 cycles
    sta (buffer_entry_ptr), Y   ; 6 cycles
    iny                         ; 2 cycles

    ; X screen coord (10 cycles)
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
    facing_offset    = local_0
    palette_idx      = local_0 ; safe to reuse
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
        ; Store screen Y coord to OAM buffer
        ldy #0
        lda ActorOffset::POSITION_Y+1, X      ; A = MSB(actor::position::yval)
        .if I = 1 || I = 2
            ; bottom two quadrants: add 8px to screen y
            adc #8
        .endif
        sta (buffer_entry_ptr), Y
        iny

        ; This quadrant's tile number is determined by:
        ;  actor::base_tile + quadrant_offset + facing_offset
        .scope 

        ; Get facing offset and stash it
        lda ActorOffset::RENDER_FLAGS, X   ; 4 cycles
        and #ActorRenderFlagMask::facing_tile_offset ; 2 cycles
        ; conveniently the offset is already multiplied by 2 (actor size) in
        ; the RENDER_FLAGS
        sta facing_offset           ; 3 cycles
        ; Determine tile offset for this quadrant, which depends on possible
        ; horizontal/vertical mirroring, and add it to actor's base_tile
        lda ActorOffset::RENDER_FLAGS, X   ; 4 cycles
        and #ActorRenderFlagMask::facing_oam ; 2 cycles
        ; determine quadrant offset
        cmp #%00000000
        bne :+
            lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
            clc
            .if I = 0
                adc #1
            .elseif I = 1
                adc #17
            .elseif I = 2
                adc #16
            .elseif I = 3
                nop
            .endif
            jmp set_tile
        :
        cmp #%01000000
        bne :+
            ; horiz. flip
            lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
            clc
            .if I = 0
                nop
            .elseif I = 1
                adc #16
            .elseif I = 2
                adc #17
            .elseif I = 3
                adc #1
            .endif
            jmp set_tile
        :
        cmp #%10000000
        bne :+
            ; vert. flip
            lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
            clc
            .if I = 0
                adc #17
            .elseif I = 1
                adc #1
            .elseif I = 2
                nop
            .elseif I = 3
                adc #16
            .endif
            jmp set_tile
        :
        cmp #%11000000
        bne :+
            ; horiz. and vert. flip
            lda ActorOffset::BASE_TILE, X      ; A = actor::base_tile
            clc
            .if I = 0
                adc #16
            .elseif I = 1
                nop
            .elseif I = 2
                adc #1
            .elseif I = 3
                adc #17
            .endif
            ;jmp set_tile
        :
        set_tile:
        .endscope
        ; Add facing offset to base_tile
        adc facing_offset           ; 3 cycles
        ; Store final effective tile number to OAM buffer
        sta (buffer_entry_ptr), Y
        iny

        ; OAM flags (32 cycles)
        lda ActorOffset::RENDER_FLAGS, X  ; 4 cycles
        and #ActorRenderFlagMask::palette ; 2 cycles
        lsr                         ; 2 cycles
        lsr                         ; 2 cycles
        lsr                         ; 2 cycles
        sta palette_idx             ; 3 cycles
        lda ActorOffset::RENDER_FLAGS, X     ; 4 cycles
        and #ActorRenderFlagMask::facing_oam ; 2 cycles
        ora palette_idx             ; 3 cycles
        sta (buffer_entry_ptr), Y   ; 6 cycles
        iny

        ; Store screen x coord to OAM buffer
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
