.ifndef Actor_included
Actor_included = 1
.linecont +

; Reserve space for n_actors actors, with a nice scope and labels for static
; access to each.
; Annoying that ca65 doesn't support scoped macros
.macro Actor_RESERVE_ACTORS   N
    .segment "ZEROPAGE"
    .repeat N, I
        ;.out .sprintf ("actor_%02d", I)
        .scope .ident (.sprintf ("actor_%02d", I))
            actor_idx = I
            addr:
            .scope position
                xval:       .res 2
                yval:       .res 2
            .endscope
            .scope velocity
                xval:       .res 2
                yval:       .res 2
            .endscope
            ; index into Sprite pattern table
            base_tile:      .res 1
            ; See ActorRenderFlagMask
            render_flags:   .res 1
            ; Index into AI routine jump table
            ai_routine:     .res 1
        .endscope
    .endrepeat
    .out    .sprintf("INFO: Reserved %d zeropage bytes for %d actors.", \
                ActorOffset::ACTOR_SIZE * N, N)
.endmacro

.macro Actor_IMPORTZP_ACTORS   N
    .repeat N, I
        .scope .ident (.sprintf ("actor_%02d", I))
            .exportzp addr
            .scope position
                .exportzp xval
                .exportzp yval
            .endscope
            .scope velocity
                .exportzp xval
                .exportzp yval
            .endscope
            .exportzp base_tile
            .exportzp render_flags
            .exportzp ai_routine
        .endscope
    .endrepeat
.endmacro

; Offset in bytes from the beginning of an actor's record in memory of each
; state variable. Must agree with the layout that's actually reserved above by
; Actor_RESERVE_ACTORS!
.scope ActorOffset
    ; 16-bit 2d vectors, little endian
    POSITION_X = 0
    POSITION_Y = 2
    VELOCITY_X = 4
    VELOCITY_Y = 6
    ; 8-bit tile number in pattern table
    BASE_TILE  = 8
    ; 8 bits of various render control flags; see ActorRenderFlagMask
    RENDER_FLAGS = 9
    ; 8-bit index into AI routine jump-table
    AI_ROUTINE = 10

    ACTOR_SIZE = 11
.endscope

.enum ActorRenderFlagMask
    ; Actors can be 1x1 sprite or 2x2 sprites. In the latter case, the
    ; base_tile is the pattern-table index of the upper-left sprite, and
    ; different drawing logic, collision logic, etc. must apply to the actor.
    is_2x2              = %00000001
    ; Whether to move and draw this actor each frame
    is_active           = %00100000
    ; Sprite palette index (for now, same palette for all quadrants of 2x2
    ; sprites)
    palette             = %00011000

    ; Facing bits
    facing              = %11000110
    ; Facing sub-bits: OAM horizontal/vertical mirroring flags
    facing_oam          = %11000000
    ; Facing sub-bits: offset to base_tile index in pattern table
    facing_tile_offset  = %00000110
.endenum

.define __actor_make_facing(oam_76_bits, tile_offset) \
    ((oam_76_bits << 6) | (tile_offset << 1) & ActorRenderFlagMask::facing)

.enum ActorFacing
; Actor facing    | OAM.76    | Tile offset (times actor size, 0, 1, 2 or 3)
; ----------------+-----------+------------
; RIGHT           | 00        | 0
  RIGHT         = __actor_make_facing(%00, 0)
; LEFT            | 01        | 0
  LEFT          = __actor_make_facing(%01, 0)
; UP              | 00        | 1
  UP            = __actor_make_facing(%00, 1)
; DOWN            | 10        | 1
  DOWN          = __actor_make_facing(%10, 1)
; UP-RIGHT        | 00        | 2
  UP_RIGHT      = __actor_make_facing(%00, 2)
; UP-LEFT         | 01        | 2
  UP_LEFT       = __actor_make_facing(%01, 2)
; DOWN-RIGHT      | 10        | 2
  DOWN_RIGHT    = __actor_make_facing(%00, 3)
; DOWN-LEFT       | 11        | 2
  DOWN_LEFT     = __actor_make_facing(%01, 3)
; Special facing constant meaning leave current facing alone
  NO_CHANGE     = $ff
.endenum

; Set an actor's facing from a macro argument or from the accumulator if the
; argument is blank.
; @macroparam facing A constant from the ActorFacing enum -- either an
; immediate-mode constant or an address.
; @param X actor::addr
; @clobbers A, local_0 (if the_facing is blank)
.macro Actor_set_facing the_facing
    .ifblank the_facing
        Actor_set_facing_fromA
    .else
        .local done
        ; This should really be unnecessary if the caller gives an immediate-mode
        ; literal.
        lda the_facing
        cmp #ActorFacing::NO_CHANGE
        beq done
        ; first zero out the actor's facing bits
        lda #(ActorRenderFlagMask::facing ^ $ff); A = Inverted mask of which bits determine facing
        and ActorOffset::RENDER_FLAGS, X  ; AND w/ actor's current flags, i.e., A = actor's
                                   ; flags with facing bits zeroed
        ora the_facing             ; OR in desired facing bits
        sta ActorOffset::RENDER_FLAGS, X  ; store back as the actor's new flags
        done:
    .endif
.endmacro

; Set an actor's facing from the accumulator.
; @param X actor::addr
; @clobbers A, local_0
.macro Actor_set_facing_from_A
    .local done
    cmp #ActorFacing::NO_CHANGE
    beq done
    ; stash A
    sta local_0
    ; first zero out the actor's facing bits
    lda #(ActorRenderFlagMask::facing ^ $ff); A = Inverted mask of which bits determine facing
    and ActorOffset::RENDER_FLAGS, X  ; AND w/ actor's current flags, i.e., A = actor's
                               ; flags with facing bits zeroed
    ora local_0                ; OR in desired facing bits
    sta ActorOffset::RENDER_FLAGS, X  ; store back as the actor's new flags
    done:
.endmacro

; sets the zero flag if actor's render_flags indicate it is not active, clears
; the zero flag if it is active
.macro Actor_is_active render_flags
    lda render_flags
    and #ActorRenderFlagMask::is_active
.endmacro

.macro Actor_set_palette palette_index
    .assert palette_index >= 0 && palette_index < 4, error, \
        .sprintf("Bad palette index %d", palette_index)
    ; first zero out the actor's palette bits
    lda #(ActorRenderFlagMask::palette ^ $ff); A = Inverted mask of which bits determine palette
    and ActorOffset::RENDER_FLAGS, X  ; AND w/ actor's current flags, i.e., A = actor's
                                      ; flags with palette bits zeroed
    ora #(palette_index << 3)         ; OR in desired palette bits
    sta ActorOffset::RENDER_FLAGS, X  ; store back as the actor's new flags
.endmacro

.macro Actor_lda_palette
    lda #ActorRenderFlagMask::palette
    and ActorOffset::RENDER_FLAGS, X  ; A = actor's flags with non-palette bits zeroed
    lsr
    lsr
    lsr
.endmacro

.macro Actor_draw_actors N, oam_offset, draw_1x1, draw_2x2
    ; address of current 4-byte oam buffer entry we are updating
    buffer_entry_ptr = addr_0
    lda #<(PPU::oam_buffer + (oam_offset * 4))
    sta buffer_entry_ptr
    lda #>(PPU::oam_buffer + (oam_offset * 4))
    sta buffer_entry_ptr + 1
    ; inlined loop over all actors
    .repeat N, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            Actor_is_active actor_i::render_flags
            beq do_draw_done
            .if I > 0
                ; advance the buffer entry pointer
                lda buffer_entry_ptr ; 3 cycles
                clc                  ; 2 cycles
                adc #4               ; 2 cycles
                sta buffer_entry_ptr ; 3 cycles
            .endif
            ; draw actor's sprites with appropriate routine, depending whether
            ; it's a 1x1 or 2x2 actor. Both routines take the actor's base addr
            ; (in zeropage) from X, and the current OAM buffer entry pointer
            ; from addr_0
            ldx #actor_i::addr
            lda actor_i::render_flags
            and #ActorRenderFlagMask::is_2x2
            beq do_draw_1x1
            ; XXX Think about whether we want to inline these subroutines as
            ; macros. Could save a good few cycles by knowing all the addresses
            ; at assemble-time (no need for passing actor_i::addr through X)
            do_draw_2x2:
                ;jsr draw_2x2_actor_sprites
                draw_2x2
                jmp do_draw_done
            do_draw_1x1:
                ;jsr draw_1x1_actor_sprite
                draw_1x1
            do_draw_done:
        .endscope
        .undefine actor_i
    .endrepeat
.endmacro

.endif ; Actor_included
