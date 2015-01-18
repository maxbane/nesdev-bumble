.ifndef Actor_included
Actor_included = 1

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
        .endscope
    .endrepeat
    .out    .sprintf("Reserved %d zeropage bytes for %d actors.", ActorOffset::ACTOR_SIZE * N, N)
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
    ; 8 bits of various state flags; see ActorFlagMask
    FLAGS      = 9

    ACTOR_SIZE = 10
.endscope

.enum ActorFlagMask
    ; Actors can be 1x1 sprite or 2x2 sprites. In the latter case, the
    ; base_tile is the pattern-table index of the upper-left sprite, and
    ; different drawing logic, collision logic, etc. must apply to the actor.
    is_2x2  = %00000001
.endenum

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
            .exportzp flags
        .endscope
    .endrepeat
.endmacro

.macro Actor_draw_actors N, draw_1x1, draw_2x2
    ; address of current 4-byte oam buffer entry we are updating
    buffer_entry_ptr = addr_0
    lda #<PPU::oam_buffer
    sta buffer_entry_ptr
    lda #>PPU::oam_buffer
    sta buffer_entry_ptr + 1
    ; inlined loop over all actors
    .repeat N, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
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
            lda actor_i::flags
            and #ActorFlagMask::is_2x2
            beq do_draw_1x1
            ; XXX Think about whether we want to inline these subroutines as
            ; macros. Could save a good few cycles by knowing all the addresses
            ; at run-time (no need for passing actor_i::addr through X)
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
