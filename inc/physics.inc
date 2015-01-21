; Inline macros for actor physics. Macros assume that all of the actor_NN
; labels have been reserved or imported in the current assembly unit

.ifndef Physics_included
Physics_included = 1
    
.include "actor.inc"
.include "math_macros.inc"

.macro Physics_do_gravity_inline N_ACTORS, DDY
    .scope 
        .repeat N_ACTORS, I
            .define actor_i .ident (.sprintf ("actor_%02d", I))
            .scope 
                Actor_is_active actor_i::render_flags
                beq next
                dy = actor_i::velocity::yval
                mathmac_add16 #DDY, dy, dy
                next:
            .endscope
            .undefine actor_i
        .endrepeat
        ;rts
    .endscope
.endmacro

; TODO: collision boxes and such
; FLOOR_Y and CEILING_Y may be blank, disabling the corresponding bounce
.macro Physics_move_actors_with_bounce_inline N_ACTORS, FLOOR_Y, CEILING_Y
    .repeat N_ACTORS, I
        .define actor_i .ident (.sprintf ("actor_%02d", I))
        .scope 
            Actor_is_active actor_i::render_flags
            beq next
            px = actor_i::position::xval
            dx = actor_i::velocity::xval
            py = actor_i::position::yval
            dy = actor_i::velocity::yval
            mathmac_add16 px, dx, px
            mathmac_add16 py, dy, py

            .ifnblank CEILING_Y
                ; floor and ceiling bounce wrt most significant byte of py Note
                ; we are inlining this for each actor!
                lda py+1
                cmp #CEILING_Y
                bcc hit_ceiling
            .endif

            .ifnblank FLOOR_Y
                ; 2x2 actors effectively have their floor shifted up by 8 pixels
                lda actor_i::render_flags
                and #ActorRenderFlagMask::is_2x2
                beq :+
                    ; 2x2 actor
                    lda py+1
                    cmp #(FLOOR_Y - 8)
                    bcc collision_done
                    jmp hit_floor
                :
                ; 1x1 actor
                lda py+1
                cmp #FLOOR_Y
                bcc collision_done
                hit_floor:
                    ; clip to floor
                    lda actor_i::render_flags
                    and #ActorRenderFlagMask::is_2x2
                    beq :+
                        mathmac_set16 #((FLOOR_Y - 9) * $100), py
                        jmp hit_common
                    :
                    mathmac_set16 #((FLOOR_Y - 1) * $100), py
                    jmp hit_common
            .endif ; FLOOR_Y
            .ifnblank CEILING_Y
                hit_ceiling:
                    ; clip to ceiling
                    mathmac_set16 #((CEILING_Y + 1) * $100), py
            .endif
            .if (!.blank(CEILING_Y)) || (!.blank(FLOOR_Y))
                hit_common:
                    ; negate velocity y-component
                    mathmac_neg16 dy, dy
                    ; divide velocity y-component by two
                    lda dy + 1
                    asl
                    ror dy + 1
                    ror dy + 0
                collision_done:
            .endif
            next:
        .endscope
        .undefine actor_i
    .endrepeat
.endmacro

.endif ; Physics_included
