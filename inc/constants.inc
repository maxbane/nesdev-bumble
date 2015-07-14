.ifndef Constants_included
Constants_included = 1

; Game constants
.scope Constants
    N_ACTORS                = 2
    ; First N_EFFECTS entries in the OAM buffer reserved for non-actor sprites
	N_EFFECTS				= 8
    ;PLAYER_TILE_BASE        = 1
    ACTOR_BASE_OAM_SPRITENO = 1
    ; gravitational acceleration 
    GRAVITY_DDY             = $0004
    ; Amount by which to accelerate player if flapping straight up
    BASE_FLAP_ACCEL_VERT    = $0080 ; -ddy
    ; Amount to accelerate if flapping up and to the left/right
    ; Idea is to keep vector magnitude the same
    BASE_FLAP_ACCEL_DIAG    = $005A ; sqrt((ddy^2)/2)

    FLOOR_Y                 = 223
    CEILING_Y               = 8
    SCREEN_WIDTH            = 256
.endscope

.endif ; Constants_included
