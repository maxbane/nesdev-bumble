; NES Gamepad module
; Useful constants and subroutines

.ifnblank Joy::__included
.scope Joy
    __included = 1
    .import PAD0_ADDR
    .import PAD1_ADDR

    ; button state masks
    BUTTON_A      = $01
    BUTTON_B      = $02
    BUTTON_SELECT = $04
    BUTTON_START  = $08
    BUTTON_UP     = $10
    BUTTON_DOWN   = $20
    BUTTON_LEFT   = $40
    BUTTON_RIGHT  = $80

    .importzp pad0
    .import   poll
.endscope ; Joy
.endif ; .ifndef Joy::__included
