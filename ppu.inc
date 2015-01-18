.ifndef PPU_included
PPU_included = 1

.scope PPU
    ; PPU Register Address Constants
    .import REG_CTRL
    .import REG_MASK
    .import REG_STATUS
    .import REG_OAM_ADDR
    .import REG_OAM_DATA
    .import REG_OAM_DMA
    .import REG_SCROLL
    .import REG_ADDR
    .import REG_DATA

    ; Buffer addresses
    .import nmt_buffer
    .import palette_buffer
    .import oam_buffer

    ; Zeropage variables used by PPU routines
    .importzp nmi_lock
    .importzp nmi_count
    .importzp nmi_ready
    .importzp nmt_buffer_len
    .importzp scroll_x, scroll_y
    .importzp scroll_nmt

    ; PPU Routines
    .import reset
    .import nmi_buffered
    .import update
    .import skip
    .import off
    .import address_tile
    .import update_tile
    .import update_byte
    .import clear_background

.endscope ; PPU
.endif ; .ifndef PPU_included
