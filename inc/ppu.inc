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
	.importzp mask

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

.macro PPU_sta_oam_byte oam_entry, byteno
	sta oam_buffer + (oam_entry * 4) + byteno
.endmacro

; example:
;   ; sprite 2 gets x=234, y=124, tile number = 3, attributes = 0
;   PPU_update_oam_entry 2, #234, #3, #0, #124
; oam_entry must be a bare numeral
; remaining arguments may be immediate mode or addresses, including indirect
; Clobbers X and A.
.macro PPU_update_known_oam_entry oam_entry, ypos, tileno, attributes, xpos
	ldx #(oam_entry * 4)

	; load and store our four arguments
	lda ypos
	sta oam_buffer, X
	inx
	lda tileno
	sta oam_buffer, X
	inx
	lda attributes
	sta oam_buffer, X
	inx
	lda xpos
	sta oam_buffer, X
.endmacro

; call with:
;	A = index of oam_entry to update
; Clobbers X and A.
.macro PPU_update_oam_entry ypos, tileno, attributes, xpos
	; A = A * 4
	asl
	asl
	; X = A
	tax
	; load and store our four arguments
	lda ypos
	sta oam_buffer, X
	inx
	lda tileno
	sta oam_buffer, X
	inx
	lda attributes
	sta oam_buffer, X
	inx
	lda xpos
	sta oam_buffer, X
.endmacro

.endif ; .ifndef PPU_included
