; NES Picture Processing Unit module
; Useful constants, macros, and subroutines

.fileopt    comment, "NES Picture Processing Unit module, v1.0"
.fileopt    author,  "Max Bane"

.scope PPU

;
; PPU Register Address Constants
;

.export REG_CTRL     := $2000
; Writable
; 7654 3210
; |||| ||||
; |||| ||++- Base nametable address
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA
; |||| |     (0: add 1, going across; 1: add 32, going down)
; |||| +---- Sprite pattern table address for 8x8 sprites
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
; |||+------ Background pattern table address (0: $0000; 1: $1000)
; ||+------- Sprite size (0: 8x8; 1: 8x16)
; |+-------- PPU master/slave select
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
; +--------- Generate an NMI at the start of the
;            vertical blanking interval (0: off; 1: on)
; (Diagram from wiki.nesdev.com)

.export REG_MASK     := $2001
; Writable
; 76543210
; ||||||||
; |||||||+- Grayscale (0: normal color; 1: produce a monochrome display)
; ||||||+-- 1: Show background in leftmost 8 pixels of screen; 0: Hide
; |||||+--- 1: Show sprites in leftmost 8 pixels of screen; 0: Hide
; ||||+---- 1: Show background
; |||+----- 1: Show sprites
; ||+------ Intensify reds (and darken other colors)
; |+------- Intensify greens (and darken other colors)
; +-------- Intensify blues (and darken other colors)
; (Diagram from wiki.nesdev.com)

.export REG_STATUS   := $2002
; Readable
; 7654 3210
; |||| ||||
; |||+-++++- Least significant bits previously written into a PPU register
; |||        (due to register not being updated for this address)
; ||+------- Sprite overflow. The intent was for this flag to be set
; ||         whenever more than eight sprites appear on a scanline, but a
; ||         hardware bug causes the actual behavior to be more complicated
; ||         and generate false positives as well as false negatives; see
; ||         PPU sprite evaluation. This flag is set during sprite
; ||         evaluation and cleared at dot 1 (the second dot) of the
; ||         pre-render line.
; |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
; |          a nonzero background pixel; cleared at dot 1 of the pre-render
; |          line.  Used for raster timing.
; +--------- Vertical blank has started (0: not in VBLANK; 1: in VBLANK).
;            Set at dot 1 of line 241 (the line *after* the post-render
;            line); cleared after reading $2002 and at dot 1 of the
;            pre-render line.
; (Diagram from wiki.nesdev.com)

.export REG_OAM_ADDR := $2003
; Writable

.export REG_OAM_DATA := $2004
; Read/Write

.export REG_OAM_DMA  := $4014
; Fancy Write

.export REG_SCROLL   := $2005
; Write x2

.export REG_ADDR     := $2006
; Write x2

.export REG_DATA     := $2007
; Read/Write

;
; Buffers reserved in ram for updating the PPU
;
.segment "RAM"

; nametable update entry buffer for PPU update
nmt_buffer: .res 256
.export nmt_buffer

; palette buffer for PPU update
palette_buffer:    .res 32
.export palette_buffer

; Reserve a page for an object attribute map buffer
.segment "OAM"

; An entry in the sprite object attribute memory
.struct OAMEntry
	y_coord		.byte
	tile_index	.byte
	attributes	.byte
	x_coord		.byte
.endstruct

; sprite OAM data to be uploaded by DMA
oam_buffer: .res 64 * .sizeof(OAMEntry)
.export oam_buffer

;
; Zeropage variables used by our PPU routines
;
.segment "ZEROPAGE"

; prevents NMI re-entry
nmi_lock:       .res 1 
.exportzp nmi_lock

; is incremented every NMI
nmi_count:      .res 1 
.exportzp nmi_count

; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmi_ready:      .res 1 
.exportzp nmi_ready

; number of bytes in nmt_update buffer
nmt_buffer_len: .res 1 
.exportzp nmt_buffer_len

; x,y scroll positions
scroll_x:       .res 1 
scroll_y:       .res 1 
.exportzp scroll_x, scroll_y

; nametable select (0-3 = REG_CTRL,$2400,$2800,$2C00)
scroll_nmt:     .res 1 
.exportzp scroll_nmt

; ppumask buffer
mask:           .res 1

_temp:           .res 1 ; temporary variable


;
; PPU routines
;
.segment "CODE"

; Generic PPU reset/startup routine. Disables rendering and NMI, waits for two
; vblanks, and finally enables the NMI.
.export reset
reset:
    lda #0
    sta REG_CTRL ; disable NMI
    sta REG_MASK ; disable rendering
    ; wait for first vblank
    bit REG_STATUS
    :
        bit REG_STATUS
        bpl :-
    ; place all sprites offscreen at Y=255
    lda #255
    ldx #0
    :
        sta oam_buffer, X
        inx
        inx
        inx
        inx
        bne :-
    ; wait for second vblank
    :
        bit REG_STATUS
        bpl :-
    ; PPU is initialized
    ; enable the NMI for graphical updates
    lda #%10001000
    sta REG_CTRL
    rts

;
; Generic buffer-copying, scrolling NMI routine
;
.export nmi_buffered
nmi_buffered:
    ; save registers
    pha
    txa
    pha
    tya
    pha
    ; prevent NMI re-entry
    lda nmi_lock
    beq :+
        jmp @nmi_end
    :
    lda #1
    sta nmi_lock
    ; increment frame counter
    inc nmi_count
    ;
    lda nmi_ready
    bne :+ ; nmi_ready == 0 not ready to update PPU
        jmp @update_end
    :
    cmp #2 ; nmi_ready == 2 turns rendering off
    bne :+
        lda mask
        sta REG_MASK
        ldx #0
        stx nmi_ready
        jmp @update_end
    :
    ; sprite OAM DMA
    ldx #0
    stx REG_OAM_ADDR
    lda #>oam_buffer
    sta REG_OAM_DMA
    ; palettes
    lda #%10001000
    sta REG_CTRL ; set horizontal nametable increment
    lda REG_STATUS
    lda #$3F
    sta REG_ADDR
    stx REG_ADDR ; set PPU address to $3F00
    ldx #0
    :
        lda palette_buffer, X
        sta REG_DATA
        inx
        cpx #32
        bcc :-
    ; nametable update
    ldx #0
    cpx nmt_buffer_len
    bcs @scroll
    @nmt_update_loop:
        lda nmt_buffer, X
        sta REG_ADDR
        inx
        lda nmt_buffer, X
        sta REG_ADDR
        inx
        lda nmt_buffer, X
        sta REG_DATA
        inx
        cpx nmt_buffer_len
        bcc @nmt_update_loop
    lda #0
    sta nmt_buffer_len
@scroll:
    lda scroll_nmt
    and #%00000011 ; keep only lowest 2 bits to prevent error
    ora #%10001000
    sta REG_CTRL
    lda scroll_x
    sta REG_SCROLL
    lda scroll_y
    sta REG_SCROLL
    ; enable rendering
    ; TODO: get this from a variable the user can set
    lda #%00011110
    sta REG_MASK
    ; flag PPU update complete
    ldx #0
    stx nmi_ready
@update_end:
    ; if this engine had music/sound, this would be a good place to play it
    ; unlock re-entry flag
    lda #0
    sta nmi_lock
@nmi_end:
    ; restore registers and return
    pla
    tay
    pla
    tax
    pla
    rti

;
; update: intended to be called once per logical game frame. Waits until
; next NMI, where the NMI handler, if set to nmi_buffered, will turn
; rendering on (if not already), upload OAM, palette, and nametable update to
; PPU.
;
.export update
update:
    lda #1
    sta nmi_ready
    :
        lda nmi_ready
        bne :-
    rts

; skip: waits until next NMI, nmi_buffered will not update PPU
.export skip
skip:
    lda nmi_count
    :
        cmp nmi_count
        beq :-
    rts

; off: waits until next NMI, numi_buffered will turn rendering off; now
; safe to write PPU directly via REG_DATA after off returns.
.export off
off:
    lda #2
    sta nmi_ready
    :
        lda nmi_ready
        bne :-
    rts

; address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a REG_DATA write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
.export address_tile
address_tile:
    lda REG_STATUS ; reset latch
    tya
    lsr
    lsr
    lsr
    ora #$20 ; high bits of Y + $20
    sta REG_ADDR
    tya
    asl
    asl
    asl
    asl
    asl
    sta _temp
    txa
    ora _temp
    sta REG_ADDR ; low bits of Y + X
    rts

; update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call update
.export update_tile
update_tile:
    pha ; temporarily store A on stack
    txa
    pha ; temporarily store X on stack
    ldx nmt_buffer_len
    tya
    lsr
    lsr
    lsr
    ora #$20 ; high bits of Y + $20
    sta nmt_buffer, X
    inx
    tya
    asl
    asl
    asl
    asl
    asl
    sta _temp
    pla ; recover X value (but put in A)
    ora _temp
    sta nmt_buffer, X
    inx
    pla ; recover A value (tile)
    sta nmt_buffer, X
    inx
    stx nmt_buffer_len
    rts

; update_byte: like update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
.export update_byte
update_byte:
    pha ; temporarily store A on stack
    tya
    pha ; temporarily store Y on stack
    ldy nmt_buffer_len
    txa
    sta nmt_buffer, Y
    iny
    pla ; recover Y value (but put in Y)
    sta nmt_buffer, Y
    iny
    pla ; recover A value (byte)
    sta nmt_buffer, Y
    iny
    sty nmt_buffer_len
    rts

;
; clear_background: clears all nametable tile entries and attributes to 0.
; Writes directly to REG_DATA, so call when rendering is off.
;
.export clear_background
clear_background:
    ; first nametable, start by clearing to empty
    lda REG_STATUS ; reset latch
    lda #$20
    sta REG_ADDR
    lda #$00
    sta REG_ADDR
    ; empty nametable
    lda #0
    ldy #30 ; 30 rows
    :
        ldx #32 ; 32 columns
        :
            sta REG_DATA
            dex
            bne :-
        dey
        bne :--
    ; set all attributes to 0
    ldx #64 ; 64 bytes
    :
        sta REG_DATA
        dex
        bne :-
    rts

.endscope ; PPU
