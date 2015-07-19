.include "math_macros.inc"

.scope Coroutine
	.segment "ZEROPAGE"
	.exportzp self
	self: .res 2
	; XXX see if we can use existing locals
	tmp_addr: .res 2

	.segment "CODE"
	.scope Offset
		.exportzp PROG		= 0
		.exportzp STATUS	= 2
		.exportzp ACCUM		= 3
		.exportzp XREG		= 4
		.exportzp YREG		= 5
		.exportzp DATA0		= 6
		.exportzp DATA1		= 7
		.exportzp SIZE		= 8
	.endscope

	.export clear_state
	.proc clear_state
		; zero STATUS
		lda #%00110000
		ldy #Offset::STATUS
		sta (self), Y
		; zero accum, regs, data
		lda #$00
		iny
		sta (self), Y	; ACCUM
		iny
		sta (self), Y	; XREG
		iny
		sta (self), Y	; YREG
		iny
		sta (self), Y	; DATA0
		iny
		sta (self), Y	; DATA1
		rts
	.endproc
	
	; jsr with self = coroutine
	.export yield
	.proc yield
		; immediately stash P and A
		php
		pha
		; store Y
		tya
		ldy #Offset::YREG
		sta (self), Y
		; store X
		txa
		ldy #Offset::XREG
		sta (self), Y
		; store stashed A
		pla
		ldy #Coroutine::Offset::ACCUM
		sta (Coroutine::self), Y
		; store stashed P
		pla
		ldy #Coroutine::Offset::STATUS
		sta (Coroutine::self), Y
		; next two bytes on stack are now return address of our caller. pop
		; those and store them as the coroutine's PROG.
		ldy #Offset::PROG
		pla
		sta (self), Y ; low byte
		iny
		pla
		sta (self), Y ; high byte
		; off we go. note because we have popped our caller's return address,
		; we are actually returning to our caller's caller. (or to whomever the
		; caller arranged for us to yield to.)
		rts
	.endproc

	; jsr with
	;   Zeropage pointer Coroutine::self set to the address of the head
	;     of a list of Coroutines, terminated by $FFFF aligned with
	;     Offset::PROG 
	;   AY = pointer to coroutine code, at starting entry point
	; rts's with self = addr of new coroutine, or zero if no free space
	; clobbers AXY, Coroutine::self
	.export new
	.proc new
		sta tmp_addr
		tya
		sta tmp_addr+1

		; free coroutines are marked by PROG being $0000. End of list of
		; coroutines marked by PROG $ffff
		each:
			ldx #0
			ldy #Offset::PROG
			lda (self), Y
			bne increment_self ; self not free
			cmp #$ff
			bne :+
				; use X to remember that the first byte was $ff
				ldx #1
			:
			iny
			lda (self), Y
			bne increment_self ; self not free
			cmp #$ff
			bne :+
				; second byte was $ff. if first byte also was, as remembered
				; by X, then we've hit the end of the list
				dex
				beq end_of_list
			:
			; self is free. store user-supplied address in PROG, clear the
			; coroutine's state, and return
			dey ; Y = #Offset::PROG
			lda tmp_addr
			sta (self), Y
			lda tmp_addr + 1
			iny
			sta (self), Y
			jsr clear_state
			rts

			increment_self:
				mathmac_add16 #Offset::SIZE, self, self
				jmp each

		end_of_list:
			mathmac_clr16 self ; set self=$0000 to indicate failure
			rts
	.endproc

	; halt: meant to be called by the coroutine code itself.
	; jmp with self = coroutine
	.proc halt
		ldy #Offset::PROG
		lda #$00
		sta (self), Y
		iny
		sta (self), Y
		rts
	.endproc

	; next: jsr with self = coroutine
	.export next
	.proc next
		; first make sure PROG != $0000 (halted)
		ldx #0
		lda self, X
		bne :+
			inx
			lda self, X
			bne :+
			rts
		:	; PROG != $0000
		
		; set X
		ldy #Offset::XREG
		lda (self), Y
		tax

		; stash status
		ldy #Offset::STATUS
		lda (self), Y
		pha 

		; stash A
		ldy #Offset::ACCUM
		lda (self), Y
		pha

		; set Y
		ldy #Offset::YREG
		lda (self), Y
		tay

		pla ; set A
		plp ; set status
		jmp (self) ; rts happens when coroutine yields or halts
	.endproc
.endscope
