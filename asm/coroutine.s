.include "math_macros.inc"
.include "coroutine.inc"

.segment "ZEROPAGE"
Coroutine::self:	.res 2
tmp_addr:			.res 2	; XXX see if we can use existing locals

.segment "CODE"

Coroutine::clear_state = clear_state
.proc clear_state
	; zero STATUS
	lda #%00110000
	ldy #Coroutine::State::STATUS
	sta (Coroutine::self), Y
	; zero accum, regs, data
	lda #$00
	iny
	sta (Coroutine::self), Y	; ACCUM
	iny
	sta (Coroutine::self), Y	; XREG
	iny
	sta (Coroutine::self), Y	; YREG
	iny
	sta (Coroutine::self), Y	; DATA0
	iny
	sta (Coroutine::self), Y	; DATA1
	rts
.endproc

; jsr with Coroutine::self = coroutine
Coroutine::yield = yield
.proc yield
	; immediately stash P and A
	php
	pha
	; store Y
	tya
	ldy #Coroutine::State::YREG
	sta (Coroutine::self), Y
	; store X
	txa
	ldy #Coroutine::State::XREG
	sta (Coroutine::self), Y
	; store stashed A
	pla
	ldy #Coroutine::State::ACCUM
	sta (Coroutine::self), Y
	; store stashed P
	pla
	ldy #Coroutine::State::STATUS
	sta (Coroutine::self), Y
	; next two bytes on stack are now return address of our caller. pop
	; those and store them as the coroutine's PROG.
	ldy #Coroutine::State::PROG
	pla
	sta (Coroutine::self), Y ; low byte
	iny
	pla
	sta (Coroutine::self), Y ; high byte
	; off we go. note because we have popped our caller's return address,
	; we are actually returning to our caller's caller. (or to whomever the
	; caller arranged for us to yield to.)
	lda #0
	rts
.endproc

; jsr with
;   Zeropage pointer Coroutine::Coroutine::self set to the address of the head
;     of a list of Coroutines, terminated by $FFFF aligned with
;     Coroutine::State::PROG 
;   AY = pointer to coroutine code, at starting entry point
; rts's with Coroutine::self = addr of new coroutine, or zero if no free space
; clobbers AXY, Coroutine::Coroutine::self
Coroutine::new = new
.proc new
	; XXX use stack instead?
	sta tmp_addr
	tya
	sta tmp_addr+1

	; free coroutines are marked by PROG being $0000. End of list of
	; coroutines marked by PROG $ffff
	each:
		ldx #0
		ldy #Coroutine::State::PROG
		lda (Coroutine::self), Y
		bne increment_self ; Coroutine::self not free
		cmp #$ff
		bne :+
			; use X to remember that the first byte was $ff
			ldx #1
		:
		iny
		lda (Coroutine::self), Y
		bne increment_self ; Coroutine::self not free
		cmp #$ff
		bne :+
			; second byte was $ff. if first byte also was, as remembered
			; by X, then we've hit the end of the list
			dex
			beq end_of_list
		:
		; Coroutine::self is free. store user-supplied address in PROG, clear the
		; coroutine's state, and return
		dey ; Y = #Coroutine::State::PROG
		lda tmp_addr
		sta (Coroutine::self), Y
		lda tmp_addr + 1
		iny
		sta (Coroutine::self), Y
		jsr clear_state
		lda #0 ; success
		rts

		increment_self:
			mathmac_add16 #.sizeof(Coroutine::State), Coroutine::self, Coroutine::self
			jmp each

	end_of_list:
		mathmac_clr16 Coroutine::self ; set Coroutine::self=$0000 to indicate failure
		lda #1 ; error indicator
		rts
.endproc

; halt: meant to be called by the coroutine code itself.
; jmp with Coroutine::self = coroutine
Coroutine::halt = halt
.proc halt
	ldy #Coroutine::State::PROG
	lda #$00
	sta (Coroutine::self), Y
	iny
	sta (Coroutine::self), Y
	lda #1
	rts
.endproc

; so deep, man
Coroutine::free = halt

; step: jsr with Coroutine::self = coroutine
Coroutine::step = step
.proc step
	; first make sure PROG != $0000 (halted)
	;ldy #0
	;lda (Coroutine::self), Y
	;bne :+
	;	iny
	;	lda (Coroutine::self), Y
	;	bne :+
	;	rts
	;:	; PROG != $0000
	
	; push PROG onto stack for eventual RTS
	ldy #Coroutine::State::PROG+1
	lda (Coroutine::self), Y
	pha
	dey
	lda (Coroutine::self), Y
	pha
	
	; set X
	ldy #Coroutine::State::XREG
	lda (Coroutine::self), Y
	tax

	; stash status
	ldy #Coroutine::State::STATUS
	lda (Coroutine::self), Y
	pha 

	; stash A
	ldy #Coroutine::State::ACCUM
	lda (Coroutine::self), Y
	pha

	; set Y
	ldy #Coroutine::State::YREG
	lda (Coroutine::self), Y
	tay

	pla ; set A
	plp ; set status
	rts ; jmp to PROG address+1
.endproc

Coroutine::step_all = step_all
.proc step_all
	; make sure PROG != $0000 (halted)
	ldy #Coroutine::State::PROG
	lda (Coroutine::self), Y
	bne :+
		iny
		lda (Coroutine::self), Y
		beq advance_self ; PROG == $0000 
	:
	; PROG != $0000
	; make sure PROG != $ffff
	ldy #Coroutine::State::PROG
	lda (Coroutine::self), Y
	cmp #$ff
	bne :+
		iny
		lda (Coroutine::self), Y
		cmp #$ff
		bne :+
		; PROG == $FFFF, end of array
		rts
	:
	; PROG != $FFFF
	; step it
	jsr Coroutine::step
	; advance to next coroutine
	advance_self:
	mathmac_add16 #.sizeof(Coroutine::State), Coroutine::self, Coroutine::self
	jmp step_all
.endproc

