
.include "coroutine.inc"

N_COROUTINES = 3

.segment "RAM"
my_coroutines: .res .sizeof(Coroutine::State)*N_COROUTINES + 2
my_coroutines_guard = my_coroutines + .sizeof(Coroutine::State)*N_COROUTINES

.segment "CODE"
.global test_coroutines
.proc test_coroutines
	; set guard on my_coroutines list (end-of-list marker)
	lda #$ff
	sta my_coroutines_guard
	sta my_coroutines_guard + 1

	; point Coroutine::self to the head of our array
	lda #<my_coroutines
	sta Coroutine::self
	lda #>my_coroutines
	sta Coroutine::self + 1

	; create coroutines
	lda #<(coroutine0-1)
	ldy #>(coroutine0-1)
	jsr Coroutine::new
	; TODO: check for self==$0000

	; iterate through coroutine0 till it halts
	lda #<my_coroutines
	sta Coroutine::self
	lda #>my_coroutines
	sta Coroutine::self + 1
	do_next1:
	jsr Coroutine::next
	do_next2:
	jsr Coroutine::next
	
	debug1:
	rts
.endproc

.proc coroutine0
	nop1:
	nop
	jsr Coroutine::yield
	nop2:
	nop
	jmp Coroutine::halt
.endproc

.proc coroutine1
	lda #$33
	ldy #Coroutine::State::DATA0
	sta (Coroutine::self), Y
	jsr Coroutine::yield
	ldx #$22
	lda #$11
	jsr Coroutine::yield
	dex
	txa
	iny ; DATA1
	sta (Coroutine::self), Y
	jmp Coroutine::halt
.endproc
