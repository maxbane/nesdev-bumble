
.include "coroutine.inc"
.include "math_macros.inc"

N_COROUTINES = 3

.segment "RAM"
my_coroutines: .res .sizeof(Coroutine::State)*N_COROUTINES + 2
my_coroutines_guard = my_coroutines + .sizeof(Coroutine::State)*N_COROUTINES

.segment "CODE"

.proc ensure_guard
	; set guard on my_coroutines list (end-of-list marker)
	lda #$ff
	sta my_coroutines_guard
	sta my_coroutines_guard + 1
	rts
.endproc

.global test_coroutines
.proc test_coroutines
	jsr ensure_guard

	; point Coroutine::self to the head of our array and create coroutines
	Coroutine_new coroutine0, my_coroutines
	Coroutine_new coroutine1
	; TODO: check for error

	; self = coroutine1
	; iterate through coroutine1 till it halts
	:
		jsr Coroutine::step
		beq :-
	coroutine1_done:
	; iterate through coroutine0 till it halts
	Coroutine_select my_coroutines ; self = coroutine0
	:
		jsr Coroutine::step
		beq :-
	coroutine0_done:
	; both coroutines have halted
	; initialize some more and step them all twice
	Coroutine_new coroutine1, my_coroutines
	Coroutine_new coroutine0
	Coroutine_new coroutine2
	Coroutine_select my_coroutines
	jsr Coroutine::step_all
	Coroutine_select my_coroutines
	jsr Coroutine::step_all
	rts
.endproc

.proc coroutine0
	nop1:
	nop
	Coroutine_yield
	nop2:
	nop
	Coroutine_halt
.endproc

.proc coroutine1
	lda #$33
	ldy #Coroutine::State::DATA0
	sta (Coroutine::self), Y
	Coroutine_yield
	resume1:
	ldx #$22
	lda #$11
	Coroutine_yield
	resume2:
	dex
	txa
	iny ; DATA1
	sta (Coroutine::self), Y
	Coroutine_halt
.endproc

.proc coroutine2
	ldy #Coroutine::State::DATA0
	ldx #5
	:
		txa
		sta (Coroutine::self), Y
		Coroutine_yield
		resume3:
		dex
		bne :-
		
	Coroutine_halt
.endproc
