.ifndef Coroutine_included
Coroutine_included = 1

; XXX technically semicoroutines (generators, as in Python). for full
; coroutines, we'd need to split yield and halt respectively into
; yield_to/yield_to_caller and halt_to/halt_to_caller, where the more generic
; variants take an additional parameter, the address of the coroutine to
; halt/yield to, this destination coroutine's PROG being used for the RTS
; (rather than using the call stack), and Coroutine::self being set to the
; destination coroutine in between saving state and RTS'ing.
.scope Coroutine
	; "current" coroutine pointer (2 bytes in ZP), on which most procedures
	; operate, assumed to point to an element of a $ffff-terminated array of
	; coroutines
	.globalzp self

	; Convenience macro
	.macro Coroutine_select coroutine
		lda #<coroutine
		sta Coroutine::self
		lda #>coroutine
		sta Coroutine::self + 1
	.endmacro

	; Each coroutine is associated with 8 bytes of state.
	.struct State
		; execution context as of last yield
		PROG	.addr;= 0 (next addr - 1, i.e., RTS compatible)
		STATUS  .byte;= 2
		ACCUM	.byte
		XREG	.byte
		YREG	.byte
		; two bytes of user data. can be used by coroutine code as local
		; mutables, and/or as communication channel
		DATA0	.byte
		DATA1	.byte
	.endstruct

	.macro Coroutine_sta_state offset
		ldy #offset
		sta (Coroutine::self), Y
	.endmacro
	
	.macro Coroutine_lda_state offset
		ldy #offset
		lda (Coroutine::self), Y
	.endmacro
	
	; new (subroutine):
	;	given a user-supplied, guard-terminated array of coroutine structs,
	;	finds an unused one and initializes it to execute code at a
	;	user-supplied address.
	; Caller: jsr with self = head of list, AY = pointer to coroutine code - 1.
	; list must be terminated by guard #$ffff following the final coroutine
	; struct (i.e., what would be the PROG of the next coroutine if there were
	; one).  Coroutines in the list with $0000 as their PROG are considered
	; "free"/available.
	; Returns with self = addr of new coroutine, $0000 if no free coroutines
	; are available; A = nonzero if no free coroutines
	.global new

	; convenience macro
	.macro Coroutine_new code_addr, coroutines, on_failure
		.ifnblank coroutines
			Coroutine_select coroutines
		.endif
		lda #<(code_addr-1)
		ldy #>(code_addr-1)
		jsr Coroutine::new
		.ifnblank on_failure
			bne on_failure
		.endif
	.endmacro

	; step (subroutine): meant to be called by a user of the coroutine. jsr
	; with self = coroutine. (re-)enters the coroutine code, executing through
	; the next yield or halt. Think of calling some_generator.next() in Python.
	; Returns with A=1 if the coroutine has halted, 0 otherwise.
	.global step

	; step_all (subroutine): meant to be called by a user of the
	; guard-terminated array of coroutines pointed to by Coroutine::self. jsr
	; with self = head of array. Calls step once on each non-halted coroutine
	; in the array.
	.global step_all

	; yield (subroutine): meant to be called by the coroutine code itself.
	; Caller: jsr with self = coroutine; address at top of stack (before jsr)
	; is where flow will return to, usually the callsite of the step-call that
	; stepped the coroutine that is now calling yield. A will = 0 when flow
	; returns. Think of the yield keyword in Python
	.global yield

	; halt (jmp point): meant to be jmpd to by the coroutine code itself.
	; jmp with self = coroutine. like a yield, but without saving any state,
	; and marks self as free for use by Coroutine::new. control flow passes to
	; address at top of stack.  A will = 1 when flow returns.
	; Think of raising StopIteration in Python. 
	.global halt

	; convenient macros for yield and halt so you don't have to remember which
	; is for jsr'ing and which is for jmp'ing.
	.macro Coroutine_yield
		jsr Coroutine::yield
	.endmacro
	.macro Coroutine_halt
		jmp Coroutine::halt
	.endmacro

	; free (subroutine): meant to be called by a user of the coroutine. jsr
	; with self = coroutine. simply marks the struct as free (PROG = $0000) and
	; rts's
	.global free

	; clear_state: clears the state (a, x, y, status, data0, data1, but not PC)
	; of self.
	; Caller: jsr with self = coroutine
	.global clear_state
.endscope


.endif ; Coroutine_included
