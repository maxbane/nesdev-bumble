.ifndef Coroutine_included
Coroutine_included = 1

.scope Coroutine
	; "current" coroutine pointer (2 bytes in ZP), on which most procedures
	; operate 
	.globalzp self

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
	
	; new: given a user-supplied list of coroutine structs, finds an unused one
	; and initializes it to execute code at a user-supplied address.
	; Caller: jsr with self = head of list, AY = pointer to coroutine code - 1.
	; list must be terminated by #$ffff following the final coroutine struct
	; (i.e., what would be the PROG of the next coroutine if there were one).
	; Coroutines in the list with $0000 as their PROG are considered
	; "free"/available.
	; Returns with self = addr of new coroutine, $0000 if no free coroutines
	; are available
	.global new

	; next: meant to be called by a user of the coroutine. jsr with self =
	; coroutine. reenters the coroutine code and runs until the next yield or
	; halt
	.global next

	; yield: meant to be called by the coroutine code itself.
	; Caller: jsr with self = coroutine; address at top of stack (before jsr)
	; is where we yield to (typically your own next-caller)
	; XXX actually this is yield_to_caller. Would need a separate yield_to that
	; handles updating the self pointer just before jmp'ing, and reentry into
	; the coroutine yielded to
	.global yield

	; halt: meant to be called by the coroutine code itself.
	; jmp with self = coroutine. like a yield, but without saving any state,
	; and marks the coroutine struct at self as free. control flow passes to
	; address at top of stack, typically wherever next was last called on the
	; coroutine, but the coroutine can manipulate this to control who we "halt
	; to", just the same as with yield.
	.global halt

	; clear_state: clears the state (a, x, y, status, data0, data1, but not PC)
	; of self.
	; Caller: jsr with self = coroutine
	.global clear_state
.endscope


.endif ; Coroutine_included
