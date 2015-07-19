.ifndef Coroutine_included
Coroutine_included = 1

.scope Coroutine
	.importzp self

	.scope Offset
		.importzp PROG
		.importzp STATUS
		.importzp ACCUM
		.importzp XREG
		.importzp YREG
		.importzp DATA0
		.importzp DATA1
		.importzp SIZE
	.endscope
	
	; new: given a user-supplied list of coroutine structs, finds an unused one
	; and initializes it to execute code at a user-supplied address.
	; Caller: jsr with self = head of list, AY = pointer to coroutine code.
	; list must be terminated by #$ffff following the final coroutine struct
	; (i.e., what would be the PROG of the next coroutine if there were one).
	; Coroutines in the list with $0000 as their PROG are considered
	; "free"/available.
	; Returns with self = addr of new coroutine, $0000 if no free coroutines
	; are available
	.import new

	; next: meant to be called by a user of the coroutine. jsr with self =
	; coroutine. reenters the coroutine code and runs until the next yield or
	; halt
	.import next

	; yield: meant to be called by the coroutine code itself.
	; Caller: jsr with self = coroutine; address at top of stack (before jsr)
	; is where we yield to (typically your own next-caller)
	.import yield

	; halt: meant to be called by the coroutine code itself.
	; jmp with self = coroutine. like a yield, but without saving any state,
	; and marks the coroutine struct at self as free. control flow passes to
	; address at top of stack, typically wherever next was last called on the
	; coroutine, but the coroutine can manipulate this to control who we "halt
	; to", just the same as with yield.
	.import halt

	; clear_state: clears the state (a, x, y, status, data0, data1, but not PC)
	; of self.
	; Caller: jsr with self = coroutine
	.import clear_state
.endscope


.endif ; Coroutine_included
