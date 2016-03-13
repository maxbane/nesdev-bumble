.ifndef Coroutine_included
Coroutine_included = 1 ;;

; Simple Coroutine API
; --------------------
; 
; To use the API, include the ``coroutine.inc`` include file and link against
; the ``coroutine.o`` object module. All symbols are organized under the
; :scope:`Coroutine <coroutine.inc:Coroutine>` scope declared by
; ``coroutine.inc``, which also imports them for use by the including code.

;;
;; .. module:: coroutine.o
;;

.scope Coroutine
    ;;
    ;; Symbols for simple coroutines. All symbols are exported by object module
    ;; ``coroutine.o``. In addition to space for implementation code in the
    ;; ``CODE`` segment, the module also reserves four bytes in the
    ;; ``ZEROPAGE`` segment: two bytes for the :label:`self` pointer, and two
    ;; additional bytes for internal, temporary storage (not exported).

    .globalzp self
        ;;
        ;; The "current" coroutine pointer, on which most procedures
        ;; operate, assumed to point to an element of a ``$ffff``-terminated
        ;; array of coroutine :struct:`State` structures.
        ;;
        ;; :size: 2 bytes.

    .macro Coroutine_select coroutine
        ;;
        ;; Sets :label:`self` to point to the given address.
        ;;
        ;; :param coroutine:    2-byte constant expression, address of the
        ;;                      desired coroutine's :struct:`State`.
        ;; :clobbers:           A
        ;;
        lda #<coroutine
        sta Coroutine::self
        lda #>coroutine
        sta Coroutine::self + 1
    .endmacro

    .struct State
        ;;
        ;; Each coroutine is associated with 8 bytes of state, including its
        ;; execution context as of its last yield, and two bytes of user data.

        PROG    .addr
            ;;
            ;; The coroutine's program counter. The next address that the
            ;; coroutine will execute *minus one*, i.e. ``rts``-compatible.
        STATUS  .byte
            ;;
            ;; The coroutine's status register.
        ACCUM    .byte
            ;;
            ;; The coroutine's accumulator.
        XREG    .byte
            ;;
            ;; The coroutine's X register.
        YREG    .byte
            ;;
            ;; The coroutine's Y register.

        DATA0    .byte
            ;;
            ;; A byte of user data, which can be used by coroutine code as a local
            ;; mutable, as a communication channel, etc.
        DATA1    .byte
            ;;
            ;; Another byte of user data.
    .endstruct

    .macro Coroutine_sta_state offset
        ;;
        ;; Convenience macro stores the accumulator in the given field of the
        ;; coroutine :struct:`State` structure pointed to by :label:`self`.
        ;;
        ;; :param offset:   Offset from :label:`self` at which to store the
        ;;                  accumulator; should be one of the fields of
        ;;                  :struct:`State`.
        ;; :clobbers:       Y
        ldy #offset
        sta (Coroutine::self), Y
    .endmacro
    
    .macro Coroutine_lda_state offset
        ;;
        ;; Convenience macro loads the accumulator with the value of the given
        ;; field of :struct:`self`.
        ;;
        ;; :param offset:   Offset from :label:`self` of the address to load;
        ;;                  should be one of the fields of :struct:`State`.
        ;; :clobbers:       Y
        ldy #offset
        lda (Coroutine::self), Y
    .endmacro
    
    .global new     ;; .. subroutine:: new
        ;; :segment: CODE
        ;;
        ;; Given a user-supplied, guard-terminated array of coroutine
        ;; :struct:`State` structures, finds an unused one and initializes
        ;; it to execute code at a user-supplied address. The new
        ;; coroutine's A, X, and Y registers and user data are initialized to
        ;; zero, and its status register to ``%00110000``.
        ;;
        ;; :callwith:   ``jsr``
        ;; :input self: Pointer to the head of the list. The list must be
        ;;              terminated by a guard value of ``$ffff`` following the
        ;;              final :struct:`State` struct (i.e., what would be the
        ;;              :field:`PROG <State::PROG>` of the next coroutine if
        ;;              there were one).  Structures in the list with ``$0000``
        ;;              as their :field:`PROG <State::PROG>` are considered
        ;;              "free" and available.
        ;; :input registers AY: The address of the coroutine code *minus one*.
        ;; :output self:        Points to the new coroutine, or ``$0000`` if no
        ;;                      free coroutines are available.
        ;; :output register A:  Nonzero if there are no free coroutines.
        ;; :clobbers:           A, X, Y, :label:`self`.

    .macro Coroutine_new code_addr, coroutines, on_failure
        ;;
        ;; Convenience macro to initialize a new coroutine in a given array of
        ;; :struct:`State` structures and check for success.
        ;;
        ;; :param code_addr:    Address of the coroutine code (the macro will
        ;;                      automatically subtract one as necessary).
        ;; :param coroutines:   (Optional) Adress of the head of the array; if
        ;;                      provided, the macro expansion will call
        ;;                      :macro:`Coroutine_select` on this address
        ;;                      before initializing the new coroutine.
        ;; :param on_failure:   (Optional) Destination to which to branch if
        ;;                      initialization fails because no free coroutines
        ;;                      are available in the array.
        ;; :output self:        Points to the new coroutine, or ``$0000`` (see
        ;;                      :subr:`new`).
        ;; :output register A:  Nonzero if there are no free coroutines.
        ;; :clobbers:           A, X, Y, :label:`self`.
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

    ;; .. segment:: CODE

    .global step    ;; .. subroutine:: step
        ;;
        ;; Enters or re-enters the coroutine code of :label:`self`, which
        ;; executes until the coroutine next calls :subr:`yield` or
        ;; :proc:`halt`, when control flow returns to the call-site of
        ;; :subr:`step`.  Generally meant to be called by a user of the
        ;; coroutine, rather than by the coroutine code itself (unless of
        ;; course the coroutine is stepping through *another* coroutine, in
        ;; which case it should take care with the :label:`self` pointer).
        ;;
        ;; Think of calling ``some_generator.next()`` in Python.
        ;; 
        ;; From the coroutine code's point of view, it will resume execution
        ;; with all registers in the same state as when it last called
        ;; :subr:`yield` (or zeroed on first execution), unless the user has
        ;; directly modified its :struct:`State` in the meantime.
        ;;
        ;; :callwith:           ``jsr``
        ;; :input self:         The coroutine to step.
        ;; :output register A:  A=1 if the coroutine has halted, 0 otherwise
        ;;                      (i.e., it yielded).
        ;; :clobbers:           A, X, Y, not to mention any side effects of the
        ;;                      coroutine code itself.

    .global step_all    ;; .. subroutine:: step_all
        ;;
        ;; Calls :subr:`step` once on each non-halted coroutine in the array
        ;; beginning at :label:`self`. Generally meant to be called by the user
        ;; of the coroutines, rather than by the coroutines themselves.
        ;;
        ;; :callwith:           ``jsr``
        ;; :input self:         Pointer to the beginning of the array of
        ;;                      coroutine :struct:`State` structures to step.
        ;;                      The array must be terminated by ``$ffff``.
        ;; :clobbers:           A, X, Y, not to mention any side effects of the
        ;;                      coroutines themselves.

    ; yield (subroutine): meant to be called by the coroutine code itself.
    ; Caller: jsr with self = coroutine; address at top of stack (before jsr)
    ; is where flow will return to, usually the callsite of the step-call that
    ; stepped the coroutine that is now calling yield. A will = 0 when flow
    ; returns. Think of the yield keyword in Python
    .global yield       ;; .. subroutine:: yield
        ;;
        ;; Foo.
        ;;
        ;; Think of executing the ``yield`` statement within the body of a
        ;; generator in Python.
        ;;
        ;; :callwith: ``jsr``

    ; halt (jmp point): meant to be jmpd to by the coroutine code itself.
    ; jmp with self = coroutine. like a yield, but without saving any state,
    ; and marks self as free for use by Coroutine::new. control flow passes to
    ; address at top of stack.  A will = 1 when flow returns.
    ; Think of raising StopIteration in Python. 
    .global halt        ;; .. procedure:: halt
        ;;
        ;; Foo.
        ;;
        ;; Think of returning from the body of a generator in Python.
        ;;
        ;; :callwith: ``jmp``

    ;; Two convenience macros are defined to wrap :subr:`yield` and
    ;; :proc:`halt` so that one doesn't have to remember which is for
    ;; ``jsr``-ing and which is for ``jmp``-ing.
    .macro Coroutine_yield
        ;;
        ;; Convenience macro for coroutine code. Evaluates to ``jsr
        ;; Coroutine::yield``.
        jsr Coroutine::yield
    .endmacro

    .macro Coroutine_halt
        ;;
        ;; Convenience macro for coroutine code. Evaluates to ``jmp
        ;; Coroutine::halt``.
        jmp Coroutine::halt
    .endmacro

    .global free    ;; .. subroutine:: free
        ;;
        ;; Frees the current :label:`self` coroutine by setting its program
        ;; counter, :field:`State::PROG`, to zero, so that the struct is
        ;; available for initialization of a new coroutine by :subr:`new`.
        ;; Generally meant to be called by a user of the coroutine, rather than
        ;; by the coroutine code itself.
        ;; 
        ;; Technically ``jsr Coroutine::free`` is equivalent to ``jsr
        ;; Coroutine::halt``, but note that :proc:`Coroutine::halt` is
        ;; normally ``jmp``-ed to from within coroutine code, rather than
        ;; ``jsr``-ed.
        ;;
        ;; :callwith:       ``jsr``
        ;; :input self:     The coroutine to free.
        ;; :clobbers:       A, Y.

    ; clear_state: clears the state (a, x, y, status, data0, data1, but not PC)
    ; of self.
    ; Caller: jsr with self = coroutine
    .global clear_state ;; .. subroutine:: clear_state
        ;;
        ;; Clears the state of coroutine :label:`self`,
        ;; including its accumulator :field:`ACCUM <State::ACCUM>`, registers
        ;; :field:`XREG <State::XREG>`, :field:`YREG <State::YREG>`, and
        ;; :field:`STATUS <State::STATUS>`, and data fields :field:`DATA0
        ;; <State::DATA0>`, :field:`DATA1 <State::DATA1>`; but *not* its
        ;; program counter :field:`PROG <State::PROG>`.
        ;;
        ;; When cleared, the accumulator, X and Y registers, and data fields
        ;; are set to zero, and the status register is set to ``%00110000``.
        ;;
        ;; :callwith:       ``jsr``
        ;; :input self:     The coroutine whose state to clear.
        ;; :clobbers:       A, Y.
.endscope


.endif ; Coroutine_included
