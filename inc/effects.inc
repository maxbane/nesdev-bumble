.ifndef Effects_included
Effects_included = 1

.include "coroutine.inc"

.scope Effects

    ; effects (addr):
    ; Guard-terminated list of effects in RAM, initialized by Effects::init for
    ; Constants::N_EFFECTS
    .global effects

    ; init (subroutine):
    ; Effect system initialization. Clears out list of effects, freeing them,
    ; and ensures guard at end of list.
    ; Clobbers:
    ;   - registers
    ;   - addr_0
    .global  init

    ; strobe_actor_palette (subroutine):
    ; Create new effect: cycle a given actor's palette index through the
    ; available sprite palettes, a given number of times, at a given framerate.
    ; Effect finishes with actor having palette 0.
    ; Calling convention:
    ;   X = Actor index
    ;   A = Number of cycles
    ;   Y = Frames per palette (i.e., Y*4 = frames per cycle)
    ; Returns with:
    ;   A = 0 if successful, 1 if failed
    ;   X = Actor index
    ;   Y = ?
    ;   Coroutine::self = addr of coroutine or $0000 if failed
    .global strobe_actor_palette

    .global thrust_actor_jet

.endscope

.endif ; Effects_included
