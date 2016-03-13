.ifndef Random_included
Random_included = 1 ;;

.scope Random
    .globalzp crc_lo
    .globalzp crc_hi

    ;; .. segment:: CODE

    .macro Random_seed_crc16 val16
        .if (.match (.left (1, {val16}), #))
            lda #<(.right (.tcount ({val16})-1, {val16}))
        .else
            lda val16 + 0
        .endif
        sta Random::crc_lo

        .if (.match (.left (1, {val16}), #))
            lda #>(.right (.tcount ({val16})-1, {val16}))
        .else
            lda val16 + 1
        .endif
        sta Random::crc_hi
    .endmacro

    .global random_crc16    ;; .. subroutine:: random_crc16
        ;;
        ;; Places the next two random bytes in A and X. Implemented via
        ;; :subr:`update_crc16` by updating the CRC-16 for one new ``$00`` byte
        ;; of "data." The new CRC-16 is the next 16-bit pseudorandom value.
        ;;
        ;; :callwith:           ``jsr``
        ;; :output register A:  "High" byte of the next random 16-bit value.
        ;;                      Also available as :label:`crc_hi`.
        ;; :output register X:  "Low" byte of the next random 16-bit value.
        ;;                      Also available as :label:`crc_lo`.
        ;; :clobbers:           A, X.

    .global update_crc16

.endscope

.endif ; Random_included
