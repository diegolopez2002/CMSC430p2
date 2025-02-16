#lang racket
(provide (all-defined-out))
(require a86/ast)
(module+ test
  (require rackunit)
  (require a86/interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some problems using bitwise operations

;; Define a sequence of assembly instructions that zeroes out the least
;; significant 4 bits of the rax register.

(define zero-lower-4-rax
  (seq
    (Instr 'and 'rax #xFFFFFFFFFFFFFFF0))) ; Mask to clear the lower 4 bits

;; Define a sequence of assembly instructions that checks if the least
;; significant 4 bits of rax are equal to 5, and if so, sets rcx to 1,
;; otherwise sets rcx to 0.

(define check-lower-4-rax
  (seq
    (Push 'rax)                     ; Save rax on the stack
    (And 'rax #xF)                  ; Mask rax to get only the lower 4 bits
    (Mov 'rcx 0)                    ; Default rcx to 0
    (Cmp 'rax 5)                    ; Compare lower 4 bits with 5
    (Mov 'rax 1)                    ; Set rax to 1
    (Cmove 'rcx 'rax)               ; If equal, move rax to rcx (set rcx to 1)
    (Pop 'rax)))                    ; Restore rax

(module+ test
  ;; Int64 -> Int64
  (define (t1 n)
    (asm-interp
     (prog (Global 'entry)
           (Label 'entry)
           (Mov 'rax n)
           zero-lower-4-rax
           (Ret))))

  (check-equal? (t1 0) 0)
  (check-equal? (t1 5) 0)
  (check-equal? (t1 15) 0)
  (check-equal? (t1 16) 16)
  (check-equal? (t1 (sub1 (expt 2 32))) (- (sub1 (expt 2 32)) 15))
  (check-equal? (t1 (expt 2 32)) (expt 2 32))
  (check-equal? (t1 (sub1 (expt 2 64))) -16))

  (define (t2 n)
    (zero?
     (asm-interp
      (prog (Global 'entry)
            (Label 'entry)
            (Mov 'rax n)
            check-lower-4-rax
            (Cmp 'rax n)
            (Mov 'rax 0)
            (Mov 'rdx 1)
            (Cmovne 'rax 'rdx)
            (Cmp 'rcx (if (= 5 (bitwise-and n #b1111)) 1 0))
            (Cmovne 'rax 'rdx)
            (Ret)))))

  (check-true (t2 0))
  (check-true (t2 5))
  (check-true (t2 15))
  (check-true (t2 16)))

