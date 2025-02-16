#lang racket
(provide (all-defined-out))
(require a86)
(module+ test
  (require rackunit))

(define zero-lower-4-rax
  (seq
    (And 'rax #xFFFFFFFFFFFFFFF0)))

(define check-lower-4-rax
  (seq
    (Push 'rax)
    (And 'rax #xF)
    (Mov 'rcx 0)
    (Cmp 'rax 5)
    (Mov 'rax 1)
    (Cmove 'rcx 'rax)
    (Pop 'rax)))

(module+ test
  (define (t1 n)
    (asm-interp  ; Use asm-interp provided by a86
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
  (check-true (t2 16))

  (check-true (t2 0))
  (check-true (t2 5))
  (check-true (t2 15))
  (check-true (t2 16))
