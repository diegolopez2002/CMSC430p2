#lang racket
(provide (all-defined-out))
(require a86/ast)
(require a86/interp)
(module+ test
  (require rackunit))

(define zero-lower-4-rax
  (seq
    (and 'rax #xFFFFFFFFFFFFFFF0)))

(define check-lower-4-rax
  (seq
    (push 'rax)
    (and 'rax #xF)
    (mov 'rcx 0)
    (cmp 'rax 5)
    (mov 'rax 1)
    (cmove 'rcx 'rax)
    (pop 'rax)))

(module+ test
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

  (define (t2 n)
    (zero?
     (asm-interp
      (prog (Global 'entry)
            (Label 'entry)
            (Mov 'rax n)
            check-lower-4-rax
            (Cmp 'rcx (if (= 5 (bitwise-and n #xF)) 1 0))
            (Mov 'rax 0)
            (Mov 'rdx 1)
            (Cmovne 'rax 'rdx)
            (Ret)))))
  (check-true (t2 0))
  (check-true (t2 5))
  (check-true (t2 15))
  (check-true (t2 16)))

