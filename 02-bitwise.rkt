#lang racket

(provide (all-defined-out))
(require rackunit)

;; Simulate the registers as a hash table
(define registers (make-hash))

;; Helper functions for operations
(define (mov reg value)
  (hash-set! registers reg value))

(define (and reg value)
  (hash-set! registers reg (bitwise-and (hash-ref registers reg) value)))

(define (cmp reg value)
  (hash-set! registers 'cmp-result (if (= (hash-ref registers reg) value) 1 0)))

(define (cmove reg1 reg2)
  (when (= (hash-ref registers 'cmp-result) 1)
    (hash-set! registers reg1 (hash-ref registers reg2))))

(define (push reg) (displayln (string-append "Pushed " (symbol->string reg))))
(define (pop reg) (displayln (string-append "Popped " (symbol->string reg))))

;; seq to execute a series of operations
(define-syntax seq
  (syntax-rules ()
    ((_ . forms) (begin . forms))))

;; Test for zero-lowering 4 bits of rax
(define zero-lower-4-rax
  (seq
    (and 'rax #xFFFFFFFFFFFFFFF0)))

;; Test for checking and manipulating the lower 4 bits of rax
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
    (begin
      (mov 'rax n)
      zero-lower-4-rax
      (hash-ref registers 'rax)))

  (check-equal? (t1 0) 0)
  (check-equal? (t1 5) 0)
  (check-equal? (t1 15) 0)
  (check-equal? (t1 16) 16)

  (define (t2 n)
    (let ((result (begin
                   (mov 'rax n)
                   check-lower-4-rax
                   (cmp 'rcx (if (= 5 (bitwise-and n #xF)) 1 0))
                   (mov 'rax 0)
                   (mov 'rdx 1)
                   (cmove 'rax 'rdx))))
      (= (hash-ref registers 'rax) 1)))

  (check-true (t2 0))
  (check-true (t2 5))
  (check-true (t2 15))
  (check-true (t2 16)))
