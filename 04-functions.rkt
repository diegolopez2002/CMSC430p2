#lang racket
(provide (all-defined-out))
(require a86/ast)
(module+ test
  (require rackunit)
  (require a86/interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions in assembly


;; Define a an assembly function named mult3, i.e. a sequence that
;; starts with the label mult3 and and ends with a return that leaves
;; the stack and all callee-saved registers in the same state it
;; started in, that is given a number n in rax and returns
;; with with n*3 in rax.

;; You may assume the result doesn't overflow.

(define mult3
  (seq
   (Label 'mult3)
   (Mov 'rbx 'rax)    ;; Copy the value of rax to rbx
   (Add 'rax 'rbx)    ;; rax = rax + rbx (this gives 2n)
   (Add 'rax 'rbx)    ;; rax = rax + rbx (this gives 3n)
   (Ret))) 

(module+ test
  ;; Int64 -> Int64
  (define (m3 n)
    (asm-interp
     (prog (Global 'entry)
           (Label 'entry)
           (Mov 'rax n)
           (Call 'mult3)
           (Ret)
           mult3)))

  (check-equal? (m3 0) 0)
  (check-equal? (m3 1) 3)
  (check-equal? (m3 2) 6)
  (check-equal? (m3 3) 9)
  (check-equal? (m3 4) 12)
  (check-equal? (m3 5) 15)
  (check-equal? (m3 -5) -15)
  (check-equal? (m3 17) (* 17 3))
  (check-equal? (m3 19) (* 19 3)))


;; Define a an assembly function named fib, i.e. a sequence that
;; starts with the label fib and and ends with a return that leaves
;; the stack and all callee-saved registers in the same state it
;; started in, that is given a natural number n in rax and returns
;; with the nth fibonacci number in rax.

;; You may assume the result doesn't overflow.

;; The computation does not need to be efficient.

(define fib
  (seq
   (Label 'fib)
   (Cmp 'rax 0)          ;; Compare n with 0
   (Je 'fib_zero)        ;; If n == 0, jump to fib_zero
   (Cmp 'rax 1)          ;; Compare n with 1
   (Je 'fib_one)         ;; If n == 1, jump to fib_one

   ;; For n >= 2, calculate Fibonacci using two registers
   (Mov 'rbx 0)          ;; Initialize F(0)
   (Mov 'rcx 1)          ;; Initialize F(1)
   (Mov 'rdx 'rax)       ;; Move n to rdx (counter)

   (Label 'fib_loop)
   (Sub 'rdx 1)          ;; Decrement counter
   (Jz 'fib_done)        ;; If counter is 0, we're done
   (Add 'rbx 'rcx)       ;; rbx = F(n-2) + F(n-1)
   (Mov 'rcx 'rbx)       ;; rcx = F(n-2)
   (Mov 'rbx 'rdx)       ;; Move current n to rbx for next iteration
   (Jmp 'fib_loop)       ;; Continue the loop

   (Label 'fib_zero)     ;; Base case: F(0) = 0
   (Xor 'rax 'rax)       ;; Clear rax (set to 0)
   (Ret)

   (Label 'fib_one)      ;; Base case: F(1) = 1
   (Mov 'rax 1)          ;; Set rax to 1
   (Ret)

   (Label 'fib_done)     ;; End of loop
   (Mov 'rax 'rbx)       ;; Move result to rax
   (Ret)))

(module+ test
  ;; Int64 -> Int64
  (define (f n)
    (asm-interp
     (prog (Global 'entry)
           (Label 'entry)
           (Mov 'rax n)
           (Call 'fib)
           (Ret)
           fib)))

  (check-equal? (f 0) 0)
  (check-equal? (f 1) 1)
  (check-equal? (f 2) 1)
  (check-equal? (f 3) 2)
  (check-equal? (f 4) 3)
  (check-equal? (f 5) 5)
  (check-equal? (f 17) 1597)
  (check-equal? (f 19) 4181))
