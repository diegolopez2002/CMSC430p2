#lang racket
(provide (all-defined-out))
(require a86/ast)
(module+ test
  (require rackunit)
  (require a86/interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions in assembly


;; Define an assembly function named mult3 that multiplies n by 3

(define mult3
  (seq
   (Label 'mult3)
   (Mov 'rcx 3)          ; Load 3 into rcx
   (Imul 'rax 'rcx)      ; Multiply rax by rcx (n * 3)
   (Ret)))               ; Return


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


;; Define an assembly function named fib that computes the nth Fibonacci number

(define fib
  (seq
   (Label 'fib)
   (Cmp 'rax 0)                  ; Compare rax with 0
   (Je 'fib_base0)               ; If rax == 0, jump to fib_base0
   (Cmp 'rax 1)                  ; Compare rax with 1
   (Je 'fib_base1)               ; If rax == 1, jump to fib_base1

   (Push 'rax)                   ; Save n on the stack
   (Push 'rbx)                   ; Save rbx on the stack

   (Dec 'rax)                    ; rax = n - 1
   (Call 'fib)                   ; fib(n-1)
   (Mov 'rbx 'rax)               ; Store fib(n-1) in rbx

   (Pop 'rbx)                    ; Restore rbx from stack
   (Push 'rax)                   ; Save fib(n-1) result on stack

   (Dec 'rax)                    ; rax = n - 2
   (Call 'fib)                   ; fib(n-2)

   (Pop 'rbx)                    ; Retrieve fib(n-1)
   (Add 'rax 'rbx)               ; fib(n-1) + fib(n-2)

   (Pop 'rbx)                    ; Restore original rbx
   (Ret)                         ; Return

   (Label 'fib_base0)            ; Base case fib(0) = 0
   (Mov 'rax 0)
   (Ret)

   (Label 'fib_base1)            ; Base case fib(1) = 1
   (Mov 'rax 1)
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
  (check-equal? (f 19) 4181)))

