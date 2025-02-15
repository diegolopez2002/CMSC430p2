#lang racket
(provide (all-defined-out))
(require a86/ast)
(module+ test
  (require rackunit)
  (require a86/interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some problems using the stack


;; Define a sequence of assembly instructions that pops the first four
;; elements of the stack and leaves their sum in rax.

;; You may assume the stack has at least four elements and that the
;; sum doesn't overflow.

;; The sequence should leave the stack with four fewer elements than
;; it started with and all callee-saved registers in the same state it
;; started in.

(define pop-sum-4
  ;; TODO
  (seq))

(module+ test
  ;; Int64 Int64 Int64 Int64 -> Int64
  (define (t1 n1 n2 n3 n4)
    (asm-interp
     (prog (Global 'entry)
           (Label 'entry)
           (Mov 'rax n1)
           (Push 'rax)
           (Mov 'rax n2)
           (Push 'rax)
           (Mov 'rax n3)
           (Push 'rax)
           (Mov 'rax n4)
           (Push 'rax)
           pop-sum-4
           (Ret))))

  (check-equal? (t1 0 0 0 0) 0)
  (check-equal? (t1 1 2 3 4) 10)
  (check-equal? (t1 4 3 2 1) 10)
  (check-equal? (t1 -1 2 3 4) 8))


;; Define a sequence of assembly instructions that sums the first four
;; elements of the stack, leaving their sum in rax, but also leaving the
;; stack as it was.

;; You may assume the stack has at least four elements and that the
;; sum doesn't overflow.

;; The sequence should leave the stack and all callee-saved registers
;; in the same state it started in.

(define stack-sum-4
  ;; TODO
  (seq))

(module+ test
  ;; Int64 Int64 Int64 Int64 -> Boolean
  (define (t2 n1 n2 n3 n4)
    (zero?
     (asm-interp
      (prog (Global 'entry)
            (Label 'entry)
            (Mov 'rax n1)
            (Push 'rax)
            (Mov 'rax n2)
            (Push 'rax)
            (Mov 'rax n3)
            (Push 'rax)
            (Mov 'rax n4)
            (Push 'rax)
            stack-sum-4
            (Mov 'rcx 1)
            (Cmp 'rax (+ n1 n2 n3 n4))
            (Mov 'rax 0)
            (Cmovne 'rax 'rcx)
            (Pop 'r8)
            (Cmp 'r8 n4)
            (Cmovne 'rax 'rcx)
            (Pop 'r8)
            (Cmp 'r8 n3)
            (Cmovne 'rax 'rcx)
            (Pop 'r8)
            (Cmp 'r8 n2)
            (Cmovne 'rax 'rcx)
            (Pop 'r8)
            (Cmp 'r8 n1)
            (Cmovne 'rax 'rcx)
            (Ret)))))

  (check-true (t2 0 0 0 0))
  (check-true (t2 1 2 3 4))
  (check-true (t2 4 3 2 1))
  (check-true (t2 -1 2 3 4)))


;; Define a sequence of assembly instructions that is given a natural
;; number in rax and pops that many elements of the stack and leaves
;; their sum in rax.

;; You may assume the stack has at least rax elements and that the
;; sum doesn't overflow.

;; The sequence should leave the stack with rax fewer elements than it
;; started with and all callee-saved registers in the same state it
;; started in.

(define pop-sum-rax
  ;; TODO
  (seq))

(module+ test
  ;; [Listof Int64] -> Int64
  (define (t3 ns)
    (define (push-ns ns)
      (match ns
        ['() (seq)]
        [(cons n ns)
         (seq (Mov 'rax n)
              (Push 'rax)
              (push-ns ns))]))

    (asm-interp
     (prog (Global 'entry)
           (Label 'entry)
           (push-ns ns)
           (Mov 'rax (length ns))
           pop-sum-rax
           (Ret))))

  (check-equal? (t3 '()) 0)
  (check-equal? (t3 '(8)) 8)
  (check-equal? (t3 '(0 0 0 0)) 0)
  (check-equal? (t3 '(1 2 3 4)) 10)
  (check-equal? (t3 '(4 3 2 1)) 10)
  (check-equal? (t3 '(-1 2 3 4)) 8)
  (check-equal? (t3 (build-list 36 add1)) 666))
