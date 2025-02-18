#lang racket

(provide (all-defined-out))
(require rackunit)
(require a86/ast)
(module+ test
  (require rackunit)
  (require a86/interp))

;; Define Instr as a structure with operation (op), argument1 (arg1), and argument2 (arg2)
(define-struct instr (op arg1 arg2))

;; Define a helper function to create an Instr
(define (create-instr op arg1 arg2)
  (make-instr op arg1 arg2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a sequence of assembly instructions that pops the first four
;; elements of the stack and leaves their sum in rax.

(define pop-sum-4
  (list
    (create-instr 'mov 'rcx 'rax)       ;; Copy the count from rax to rcx
    (create-instr 'xor 'rax 'rax)       ;; Zero out rax to accumulate the sum
    (create-instr 'Label 'loop-start #f) ;; Label for the loop start
    (create-instr 'pop 'rdx 'noarg)      ;; Pop one element into rdx (using 'noarg' or similar)
    (create-instr 'add 'rax 'rdx)        ;; Add the value in rdx to rax
    (create-instr 'sub 'rcx 1)           ;; Decrement the counter in rcx
    (create-instr 'jnz 'loop-start 'noarg))) ;; Jump to loop-start if rcx != 0

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

(define stack-sum-4
  (list
    (create-instr 'pop 'rax 'noarg)          ;; Pop the first element into rax
    (create-instr 'pop 'rcx 'noarg)          ;; Pop the second element into rcx
    (create-instr 'add 'rax 'rcx)            ;; Add rcx to rax
    (create-instr 'pop 'rcx 'noarg)          ;; Pop the third element into rcx
    (create-instr 'add 'rax 'rcx)            ;; Add rcx to rax
    (create-instr 'pop 'rcx 'noarg)          ;; Pop the fourth element into rcx
    (create-instr 'add 'rax 'rcx)            ;; Add rcx to rax
    (create-instr 'push 'rcx 'noarg)         ;; Push fourth element back
    (create-instr 'push 'rcx 'noarg)         ;; Push third element back
    (create-instr 'push 'rcx 'noarg)         ;; Push second element back
    (create-instr 'push 'rcx 'noarg)))       ;; Push first element back


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

(define sum-stack-elements
  (list
    (create-instr 'mov 'rcx 'rax)       ;; Copy the count from rax to rcx
    (create-instr 'xor 'rax 'rax)       ;; Zero out rax to accumulate the sum
    (create-instr 'Label 'loop-start 'noarg) ;; Label for the loop start
    (create-instr 'pop 'rdx 'noarg)      ;; Pop one element into rdx
    (create-instr 'add 'rax 'rdx)        ;; Add the value in rdx to rax
    (create-instr 'sub 'rcx 1)           ;; Decrement the counter in rcx
    (create-instr 'jnz 'loop-start 'noarg))) ;; Jump to loop-start if rcx != 0

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
           sum-stack-elements ;; Changed from pop-sum-rax to sum-stack-elements
           (Ret))))

  (check-equal? (t3 '()) 0)
  (check-equal? (t3 '(8)) 8)
  (check-equal? (t3 '(0 0 0 0)) 0)
  (check-equal? (t3 '(1 2 3 4)) 10)
  (check-equal? (t3 '(4 3 2 1)) 10)
  (check-equal? (t3 '(-1 2 3 4)) 8)
  (check-equal? (t3 (build-list 36 add1)) 666))  ;; Test with a larger list
