#lang racket

(require "helper-functions.rkt"
         "interpreter.rkt"
         "mix.rkt")

(define tm-program1
  '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

(define mix-generated-compiler
  '((read m0-vs0)
    ("mix0-init1-0" (:= m0-pending (list (list "init1" m0-vs0))) (:= m0-marked '()) (:= residual '((read Right))) (if (not (eq? m0-pending '())) "mix0-pending-loop-2" "mix0-pending-loop-end-1"))
    ("mix0-pending-loop-end-1" (return residual))
    ("mix0-pending-loop-2"
     (:= pp (caar m0-pending))
     (:= m0-vs (cadar m0-pending))
     (:= m0-pending (cdr m0-pending))
     (:= m0-marked (set-add m0-marked (list pp m0-vs)))
     (:=
      bb
      (lookup
       pp
       '((read program Right)
         ("init1" (:= program-tail program) (:= Left '()) (goto "loop"))
         ("loop" (if (eq? program-tail '()) "stop" "case0"))
         ("case0" (:= instr (cdar program-tail)) (:= program-tail (cdr program-tail)) (:= operator (car instr)) (if (eq? operator 'if) "interpret-if" "case1"))
         ("case1" (if (eq? operator 'goto) "interpret-goto" "case2"))
         ("case2" (if (eq? operator 'write) "interpret-write" "case3"))
         ("case3" (if (eq? operator 'right) "interpret-right" "case4"))
         ("case4" (if (eq? operator 'left) "interpret-left" "error"))
         ("interpret-if" (:= symbol (cadr instr)) (:= instr (cddr instr)) (if (eq? (first-symbol Right) symbol) "interpret-goto" "loop"))
         ("interpret-goto" (:= program-tail (list-tail program (list-ref instr 1))) (goto "loop"))
         ("interpret-write" (:= symbol (cadr instr)) (:= Right (cons symbol (move-tape Right))) (goto "loop"))
         ("interpret-left" (:= Right (cons (first-symbol Left) Right)) (:= Left (move-tape Left)) (goto "loop"))
         ("interpret-right" (:= Left (cons (first-symbol Right) Left)) (:= Right (move-tape Right)) (goto "loop"))
         ("error" (return (list 'syntax-error instr)))
         ("stop" (return (append (reverse Left) Right))))))
     (:= m0-code (initial-code pp m0-vs))
     (:= m0-command (car bb))
     (:= bb (cdr bb))
     (if (is-assignment m0-command) "mix0-case-assignment-16" "mix0-case0-3"))
    ("mix0-case0-3" (if (is-goto m0-command) "mix0-case-goto-15" "mix0-case1-4"))
    ("mix0-case1-4" (if (is-if m0-command) "mix0-case-if-10" "mix0-case2-5"))
    ("mix0-case2-5" (if (is-return m0-command) "mix0-case-return-7" "mix0-error-6"))
    ("mix0-error-6" (return 'runtime-error))
    ("mix0-case-return-7" (:= m0-exp (cadr m0-command)) (:= m0-code (extend m0-code `(return ,(quote-list (reduce m0-exp m0-vs))))) (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-bb-loop-end-8" (:= residual (extend residual m0-code)) (if (not (eq? m0-pending '())) "mix0-pending-loop-2" "mix0-pending-loop-end-1"))
    ("mix0-bb-loop-9" (:= m0-command (car bb)) (:= bb (cdr bb)) (if (is-assignment m0-command) "mix0-case-assignment-16" "mix0-case0-3"))
    ("mix0-case-if-10"
     (:= m0-exp (cadr m0-command))
     (:= m0-pp1 (caddr m0-command))
     (:= m0-pp2 (cadddr m0-command))
     (if (is-static-exp? '((operator program program-tail instr symbol) (Right Left)) m0-exp) "mix0-cond-static-12" "mix0-cond-dynamic-11"))
    ("mix0-cond-dynamic-11"
     (:= m0-pending (if (set-member? m0-marked (list m0-pp1 m0-vs)) m0-pending (set-add m0-pending (list m0-pp1 m0-vs))))
     (:= m0-pending (if (set-member? m0-marked (list m0-pp2 m0-vs)) m0-pending (set-add m0-pending (list m0-pp2 m0-vs))))
     (:= m0-code (extend m0-code `(if ,(reduce m0-exp m0-vs) ',(list m0-pp1 m0-vs) ',(list m0-pp2 m0-vs))))
     (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-cond-static-12" (if (eval-expr m0-vs m0-exp) "mix0-go-pp1-14" "mix0-go-pp2-13"))
    ("mix0-go-pp2-13"
     (:=
      bb
      (lookup
       m0-pp2
       '((read program Right)
         ("init1" (:= program-tail program) (:= Left '()) (goto "loop"))
         ("loop" (if (eq? program-tail '()) "stop" "case0"))
         ("case0" (:= instr (cdar program-tail)) (:= program-tail (cdr program-tail)) (:= operator (car instr)) (if (eq? operator 'if) "interpret-if" "case1"))
         ("case1" (if (eq? operator 'goto) "interpret-goto" "case2"))
         ("case2" (if (eq? operator 'write) "interpret-write" "case3"))
         ("case3" (if (eq? operator 'right) "interpret-right" "case4"))
         ("case4" (if (eq? operator 'left) "interpret-left" "error"))
         ("interpret-if" (:= symbol (cadr instr)) (:= instr (cddr instr)) (if (eq? (first-symbol Right) symbol) "interpret-goto" "loop"))
         ("interpret-goto" (:= program-tail (list-tail program (list-ref instr 1))) (goto "loop"))
         ("interpret-write" (:= symbol (cadr instr)) (:= Right (cons symbol (move-tape Right))) (goto "loop"))
         ("interpret-left" (:= Right (cons (first-symbol Left) Right)) (:= Left (move-tape Left)) (goto "loop"))
         ("interpret-right" (:= Left (cons (first-symbol Right) Left)) (:= Right (move-tape Right)) (goto "loop"))
         ("error" (return (list 'syntax-error instr)))
         ("stop" (return (append (reverse Left) Right))))))
     (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-go-pp1-14"
     (:=
      bb
      (lookup
       m0-pp1
       '((read program Right)
         ("init1" (:= program-tail program) (:= Left '()) (goto "loop"))
         ("loop" (if (eq? program-tail '()) "stop" "case0"))
         ("case0" (:= instr (cdar program-tail)) (:= program-tail (cdr program-tail)) (:= operator (car instr)) (if (eq? operator 'if) "interpret-if" "case1"))
         ("case1" (if (eq? operator 'goto) "interpret-goto" "case2"))
         ("case2" (if (eq? operator 'write) "interpret-write" "case3"))
         ("case3" (if (eq? operator 'right) "interpret-right" "case4"))
         ("case4" (if (eq? operator 'left) "interpret-left" "error"))
         ("interpret-if" (:= symbol (cadr instr)) (:= instr (cddr instr)) (if (eq? (first-symbol Right) symbol) "interpret-goto" "loop"))
         ("interpret-goto" (:= program-tail (list-tail program (list-ref instr 1))) (goto "loop"))
         ("interpret-write" (:= symbol (cadr instr)) (:= Right (cons symbol (move-tape Right))) (goto "loop"))
         ("interpret-left" (:= Right (cons (first-symbol Left) Right)) (:= Left (move-tape Left)) (goto "loop"))
         ("interpret-right" (:= Left (cons (first-symbol Right) Left)) (:= Right (move-tape Right)) (goto "loop"))
         ("error" (return (list 'syntax-error instr)))
         ("stop" (return (append (reverse Left) Right))))))
     (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-case-goto-15"
     (:= m0-pp1 (cadr m0-command))
     (:=
      bb
      (lookup
       m0-pp1
       '((read program Right)
         ("init1" (:= program-tail program) (:= Left '()) (goto "loop"))
         ("loop" (if (eq? program-tail '()) "stop" "case0"))
         ("case0" (:= instr (cdar program-tail)) (:= program-tail (cdr program-tail)) (:= operator (car instr)) (if (eq? operator 'if) "interpret-if" "case1"))
         ("case1" (if (eq? operator 'goto) "interpret-goto" "case2"))
         ("case2" (if (eq? operator 'write) "interpret-write" "case3"))
         ("case3" (if (eq? operator 'right) "interpret-right" "case4"))
         ("case4" (if (eq? operator 'left) "interpret-left" "error"))
         ("interpret-if" (:= symbol (cadr instr)) (:= instr (cddr instr)) (if (eq? (first-symbol Right) symbol) "interpret-goto" "loop"))
         ("interpret-goto" (:= program-tail (list-tail program (list-ref instr 1))) (goto "loop"))
         ("interpret-write" (:= symbol (cadr instr)) (:= Right (cons symbol (move-tape Right))) (goto "loop"))
         ("interpret-left" (:= Right (cons (first-symbol Left) Right)) (:= Left (move-tape Left)) (goto "loop"))
         ("interpret-right" (:= Left (cons (first-symbol Right) Left)) (:= Right (move-tape Right)) (goto "loop"))
         ("error" (return (list 'syntax-error instr)))
         ("stop" (return (append (reverse Left) Right))))))
     (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-case-assignment-16" (:= X (cadr m0-command)) (:= m0-exp (caddr m0-command)) (if (is-static? '((operator program program-tail instr symbol) (Right Left)) X) "mix0-X-static-18" "mix0-X-dynamic-17"))
    ("mix0-X-dynamic-17" (:= m0-code (extend m0-code `(:= ,X ,(quote-list (reduce m0-exp m0-vs))))) (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))
    ("mix0-X-static-18" (:= m0-vs (set-state m0-vs X (eval-expr m0-vs m0-exp))) (if (not (eq? bb '())) "mix0-bb-loop-9" "mix0-bb-loop-end-8"))))

(define (compile-program1)
  (change-labels (interpret-fl (change-labels proj2) (list (init-state '[program] [list tm-prog])))))

(define compiled-program1
  '((read Right)
    ("init1-0" (:= Left '()) (if (eq? (first-symbol Right) 0) "interpret-goto-2" "loop-1"))
    ("loop-1" (:= Left (cons (first-symbol Right) Left)) (:= Right (move-tape Right)) (if (eq? (first-symbol Right) 0) "interpret-goto-2" "loop-1"))
    ("interpret-goto-2" (:= Right (cons 1 (move-tape Right))) (return (append (reverse Left) Right)))))