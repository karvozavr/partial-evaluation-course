#lang racket

(require racket/set
         racket/dict
         "interpreter.rkt"
         "tm-interpreter.rkt"
         "helper-functions.rkt")

(provide main)

(define mix-trick
  '([read m1-program m1-division m1-vs0]
    ["mix1-init1" (:= m1-pending (list (list (caadr m1-program) m1-vs0)))
                  (:= m1-marked '())
                  (:= m1-residual (list (generate-read-block (car m1-program) m1-division)))
                  (goto "mix1-while-pending")]
    ; ------------------
    ; The Trick


    ["mix1-the-trick" (:= m1-program-tail m1-program)
                      (goto "mix1-the-trick-loop")]

    ["mix1-the-trick-loop" (if (empty? m1-program-tail) "mix1-error" "mix1-the-trick-loop-cond")]
    ["mix1-the-trick-loop-cond" (if (equal? (caar m1-program-tail) m1-pp) "mix1-the-trick-found" "mix1-the-trick-continue")]
    ["mix1-the-trick-found" (:= m1-bb (cdar m1-program-tail))
                            (goto "mix1-while-bb")]
    ["mix1-the-trick-continue" (:= m1-program-tail (cdr m1-program-tail))
                               (goto "mix1-the-trick-loop")]

    ; End of The Trick
    ; ------------------
    
    ["mix1-while-pending" (if (not (empty? m1-pending)) "mix1-pending-loop" "mix1-pending-loop-end")]
    ["mix1-pending-loop" (:= m1-pp (caar m1-pending))
                         (:= m1-vs (cadar m1-pending))
                         (:= m1-pending (cdr m1-pending))
                         (:= m1-marked (set-add m1-marked (list m1-pp m1-vs)))
                         (:= m1-code (initial-code m1-pp m1-vs))
                         (goto "mix1-the-trick")] 

    ["mix1-while-bb" (if (not (eq? m1-bb '())) "mix1-bb-loop" "mix1-bb-loop-end")]
    ["mix1-bb-loop" (:= m1-command (car m1-bb))
                    (:= m1-bb (cdr m1-bb))
                    (goto "mix1-case")]

    ["mix1-case" (if (is-assignment m1-command) "mix1-case-assignment" "mix1-case0")]
    ["mix1-case0" (if (is-goto m1-command) "mix1-case-goto" "mix1-case1")]
    ["mix1-case1" (if (is-if m1-command) "mix1-case-if" "mix1-case2")]
    ["mix1-case2" (if (is-return m1-command) "mix1-case-return" "mix1-error")]

    ["mix1-case-assignment" (:= X (cadr m1-command))
                            (:= m1-exp (caddr m1-command))
                            (if (is-static? m1-division X) "mix1-X-static" "mix1-X-dynamic")]
    ["mix1-X-static" (:= m1-vs (set-state m1-vs X (eval-expr m1-vs m1-exp)))
                     (goto "mix1-while-bb")]
    ["mix1-X-dynamic" (:= m1-code (extend m1-code `(:= ,X ,(quote-list (reduce m1-exp m1-vs)))))
                      (goto "mix1-while-bb")]

    ["mix1-case-goto" (:= m1-pp1 (cadr m1-command))
                      (:= m1-pp m1-pp1)
                      (goto "mix1-the-trick")]

    ["mix1-case-if" (:= m1-exp (cadr m1-command))
                    (:= m1-pp1 (caddr m1-command))
                    (:= m1-pp2 (cadddr m1-command))
                    (if (is-static-exp? m1-division m1-exp) "mix1-cond-static" "mix1-cond-dynamic")]
    ["mix1-cond-static" (if (eval-expr m1-vs m1-exp) "mix1-go-pp1" "mix1-go-pp2")]
    ["mix1-go-pp1" (:= m1-pp m1-pp1) 
                   (goto "mix1-the-trick")]
    ["mix1-go-pp2" (:= m1-pp m1-pp2)
                   (goto "mix1-the-trick")]
    ["mix1-cond-dynamic" (:= m1-pending (update-pending m1-pending m1-marked m1-pp1 m1-pp2 m1-vs))
                         (:= m1-code (extend m1-code `(if ,(reduce m1-exp m1-vs) ',(list m1-pp1 m1-vs) ',(list m1-pp2 m1-vs))))
                         (goto "mix1-while-bb")]
    
    ["mix1-case-return" (:= m1-exp (cadr m1-command))
                        (:= m1-code (extend m1-code `(return ,(quote-list (reduce m1-exp m1-vs)))))
                        (goto "mix1-while-bb")]
    
    ["mix1-bb-loop-end" (:= m1-residual (extend m1-residual m1-code))
                        (goto "mix1-while-pending")]

    ["mix1-pending-loop-end" (return m1-residual)]
    
    ["mix1-error" (return 'runtime-error)]
    ))

; 1-st Fatamura projection
(define proj1
  (interpret-fl
   mix-trick
   (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)) (init-state (list 'program) (list tm-prog)))))


; 2-nd Fatamura projection
(define proj2
  (interpret-fl
   mix-trick
   (list mix-trick
         (list '(m1-program m1-division m1-program-tail m1-bb m1-pp1 m1-pp2 m1-command X m1-exp) '(m1-pp m1-vs m1-pending m1-marked m1-vs0 m1-code m1-residual))
         (init-state (list 'm1-program 'm1-division) (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)))))))
        
(define (main)
  (begin
    (printf "1-st projection:\n\n")
    (fl-pretty-print (change-labels proj1))
    
    (printf "Compiler from TM to FL:\n\n")
    (fl-pretty-print (change-labels proj2))
                     ))

