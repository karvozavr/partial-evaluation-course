#lang racket

(require racket/set
         racket/dict
         "interpreter.rkt"
         "tm-interpreter.rkt"
         "helper-functions.rkt")

(provide main)

(define mix
  '([read m0-program m0-division m0-vs0]
    ["mix0-init1" (:= m0-pending (list (list (caadr m0-program) m0-vs0)))
                  (:= m0-marked '())
                  (:= residual (list (generate-read-block (car m0-program) m0-division)))
                  (goto "mix0-while-pending")]
    
    ["mix0-while-pending" (if (not (eq? m0-pending '())) "mix0-pending-loop" "mix0-pending-loop-end")]
    ["mix0-pending-loop" (:= pp (caar m0-pending))
                         (:= m0-vs (cadar m0-pending))
                         (:= m0-pending (cdr m0-pending))
                         (:= m0-marked (set-add m0-marked (list pp m0-vs)))
                         (:= bb (lookup pp m0-program))
                         (:= m0-code (initial-code pp m0-vs))
                         (goto "mix0-bb-loop")] 

    ["mix0-while-bb" (if (not (eq? bb '())) "mix0-bb-loop" "mix0-bb-loop-end")]
    ["mix0-bb-loop" (:= m0-command (car bb))
                    (:= bb (cdr bb))
                    (goto "mix0-case")]

    ["mix0-case" (if (is-assignment m0-command) "mix0-case-assignment" "mix0-case0")]
    ["mix0-case0" (if (is-goto m0-command) "mix0-case-goto" "mix0-case1")]
    ["mix0-case1" (if (is-if m0-command) "mix0-case-if" "mix0-case2")]
    ["mix0-case2" (if (is-return m0-command) "mix0-case-return" "mix0-error")]

    ["mix0-case-assignment" (:= X (cadr m0-command))
                            (:= m0-exp (caddr m0-command))
                            (if (is-static? m0-division X) "mix0-X-static" "mix0-X-dynamic")]
    ["mix0-X-static" (:= m0-vs (set-state m0-vs X (eval-expr m0-vs m0-exp)))
                     (goto "mix0-while-bb")]
    ["mix0-X-dynamic" (:= m0-code (extend m0-code `(:= ,X ,(quote-list (reduce m0-exp m0-vs)))))
                      (goto "mix0-while-bb")]

    ["mix0-case-goto" (:= m0-pp1 (cadr m0-command))
                      (:= bb (lookup m0-pp1 m0-program))
                      (goto "mix0-while-bb")]

    ["mix0-case-if" (:= m0-exp (cadr m0-command))
                    (:= m0-pp1 (caddr m0-command))
                    (:= m0-pp2 (cadddr m0-command))
                    (if (is-static-exp? m0-division m0-exp) "mix0-cond-static" "mix0-cond-dynamic")]
    ["mix0-cond-static" (if (eval-expr m0-vs m0-exp) "mix0-go-pp1" "mix0-go-pp2")]
    ["mix0-go-pp1" (:= bb (lookup m0-pp1 m0-program)) 
                   (goto "mix0-while-bb")]
    ["mix0-go-pp2" (:= bb (lookup m0-pp2 m0-program))
                   (goto "mix0-while-bb")]
    ["mix0-cond-dynamic" (:= m0-pending (if (set-member? m0-marked (list m0-pp1 m0-vs)) m0-pending (set-add m0-pending (list m0-pp1 m0-vs))))
                         (:= m0-pending (if (set-member? m0-marked (list m0-pp2 m0-vs)) m0-pending (set-add m0-pending (list m0-pp2 m0-vs))))
                         (:= m0-code (extend m0-code `(if ,(reduce m0-exp m0-vs) ',(list m0-pp1 m0-vs) ',(list m0-pp2 m0-vs))))
                         (goto "mix0-while-bb")]
    
    ["mix0-case-return" (:= m0-exp (cadr m0-command))
                        (:= m0-code (extend m0-code `(return ,(quote-list (reduce m0-exp m0-vs)))))
                        (goto "mix0-while-bb")]
    
    ["mix0-bb-loop-end" (:= residual (extend residual m0-code))
                        (goto "mix0-while-pending")]

    ["mix0-pending-loop-end" (return residual)]
    
    ["mix0-error" (return 'runtime-error)]
    ))

; 1-st Fatamura projection
(define proj1
  (interpret-fl
   mix
   (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)) (init-state (list 'program) (list tm-prog)))))


; 2-nd Fatamura projection
(define proj2
  (interpret-fl
   mix
   (list mix
         (list '(m0-program m0-division) '(m0-pending m0-marked pp m0-vs0 m0-vs m0-command X m0-code bb m0-pp1 m0-pp2 m0-exp residual))
         (init-state (list 'm0-program 'm0-division) (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)))))))
        
(define (main)
  (begin
    (printf "1-st projection:\n\n")
    (fl-pretty-print (change-labels proj1))
    
    (printf "Compiler from TM to FL:\n\n")
    (fl-pretty-print (change-labels proj2))))

