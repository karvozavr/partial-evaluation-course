#lang racket

(require racket/set
         racket/dict
         "interpreter.rkt"
         "tm-interpreter.rkt"
         "helper-functions.rkt")

(provide main)

(define mix
  '([read program division vs0]
    [init1 (:= pending (list (list (caadr program) vs0)))
           (:= marked '())
           (:= residual (list (generate-read-block (car program) division)))
           (goto while-pending)]
    
    [while-pending (if (not (eq? pending '())) pending-loop pending-loop-end)]
    [pending-loop (:= pp (caar pending))
                  (:= vs (cadar pending))
                  (:= pending (cdr pending))
                  (:= marked (set-add marked (list pp vs)))
                  (:= bb (lookup pp program))
                  (:= code (initial-code pp vs))
                  (goto bb-loop)] 

    [while-bb (if (not (eq? bb '())) bb-loop bb-loop-end)]
    [bb-loop (:= command (car bb))
             (:= bb (cdr bb))
             (goto case)]

    [case (if (is-assignment command) case-assignment case0)]
    [case0 (if (is-goto command) case-goto case1)]
    [case1 (if (is-if command) case-if case2)]
    [case2 (if (is-return command) case-return error)]

    [case-assignment (:= X (cadr command))
                     (:= exp (caddr command))
                     (if (is-static? division X) X-static X-dynamic)]
    [X-static (:= vs (set-state vs X (eval-expr vs exp)))
              (goto while-bb)]
    [X-dynamic (:= code (extend code `(:= ,X ,(quote-list (reduce exp vs)))))
               (goto while-bb)]

    [case-goto (:= pp1 (cadr command))
               (:= bb (lookup pp1 program))
               (goto while-bb)]

    [case-if (:= exp (cadr command))
             (:= pp1 (caddr command))
             (:= pp2 (cadddr command))
             (if (is-static-exp? division exp) cond-static cond-dynamic)]
    [cond-static (if (eval-expr vs exp) go-pp1 go-pp2)]
    [go-pp1 (:= bb (lookup pp1 program)) 
            (goto while-bb)]
    [go-pp2 (:= bb (lookup pp2 program))
            (goto while-bb)]
    [cond-dynamic (:= pending (if (set-member? marked (list pp1 vs)) pending (set-add pending (list pp1 vs))))
                  (:= pending (if (set-member? marked (list pp2 vs)) pending (set-add pending (list pp2 vs))))
                  (:= code (extend code `(if ,(reduce exp vs) ',(list pp1 vs) ',(list pp2 vs))))
                  (goto while-bb)]
    
    [case-return (:= exp (cadr command))
                 (:= code (extend code `(return ,(quote-list (reduce exp vs)))))
                 (goto while-bb)]
    
    [bb-loop-end (:= residual (extend residual code))
                 (goto while-pending)]

    [pending-loop-end (return residual)]
    
    [error (return 'runtime-error)]
    ))

; 1-st Fatamura projection
(define proj1
  (interpret-fl
   mix
   (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)) (init-state (list 'program) (list tm-prog)))))

(define (main)
  (begin
    (printf "TM Program:\n\n")
    (fl-pretty-print tm-prog)
    (printf "Specified interpreter Program:\n\n")
    (fl-pretty-print (change-labels proj1))))

; 2-nd Fatamura projection
; (define proj2
;  (interpret-fl
;   mix
;   (list mix
;         (list '(program division) '(vs0 pending mapped pp vs command X code bb pp1 pp2 exp residual))
;         (init-state (list 'program 'division) (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)))))))
         


