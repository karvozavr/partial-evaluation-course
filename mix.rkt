#lang racket

(require racket/set)

(define mix
  '([read program division vs0]
    [init1 (:= pending (list->set (list (list pp0 vs0))))
           (goto init2)]
    [init2 (:= marked (list->set '()))
           (goto while-pending)]
    
    [while-pending (if (not (eq? pending '())) pending-loop pending-loop-end)]
    [pending-loop (:= pp (caar pending))
                  (:= vs (cadar pending))
                  (:= pending (cdr pending))
                  (:= marked (set-add marked (list pp vs)))
                  (:= bb (lookup pp program)) ; TODO
                  (:= code (initial-code pp vs)) ; TODO
                  (goto bb-loop)] 

    [while-bb (if (not (eq? bb '())) bb-loop bb-loop-end)]
    [bb-loop (:= command (car bb))
             (:= bb (cdr bb))
             (goto case)]

    [case (if (is-assignment command) case-assignment case0)] ; TODO
    [case0 (if (is-goto command) case-goto case1)] ; TODO
    [case1 (if (is-if command) case-if case2)] ; TODO
    [case2 (if (is-return command) case-return error)] ; TODO

    [case-assignment (:= X (cadr command))
                     (:= exp (caddr command))
                     (if (is-static division X) X-static X-dynamic)] ; TODO
    [X-static (:= vs (cons `(,X (eval-exp exp vs)) vs)) ; TODO
              (goto while-bb)]
    [X-dynamic (:= code (extend code `(:= ,X ,(reduce exp vs))))
               (goto while-bb)] ; TODO

    [case-goto (:= pp1 (cadr command))
               (:= bb (lookup pp1 program))
               (goto while-bb)] ; TODO

    [case-if (:= exp (cadr command))
             (:= pp1 (caddr command))
             (:= pp2 (cadddr command))
             (if (is-static division exp) cond-static cond-dynamic)] ; TODO
    [cond-static (if (eval-exp exp vs) go-pp1 go-pp2)]
    [go-pp1 (:= bb (lookup pp1 program))
            (goto while-bb)]
    [go-pp2 (:= bb (lookup pp2 program))
            (goto while-bb)]
    [cond-dynamic (:= pending (if (set-member marked (list pp1 vs)) pending (set-add pending (list pp1 vs))))
                  (:= pending (if (set-member marked (list pp2 vs)) pending (set-add pending (list pp2 vs))))
                  (:= code (extend code `(if ,(reduce exp vs) ,(list pp1 vs) ,(list pp2 vs))))]
    
    [case-return (:= exp (cadr command))
                 (code (extend code `(return ,(reduce exp vs))))] ; TODO
    
    [bb-loop-end (:= residual (extend residual code)) ; TODO
                 (goto while-pending)]

    [pending-loop-end (return residual)]
    
    [error (return 'runtime-error)]
    ))