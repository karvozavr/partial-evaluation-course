#lang racket

(require racket/set)

(define mix
  '([read program division vs0]
    [init1 (:= pending '((pp0 vs0)))
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
             (:= bb (cdr bb))]

    [bb-loop-end (:= residual (extend residual code))
                 (goto while-pending)]

    [pending-loop-end (return residual)]
    [error (return 'runtime-error)]
    ))