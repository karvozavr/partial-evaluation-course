#lang racket

(require racket/dict
         "helper-functions.rkt")

(provide interpret-fl)

(define (interpret-fl program args)
  (let ([state (init-state (cdar program) args)])
    (interpret-block (cdr program) state (cdadr program))))

; Interpret Flowchart basic block (line)
(define (interpret-block program state block)
  (begin
    
   ; (displayln (list "block: " block))
   ; (displayln "")
    
  (match block
    ['() (error "Empty basic block.")]
    [`(,head)
     (interpret-jump program state head)]
    [(cons head tail)
     (interpret-block program (interpret-assign state head) tail)])))

; Interpret Flowchart assignment and update state
(define (interpret-assign state assign)
  (match assign
   ; [`(:= ,var ,expr) (let ([value (eval-expr state expr)]) (begin (displayln value) (set-state state var value)))]
    [`(:= ,var ,expr) (begin (set-state state var (eval-expr state expr)))]
    [_ (error "Assignment expected.")]))

; Interpret Flowchart jump statement
(define (interpret-jump program state jump)
  (match jump
    [`(goto ,label) (interpret-block program state (find-block program label))]
    [`(if ,expr ,label1 ,label2)
     (if (eval-expr state expr)
         (interpret-block program state (find-block program label1))
         (interpret-block program state (find-block program label2)))]
    [`(return ,expr) (eval-expr state expr)]))



