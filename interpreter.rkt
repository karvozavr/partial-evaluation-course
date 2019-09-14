#lang racket

(require racket/dict)

(provide interpret-fl)

(define (interpret-fl program args)
  (let ([state (init-state (cdar program) args)])
    (interpret-block (cdr program) state (cdadr program))))

; Interpret Flowchart basic block (line)
(define (interpret-block program state block)
  (match block
    ['() (error "Empty basic block.")]
    [`(,head)
     (interpret-jump program state head)]
    [(cons head tail)
     (interpret-block program (interpret-assign state head) tail)]))

; Interpret Flowchart assignment and update state
(define (interpret-assign state assign)
  (match assign
    [`(:= ,var ,expr) (let ([value (eval-expr state expr)]) (set-state state var value))]
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

; Evaluate expression in a context of state 
(define (eval-expr state expr)
  (let ([st (map (lambda (x) (list (car x) (cdr x))) state)])
    (eval `(let ,st ,expr))))

(define (init-state vars values)
  (map cons vars values))

(define (set-state state var value)
  (dict-set state var value))

(define (find-block program label)
  (match program
    ['() (error "Label not found.")]
    [(cons head tail)
     (if (equal? (car head) label)
         (cdr head)
         (find-block tail label))]))

