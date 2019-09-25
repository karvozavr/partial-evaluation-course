#lang racket

(require racket/dict)

(provide interpret-fl set-state interpret-assign init-state)

(define (interpret-fl program args ns)
  (let ([state (init-state (cdar program) args)])
    (interpret-block (cdr program) state (cdadr program) ns)))

; Interpret Flowchart basic block (line)
(define (interpret-block program state block ns)
  (match block
    ['() (error "Empty basic block.")]
    [`(,head)
     (interpret-jump program state head ns)]
    [(cons head tail)
     (interpret-block program (interpret-assign state head ns) tail ns)]))

; Interpret Flowchart assignment and update state
(define (interpret-assign state assign ns)
  (match assign
   ; [`(:= ,var ,expr) (let ([value (eval-expr state expr ns)]) (begin (displayln value) (set-state state var value)))]
    [`(:= ,var ,expr) (begin (set-state state var (eval-expr state expr ns)))]
    [_ (error "Assignment expected.")]))

; Interpret Flowchart jump statement
(define (interpret-jump program state jump ns)
  (match jump
    [`(goto ,label) (interpret-block program state (find-block program label) ns)]
    [`(if ,expr ,label1 ,label2)
     (if (eval-expr state expr ns)
         (interpret-block program state (find-block program label1) ns)
         (interpret-block program state (find-block program label2) ns))]
    [`(return ,expr) (eval-expr state expr ns)]))

; Evaluate expression in a context of state 
(define (eval-expr state expr ns)
  (let ([st (map (lambda (x) (list (car x) (cdr x))) state)])
    (begin
      (displayln `(let ,st ,expr))
      (eval `(let ,st ,expr) ns))))

(define (init-state vars values)
  (map (lambda (x y) (cons x `',y)) vars values))

(define (set-state state var value)
  (dict-set state var `',value))

(define (find-block program label)
  (match program
    ['() (error "Label not found.")]
    [(cons head tail)
     (if (equal? (car head) label)
         (cdr head)
         (find-block tail label))]))

