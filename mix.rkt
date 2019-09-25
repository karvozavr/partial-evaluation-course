#lang racket

(require racket/set)
(require "interpreter.rkt")
(require "tm-interpreter.rkt")
(require racket/dict)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; Reduce expression
(define (reduce exp vs)
  (with-handlers
      ([(lambda (v) #t)
        (lambda (err)
          (if (list? exp)
              (map (lambda (x) (reduce x vs)) exp)
              exp))])
    (eval `(let ,vs ,exp))))

(define (is-static-exp? division exp)
  (if (list? exp)
      (andmap (lambda (x) (is-static-exp? division x)) exp)
      (not (is-dynamic? division exp))))

(define (is-static? division X)
  (set-member? (car division) X))

(define (is-dynamic? division X)
  (set-member? (cadr division) X))

(define (eval-exp exp vs)
  (begin
    (displayln `(let ,vs ,exp))
    (eval `(let ,vs ,exp))))

; Find block by label
(define (lookup label program)
  (match program
    ['() (error "Label not found.")]
    [(cons head tail)
     (if (equal? (car head) label)
         (cdr head)
         (lookup label tail))]))

(define (is-assignment command)
  (eq? (car command) ':=))

(define (is-goto command)
  (eq? (car command) 'goto))

(define (is-if command)
  (eq? (car command) 'if))

(define (is-return command)
  (eq? (car command) 'return))

; Code generation

(define (initial-code pp vs)
  (list `(,pp ,vs)))

(define (extend code cmd)
  (append code (list cmd)))

; ---------------

(define mix
  '([read program division vs0]
    [init1 (:= pending (list (list (caadr program) vs0)))
           (:= marked '())
           (:= residual '())
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
    [X-static (:= vs (begin (displayln (eval-exp exp vs)) (set-state vs X (eval-exp exp vs))))
              (goto while-bb)]
    [X-dynamic (:= code (extend code `(:= ,X ,(reduce exp vs))))
               (goto while-bb)]

    [case-goto (:= pp1 (cadr command))
               (:= bb (lookup pp1 program))
               (goto while-bb)]

    [case-if (:= exp (cadr command))
             (:= pp1 (caddr command))
             (:= pp2 (cadddr command))
             (if (is-static-exp? division exp) cond-static cond-dynamic)]
    [cond-static (if (eval-exp exp vs) go-pp1 go-pp2)]
    [go-pp1 (:= bb (lookup pp1 program)) 
            (goto while-bb)]
    [go-pp2 (:= bb (lookup pp2 program))
            (goto while-bb)]
    [cond-dynamic (:= pending (if (set-member? marked (list pp1 vs)) pending (set-add pending (list pp1 vs))))
                  (:= pending (if (set-member? marked (list pp2 vs)) pending (set-add pending (list pp2 vs))))
                  (:= code (extend code `(if ,(reduce exp vs) ,(list pp1 vs) ,(list pp2 vs))))
                  (goto while-bb)]
    
    [case-return (:= exp (cadr command))
                 (:= code (extend code `(return ,(reduce exp vs))))
                 (goto while-bb)]
    
    [bb-loop-end (:= residual (extend residual code))
                 (goto while-pending)]

    [pending-loop-end (return residual)]
    
    [error (return 'runtime-error)]
    ))

; 1-st Fatamura projection
(define tm-proj1
  (interpret-fl
   mix
   (list tm-interpreter (list '(operator program program-tail instr symbol) '(Right Left)) (list `(program ',tm-prog))) ns))
         ;(list '(operator program program-tail instr symbol) '(Right Left))
         
         
         ;(list `(program ,tm-prog))
         


