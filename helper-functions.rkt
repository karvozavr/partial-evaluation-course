#lang racket

(provide eval-expr ; Flowchart
         set-state
         init-state
         find-block 
         move-tape ; TM
         first-symbol
         reduce ; mix
         initial-code)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; --------------------------------------------
; Flowchart functions
; --------------------------------------------

; Evaluate expression in a context of state 
(define (eval-expr state expr)
  (let ([st (map (lambda (x) (list (car x) (cdr x))) state)])
    (begin
 ;     (displayln `(let ,st ,expr))
      (eval `(let ,st ,expr) ns))))

; Initialize the dict of variables
(define (init-state vars values)
  (map (lambda (x y) (cons x `',y)) vars values))

; Set state var to value
(define (set-state state var value)
  (dict-set state var `',value))

; Find basic block by label
(define (find-block program label)
  (match program
    ['() (error "Label not found.")]
    [(cons head tail)
     (if (equal? (car head) label)
         (cdr head)
         (find-block tail label))]))

; --------------------------------------------
; TM interpreter functions
; --------------------------------------------

(define (move-tape l)
  (match l
    ['() '(_)]
    ;[`(,el) `(,el _)]
    [(cons head tail) tail]))

(define (first-symbol l)
  (match l
    ['() '_]
    [(cons head tail) head]))

; --------------------------------------------
; mix functions
; --------------------------------------------

; Reduce expression
(define (reduce exp vs)
  (with-handlers
      ([(lambda (v) #t)
        (lambda (err)
          (if (list? exp)
              (map (lambda (x) (reduce x vs)) exp)
              exp))])
    (let ([result (eval-expr vs exp)])
      (if (procedure? result) exp result))))

(define (is-static-exp? division exp)
  (if (list? exp)
      (andmap (lambda (x) (is-static-exp? division x)) exp)
      (not (is-dynamic? division exp))))

(define (is-static? division X)
  (set-member? (car division) X))

(define (is-dynamic? division X)
  (set-member? (cadr division) X))

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

(define (initial-code pp vs)
  (list `',`(,pp ,vs)))

(define (generate-read-block read-block division)
  (cons 'read (set-subtract (cdr read-block) (car division))))

(define (quote-list exp)
  (if (and (not (eq? exp '())) (and (not (list? (car exp))) (procedure? (eval (car exp))))) exp `',exp))

(define (extend code cmd)
  (append code (list cmd)))
