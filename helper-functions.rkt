#lang racket

(provide eval-expr ; Flowchart
         set-state
         init-state
         find-block 
         move-tape ; TM
         first-symbol
         reduce ; mix
         initial-code
         fl-pretty-print
         change-labels
         fl-pretty-print)

(require racket/list
         racket/dict)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; --------------------------------------------
; Flowchart functions
; --------------------------------------------

; Evaluate expression in a context of state 
(define (eval-expr state expr)
  (let ([st (map (lambda (x) (list (car x) (cdr x))) state)])
    (begin
      ;(displayln `(let ,st ,expr))
      ;(displayln "")
      (let ([v (eval `(let ,st ,expr) ns)]) v))))

; Initialize the dict of variables
(define (init-state vars values)
  (begin ;(displayln (list vars values))
    (map (lambda (x y) (cons x `',y)) vars values)))

; Set state var to value
(define (set-state state var value)
  (dict-set state var `',value))

; Find basic block by label
(define (find-block program label)
  (match program
    ['() (error (list "Label not found:" label))]
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

; update pending
(define (update-pending pending marked pp1 pp2 vs)
  (if (set-member? marked (list pp1 vs))
      (if (set-member? marked (list pp2 vs)) pending (set-add pending (list pp2 vs)))
      (let ([pending (set-add pending (list pp1 vs))] )
        (if (set-member? marked (list pp2 vs)) pending (set-add pending (list pp2 vs))))
      ))

; Reduce expression
(define (reduce exp r-vs)
  (with-handlers
      ([(lambda (v) #t)
        (lambda (err)
          [if (list? exp) `,(map (lambda (x) (reduce x r-vs)) exp) exp])])
    (if (and [list? exp] [and [not (empty? exp)] [equal? 'quote (first exp)]])
        exp
        (let ([result (eval-expr r-vs exp)]) 
          (if (procedure? result) exp (if (list? result) `',result result))))))

(define (reduce-new exp r-vs)
  (cond
    [(null? exp) exp]
    [(number? exp) exp]
    [#t exp]
))

(define (is-static-exp? division exp)
    (let ([stst (if (list? exp)
        (andmap (lambda (x) (is-static-exp? division x)) exp)
        (not (is-dynamic? division exp)))])
    (begin
        ;(displayln (list division exp stst))
        ;(displayln "")
        stst
      )))

(define (is-static? division X)
  (set-member? (car division) X))

(define (is-dynamic? division X)
  (set-member? (cadr division) X))

; Find block by label
(define (lookup label program)
  (begin
    ; (displayln "lookup call")
    ; (displayln "")
    (match program
      ['() (error "Label not found.")]
      [(cons head tail)
       (if (equal? (car head) label)
           (cdr head)
           (lookup label tail))])))

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
  (if (eq? exp '()) `',exp exp))

(define (extend code cmd)
  (append code (list cmd)))

; -------------------------------------------------------------

(define (fl-pretty-print program)
  (match program
    ['() (printf "\n")]
    [(cons x xs) (printf "~s\n" x)
                 (fl-pretty-print xs)]))

(define (enumerate n)
  (define (build-list m)
    (cond ((<= m 0) '())
          (else (cons (- m 1)
                      (build-list (- m 1))))))
  (reverse (build-list n)))

(define (get-labels prog)
  (let ([prog (cdr prog)])
    (map (lambda (x i) (cons (car x) (string-append (caadar x) "-" (number->string i)))) prog (enumerate (length prog)))))

(define (change-labels prog)
  (let ([mapping (get-labels prog)])
    (for/list ([el prog]) (map-labels el mapping))))

(define (map-labels x mapping)
  (cond ((dict-has-key? mapping x) (dict-ref mapping x))
        ((list? x) (map (lambda (y) (map-labels y mapping)) x))
        (else x)))
