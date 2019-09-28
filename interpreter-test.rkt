#lang racket

(require rackunit
         rackunit/text-ui
         "interpreter.rkt")

(define test-program 
  '((read x) 
    (0 (if (eq? x 42) 2 1)) 
    (1 (return (- 42 x))) 
    (2 (return 42))))

(define interpreter-tests
  (test-suite
   "Tests for interpreter.rkt"
   (check-equal? (interpret-fl test-program '(42)) 42 "Test 42")
   (check-equal? (interpret-fl test-program '(22)) 20 "Test 20")
   (check-equal? (interpret-fl test-program '(-1)) 43 "Test -1")))

(run-tests interpreter-tests)