#lang racket

; Flowchart implementation of Turing Machine interpreter
(define tm-interpreter
  '((read program Right)
    
    (init (:= program-tail program)
          (:= Left '())
          (goto loop))
    
    (loop (if (eq? program-tail '()) stop case0))
    
    (case0 (:= instr (cdar program-tail))
           (:= program-tail (cdr program-tail))
           (:= operator (car instr))
           (if (eq? operator 'if) interpret-if case1) )
    (case1 (if (eq? operator 'goto) interpret-goto case2))
    (case2 (if (eq? operator 'write) interpret-write case3))
    (case3 (if (eq? operator 'right) interpret-right case4))
    (case4 (if (eq? operator 'left) interpret-left error))

    (interpret-if (:= symbol (cadr instr))
                  (:= instr (cddr instr))
                  (if (eq? (car Right) symbol) interpret-goto loop))
    
    (interpret-goto (:= program-tail (list-tail program (list-ref instr 1)))
                    (goto loop))

    (interpret-write (:= symbol (cadr instr))
                     (:= Right (cons symbol (cdr Right)))
                     (goto loop))

    (interpret-left (:= Right ((car Left) . Right))
                     (:= Left (cdr Left))
                     (goto loop))

    (interpret-right (:= Left (cons (car Right) Left))
                     (:= Right (cdr Right))
                     (goto loop))
    
    (error (return (list 'syntax-error instr)))
    (stop (return (append Left Right)))))

(define tm-prog '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))