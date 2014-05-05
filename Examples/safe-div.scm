(load "stdlib.scm")
(load "monad.scm")

(define (safe-div a b)
    (if (= b 0)
        'Nothing
        (/ a b)))

(define (div-by b) (lambda (a) (safe-div a b)))

; divide by two twice
(define (div-by-four a)
    (maybe-bind
      (maybe-bind a (div-by 2)) (div-by 2)))

; 8 / (4 / 4) = 8
(print (safe-div 8 (div-by-four 4)))

; 8 / (2 / 4) = Nothing
(print (safe-div 8 (div-by-four 2)))
