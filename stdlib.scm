; stdlib functions from the Scheme Wiki book

(define (not x) (if x #f #t))

(define (null? x) (eqv? x '()))
(define (list . objs) objs)
(define fold foldl)
(define reduce fold)
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold and #t lst))
(define (or . lst) (fold or #f lst))

(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))

(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

; added stdlib functions

(define (in-array x lst)
    (if (null? lst)
        #f
        (|| (eqv? (car lst) x) (in-array x (cdr lst)))))

(define (between? a b n)
    (&& (<= a n)
        (>= b n)))
(define (string-length s) (length (string->list s)))

(define (append a b) (foldr cons b a))
; (print (append '(4 5) '(5 6)))

(define (concat lst) (foldr (lambda (x y) (append x y)) '() lst))
; (print (concat '( (4 5) (5 6) (6 7) )))

(define (string-charat n s)
    (list-index n (string->list s)))
(define (string-concat lst) (foldr (lambda (x y) (string-append x y)) "" lst))

(define (init lst) (reverse (cdr (reverse lst))))
(define (string-init s) (list->string (init (string->list s))))

(define (random-choice lst)
    (list-index (get-random 0 (- (length lst) 1)) lst))

(define string-null? (compose null? string->list))
(define is-alpha? (lambda (w) (in-array w (string->list "abcdefghijklmnopqrstuvwxyz"))))
(define lines (curry string-split "\n"))
(define words (curry string-split " "))
