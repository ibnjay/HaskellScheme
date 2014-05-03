(define identity-bind (lambda (a f) (f a)))
(define identity-unit (lambda (a) a))

(define maybe-bind (lambda (a f) (if a (f a) 'Nothing)))
(define maybe-unit (lambda (a) a))

(define maybe-fmap (lambda (a f) (if a (f a) 'Nothing)))
