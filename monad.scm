; monad M definition
;   bind: a -> (a -> M b) -> M b
;   unit: a -> M a
;
; with an optional field (unimplemented):
;   fail: String -> M a
; if the bind function returns #f
;

; identity monad
(define identity-bind (lambda (a f) (f a)))
(define identity-unit (lambda (a) a))

; maybe monad
(define (maybe-bind a f)
    (if (eq? a 'Nothing) 'Nothing (f a)))
(define maybe-unit (lambda (a) a))

; maybe functor
(define (maybe-fmap a f)
    (if (eq? a 'Nothing) 'Nothing (f a)))

; list functor
(define list-fmap map)

; list monad
(define list-bind (lambda (a f) (concat (map f a))))
