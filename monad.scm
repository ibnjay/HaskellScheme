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
(define maybe-bind (lambda (a f) (if a (f a) 'Nothing)))
(define maybe-unit (lambda (a) a))

; maybe functor
(define maybe-fmap (lambda (a f) (if a (f a) 'Nothing)))

; list functor
(define list-fmap map)

; list monad
(define list-bind (lambda (a f) concat (map f a)))
