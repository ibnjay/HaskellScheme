(define secret-word "Scheme in Haskell is fun!")

(define (guessing-game)
    (write "Guess the secret phrase: ")
    (if (string=? (apply read-line) secret-word)
        (write "Bar")
        (guessing-game)))

(guessing-game)
