(define secret-word "Scheme in Haskell is fun!")

(define (guessing-game)
    (write "Guess the secret word: ")
    (if (string=? (apply read) secret-word)
        (write "Bar")
        (guessing-game)))

(guessing-game)
