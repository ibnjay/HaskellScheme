(define secret-word "Scheme in Haskell is fun!")

(define (guessing-game)
    (write "Guess the secret word: ")
    (if (string=? (apply read) "Mike")
        (write "Bar")
        (guessing-game)))
