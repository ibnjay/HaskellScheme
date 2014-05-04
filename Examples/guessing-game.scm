(define secret-word "Scheme in Haskell is fun!")

(define (guessing-game-wrong n)
    (write (if (= n 0) "Try again: " 
        (string-append
            "Try again ("
            (number->string n)
            "): ")))
    (guessing-game (+ n 1)))

(define (guessing-game n)
    (if (string=? (apply read-line) secret-word)
        (write-line "You were correct!")
        (guessing-game-wrong n)))

(write "Guess the secret phrase: ")
(guessing-game 0)
