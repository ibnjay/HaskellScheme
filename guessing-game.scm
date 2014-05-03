(define secret-word "Scheme in Haskell is fun!")

(define (guessing-game-helper guess)
    (write (string-append "You guessed wrong: " guess))
    (write "Try again: ")
    (guessing-game))

(define (guessing-game)
    (define result (apply read-line))
    (if (string=? result secret-word)
        (write "You were correct!")
        (guessing-game-helper result)))

(define (main)
    (write "Guess the secret phrase: ")
    (guessing-game))

(main)
