(define randnom (apply get-random 1 100))

(define (guessLower)
   (write-line "Guess Lower")
   (guessing-game))

(define (guessHigher)
   (write-line "Guess Higher")
   (guessing-game))

(define (guessing-game)
    (define input (apply read-line))
    (if (= input randnom)
        (write-line "You were correct!")
        (if (> input randnom)
	          (guessLower)
         		(guessHigher))))

(define (main)
    (write "Guess a number between 1 - 100: ")
    (guessing-game))

(main)
