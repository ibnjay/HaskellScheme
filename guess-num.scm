(define randnom 5)

(define (guessLower)
   (write "Guess Lower")
   (guessing-game))

(define (guessHigher)
   (write "Guess Higher")
   (guessing-game))

(define (guessing-game)
    (define input (apply read-line))
    (if (= input randnom)
        (write "You were correct!")
        (if (> input randnom)
	    (guessLower)
		(guessHigher))))

(define (main)
    (write "Guess a number between 1 - 100: ")
    (guessing-game))

(main)
