(load "stdlib.scm")

(define (show-status letters word)
    (string-concat
        (map
          (lambda (x) (if (in-array x letters)
            (string-cons x "") "_ "))
        word)))

(define (get-word pred)
    (define xs (lines (apply read-contents "input/wordsEn.txt")))
    (define theword (string-init (random-choice xs)))
    (if (pred theword) theword (get-word pred)))

(define (game-over? picked-letters word)
    (if (null? word)
        #t
        (&& (in-array (car word) picked-letters)
            (game-over? picked-letters (cdr word)))))

(define (game-repeated-letter picked-letters word letter)
    (write-line "... you already guessed that.")
    (game picked-letters word))

(define (game-letter-try picked-letters word letter)
    (write-line (if (in-array letter word)
        "... success!" "... try again!"))
    (game (cons letter picked-letters) word))

(define (game picked-letters word)
    (write-line "")
    (write-line (show-status picked-letters word))
    
    (write "Guess a letter: ")
    (define guess (apply read-line))
    (write (string-append "You guessed: " guess))

    (define letter (string-charat 0 guess))
    (if (game-over? (cons letter picked-letters) word)
        (write-line "... game over, you won!")
        (if (in-array letter picked-letters)
            (game-repeated-letter picked-letters word letter)
            (game-letter-try picked-letters word letter))))

(write "Pick a difficulty [easy/medium/hard]: ")

(define (pred input)
    (if (string=? input "easy")
        (lambda (word) (between? 1 5 (string-length word)))
        (if (string=? input "medium")
            (lambda (word) (between? 6 8 (string-length word)))
            (lambda (word) (between? 8 9999 (string-length word)))
        )))
(define theword (get-word (pred (apply read-line))))
(write-line (string-append "The word is: " theword))
(game '() (string->list theword))
