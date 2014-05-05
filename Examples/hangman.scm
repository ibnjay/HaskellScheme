(load "stdlib.scm")

(define NUM_TRIES 6)

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

(define (missed-letters word letters)
    (filter (lambda (x) (not (in-array x word))) letters))

(define (print-hangman letters word)
    (define n (length (missed-letters word letters)))
    (apply read-contents
        (string-append
            (string-append "Examples/hangman/" (number->string n))
            ".txt")))

(define (game-over? picked-letters word)
    (if (null? word)
        #t
        (&& (in-array (car word) picked-letters)
            (game-over? picked-letters (cdr word)))))

(define (lost-game? word picked-letters)
    (<= NUM_TRIES (length (missed-letters word picked-letters))))

(define (game-input-letter game picked-letters word guess)
    (if (|| (null? guess)
            (not (is-alpha? (car guess))))
        (game-recur "... please enter a letter!" game picked-letters word)
        (game-input-letter2 game picked-letters word (car guess))
    ))

(define (game-recur message game picked-letters word)
    (write-line message)
    (game picked-letters word))

(define (game-input-letter2 game picked-letters word letter)
    (define letters2 (cons letter picked-letters))
    (if (game-over? letters2 word)
        (write-line "... game over, you won!")
        (if (in-array letter picked-letters)
            (game-recur "... you already guessed that." game picked-letters word)
            (if (in-array letter word)
                (game-recur "... success!" game letters2 word)
                (if (lost-game? word letters2)
                    (write-line "... game over, you lost!")
                    (game-recur "... try again!" game letters2 word))
                ))))

(define (game picked-letters word)
    (write-line "")
    (write-line (print-hangman picked-letters word))
    (write-line (show-status picked-letters word))

    (write "Guess a letter: ")
    (define guess (apply read-line))
    (write (string-append "You guessed: " guess))
    (game-input-letter game picked-letters word (string->list guess)))

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
