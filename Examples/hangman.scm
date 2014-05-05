(load "stdlib.scm")

(define NUM_TRIES 9)

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

(define (lost-game? picked-letters)
    (<= NUM_TRIES (length picked-letters)))

(define (game-input-letter game picked-letters word guess)
    (if (|| (null? guess)
            (not (is-alpha? (list-index 0 guess))))
        (game-recur "... please enter a letter!" game picked-letters word)
        (game-input-letter2 game picked-letters word (list-index 0 guess))
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
                (if (lost-game? letters2)
                    (write-line "... game over, you lost!")
                    (game-recur "... try again!" game letters2 word))
                ))))

(define (game picked-letters word)
    (write-line "")
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
