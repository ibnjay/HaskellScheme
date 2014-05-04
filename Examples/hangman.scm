(load "stdlib.scm")

(define (show-status letters word)
    (string-concat
        (map
          (lambda (x) (if (in-array x letters)
            (string-cons x "") "_ "))
        word)))

(define (get-word pred)
    (define contents (apply read-contents "input/three.txt"))
    (define lines (string-split "\n" contents))
    (define words (concat (map (lambda (x) (string-split " " x)) lines)))
    (define words2 (filter pred words))
    (define idx (get-random 0 (length words2)))
    (list-index idx words2))

(define (game picked-letters word)
    (write-line "")
    (write-line (show-status picked-letters word))
    
    (write "Guess a letter: ")
    (define guess (apply read-line))
    (write (string-append "You guessed: " guess))
    (define letter (string-charat 0 guess))

    (write-line (if (in-array letter word)
        "... success!" "... try again!"))
    (print letter)
    (print (in-array letter word))
    (print word)
    (game (cons letter picked-letters) word)
    )

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
