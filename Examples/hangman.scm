(load "stdlib.scm")

(define (show-status letters word)
    (string-concat
        (map
          (lambda (x) (if (in-array x letters)
            (string-cons x "") "_ "))
        (char-list word))))

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

    (write-line (if (in-array letter (char-list word))
        "... success!" "... try again!"))
    (print letter)
    (print (in-array letter (char-list word)))
    (print (char-list word))
    (game (cons letter picked-letters) word)
    )

(define (main)
    (write "Pick a difficulty [easy/medium/hard]: ")
    (define input (apply read-line))

    (define pred
        (if (string=? input "easy")
            (lambda (word) (between? 1 6 (string-length word)))
            (if (string=? input "medium")
                (lambda (word) (between? 6 8 (string-length word)))
                (lambda (word) (between? 8 9999 (string-length word)))
            )))
    (define theword (get-word pred))
    (write-line (string-append "The word is: " theword))
    (game '() theword))

(main)
