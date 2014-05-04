(load "stdlib.scm")

; track guessed letters
; [String] -> String -> String (with underscores)

(define (show-status letters word)
    (string-concat
        (map
          (lambda (x) (if (in-array x letters) x "_ "))
        (char-list word))))

(define (get-word pred)
    (define contents (apply read-contents "three.txt"))
    (define lines (string-split "\n" contents))
    (define words (concat (map (lambda (x) (string-split " " x)) lines)))
    (define words2 (filter pred words))
    (define idx (get-random 0 (length words2)))
    (list-index idx words2))

(define (main)
    (write "Pick a difficulty [easy/medium/hard]: ")
    ; (define input (apply read-line))
    (define input "hard")

    (define pred
        (if (string=? input "easy")
            (lambda (word) (between? 1 6 (string-length word)))
            (lambda (word) (between? 6 10 (string-length word)))
              ))
    (define theword (get-word pred))

    (write-line (show-status '() theword))
    (write "Guess a letter: ")
    (define result (apply read-line))

    (print theword))

(main)
