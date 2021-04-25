(define (string->symbol-list str)
  ;; takes a string, returns list of symbols
  ;; spaces are used to delimit strings representing symbols
  ;; newlines are turned into the symbol 'newline in the output list
  (define (string->symbol-list-aux char-lst)
    (cond ((null? char-lst) '())
          ((eq? (car char-lst) #\space)
           (string->symbol-list-aux (cdr char-lst)))
          ((eq? (car char-lst) #\newline)
           (cons 'newline (string->symbol-list-aux (cdr char-lst))))
          (else
           (cons (get-first-symbol char-lst)
                 (string->symbol-list-aux (remove-first-symbol char-lst))))))
  (string->symbol-list-aux (string->list str)))

(define (get-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns a symbol made from the characters up to a space newline or end of list
  (define (gfs-iter char-lst sofar)
    (cond ((or (null? char-lst)
               (eq? (car char-lst) #\space)
               (eq? (car char-lst) #\newline))
           (string->symbol (list->string (reverse sofar))))
          (else
           (gfs-iter (cdr char-lst) (cons (car char-lst) sofar)))))
  (gfs-iter char-lst '()))

(define (remove-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns the list with all characters up to a space, newline,
  ; or end of list removed
  (cond ((or (null? char-lst)
             (eq? (car char-lst) #\space)
             (eq? (car char-lst) #\newline))
         char-lst)
        (else
         (remove-first-symbol (cdr char-lst)))))

(define (symbol-list->string sym-lst)
  ; given a list of symbols, turns it into a string
  ; by adding space characters between strings representing
  ; symbols, and adds newline characters wherever there is
  ; the 'newline symbol
  (cond ((null? sym-lst) "")
        ((eq? (car sym-lst) 'newline)
         (string-append (list->string (list #\newline)) (symbol-list->string (cdr sym-lst))))
        (else (string-append (symbol->string (car sym-lst))
                             " "
                             (symbol-list->string (cdr sym-lst))))))

(define (surfin-bird) 
  "a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird the bird is the word
a well a bird bird bird well the bird is the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word
a well a

a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a dont you know about the bird
well everybodys talking about the bird
a well a bird bird b-birds the word
a well a bird

surfin bird
bbbbbbbbbbbbbbbbbb aaah

pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa
pa pa pa pa pa pa pa pa pa pa pa pa pa pa ooma mow mow
papa ooma mow mow

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
oom oom oom oom ooma mow mow
papa ooma mow mow papa oom oom oom
oom ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa a mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
papa ooma mow mow ooma mow mow
papa oom oom oom oom ooma mow mow
oom oom oom oom ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
well dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow")

; see what (string->symbol-list (surfin-bird) produces


;; extra functions using bit sequences
;; the strings here are strings of 0s and 1s
;; a bit-sequence is simply an integer
;; a 1 is added to the start of each bit-sequence
;; as (number->string x 2) drops leading zeroes

(define (bit-sequence->string num)
  (define (rem-first string)
    (substring string 1 (string-length string)))
  (rem-first (number->string num 2)))

(define (string->bit-sequence  str)
  (string->number (string-append "1" str) 2))

;; try (bit-sequence->string 1729) and then invert it with string->bit-sequence

(define (log2 x)
  (/ (log x)(log 2)))