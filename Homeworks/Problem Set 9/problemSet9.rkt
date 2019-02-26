"Problem 1"
(define (make-checking beginning-balance)
  
  (let* ((balance beginning-balance)
         (statements (string-append "beginning balance: " (number->string beginning-balance) "\n" )))
    
    (define (write-check funds)
      (set! balance (- balance funds))
      (set! statements (string-append statements "transaction: check amount: -" (number->string funds) "\n" )))
    
    (define (deposit funds)
      (set! balance (+ balance funds))
      (set! statements (string-append statements  "transaction: deposit amount: " (number->string funds) "\n"))
      )
    (define (print-statement)
      (string-append statements  "balance: " (number->string balance) "\n"))
      
    (define (current-balance)
      balance)
     
    (lambda (method)
      (cond ((eq? method 'write-check) write-check)
            ((eq? method 'deposit) deposit)
            ((eq? method 'print-statement) print-statement)
            ((eq? method 'balance) current-balance)
            (else 'undefined-operation)))))

;(define checking (make-checking 100))
;(display ((checking 'print-statement)))
;(display ((checking 'print-statement)))
;((checking 'write-check) 10) 
;((checking 'write-check) 10)
;((checking 'deposit) 100)
;((checking 'write-check) 10)
;((checking 'print-statement))
;(display ((checking 'print-statement)))
;(display ((checking 'print-statement)))
;((checking 'balance))


"Problem 2"
(define (make-clock hours minutes)
  (let ((mins (number->string minutes)))

    (if (< (string->number mins) 10)
        (set! mins (string-append "0" mins))
        mins)

    (define (tick)
      (if (< (string->number mins) 9)
          (set! mins (string-append "0" (number->string (+ (string->number mins) 1))))
          (set! mins (number->string (+ (string->number mins) 1))))
      (if (= (string->number mins) 60)
          (begin (set! hours (+ 1 hours)) (set! mins "00")))
      (if (= hours 24)
          (set! hours 0)))
    
    (define (time)
      (cond ((= hours 0)
             (string-append "12:" mins " AM"))
            ((= hours 24)
             (string-append (number->string (- hours 12)) ":" mins " AM"))
            ((> hours 12)  
             (string-append (number->string (- hours 12)) ":" mins " PM" ))
            ((= hours 12)
             (string-append (number->string hours) ":" mins " PM"))
            (else (string-append (number->string hours) ":" mins " AM" ) )))   
      
    (define (military)
      (if (< hours 10)
          (string-append "0" (number->string hours) ":" mins)
          (string-append (number->string hours) ":" mins )))      

    (lambda (method)
      (cond ((eq? method 'tick) tick)
            ((eq? method 'time) time)
            ((eq? method 'military) military)
            (else 'undefined-operation)))))

(define clock (make-clock 12 59))
(define get-time (clock 'time))
(define get-mil (clock 'military))
;(display (get-time))
(display (get-mil))
((clock 'tick))
;(display (get-time))
;((clock 'tick))
;(display (get-time))
(display (get-mil))

;Outputs
;9:00 PM
;9:01 PM
;9:02 PM
;21:02

#| (define clock (make-clock 2 0))
(define get-time (clock 'time))
(define get-mil (clock 'military))
(display (get-time))
(display (get-mil))
((clock 'tick))
(display (get-time))
(display (get-mil)) |#

;2:00 AM
;02:00
;2:01 AM
;02:01

"Problem 3"
(define (make-book title author)
  (define (get-author)
    author)

  (define (get-title)
    title)
  
  (lambda (method)
    (cond ((eq? method 'get-author) get-author)
          ((eq? method 'get-title) get-title)
          
          (else 'undefined-operation))))

;(define test (make-book "EE" "Uncle Dynamite"))
;((test 'get-title))


"Problem 4"
(define (make-library)
  (let ((library '()))

    (define (add book-obj)
      (set! library (cons book-obj library)))

    (define (find-author author-name)
      (define (help-author name thelib)
        (cond ((null? thelib) '())
              ((string=? (((car thelib) 'get-author)) name)
               (cons
                                                             (cons (((car thelib) 'get-author)) (((car thelib) 'get-title)))
                                                             (help-author name (cdr thelib))))
              (else (help-author name (cdr thelib)))))
      (help-author author-name library))

    (define (find-title book-title)
      (define (help-title name thelib)
        (cond ((null? thelib) '())
              ((string=? (((car thelib) 'get-title)) name)
               (cons
                                                            (cons (((car thelib) 'get-author))  (((car thelib) 'get-title)))
                                                            (help-title name (cdr thelib))))
              (else (help-title name (cdr thelib)))))
      (help-title book-title library))

    (lambda (method)
      (cond ((eq? method 'add) add)
            ((eq? method 'find-author) find-author)
            ((eq? method 'find-title) find-title)
            (else 'undefined-operation)))))

#| ( define mylib ( make-library ))
(( mylib 'add ) ( make-book "Harry Potter and the Philosopher’s Stone" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Chamber of Secrets" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Prisoner of Azkaban" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Goblet of Fire" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Order of the Phoenix" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Half-Blood Prince" "Rowling" ))
(( mylib 'add ) ( make-book "Harry Potter and the Deathly Hallows" "Rowling" ))
(( mylib 'add ) ( make-book "The Hunger Games" "Suzanne Collins"))
(( mylib 'add ) ( make-book "Catching Fire" "Suzanne Collins" ))
(( mylib 'add ) ( make-book "Mockingjay" "Suzanne Collins" ))
(( mylib 'add ) ( make-book "The Magician" "Michael Scott" ))
(( mylib 'add ) ( make-book "The Magician" "W . Somerset Maugham" ))
(( mylib 'find-title) "The Magician" )
(( mylib 'find-author) "Suzanne Collins" )
(( mylib 'find-author) "Rowling" ) |#

"Problem 5"
(define (make-track title)
  (let ((name title)
        (creator "unknown")
        (vinyl "unknown"))

    (define (get-title)
      name)

    (define (get-artist)
      creator)

    (define (get-album)
      vinyl)

    (define (set-title newtitle)
      (set! name newtitle))

    (define (set-artist artist)
      (set! creator artist))

    (define (set-album album)
      (set! vinyl album))

    (lambda (method)
      (cond ((eq? method 'get-title) get-title)
            ((eq? method 'get-artist) get-artist)
            ((eq? method 'get-album) get-album)
            ((eq? method 'set-title) set-title)
            ((eq? method 'set-artist) set-artist)
            ((eq? method 'set-album) set-album)
            (else 'undefined-operation)))))

;(define mycd (make-track "first steps"))
;((mycd 'get-title))
;((mycd 'set-title) "32")
;((mycd 'get-title))
"Problem 6"
(define (make-music-library)
  (let ((itunes '()))
    (define (add cd)
      (set! itunes (cons cd itunes)))

    (define (find-by-artist artist)
      (define (help-artist name ilib)
        (cond ((null? ilib) '())
              ((string=? (((car ilib) 'get-artist)) name)
               (cons
                (list (((car ilib) 'get-title)) (((car ilib) 'get-artist)) (((car ilib) 'get-album)))
                (help-artist name (cdr ilib))))
              (else (help-artist name (cdr ilib)))))
      (help-artist artist itunes))

    (define (find-by-title title)
      (define (help-title name thelib)
        (cond ((null? thelib) '())
              ((string=? (((car thelib) 'get-title)) name)
               (cons
                (list (((car thelib) 'get-title))  (((car thelib) 'get-artist)) (((car thelib) 'get-album)))
                (help-title name (cdr thelib))))
              (else (help-title name (cdr thelib)))))
      (help-title title itunes))

    (define (find-by-album album)
      (define (help-album name thelib)
        (cond ((null? thelib) '())
              ((string=? (((car thelib) 'get-album)) name)
               (cons
                (list (((car thelib) 'get-title))  (((car thelib) 'get-artist)) (((car thelib) 'get-album)))
                (help-album name (cdr thelib))))
              (else (help-album name (cdr thelib)))))
      (help-album album itunes))

    (lambda (method)
      (cond ((eq? method 'add) add)
            ((eq? method 'find-by-artist) find-by-artist)
            ((eq? method 'find-by-title) find-by-title)
            ((eq? method 'find-by-album) find-by-album)
            (else 'undefined-operation)))))


;( define mylib ( make-music-library ))
;(( mylib 'add ) (make-track "Hot Pants"))
;(( mylib 'add ) (((make-track "Highway 51" ) 'set-artist) "James Brown"))
;(( mylib 'find-by-artist) "James Brown" )
;(( mylib 'find-by-artist) "Bobby Brown" ) ;not in lib
;(( mylib 'find-by-album) "12 x 5" )
;(( mylib 'find-by-album) "Aftermath" ) ;not in lib
;(( mylib 'find-by-title) "Hot Pants" )
;(( mylib 'find-by-title) "Highway 51" ) ;not in lib