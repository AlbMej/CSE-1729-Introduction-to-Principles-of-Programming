"Problem 1a"
(define (fact n)
  (let ((result 1)
        (count 0))
    (define (helper)
      (cond ((= count n) result)
            (else
             (set! count (+ count 1))
             (set! result (* result count))
             (helper))))
    (helper)))

(fact 1)
(fact 2)
(fact 3)
(fact 4)
(fact 5)


"Problem 1b"
(define (hailstone n)
  (let ((results '()))
    (define (helper)
      (cond ((= n 1) results)
            ((even? n)  
             (set! n (/ n 2))
             (set! results (append results (list n)))
             (helper))
            (else (set! n (+ (* n 3) 1))
                  (set! results (append results (list n)))
                  (helper)
                  )))
    (cons n (helper))
    ))

(hailstone 5)
(hailstone 4)
(hailstone 1)

"Problem 2"
(define (new-account initial-balance )
  (let ((balance initial-balance)
        (interest 0.01))
    (define (deposit f )
      (set! balance (+ balance f ))
      balance )
     
    (define (withdraw f)
       
      (cond ((> f balance)
             (display "Insufficient funds. Current balance is: ")
 
             (display balance)
             (newline)
             (display "This is how much less than the balance is than the request: ")

             (display (- f balance )))
            (else
             (set! balance (- balance f ))
             balance))
      balance)
    
    (define (accrue)
      (set! balance (+ balance (* balance interest)))
      balance)
     
    (define (setrate arg)
      (set! interest arg))
       
    (define (bal-inq ) balance )
    (lambda (method )
      (cond ((eq? method 'deposit ) deposit )
            ((eq? method 'withdraw ) withdraw)
            ((eq? method 'balance-inquire ) bal-inq )
            ((eq? method 'setrate) setrate)
            ((eq? method 'accrue) accrue)))))

(define test1 (new-account 100))
((test1 'withdraw) 200)

"Problem 3"

(define mychecking (new-account 200)) ;Two identical accounts
(define mysavings (new-account 200))

((mysavings 'withdraw)100)
((mysavings 'deposit ) 50)     ;Savings is now 150
((mysavings 'balance-inquire )); Balance is 150

(newline)

((mychecking 'balance-inquire )) ;Should still be 100

(newline)

((mysavings 'withdraw ) 50)    ;Savings is now 100
((mysavings 'balance-inquire )); Balance is now 100

(newline)

((mychecking 'balance-inquire )) ;Again, still 100
((mychecking 'setrate) 0.09) ;0.09 of interest

(newline)

((mysavings 'accrue ))
((mysavings 'accrue ))
((mychecking 'accrue))

"Problem 4"
(define (make-stack)
  (define state-variable '())
  (define (is-empty?)
    (null? state-variable))
  (define (push thing-to-push)
    (set! state-variable (cons thing-to-push state-variable)))
  (define (top )
    (car state-variable))
  (define (pop)
    (let ((top (car state-variable)))
      (set! state-variable (cdr state-variable))
      top))
  (lambda (meth-name)
    (cond ((eq? meth-name 'is-empty )
           is-empty?)
          ((eq? meth-name 'push )
           push)
          ((eq? meth-name 'top )
           top)
          ((eq? meth-name 'pop )
           pop))))

"Problem 5a"
(define (nconc! x y) 
  (define (last_element lst)

    (if (null? (cdr lst))
        (begin (set-cdr! lst y) x)
        (last_element (cdr lst))))
  (if (null? x)
      y
      (last_element x)))
       

"Problem 5b"
(define a '(1 2 3))
(define b '(4 5 6))
(append a b )
a
b
(nconc! a b )
a
b

(nconc! '() (quote (d e f)))

(nconc! a a)

; The difference is that nconc makes a copy of the list and works on that and modifies a by adding b to it while append returns a new list with a and b combined
; (Not modifying a or b. This is why when you call a after ncon! it shows the result of (ncon! a b)). (nconc! a a) would evaluate to #0=(1 2 3 4 5 6 . #0#).
; The #0# is just there to indicate that this list would just circle around and produce 1 2 3 4 5 6 again because it points to the beggining of the list. 
