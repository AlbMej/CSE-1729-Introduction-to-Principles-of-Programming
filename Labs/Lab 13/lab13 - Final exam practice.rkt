"EXPERSSION IN SCHEME"
(newline)
"1"
(+ 4 8 15 16 23 42)

"2"
(* 653854321 241304201)

"3"
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
(newline)
"FUNCTIONS"
"1. Simple Functions"
(newline)
"Problem 1a"
(define (absolute num)
  (if (< num 0)
      (- num)
      num))
(absolute -5)

"Problem 1b"
(define (F->C f)
  (*  (/ 5 9) (- f 32)))
(F->C 50)
(define (C->F c)
  (+ (* (/ 9 5) c) 32))
(C->F 10)

"Problem 1c"
(define (discount price percent)
  (- price (* price (/ percent 100))))
(discount 100 10)

"Problem 1d"
(define (tip bill)
  (ceiling (* bill .15)))
(tip 100)

"Problem 1e"
(define (num-gallons length width painted?)
  (let ((area (* length width)))
    (if painted?
        (ceiling (/ area 400))
        (ceiling (/ area 500)))))
(num-gallons 20 30 #t)

"Problem 1f"
(define (ceiling-gallons radius painted?)
  (let ((sqft (* radius radius 3.1416)))
    (if painted?
        (ceiling (/ sqft 400))
        (ceiling (/ sqft 500)))))
(ceiling-gallons 50 #t)

(newline)
"2. Recursion"
(newline)
"Problem 2a"
(define (towers-of-hanoi n source temp dest)
  (cond ((not (= n 0))
         (towers-of-hanoi (- n 1) source dest temp)
         (display "Move disk ")
         (display n)
         (display " from ")
         (display source)
         (display " to ")
         (display dest)
         (newline)
         (towers-of-hanoi (- n 1) temp source dest))))
(towers-of-hanoi 4 'A 'B 'C)
(newline)

"3. Tail Recursion"
(newline)
"Problem 3a"
(define (num-odds nums)
  (cond ((null? nums) 0)
        ((= (modulo (car nums) 2) 0) (+ (num-odds (cdr nums))))
        (else (+ 1 (num-odds (cdr nums))))))
(num-odds '(1 2 3 4 5))

(define (num-odds nums)
  (define (help-num nums acc)
  (cond ((null? nums) acc)
        ((odd? (car nums)) (help-num (cdr nums) (+ acc 1)))
        (else (help-num (cdr nums) acc))))
  (help-num nums 0))
(num-odds '(1 2 3 4 5))

"Problem 3b"
(define (gcd2 int1 int2)
  (cond ((= int1 0) int2)
        ((= int2 0) int1)
        ((> int1 int2) (gcd2 (- int1 int2) int2))
        (else (gcd2 int1 (- int2 int1)))))

(gcd2 252 105)
(newline)

"4. Higher-Order Functions"
(newline)
"Problem 4a (SICP Exercise 1.43)"
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      f
      (compose f (repeated f (- n 1)) )))

((repeated (lambda (x) (* x x)) 2) 5)

"Problem 4b (SICP Exercise 1.44)"
(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

"LIST PROCESSING"
"1. Pairs"
(newline)
"Problem 1a"
"Problem 1b"
(newline)

"2. Lists"
(newline)
"Problem 2a"
"Problem 2b"
"Problem 2c"
"Problem 2d"
"Problem 2e"
"Problem 2f"
"Problem 2g"
"Problem 2h"
(newline)

"3. Trees"
(newline)
"Problem 3a"
"Problem 3b"
"Problem 3c"
(newline)

"4. Streams"
(newline)
"Problem 4a"
"Problem 4b"
(newline)

"5. Objects"
(newline)
"Problem 5a"
"Problem 5b"
"Problem 5c"
"Problem 5d"
