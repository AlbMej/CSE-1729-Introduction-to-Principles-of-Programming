"Problem 1"
(define (harmonic n)
  (if (= n 0)
      0
  (+ (/ 1 n ) (harmonic (- n 1)))))

"Problem 2"
( define ( prime? n )
   ( define ( divisor? k ) (= 0 ( modulo n k )))
   ( define ( divisors-upto k )
      ( and ( > k 1)
            ( or ( divisor? k ) ( divisors-upto (- k 1)))))
   ( not ( divisors-upto (- n 1))))

;;(prime? 3)

(define (count-primes t) (cond
                           ((= t 1) 0)
                           ((prime? t) (+ 1 (count-primes (- t 1))))
                           ((count-primes (- t 1)))))
(count-primes 6)
  


"Problem 3a"
(define (lucas n) (cond
                    ((= n 0) 2)
                    ((= n 1) 1)
                    ((> n 1) (+ (lucas (- n 1)) (lucas (- n 2))))))

(lucas 0) ;;2
(lucas 1) ;;1
(lucas 2) ;;3
(lucas 3) ;;4
(lucas 4) ;;7
(lucas 5) ;;11

"Problem 3b"
(define (l-ratio n) (/ (lucas n) (lucas (- n 1))))

(display "\n")

"L-ratio test cases"
(+ (l-ratio 20) 0.0)
(+ (l-ratio 21) 0.0)
(+ (l-ratio 22) 0.0)

;; Fibonacci function from lecture slides.

(define (fib n)
 (cond ((= n 0) 0)
 ((= n 1) 1)
 ((> n 1) (+ (fib (- n 1))
 (fib (- n 2))))
 )
)

(define (fibonacci-ratio n) (/ (fib n) (fib (- n 1))))

(display "\n")

"Fibonacci-ratio test cases" 
(+ (fibonacci-ratio 20) 0.0)
(+ (fibonacci-ratio 21) 0.0)
(+ (fibonacci-ratio 22) 0.0)

;;I noticed that the answer between fibonacci-ration and l-ratio are very very close 

"Problem 3c"
(display "\n")

"Computing L30"
;;(+ (l-ratio 30) 0.0)

"Computing L35"
;;(+ (l-ratio 35) 0.0)

"Computing L40"
;;(+ (l-ratio 40) 0.0)

"I expect L50 would take even longer to compute an answer."

( define ( fast-lucas-help n k lucas-a lucas-b )
( if (= n k )
lucas-a
( fast-lucas-help n (+ k 1) lucas-b (+ lucas-a lucas-b ))))

( define ( fast-lucas n ) ( fast-lucas-help n 0 2 1))

(= (lucas 0) (fast-lucas 0)) ;;Should be #t
(= (lucas 1) (fast-lucas 1)) ;;Should be #t
(= (lucas 2) (fast-lucas 2)) ;;Should be #t
(= (lucas 3) (fast-lucas 3)) ;;Should be #t
(= (lucas 4) (fast-lucas 4)) ;;Should be #t
(= (lucas 5) (fast-lucas 5)) ;;Should be #t

(fast-lucas 50)

;; Table answers below in form of ---> ( k , Recursive calls made by (lucas k) , Recursive calls made by (fast-lucas-help k 0 2 1) )

#|
(k = 1 , 0 , 1 )
(k = 2 , 2 , 2)
(k = 3 , 4 , 3)
(k = 4 , 8 , 4)
(k = 5 , 14 , 5)
(k = 6 , 24 , 6)
|#

"Problem 4a"

(define (golden n) (if (= n 1)
                       2
                       (+ 1 (/ 1 (golden (- n 1))))
                       ))

(golden 1) ;; Should be 2
(golden 2) ;; Output should be 3/2 or 1.5
(golden 3) ;; Should be 1 2/3

"Problem 4b"

(define (golden-sqrt n)(if (= n 0)
                           3 ;;Figure out correct base case 
                           (sqrt (+ 1 (golden-sqrt (- n 1))))))


(golden-sqrt 1) ;; Should be 2
(golden-sqrt 2) ;; Output should be 3/2 or 1.5
(golden-sqrt 3) ;; Should be 1 2/3

"Problem 5"

;;Random function from hw pdf

( define ( random-fcn )
    ( let (( multiplier ( expt 7 5))
           ( modulus (- ( expt 2 31) 1))
           ( seed 19380110.0))
      ( lambda ()
           ( set! seed ( modulo (* seed multiplier ) modulus ))
           (/ seed modulus ))))

(define random (random-fcn))

(define (one-sample)
   (let ((x (random))
        (y (random)))

     (if (<= (+ (expt (- (* 2 x) 1) 2) (expt (- (* 2 y) 1) 2)) 1)
         1
         0)))

                   
(define (pi-samples k)
   (define (fell k total)
     (if (= k 0)
       total
       (fell (- k 1) (+ total (one-sample)))))
   (/ (fell k 0) k))

(define (pi-approx n)
  (* 4 (pi-samples n)))
  
   
;;(pi-approx 200)

"Problem 6"

(define (interval-sum m n )
    ( if (= m n )
          m
         (+ m
            ( interval-sum (+ m 1) (- n 1))
             n )))

;; Test case (interval-sum 11 12) fails
;;What's going on? The issue seems to be that this function only works for pairs of even numbers. If atleast one number is odd, the function runs forever




