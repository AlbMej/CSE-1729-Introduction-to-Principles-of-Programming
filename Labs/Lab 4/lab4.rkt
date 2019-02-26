"Problem 1"
( define ( harmonic n)
( if (= n 1)
1.0
(+ (/ 1 n) ( harmonic (- n 1)))))
 
(define (harmonic-iter n)
  (define (iterator result count)
    ( if (= count 0)
         result
         (iterator (+ result (/ 1 count)) (- count 1))))
  (iterator 0 n))

(harmonic 3)
(+ (harmonic-iter 1) 0)
(harmonic-iter 2)
(+ (harmonic-iter 3) 0.0)
"Problem 2"

(define (harm-term k) (/ 1 k))

( define ( sum f n )
   ( if (= n 1)
        (f 1)
        (+ (f n) ( sum f (- n 1)))))

(define (harm-sum n) (sum harm-term n))

(+ (harm-sum 3) 0.0)

"Problem 3a"

(define (g-sum f a b)
  (if  (> a b)
      0
      (+ (f b) ( g-sum f a (- b 1)))))

"Problem 3b"

(+ (+ (harm-sum 4) (harm-sum 10) 0.0))
(+ (g-sum harm-sum 4 10) 0.0)

"Problem 3c"

(define (geometric-term k) (/ 1 (expt 2 k)))

(define (geom-series-np2 n) (g-sum geometric-term 0 n ) )

(geom-series-np2 2) 

"Problem 3d"

(define (convergent-series a b) (g-sum (lambda (k) (/ 1 (expt k 2))) a b))

"Problem 4a"


(define (find sequence test n)
  (define (help_find a b)  (cond
                            ((= b 0) (sequence (- a 1)))
                            ((test (sequence a) )(help_find (+ 1 a) (- b 1)))
                            (else (help_find (+ 1 a) b) )))
  (help_find 1 n))


"Problem 4b"

(find (lambda (n) n) even? 15)

(find (lambda (n) n) odd? 15)

"Problem 4c"

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        ((> n 2) (+ (fib (- n 1))
                    (fib (- n 2))))
        )
  )

(define (prime n)
   (define (divides a b) (= (modulo b a) 0))
   (define (smooth n k)
     (and (>= k 2)
          (or (divides k n)
              (smooth n (- k 1)))))
   (and (> n 1) (not (smooth n (floor (sqrt n))))))

(define (nth-fib-prime n) (find fib prime n))

(nth-fib-prime 6)

"Problem 5a"

(define (comp f g) (lambda (x) (f (g x)) ))

( define ( double x) (* 2 x ))
( define ( add-one x ) (+ x 1))
( define com ( comp add-one double ))
( com 3)
;;7
(( comp double add-one ) 3)
;;8

"Problem 5b"

(define pos-cos (comp abs cos))
        

"Problem 5c"
(define (square x)(* x x))

((comp square sqrt) 4)
