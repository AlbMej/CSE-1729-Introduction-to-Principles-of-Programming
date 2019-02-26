"Problem 1a"

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (harmonic n)
  (define (term n) (/ 1 (* -1 n)))
  (define (next n) (+ n 1))
  (sum term (- 0 n) next -1)
  ) 

(+ (harmonic 2) 0.0)

"Problem 1b"

(define (sum-i term a next b)
  (define (sum_help term a result next b)
                                         (if (> a b)
                                             result
                                             (sum_help term (next a) (+ result (term a)) next b)))
  (sum_help term a 0 next b))

(define (harmonic-i n)
  (define (term n) (/ 1 (* -1 n)))
  (define (next n) (+ n 1))
  (sum-i term (- 0 n) next -1)
  ) 

(+ (harmonic-i 2) 0.0)

"Problem 1c"
(harmonic 1)

(harmonic 50)

(harmonic 100)

(harmonic-i 1)

(harmonic-i 50)

(harmonic-i 100)

"Problem 2a"
(define  (product term a next b) (if (> a b)
                                     1
                                     (* (term a) (product term (next a) next b))))



"Problem 2b"
(define  (product-i term a next b)
  (define (product_help term a result next b)
  (if (> a b)
      1
      (* (term a) (product_help term (next a) (+ result (term a)) next b))))
  (product_help term a 0 next b))

"Problem 2c"

(define (pi-approx n)
  (define (next n) (+ n 1))
  (define (nterm n) (* (truncate (/ (+ n 2) 2)) 2))
  (define (dterm n) (+ (* (truncate (/ (+ n 1) 2)) 2) 1))
  
  (* (/ (product nterm 1 next n) (product dterm 1 next n)) 4.0))

(define (pi-approx2 n)
  (define (next n) (+ n 1))
  (define (eterm n) (if (odd? n )
                       (next n)
                       n))
  (define (oterm n) (if (even? n )
                       (next n)
                       n))
  
  (* (/ (product-i eterm 2 next n) (product-i oterm 3 next n)) 4.0))


"Problem 2d"

(pi-approx 1)
(pi-approx 100)
(pi-approx 1000)
(display "\n")
(pi-approx2 1)
(pi-approx2 100)
(pi-approx2 1000)

"Problem 3a"

(define (cont-frac n d k) (if (= k 0)
                            0
                            (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(cont-frac ( lambda ( i ) 1.0)
            ( lambda (i ) 1.0)
            10)

"Problem 3b"

(define (denom a)
  (cond ((= (modulo a 3) 1) (* (+ (/ (- a 1) 3) 1) 2)) 
        (else 1)))

(+ (denom 0) 0.0)
(+ (denom 5) 0.0)
(+ (denom 6) 0.0)
(+ (denom 7) 0.0)
(+ (denom 8) 0.0)
(+ (denom 9) 0.0)
(+ (denom 10) 0.0) 


(define (e-approx k) (+ 2.0 (cont-frac (lambda (i) 1.0) (lambda (a) (denom (- k a))) k)))

(e-approx 40)

"Problem 3c"

(define pi 3.14159)

(define (tan-cf x k)
  (define (num i)
    (* -1 (expt x 2)))
  
  (define (den i)
    (+ 1 (* 2 (+ (- k i) 1))))

  (/ x (+ 1 (cont-frac num den k))))

(e-approx 3)

;;(+ 0.0 (tan-cf (/ pi 5) 10000))
    
"Problem 4a"
(define (der f h) (lambda (x) (/ (- (f (+ x h)) (f x)) h )))

((der (lambda (i) (expt i 2)) 0.0001) 2)
"Problem 4b"

((der sin 0.5) 0)
((der cos 0.5) 0)
((der cos 0.5) (/ pi 2))
((der cos 0.5) pi)
((der cos 0.5) (/ (* 3 pi) 4))

"Problem 4c"
(define (fun x) (+ (- (* 3 (expt x 2)) (* 2 x)) 7))

((der fun 0.0001) 2)
((der fun 0.0001) 5)
((der fun 0.0001) 10)

"Problem 5"
(define (comp f g) (lambda (x) (f (g x)) ))

( define ( double x) (* 2 x ))
( define ( add-one x ) (+ x 1))
( define com ( comp add-one double ))

(define (repeated f n)
  (define (repeated-help n result) 
  (if (= n 0)
      result
      (repeated-help (- n 1) (comp f result))))
  (repeated-help n (lambda (x) x)))

(define (square x) (* x x))
((repeated square 2) 5)