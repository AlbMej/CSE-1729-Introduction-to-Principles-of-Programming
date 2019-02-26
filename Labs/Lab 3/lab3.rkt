"Problem 1a"
(define (num-in-gen n)
  (if (= n 0)
  1
  (* 2 (num-in-gen (- n 1)))))

(num-in-gen 3)

"Problem 1b"
(define (num-ancestors n)
  (if (= n 0)
      0
      (+ (num-ancestors (- n 1)) (num-in-gen n))))

(num-ancestors 3)

"Problem 2a"
(define (pell-num n) (cond
                       ((= n 0) 0)
                       ((= n 1) 1)
                       ((+ (* 2 (pell-num (- n 1))) (pell-num (- n 2))))))

(pell-num 2) ;;2
(pell-num 3) ;;5
(pell-num 4) ;;12

"Problem 2b"
(define (comp-pell-num n) (cond
                       ((= n 0) 2)
                       ((= n 1) 2)
                       ((+ (* 2 (comp-pell-num (- n 1))) (comp-pell-num (- n 2))))))

"Problem 2c"
(define (sqrt-2-approx n) (/ (/ (comp-pell-num n) 2 ) (pell-num n)))
(+ (sqrt-2-approx 6) 0.0)
(sqrt 2)

"Problem 3a"
(define (fastexp b e) (cond
                        ((= e 0) 1)
                        ((even? e) (expt (expt b (/ e 2)) 2))
                        ((odd? e) (* b (expt (expt b (/ (- e 1) 2)) 2)))))

"Problem  3b"
;;Since my fastexp function does repeated division as opposed to the power function, fastexp funciton is indeed faster in that its runtime is O(log(e)) where as the other function is O(e)

"Problem 4a"
(define (cont-frac k x) (if (= k 0)
                            0
                            (/ (- x 1) (+ 2 (cont-frac (- k 1) x )))))
"Problem 4b"
(define (new-sqrt x n) (+ (cont-frac n x) 1))
(sqrt 100)
(+ (new-sqrt 100 1000) 0.0)
(+ (new-sqrt 100 20) 0.0)
                         