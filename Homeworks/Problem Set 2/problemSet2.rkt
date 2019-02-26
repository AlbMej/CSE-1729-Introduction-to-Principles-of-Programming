"Problem 1a"
(define (odd-sum n)
(if (= n 0)
    0
    (+ (- (* 2 n) 1) (odd-sum (- n 1)))))

"Problem 1b - Tests" 
"(odd-sum 1)"
(odd-sum 1)
"(odd-sum 2)"
(odd-sum 2)
"(odd-sum 3)"
(odd-sum 3)
"(odd-sum 4)"
(odd-sum 4)
"(odd-sum 5)"
(odd-sum 5)
"(odd-sum 6)"
(odd-sum 6)
"(odd-sum 7)"
(odd-sum 7)

;;The sequence of numbers looks like they are the squares of the number in sequence they are. For example, odd-sum 4 is 16 and odd-sum 5 is 25.

;;Problem 1c
(define (sum-from-to a b)
  (if (> a b)
      0
      (+ a (sum-from-to (+ 1 a) b))))

;;Problem 2
(define (k-product n)
  (if (< n 2)
   1
   (* (- 1 (/ 1 n) ) (k-product (- n 1)))))

;;Problem 3a
(define (finite-sum-of-powers z k)
  (if (< k 1)
    0
    (+ (expt z k) (finite-sum-of-powers z (- k 1)))))

;;Problem 3b
(define (first-value-k-or-higher z tol k)
  (if (< (- (/ z (- 1 z)) (finite-sum-of-powers z k)) tol)
     k
     (first-value-k-or-higher z tol (+ k 1))))

(define (terms-needed z tol)
  (first-value-k-or-higher z tol 1))

(terms-needed 0.8 0.00001) ;;output should be 58
(terms-needed 0.99 0.00001)

;; 3c. The approximation works best when we have a smaller Z value. In the perspective of the number of terms needed it works worse the larger Z is. 

;;Factorial function from lecture slides
 (define (factorial n)
 (if (= n 0)
 1
 (* n (factorial (- n 1)))
 )
 )

;;Problem 4
(define (new-sin x n)
  (if (= n 0)
    x
    (- (/ (expt x (+ (* 2 n) 1)) (factorial (+ (* 2 n) 1))) (new-sin x (- n 1)))))
 
                                  