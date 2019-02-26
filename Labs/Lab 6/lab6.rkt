"Problem 1a"

(define (make-complex a b)
  (cons a b))

(make-complex 2 3)
  
"Problem 1b"
(define (real x) (car x))

"Problem 1c"
(define (imag x) (cdr x))

"Problem 2a"
(define (complex-add x y)
  (make-complex (+ (real x) (real y)) (+ (imag x) (imag y))))

"Problem 2b"
(define (complex-sub x y) (make-complex (- (real x) (real y)) (- (imag x) (imag y))))

"Problem 2c"
(define (complex-mult x y) (make-complex  (- (* (real x) (real y)) (* (imag x) (imag y))) (+ (* (imag x) (real y)) (* (real x) (imag y)))))

"Problem 3"
(define (complex-conj x)
  (make-complex (real x) (* (imag x) -1)))

"Problem 4a"
(define (count-positives lst)
  (define (help result lst)
  (cond ((null? lst) result)
        ((> (car lst) 0) (help (+ result 1) (cdr lst)))
        (else (help result (cdr lst)))))
  (help 0 lst))

( count-positives ( list 1 -23 0 -11 3 1002))

(count-positives '())

"Problem 4b"
(define (sum-list lst)
  (define (help result lst)
    (if (null? lst)
        result
        (help (+ (car lst) result) (cdr lst))))
  (help 0 lst))

( sum-list '(1 2 3 4 5))

"Problem 4c"
( define (consecutive-ints a b )
   ( if ( > a b)
        '()
        ( cons a ( consecutive-ints (+ a 1) b ))))

(define (consecutive-squares a b)
  ( if ( > a b)
        '()
        (cons (* a a) ( consecutive-squares (+ a 1) b ))))

(consecutive-squares 1 10)
  