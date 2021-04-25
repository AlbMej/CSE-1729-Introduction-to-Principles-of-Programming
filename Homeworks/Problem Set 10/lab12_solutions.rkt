;; stream primitives
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define empty-stream? null?)

;; utiity for tracing function calls 
(define (tracer name . values)
  ;  usage: if at start of function defined (foo a b c)
  ;  put in (tracer 'foo a b c)
  ;  interesting to put into something that is delayed
  (define (display-spaced item)
    (display item)
    (display " "))
  (display-spaced name)
  (display-spaced "with parameter(s):")
  (for-each display-spaced values)
  (newline))
;;;;;;;;;;;;;;;;

;1a

(define (stream-filter f str)
  (cond ((empty-stream? str)
         '())
        ((f (stream-car str))
         (cons-stream (stream-car str)
                      (stream-filter f (stream-cdr str))))
        (else
         (stream-filter f (stream-cdr str)))))
;1b
(define (stream-map f str)
  (if (null? str)
      '()
      (cons-stream (f (stream-car str))
                   (stream-map f (stream-cdr str)))))
;1c
(define (stream-nth n stream)
  (if (= n 1)
      (stream-car stream)
      (stream-nth (- n 1) (stream-cdr stream) )))

;2
(define (str-to-list str n)
  (cond ((or (empty-stream? str)(= n 0)) '())
        (else
         (cons (stream-car str)
               (str-to-list (stream-cdr str)
                            (- n 1))))))

;3a
(define (scale-stream factor stream)
  (stream-map (lambda (x) (* x factor)) stream))

;3b

(define (add-streams s1 s2)
;; note: if one stream shorter fill it with zeroes
  (cond ((and (empty-stream? s1)(empty-stream? s2)) '())
        ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (cons-stream (+ (stream-car s1)(stream-car s2))
                      (add-streams (stream-cdr s1) (stream-cdr s2))))))
;3c

(define (mult-streams s1 s2)
;; note: if one stream shorter fill it with zeroes
  (cond ((and (empty-stream? s1)(empty-stream? s2)) '())
        ((empty-stream? s1)(cons-stream 0 (mult-streams s1 (stream-cdr s2))))
        ((empty-stream? s2)(cons-stream 0 (mult-streams s2 (stream-cdr s1))))
        (else
         (cons-stream (* (stream-car s1)(stream-car s2))
                      (mult-streams (stream-cdr s1) (stream-cdr s2))))))
;3d
(define (append-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        (else
         (cons-stream (stream-car s1)
                      (append-streams (stream-cdr s1) s2)))))

;4a
(define (enumerate-integer-stream)
  (define (ints-from a)
    (cons-stream a (ints-from (+ a 1))))
  (ints-from 0))
;4b
(define (odd-factors-of k)
 (stream-filter odd? (scale-stream k (enumerate-integer-stream))))

;4c
(define (partial-sums str)
  (define (partial-sum-extend str sumprev)
    (if (empty-stream? str)
        '()
        (cons-stream (+ (stream-car str) sumprev)
                     (partial-sum-extend (stream-cdr str)
                                         (+ (stream-car str) sumprev)))))
  (partial-sum-extend str 0))

;4d
(define (square-stream)
  (define (helper str)
    (mult-streams str str))
  (helper (enumerate-integer-stream)))



         