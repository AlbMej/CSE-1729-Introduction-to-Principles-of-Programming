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

"Problem 1a"
(define (stream-filter p str)
  (cond ((empty-stream? str) '())
        ((p (stream-car str)) (cons-stream (stream-car str) (stream-filter p (stream-cdr str))))
        (else (stream-filter p (stream-cdr str)))))

(define test2 (cons-stream 1 (cons-stream 2 (cons-stream 3 '()))))
(stream-filter odd? test2)

"Problem 1b"
(define (stream-map f str)
  (if (empty-stream? str)
      str
      (cons-stream (f (stream-car str)) (stream-map f (stream-cdr str)))))

"Problem 1c"
(define (stream-nth index str)
  (if (= index 1)
      (stream-car str)
      (stream-nth (- index 1) (stream-cdr str))))

"Problem 2"
(define (str-to-list str k)
  (cond ((empty-stream? str) str)
        ((= k 0) '())
        (else (cons (stream-car str) (str-to-list (stream-cdr str) (- k 1))))))

(define test2 (cons-stream 1 (cons-stream 2 (cons-stream 3 '()))))
(str-to-list test2 1)
"Problem 3a"
(define (scale-stream k str)
  (cond ((empty-stream? str) str)
        (else (cons-stream (* (stream-car str) k) (scale-stream k (stream-cdr str))))))

"Problem 3b"
(define (add-streams str1 str2)
  (cond ((and (empty-stream? str1) (empty-stream? str2)) '())
        ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        (else (cons-stream (+ (stream-car str1) (stream-car str2)) (add-streams (stream-cdr str1) (stream-cdr str2))))))


(define zeroes (cons-stream 0 zeroes))
zeroes
"Problem 3c"
(define (mult-streams str1 str2)
  (cond ((and (empty-stream? str1) (empty-stream? str2)) '())
        ((empty-stream? str1) zeroes)
        ((empty-stream? str2) zeroes)
        (else (cons-stream (* (stream-car str1) (stream-car str2)) (mult-streams (stream-cdr str1) (stream-cdr str2))))))

"Problem 3d"
(define (append-streams str1 str2)
  (cond ((empty-stream? str1) str2)
        (else (cons-stream (stream-car str1) (append-streams (stream-cdr str1) str2)))))

"Problem 4a"
(define (enumerate-integer-stream)
  (define (help_enum start)
    (cons-stream start (help_enum (+ start 1))))
  (help_enum 0))

"Problem 4b"
(define (odd-factors-of k)
  (stream-filter odd?
                 (stream-filter (lambda (x) (= (modulo x k) 0)) (enumerate-integer-stream))))

(define (odd-factors-of2 k)
  (define (help k str)
    (if (= (modulo (stream-car str) k) 0)
        (con-streams (stream-car str) (help k (stream-cdr str)))
        (help k (stream-cdr str))))
  (help k (stream-filter odd? (enumerate-integer-stream))))

"Problem 4c"
(define (partial-sums str)
  (define (helping_psum str accterms)
    (if (empty-stream? str)
        str
        (cons-stream (+ (stream-car str) accterms)
                 (helping_psum (stream-cdr str) (+ accterms (stream-car str))))))
  (helping_psum str 0))

"Problem 4d"
(define (square-stream)
  (stream-map (lambda (x) (* x x)) 
              (enumerate-integer-stream)))

(define (square-stream2)
  (define (help str)
    (mult-streams str str))
  (help (enumerate-integer-stream)))

(define (square-streams3)
  (define (help_enum start)
    (cons-stream (* start start) (help_enum (+ start 1))))
  (help_enum 0))