;; stream primitives
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (scale-stream k str)
  (cond ((empty-stream? str) str)
        (else (cons-stream (* (stream-car str) k) (scale-stream k (stream-cdr str))))))

(define (add-streams s1 s2)
  (cond ((and (empty-stream? s1)(empty-stream? s2))'())
        ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream
               (+ (stream-car s1)(stream-car s2))
               (add-streams (stream-cdr s1)(stream-cdr s2))))))
  
(define (stream-map f str)
  (if (empty-stream? str)
      '()
      (cons-stream (f (stream-car str))
                   (stream-map f (stream-cdr str)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define empty-stream? null?)

(define (mult-streams str1 str2)
  (cond ((and (empty-stream? str1) (empty-stream? str2)) '())
        ((empty-stream? str1) zeroes)
        ((empty-stream? str2) zeroes)
        (else (cons-stream (* (stream-car str1) (stream-car str2)) (mult-streams (stream-cdr str1) (stream-cdr str2))))))

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

(define (decode z)
  (let* ((w ( floor (- ( sqrt (* 2 z )) 1/2)))
         (t (/ (+ (* w w) w) 2))
         (x (- z t ))
         (y (+ (- w x ) 2)))
    (cons (inexact->exact x) (inexact->exact y))))

(define (enumerate-integer-stream)
  (define (help_enum start)
    (cons-stream start (help_enum (+ start 1))))
  (help_enum 0))

(define (enumerate-from n)
  (define (help_enum start)
    (cons-stream start (help_enum (+ start 1))))
  (help_enum n))

(define (stream-filter p str)
  (cond ((empty-stream? str) '())
        ((p (stream-car str)) (cons-stream (stream-car str) (stream-filter p (stream-cdr str))))
        (else (stream-filter p (stream-cdr str)))))

(define (str-to-list str k)
  (cond ((empty-stream? str) str)
        ((= k 0) '())
        (else (cons (stream-car str) (str-to-list (stream-cdr str) (- k 1))))))

(define (stream-nth index str)
  (if (= index 1)
      (stream-car str)
      (stream-nth (- index 1) (stream-cdr str))))
"Problem 1a"
(define (decode-stream)
  (stream-map decode (enumerate-from 1)))

(define test (cons-stream 1
                          (cons-stream 2
                                       (cons-stream 3 '()))))

(stream-cdr (decode-stream))



"Problem 1b"
(str-to-list (decode-stream) 50)
;;The sum of each pair is never less than the previous pair sum. There is also some interleaving of the pairs and then it increments 

"Problem 2a"
(define (fibs-frm curr prev)
  (cons-stream curr
               (fibs-frm (+ curr prev) curr)))

(define fibs
  (cons-stream 0 (fibs-frm 1 0)))

"Problem 2b"
(define pow-seven
  (cons-stream 1 (scale-stream pow-seven 7)))

(define stream-of-seven
  (cons-stream 7 stream-of-seven))

stream-of-seven

(define pow-seven
  (cons-stream 1 (mult-streams pow-seven stream-of-seven)))
(str-to-list pow-seven 4) 

"Problem 3a"
(define ones (cons-stream 1 ones))
(define ints (cons-stream 1 (add-streams ones ints)))
(define ints2 (cons-stream 0 (add-streams ones ints2)))

(str-to-list ints2 10)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define zeroes (cons-stream 0 zeroes))
zeroes
(factorial 0)
(define (e-to-the-x-terms x)
  (if (= x 0)
      (cons-stream 1 zeroes)
      (cons-stream 1 (stream-map (lambda (n) (/ (expt x n) (factorial n))) ints))))

(define (e-to-the-x-terms x )
  (define (help_e x n)
    (if (= x 0)
        (cons-stream 1 zeroes)
        (cons-stream (/ (expt x n) (factorial n)) (help_e x (+ n 1)))))
  (help_e x 0))

(str-to-list (e-to-the-x-terms 2) 10)
;(str-to-list (etest 2) 10)

"Problem 3b"
(define (partial-sums str)
  (define (helping_psum str accterms)
    (if (empty-stream? str)
        str
        (cons-stream (+ (stream-car str) accterms)
                     (helping_psum (stream-cdr str) (+ accterms (stream-car str))))))
  (helping_psum str 0))

(define (e-to-the-x-approx x)
  (partial-sums (e-to-the-x-terms x)))

;(stream-cdr (e-to-the-x-terms 5))
;(str-to-list e-to-the-x-terms 2)

"Problem 4a"
(define (stream-merge str1 str2)
  (cond ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        ((> (stream-car str1) (stream-car str2))
         (cons-stream
          (stream-car str2) (stream-merge str1 (stream-cdr str2))))
        ((< (stream-car str1) (stream-car str2))
         (cons-stream
          (stream-car str1) (stream-merge (stream-cdr str1) str2)))
        (else
         (cons-stream
          (stream-car str1) (stream-merge (stream-cdr str1) (stream-cdr str2))))))

;(str-to-list (stream-merge stream-of-seven ones) 10)

"Problem 4b"

(define 2s&3s
  (cons-stream 1 (stream-merge (scale-stream 2s&3s 2)
                               (scale-stream 2s&3s 3))))

(str-to-list 2s&3s 5)

(define 235-stream
  (stream-merge 2s&3s (scale-stream 2s&3s 5)))

(str-to-list 235-stream 10)


"Problem 5a"
(define (interleave s1 s2 )
  (if (empty-stream? s1 )
      s2
      (cons-stream (stream-car s1 )
                   (interleave s2 (stream-cdr s1 )))))
(define (pairs s t)
  (cons-stream
   (cons (stream-car s) (stream-car t ))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s ) x ))
                (stream-cdr t ))
    (pairs (stream-cdr s) t ))))
(define pairs-stream
  (let ((naturals (stream-cdr (enumerate-integer-stream ))))
    (pairs naturals naturals )))

(str-to-list pairs-stream 10)
;The second element in each pair interleaves and increments according to a pattern. Also, each pair is no less than the pair before it starting with pair 2.

(define (merge-weighted str1 str2 weight)
  (cond ((empty-stream? str1) str2)
        ((empty-stream? str2) str1)
        ((> (weight (stream-car str1)) (weight (stream-car str2)))
         (cons-stream
          (stream-car str2) (merge-weighted str1 (stream-cdr str2) weight)))
        ((< (weight (stream-car str1)) (weight (stream-car str2)))
         (cons-stream
          (stream-car str1) (merge-weighted (stream-cdr str1) str2 weight)))
        ((equal? (weight (stream-car str1)) (weight (stream-car str2)))
         (cons-stream
          (stream-car str1) (cons-stream (stream-car str2) (merge-weighted (stream-cdr str1) (stream-cdr str2) weight))))
        (else
         (cons-stream
          (stream-car str1) (merge-weighted (stream-cdr str1) (stream-cdr str2) weight)))))

"Problem 5b"
(define (stream-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (stream-pairs (stream-cdr s)  t))))

(define (weighted-pairs str1 str2 weight)
  (cons-stream
   (cons (stream-car str1) (stream-car str2))
   (merge-weighted
    (stream-map (lambda (x) (cons (stream-car str1) x))
                (stream-cdr str2))
    (weighted-pairs (stream-cdr str1) str2 weight) weight)))

(define test5b (cons-stream 1
                            (cons-stream 2
                                         (cons-stream 3 '()))))
(define test25b (cons-stream 4
                             (cons-stream 5
                                          (cons-stream 6 '()))))

"Problem 5c"
(define weighted-pairs-stream
  (weighted-pairs ints ints (lambda (x) (+ (car x) (cdr x))) ))

(str-to-list weighted-pairs-stream 10)

"Problem 5d"

(define (cpw pair)
  ;pass car of stream
  (+ (expt (car pair) 3) (expt (cdr pair) 3)))
  
(define (ram-stream)
  
  (define (find-cubes str)
    (if (= (cpw (stream-car str)) (cpw (stream-car (stream-cdr str))))
        (cons-stream (list (cpw (stream-car str))
                           (stream-car str) (stream-car (stream-cdr str)))
                     (find-cubes (stream-cdr (stream-cdr str))))
        (find-cubes (stream-cdr str))))
  (find-cubes (stream-filter (lambda (x) (< (car x) (cdr x))) (weighted-pairs ints ints (lambda (x) (+ (expt (car x) 3) (expt (cdr x) 3))) ))))

(str-to-list (ram-stream) 10)

(define (ramanujan n)
  (car (stream-nth n (ram-stream))))

(ramanujan 2)

;The next five in the sequence are 4104, 13832 ,20683 ,32832 ,39312 

"Problem 6a"
;----------------------------------------------------------------------Encode Using Recursion-----------------------------------------------------------------------------
(define (encode-ps pair)
  (cond ((= (cdr pair) 1) (- (expt 2 (car pair)) 1))
        ((= (cdr pair) 2) (- (* 3 (expt 2 (- (car pair) 1))) 1))
        (else (+ (expt 2 (car pair)) (encode-ps (cons (car pair) (- (cdr pair) 1)))))))

;-------------------------------------------------------------------Faster Encode (No recursion)-----------------------------------------------------------------------------
(define (encode-ps pair)
  (let ((x (car pair))
        (y (cdr pair)))
    (cond ((= (cdr pair) 1) (- (expt 2 (car pair)) 1))
          ((= (cdr pair) 2) (- (* 3 (expt 2 (- (car pair) 1))) 1))
          (else (inexact->exact (- (* (- y .5) (expt 2 x)) 1))))))

;----------------------------------------------------------------------Local Test Cases----------------------------------------------------------------------------------
#|(encode-ps (cons 1 1)) ;1
(encode-ps (cons 2 1)) ;3
(encode-ps (cons 3 1)) ;7
(encode-ps (cons 3 3)) ;19
(encode-ps (cons 1 11)) ;20
(encode-ps (cons 4 73)) ;1159
|#
(encode-ps (cons 5 1))
(encode-ps (cons 5 2))
(encode-ps (cons 5 3))
(encode-ps (cons 5 4))
(encode-ps (cons 5 31))
(encode-ps (cons 5 32))
(encode-ps (cons 5 100000))
"Problem 6b"
;----------------------------------------------------------------------Decode Using Streams-----------------------------------------------------------------------------
(define (decode-ps n)
  (define (decode-help n str)
    (if (= n 1)
        (stream-car str )
        (decode-help (- n 1) (stream-cdr str))))
  (decode-help n pairs-stream))

;(decode-ps 1543)
;(decode-ps 1435)
;(decode-ps 1590)
;(decode-ps 17645)
;(decode-ps 3529)
;(decode-ps 100001)
;(decode-ps 1023)
;(decode-ps 1535)
;(decode-ps 2559)
;(decode-ps 67)
;----------------------------------------------------------------------Actual Decode-------------------------------------------------------------------------------------
(define (decode-ps n)
  (let* ((save '())
         (bool #f))
    (define (guess x y n print)
      ;(newline)
      ;(display  (- (* (- y .5) (expt 2 x)) 1)) (display " ") (display print) (display " | ") (display "X currently is: ") (display x) (display " | ") (display "Y currently is: ") (display Y)
      ;(display (= n (encode-ps (cons x y))))      
      (cond ((not (null? save)) save)
            ((= n (encode-ps (cons x y))) (begin (set! save (cons x y)) (set! bool #t) save))
            (bool save)
            ((and (>= n 1007) (= (modulo (- n 47) 32) 0)) (cons 5 (inexact->exact (ceiling (+ (/ (- n 63) 32) 2)))))
            ((and (> n 95) (= (modulo (- n 63) 32) 0)) (cons 6 (inexact->exact (+ (/ (- n 63) 64) 1.5))))
            ((and (> n 11) (= (modulo (- n 11) 8) 0)) (cons 3 (+ 2 (/ (- n 11) 8)))) 
            ((and (> n 5) (= (modulo (- n 5) 4) 0)) (cons 2 (+ 2 (/ (- n 5) 4))))
            ((= n (- (* (- y .5) (expt 2 x)) 1)) (begin (set! save (cons x y)) save))
            ((> n (- (* (- y .5) (expt 2 x)) 1)) (and (guess (+ x 1) y n "X" ) (guess x (+ y 1) n "Y")))))
    (cond ((= n 1) (cons 1 1))
          ((even? n) (cons 1 (/ (+ n 2) 2)))
          ((integer? (/ (log (+ n 1)) (log 2))) (cons (inexact->exact (/ (log (+ n 1)) (log 2))) 1))
          (else (guess 2 2 n "start" )))))

;Print parameter is to help me debug
;(str-to-list pairs-stream 1991)
;(decode-ps 1991)
;----------------------------------------------------------------------Local Test Cases----------------------------------------------------------------------------------
#|
(decode-ps 1590) 
(decode-ps 1545)
(decode-ps 1435)
(decode-ps 1023)
(decode-ps 1535)
(decode-ps 2559)
(decode-ps 3583)
(decode-ps 101)
(decode-ps 1729)
(decode-ps 1015)
(decode-ps 100001)
(decode-ps 1000001)
(decode-ps 1159321) |#
;(decode-ps 63)
;(decode-ps 95)
;(decode-ps 159)
;(decode-ps 223)
;(decode-ps879)
(decode-ps 1007)
(decode-ps 3199983)

;----------------------------------------------------------------------Brain Storming--------------------------------------------------------------------------------------
; n mod m = (modulo n m)
;Actual form is n = 2^k - 1 and nmod2^k = 2^(k - 1) -1
; n = (2^k)a + 2^(k - 1) - 1 where a = 1,2,3,4 --> mod2^k = 2^(k - 1) - 1  for some reason  (+ (* (expt 2 k) a) (- (expt 2 (- k 1)) 1))
;if nmod2^k = 2^(k - 1) - 1 is true do something. One of theses functions return a boolean. i increments up to n
; k  = (log(n+ 1) / log(2))
; try numbers less than 20 and greater than 1000