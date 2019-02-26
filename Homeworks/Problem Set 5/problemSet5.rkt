"Problem 1a"
(define (c->p p)
  (let* ((x (car p))
         (y (cdr p))
         (r (sqrt (+ (* x x) (* y y))))
         (theta (atan (/ y x))))
    (cons r theta)))

;;Theta is in radian.
(c->p (cons 1 2))

(define (p->c p)
  (let* ((theta (cdr p))
         (r (car p))
         (x (* r (cos theta)))
         (y (* r (sin theta))))
    (cons x y)))

;;Answer in radians

(p->c (cons 5 60))

"Problem 2"
(define (max-f p)
  (let ((fx (car p))
        (gx (cdr p)))
    (lambda (x)
      (if (> (fx x) (gx x))
          (fx x)
          (gx x)
          ))))
        
"Problem 3"
(define (zip list1 list2)
  (if (null? (cdr list1))
      (cons (cons (car list1) (car list2)) '())
      (cons (cons (car list1) (car list2)) (zip (cdr list1 ) (cdr list2 )))))

(zip '(1 2 3) '(4 5 6))

"Problem 4"
(define (unzip pair)
  (define (car-help pair)
    (if (null? pair)
        '()
        (cons (caar pair) (car-help (cdr pair)))))

  (define (cdr-help pair)
    (if (null? pair)
        '()
        (cons (cdar pair) (cdr-help (cdr pair)))))
  (cons (car-help pair) (cdr-help pair) ))

(zip '(1 2 3 4) '(5 6 7 8))


(unzip (zip '(1 2 3 4) '(5 6 7 8)))`

(unzip (zip '("r") '("e")))

(car (unzip (list (cons 1 2) (cons 3 4) (cons 4 5))))

"Problem 5a"
(define (encode pair)
  (let* ((x (car pair))
         (y (cdr pair)))
    (+ (* 0.5 (- (+ x y) 2) (- (+ x y) 1)) x)))

(encode (cons 7 8))

"Problem 5b"
(define (decode z)
  (let* ((w (floor (- (sqrt (* 2 z)) (/ 1 2))))
         (t (/ (+ (* w w) w) 2))
         (x (- z t))
         (y (+ (- w x) 2)))
    (cons x y)))

(encode (cons 40 30))

(decode 3706)

"Problem 6"
(define (positives l)
  (cond ((null? l) '())
        ((> (car l) 0) (cons (car l) (positives (cdr l))))
        (else (positives (cdr l)))))
( positives ( list -2 -1 0 1 2))
;; (1 2)
( positives ( list 2 1 0 -2 -1))
;; (2 1)
( positives '(3 1 -1 1 -1))
;;(3 1 1)

"Problem 7"
(define (remove-duplicates list)
  (define (remove v list)
    (cond ((null? list) list)
          ((= v (car list)) (remove v (cdr list)))
          (else (cons (car list) (remove v (cdr list))))))
  (if (null? list)
      '()
      (cons (car list) (remove-duplicates (remove (car list) (cdr list))))))

(remove-duplicates '( 1 3 3 5 3 1))