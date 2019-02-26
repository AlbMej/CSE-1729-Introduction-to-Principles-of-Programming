"Problem 1a"
(define (remove v elements)
  (if (null? elements)
      elements
      (if (equal? v (car elements))
          (cdr elements)
          (cons (car elements)
                (remove v (cdr elements))))))

(define (remove-if f elements)
  (define (help f list x newlist)
    (display x)
    (cond ((null? (cdr list)) (reverse newlist))
          ((f x) (help f (cdr list) (car (cdr list)) newlist))
          (else
           (help f (cdr list) (car (cdr list)) (cons x newlist)))))
  (help f elements (car elements) '()))

(remove-if even? '(0 1 2 3 4 5 5))
(remove-if odd? '(0 1 2 3 4 5))

"Problem 1b"
(define (nested-remove v elements)
  (cond ((null? elements) '())
        ((equal? v (car elements)) (nested-remove v (cdr elements)))
        ((list? (car elements)) (cons (nested-remove v (car elements)) (nested-remove v (cdr elements))))
        (else
         (cons (car elements) (nested-remove v (cdr elements)
            )))))

(nested-remove 'b' (b 2 ( a b ))) ; (2 (a)) 
(nested-remove '(a b) '(1 2 ( a b )) ) ; (1 2)
(nested-remove 0 '(0 1 2 (0 1 2) 3 0 4))
(nested-remove 2 '())
(nested-remove '(2 10) (list (list 2) 4 '(2 10) (list 2 10) 2 10))


"Problem 2a"
(define (explode x)
  (define (help n lst)
    (if (= n 0)
        lst
        (help (floor (/ n 10)) (cons (modulo n 10) lst))))
  (if (= x 0)
      (list x)
      (help x '())))
 

(explode 12345)
(explode 1458)
(explode 8541)
(explode 0)

(define (count-digits n acc)
  (if (< n 10)
    (+ acc 1)
    (count-digits (/ n 10.0) (+ acc 1))))

(count-digits 12345 0)

"Problem 2b"
(define (len lst)
(define (list-length l acc)
  (if (null? l)
    acc
    (list-length (cdr l) (+ acc 1))))
  (list-length lst 0))

(len '(1 2 3 4 5) )
(len '(3 4 5) )

(define (implode l)
  (define (help num lst)
    (if (null? lst)
        num
        (help (+ num (* (car lst) (expt 10 (- (len lst) 1) ))) (cdr lst))))
  (help 0 l))

(implode '(1 2 3 4 5))

"Problem 2c"

(define (sum-list list)
  (if (null? list)
      0
      (+ (car list) (sum-list (cdr list)))))

(sum-list '(1 4 5 8))

(define (reverse-lst items)
  (define (reverse-iter r-items rest)
    (if (null? r-items)
        rest
        (reverse-iter (cdr r-items)
                      (cons (car r-items) rest))))
  (reverse-iter items '()))

(reverse-lst '(1 2 3 4 5) )

(define (has-property x)
  (let* ((nlst (explode x))
        (sumnum (sum-list nlst))
        (sumlst (explode sumnum))
        (revlst (reverse-lst sumlst))
        (sumrev (implode revlst))
        (prop (* sumnum sumrev)))
    (= prop x)))

(has-property 1458)

"Problem 2d"
(define (find sequence test n)
  (define (help_find a b)  (cond
                            ((= b 0) (sequence (- a 1)))
                            ((test (sequence a) )(help_find (+ 1 a) (- b 1)))
                            (else (help_find (+ 1 a) b) )))
  (help_find 1 n))

(find (lambda (n) n) has-property 1) ;;Should be 1
(find (lambda (n) n) has-property 2) 
(find (lambda (n) n) has-property 3) ;;Should be 1458
(find (lambda (n) n) has-property 4)

;;The second integer with this property is 18 and the third integer is 1729. How witty.

"Problem 3a"
(define (superset? a b)
  (cond ((null? b) #t)
        ((member (car b) a) (superset? a (cdr b)))
        (else #f)))

(superset? '(1 2 3 4 5) '(1 5 3 2))
     
"Problem 3b"
(define (set-difference a b)
  (cond ((null? b) a)
        ((member (car b) a) (set-difference (remove (car b) a) (cdr b)))
        (else (set-difference a (cdr b)))))

(set-difference '(1 2 3 4 5) '(5))

"Problem 3c"

(define (combine x lst newlst)
  (if (null? lst)
      newlst
      (combine x (cdr lst) (append newlst (list (list x (car lst))) ))))

(define (cross-product a b)
  (define (newset a b axb)
    (cond ((null? a) axb)
          (else (newset (cdr a) b (append axb (combine (car a) b '()))))))
  (newset a b '()))

(combine 1 '(4 5 6) '())

(cross-product '(1 2 3) '(4 5 6))

"Problem 4"
(define (appendme list1 list2) ; in R5RS
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (nestless lst)
  (cond ((null? lst) '())
        ((pair? (car lst)) (appendme (nestless (car lst)) (nestless (cdr lst))))
        (else (cons (car lst) (nestless (cdr lst) )))))

(nestless '(( a b ) c (d e (f g))))

(nestless '(a b))

(nestless '())

"Problem 5"

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

(merge '(1 2 3) '(4 5 6))
(merge '(1 3 5) '(2 4 6))
(merge '(1 2 3) '(2 4 6))
(merge '(4 5 6) '(1 2 3))


                
    
    
      