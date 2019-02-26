"Problem 1"


(define (better-equal? a b)
  (define (case3 a b)
    (cond 
          ((and (null? a) (null? b)) #t)
          ((better-equal? (car a) (car b)) (case3 (cdr a) (cdr b)))
          (else #f)))
  
  (cond ((null? b) #f)
        ((null? a) #f)
        ((and (list? a) (list? b)) (case3 (cdr a) (cdr b)))
        ((and (and (symbol? a) (symbol? b)) (eq? a b)) #t)
        ((and (and (number? a) (number? b)) (= a b)) #t)
        (else #f)))

;(better-equal '(a b c (1 2) 3.0) '(a a b c (1 2) 3))

(better-equal? '(a b c (1 2) 3.0) '( b a b c (1 2) 3.0))
(better-equal? '(a b c (1 2) 3.0) '())

"Problem 2"
(define (remove-all x l)
    (cond ((null? l) l)
          ((equal? (car l) x) (remove-all x (cdr l)))
          (else (cons (car l) (remove-all x (cdr l))))))

(remove-all 1 '(2 3 4 1))
(remove-all 'a '(a b a c ))
;(b c)
(remove-all 'z '(a b a c ))
;(a b a c)
(remove-all 'a '(a ( a b c) b a (( a )) c) )
;(( a b c) b (( a ))) c)

"Problem 3a"
(define (dot-prod x y)
  (if (null? x)
      0
      (+ (* (car x) (car y)) (dot-prod (cdr x) (cdr y))))) 

(dot-prod '(1 2 3) '(1 5 7))
           
"Problem 3b"
(define (sum-list lst)
  (define (help result lst)
    (if (null? lst)
        result
        (help (+ (car lst) result) (cdr lst))))
  (help 0 lst))

(define (dot-prod-with-map x y)
  (sum-list (map * x y)))

(dot-prod-with-map '(1 2 3) '(1 5 7))

"Problem 4"
( define (element-of-set? x set )
   ( cond (( null? set ) #f)
          (( equal? x ( car set )) #t)
          ( else ( element-of-set? x ( cdr set )))))
( define ( adjoin-set x set )
   ( if ( element-of-set? x set )
        set
        ( cons x set )))

( define ( intersection-set set1 set2 )
   ( cond (( or ( null? set1 ) ( null? set2 )) '())
          (( element-of-set? ( car set1 ) set2 )
           ( cons ( car set1 )
                  ( intersection-set ( cdr set1 ) set2 )))
          ( else ( intersection-set ( cdr set1 ) set2 ))))

(element-of-set? 1 '(1 5 3 6))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

(union-set '(1 2 3) '(1 5 6 3))

"Problem 5"
(define (has-duplicates? lst)
  (define (help c restlst)
  (cond ((null? restlst) #f)
        ((member c restlst) #t)
        (else (help (car restlst) (cdr restlst)))))
  (help (car lst) (cdr lst)))

(has-duplicates? '(1 2 3 3))

"Problem 6"

(define (num-zeroes l)
  (cond ((null? l) 0)
        ((list? (car l)) (+ (num-zeroes (cdr l)) (num-zeroes (car l))))
        ((better-equal? (car l) 0) (+ 1 (num-zeroes (cdr l))))
        (else (num-zeroes (cdr l)))))
  
(num-zeroes '(1 0 2 3 4 0))
(num-zeroes '((a 0 c) 0.0 3 4 x y))
(num-zeroes '(1 2 3 (4 (5))))

"Problem 7"
(define (nested-reverse lst)
  (define (help lst res)
    (cond ((null? lst) res)
          ((list? (car lst)) (help (cdr lst) (cons (nested-reverse (car lst)) res)))
          ((help (cdr lst) (cons (car lst) res)))))

  (help lst '()))

( nested-reverse '(a b c ))
;(c b a )
( nested-reverse '(( a b c) 42 ( do re mi (1 2 3))))
;(((3 2 1) mi re do ) 42 (c b a ))
(nested-reverse '((a b c) (1 2 3)))

  
  
      
  


      
      