"Problem 1"
(define (make-tree value left right ) (list value left right ))
(define (value tree ) (car tree ))
(define (left tree ) (cadr tree ))
(define (right tree ) (caddr tree ))

(define example (list #\+ (list #\*
                                (list 4 '() '())
                                (list 5 '() '()))
                      (list #\+
                            (list #\/ (list 6 '() '()) '())
                            (list 7 '() '()))))

(define example2 (list #\+ (list #\*
                                (list 2 '() '())
                                (list 3 '() '()))
                      (list #\*
                            (list #\+
                                  (list 4 '() '())
                                  (list #\- (list 5 '() '()) '()))
                                  
                            (list #\/ (list 6 '() '()) '()))))

(define (nvalue T)
  (cond ((null? T) (value T))
        ((number? (value T)) (value T))
        ((eq? (value T) #\+) (+ (nvalue (left T)) (nvalue (right T)) ))
        ((eq? (value T) #\*) (* (nvalue (left T)) (nvalue (right T)) ))
        ((eq? (value T) #\/) (/ (nvalue (left T))) )
        ((eq? (value T) #\-) (* -1 (nvalue (left T)) ))))
        
(nvalue example)

"Problem 2a"
(define (prepare x )
  (cond ((number? x) (number->string x ))
        ((char? x) (string x ))))



(define (prefix T)
  (if (null? T) ""
      (string-append (prepare (value T))
                     (prefix (left T))
                     (prefix (right T)))))

(prefix example)

"Problem 2b"
(define (postfix T)
  (if (null? T) ""
      (string-append (postfix (left T))
                     (postfix (right T))
                     (prepare (value T)))))

(postfix example)

"Problem 2c"
(define (infix T)
  (cond 
    ((eq? (value T) #\/)
     (string-append "(1/" (infix (left T)) ")"))
    ((eq? (value T) #\-)
     (string-append "(-" (infix (left T)) ")")) 
    ((eq? (value T) #\+)
     (string-append "(" (infix (left T)) "+" (infix (right T)) ")"))
    ((eq? (value T) #\*)
     (string-append "(" (infix (left T)) "*" (infix (right T)) ")"))
    (else (prepare (value T)))))

(infix example)
(infix example2)
"Problem 3a"
( define testtree ( make-tree 1
                              ( make-tree 3
                                          ( make-tree 7 '() '())
                                          ( make-tree 9 '() '()))
                              ( make-tree 5 '() '())))
( define testtree2 ( make-tree 1
                              ( make-tree 9
                                          ( make-tree 7 '() '())
                                          ( make-tree 10 '() '()))
                              ( make-tree 7 '() '())))

(define (bst-element? item bs-tree)
  (cond ((null? bs-tree) #f)
        ((string=? item (value bs-tree)) #t)
        (else (or (bst-element? item (left bs-tree)) (bst-element? item (right bs-tree))) )))

;(bst-element? 60 testtree)

"Problem 3b"
(define (bst-insert item bs-tree) 
  (cond ((null? bs-tree) (make-tree item '() '()))
        ((eq? item (value bs-tree)) bs-tree)
        ((string<? item (value bs-tree)) (make-tree (value bs-tree)
                                    (bst-insert item (left bs-tree))
                                    (right bs-tree)))
        ((string>? item (value bs-tree)) (make-tree (value bs-tree)
                                    (left bs-tree)
                                    (bst-insert item (right bs-tree))))))

;(bst-insert 12 testtree)


"Problem 3c"
(define (bst-smallest bs-tree)
  (cond ((null? bs-tree) 'undefined)
        ((null? (left bs-tree)) (value bs-tree))
        (else (bst-smallest (left bs-tree)))))

(bst-smallest testtree)
 
"Problem 3d"
(define (bst-largest bs-tree)
  (cond ((null? bs-tree) 'undefined)
        ((null? (right bs-tree)) (value bs-tree))
        (else (bst-largest (right bs-tree)))))

(bst-largest testtree)

"Problem 3e"
(define (bst-equal? tree1 tree2)
  (cond ((and (null? tree1) (null? tree2)) #t)
        ((or (null? tree1) (null? tree2)) #f)
        (else (and (string=? (value tree1) (value tree2))
                   (bst-equal? (left tree1) (left tree2))
                   (bst-equal? (right tree1) (right tree2))))))
        
;(bst-equal? testtree (make-tree '() '() '()))

"Problem 3f"
(define (bst-subset? bst1 bst2)
  (if (null? bst1)
      #t
      (and (bst-element? (value bst1) bst2)
           (bst-subset? (left bst1) bst2)
           (bst-subset? (right bst1) bst2))))

;(bst-subset? testtree testtree2)
 
"Problem 3g"
(define (bst-set-equal? bst1 bst2)
  (and (bst-subset? bst1 bst2) (bst-subset? bst2 bst1)))

;(bst-set-equal? testtree testtree2)
;(bst-set-equal? (make-tree '() '() '()) testtree)
;(bst-set-equal? (make-tree '() '() '()) (make-tree '() '() '()))
;(bst-set-equal? testtree (make-tree '() '() '()))

"Problem 4a"
(define (bst-delete-min bst)
 (cond ((null? bst) bst)
        ((null? (left bst)) (right bst) )
        (else (make-tree (value bst) (bst-delete-min (left bst)) (right bst) ))))

(bst-delete-min testtree)

"Problem 4b"
(define (bst-delete-max bst)
 (cond ((null? bst) bst)
        ((null? (right bst)) (left bst) )
        (else (make-tree (value bst) (left bst) (bst-delete-max (right bst)) ))))

(bst-delete-max testtree)

"Problem 4c"
(define (bst-delete val bst)
  (cond ((null? bst) '())
        ((and (null? (left bst)) (null? (right bst)) (string=? val (value bst))) '()  )
        ((string>? val (value bst)) (make-tree (value bst) (left bst) (bst-delete val (right bst))))
        ((string<? val (value bst)) (make-tree (value bst) (bst-delete val (left bst)) (right bst)))
        ((string=? val (value bst)) (make-tree (bst-smallest (right bst)) (left bst) (bst-delete-min (right bst))))
        (else bst)))

;(bst-delete 9 testtree)


        
         



