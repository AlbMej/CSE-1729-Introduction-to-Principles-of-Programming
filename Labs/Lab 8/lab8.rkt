"Problem 1a"
(define (merge la lb )
  (cond ((null? la ) lb )
        ((null? lb ) la )
        ((< (car la )(car lb ))
         (cons (car la )(merge (cdr la ) lb )))
        (else
         (cons (car lb )(merge la (cdr lb ))))))

(merge '(3 4 1) '(5 6 2))

(define (split lst)
  (cond ((= (length lst) 1) (list (cons (car lst) '()) '()))
        (else 
         (let ((mid (truncate (/ (length lst) 2))))
           (define (1sthalf lst i)
             (cond ((= i 0) '())
                   ((= i 1) (list (car lst)))
                   (else (cons (car lst) (1sthalf (cdr lst) (- i 1))))))
           (define (2ndhalf lst i)
             (cond ((= i 0) '())
                   ((= i 1) (cdr lst))
                   (else (2ndhalf (cdr lst) (- i 1)))))
    
           (list (1sthalf lst mid) (2ndhalf lst mid))))))


(split '(1 2 3 4 5 6))
(split '(3 4 5 6 1 2))
(split '())
(split '(42))
(split '(a () () ))

"Problem 1b"
(define (mergesort l)
  (cond ((null? l) '())
        ((null? (cdr l)) l)
        (else (merge (mergesort (car (split l))) (mergesort (cadr (split l)))))))


(mergesort '(3 4 6 2 1 5))
(mergesort '())
  

"Problem 2a"
(define (make-tree value left right )
  ;; produces a tree with value at the root , and
  ;; left and right as its subtrees .
  (list value left right ))
(define (value tree )
  (car tree ))
(define ( left tree )
  (cadr tree ))
(define ( right tree )
  (caddr tree ))

(define testtree ( make-tree 1
                             ( make-tree 3
                                         ( make-tree 7 '() '())
                                         ( make-tree 9 '() '()))
                             ( make-tree 5 '() '())))


(define (tree-node-count t)
  (if (null? t)
      0
      (+ 1 (tree-node-count (left t)) (tree-node-count (right t)))))

(tree-node-count testtree)

      

"Problem 2b"
(define (tree-node-sum t)
  (if (null? t)
      0
      (+ (value t) (tree-node-sum (left t)) (tree-node-sum (right t)))))

(tree-node-sum testtree)

"Problem 2c"
(define (tree-height t)
  (if (null? t)
      -1
      (+ 1 (max (tree-height (left t)) (tree-height (right t))))))

(tree-height testtree)

"Problem 2d"
(define (tree-map f t)
  (if (null? t)
      '()
      (make-tree (f (value t)) (tree-map f (left t)) (tree-map f (right t)))))


(tree-map (lambda(x)(* 3 x)) testtree)

