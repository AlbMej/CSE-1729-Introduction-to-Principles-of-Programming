"Problem 1"
(define (num-occurs sym lst)
  (cond ((null? lst) 0)
        ((eq? sym (car lst)) (+ 1 (num-occurs sym (cdr lst))))
        (else (num-occurs sym (cdr lst)))))

(num-occurs 'uh-huh '( thats the way uh-huh uh-huh i like it uh-huh uh-huh ))

(num-occurs 'a '(a b c ( not (c b a ))))

"Problem 2"
(define (remove-all x l)
  (cond ((null? l) l)
        ((equal? (car l) x) (remove-all x (cdr l)))
        (else (cons (car l) (remove-all x (cdr l))))))
      
(define (freq-list symlst)
  (define (getsym result symlst)
    (cond ((null? symlst) result)
          (else (getsym (cons (cons (car symlst) (num-occurs (car symlst) symlst)) result)
                        (remove-all (car symlst) symlst)))))
  (getsym '() symlst))
      

(freq-list
 '(thats thats way like thats mother))


(freq-list
 '(thats the way uh-huh uh-huh i like it uh-huh uh-huh
         thats the way uh-huh uh-huh i like it uh-huh uh-huh
         thats the way uh-huh uh-huh i like it uh-huh uh-huh
         thats the way uh-huh uh-huh i like it uh-huh uh-huh ))

;; Output should be --> (( it . 4)( like . 4)( i . 4)( uh-huh . 16)( way . 4)( the . 4)( thats . 4))

"Problem 3a"
(define (create-heap vw-pair left right)
  (list vw-pair left right))

"Problem 3b"
(define (h-min H) (car H))

"Problem 3c"
(define (left H) (cadr H))

"Problem 3d"
(define (right H) (caddr H))

"Problem 3e"
(define (value vw-pair) (car vw-pair))
(define (weight vw-pair) (cdr vw-pair))

(define (insert vw-pair heap)
  (cond ((null? heap) (create-heap vw-pair '() '()))
        ((< (weight vw-pair) (weight (h-min heap))) (create-heap vw-pair
                     (right heap)
                     (insert (h-min heap) (left heap))))
        (else (create-heap (h-min heap)
                     (right heap)
                     (insert vw-pair (left heap))))))
            
            


(define testheap
  (create-heap 2 (create-heap 3 '() '()) (create-heap 4 '() '())))

testheap
;(insert '(thats . 3) testheap)
;(insert 0 (insert 1 testheap))

"Problem 3f"
(define (insert-list-of-pairs vw-pair-list heap)
  (if (null? vw-pair-list)
      heap
      (insert-list-of-pairs (cdr vw-pair-list) (insert (car vw-pair-list) heap))))

"Problem 3g"
(define (combine-heaps H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((< (weight (h-min H1)) (weight (h-min H2)))
         (create-heap (h-min H1)
                      H2
                      (combine-heaps (left H1) (right H1))))
        (else
         (create-heap (h-min H2)
                      H1
                      (combine-heaps (left H2) (right H2))))))

(define (remove-min H)
  (if (null? H)
      '()
      (combine-heaps (left H) (right H))))

"Problem 4"
(define (get-in-order heap)
  (if (null? heap)
      '()
      (cons (h-min heap) (get-in-order (remove-min heap)))))

;(get-in-order (quote ((surfin-bird . 1) ((your . 2) ((boat . 6) () ()) ((row . 3) () ())) ((doo . 4) ((pa . 7) () ()) ((da . 5) () ())))))

(define (heapsort pair-list )
   (get-in-order (insert-list-of-pairs pair-list '())))

"Problem 5"
;( freq-list '( row row your boat boat boat row your boat boat boat ))
;(( boat . 6) ( your . 2) ( row . 3))
;( heapsort ( freq-list '( row row your boat boat boat row your boat boat boat )))
;(( your . 2) ( row . 3) ( boat . 6))

