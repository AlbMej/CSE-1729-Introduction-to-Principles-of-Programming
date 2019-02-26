; Quinn Vissak
; CSE 1729 Final Review Session
; December 7, 2017
; McCartney's Exam: Wednesday, December 13th, 2017 @ 6:00pm - 8:00pm CAST 212
; Johnson's Exam: Tuesday, December 12th, 2017 @ 1:00pm - 3:00pm PB 38

; Brandon Cheng will be covering some content about trees/heaps in a
; separate document.

; Mutable Data
; Objects, Streams, and Vectors

"""
Implement a stack data structure with the following methods:
(a) push - takes a parameter and puts it at the top of the stack.
(b) pop - removes the top-most element and returns it.
Return 'undefined on an empty stack.
(c) peek - returns the top element (without removing it).
(d) invert - reverses the stack order (top becomes bottom, etc.).
(e) add - takes the 2 elements at the top of the stack, adds them,
and pushes the result back onto the stack.
(f) mult - takes the 2 elements at the top of the stack, multiplies them,
and pushes the result back onto the stack.
(g) sub - subtracts the top element of the stack from the 2nd most
top element of the stack.
Note: add, mult, sub should return the single element of a stack
if the stack size is 1. Return 'undefined if the stack size is 0.
"""

"Testing stack data structure"

"""
A Priority Queue is like a Queue, however elements are sorted
(descending order) by urgency. Each element is represented as a
pair of two integers. The first integer is the priority and the
second integer is the value. Implement a Priority Queue data structure
with the following methods:
(a) empty? - return true if the PQ is empty.
(b) size - returns the size of the queue.
(c) element? - which takes a priority and return true if there is
some pair in the queue with that priority, false otherwise.
(d) get-min - returns the minimum value of the queue. If empty,
return 'undefined.
(e) delete-min - deletes the element with the minimum value from the queue.
Should not return.
(f) delete - deletes an element x from the queue.
(g) insert - inserts an element x into the queue so that the queue
maintains its sorted order. Duplicate priorities are permitted.
(h) order - prints the priority queue in memory.
"""

"Testing priority queue data structure"

"""
Implement a function k-power-stream that takes a parameter k
and produces a stream of integers all raised to the kth power.
"""
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define empty-stream? null?)

(define (ints-from a)
  (cons-stream a (ints-from (+ a 1))))

(define (enumerate-integer-stream)
  (ints-from 0))

; helpful for testing
(define (str-to-list str n)
  (cond ((or (empty-stream? str)(= n 0)) '())
        (else
         (cons (stream-car str)
               (str-to-list (stream-cdr str)
                            (- n 1))))))

"Testing k-power-stream"

"""
Implement a funtion add-pairs that takes a stream and returns
a stream with adjacent pairs (non-repeating) added together.
i.e '(1 2 3 4 5 6 ...) => '(3 7 11)
"""

"Testing add-pairs"

"""
Implement a function stride-pair that takes a stream and a value k.
It creates a stream of pairs that contains elements with k distance
between them from the original stream.
i.e (stride-pair (ints-from 1) 4) => '((1 . 5) (2 . 6) (3. 7) (4. 8) ...) 
"""

"Testing stride-pair"

; Note: vectors will not be on McCartney's exam
; However, they *might* be a subpart of a question on Johnson's exam
; Disclaimer: TAs have NOT seen the exam!
"""
Implement a stack data structure with vectors having the following methods:
(a) empty? - returns true if the stack is empty, false otherwise.
(b) push - adds an element to the top of the stack. No return value.
(c) pop - remove an element from the top of the stack, return element.
(d) peek - return top value on top of the stack. Do not remove.
(e) size - return size of the stack.
Note: position 0 in the vector is the *bottom* of your stack.
"""

"Testing vector"

(define (new-account initial-balance password)
  (let ((balance initial-balance) (interestrate 0.01)
                                  (pw password))
    (define (deposit f password)
      (cond ((equal? pw password)
             (begin (set! balance (+ balance f)) balance))))
    (define (withdraw f password)
      (cond ((equal? pw password)
             (begin (set! balance (- balance f)) balance))))
    (define (bal-inq password)
      (cond ((equal? pw password)
             balance)
            (else "wrong password hitta")))
    (define (accrue)
      (begin (set! balance
                   (+ balance (* balance 1 interestrate)))
             balance))
    (define (setrate r)
      (set! interestrate r))
    (define (change-password old new)
      (cond ((equal? old pw)
             (set! pw new))))
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) bal-inq)
            ((eq? method 'accrue) accrue)
            ((eq? method 'setrate) setrate)
            ((eq? method 'change-pw) change-password)))))


(define my-check (new-account 100 "pp"))
((my-check 'balance-inquire) "pp")


(define (make-queue)
  (let ((head '())
        (tail '()))
    (define (value n) (car n))
    (define (next n) (cdr n))
    (define (empty?) (null? head))
    (define (front) (value head))
    (define (enqueue x)
      
      (let ((new-node (cons x '())))
        (begin
          (if (empty?)
              (set! head new-node)
              (set-cdr! tail new-node))
          (set! tail new-node)))) ;(display (value head)) (display " | ") (display (next head)) (newline))))
    (define (dequeue)
      (let ((return (value head)))
        (if (eq? head tail)
            (begin (set! head '())
                   (set! tail '())
                   return)
            (begin (set! head (next head))
                   return))))
    (define (dispatcher method)
      (cond ((eq? method 'empty) empty?)
            ((eq? method 'enqueue) enqueue)
            ((eq? method 'dequeue) dequeue)
            ((eq? method 'front) front)))
    dispatcher))

(define q (make-queue))
 ((q 'enqueue) 5)
 ((q 'enqueue) 6)
 ((q 'enqueue) 7)
 ((q 'dequeue))

 ((q 'enqueue) 8)
 ((q 'dequeue))

 ((q 'dequeue))

 ((q 'dequeue))
 ((q 'empty))


(define (bubble-up L)
    (if (null? (cdr L))   
        L    
        (if (< (car L) (cadr L))   
            (cons (car L) (bubble-up (cdr L)))   
            (cons (cadr L) (bubble-up (cons (car L) (cddr L)))))))

(bubble-up '(5 10 9 8 7))

(define (bubble-sort-aux N L)    
    (cond ((= N 1) (bubble-up L))   
          (else (bubble-sort-aux (- N 1) (bubble-up L)))))

(define (bubbleH L) 
    (bubble-sort-aux (length L) L))

(bubbleH '(5 10 9 8 7))


=